import LeanPq.Monad

/-!
Exercise the compiled Lean caller and libpq together: checking only the C marshaller misses
owned Lean declarations paired with borrowed C arguments. Inputs are created at runtime so
permanent literal objects cannot hide reference leaks. No RSS/allocator assumptions are needed.
-/
namespace Tests.Ownership
open LeanPq Extern

@[extern "lean_pq_test_object_refcount"]
private opaque refcount {α : Type} (value : @& α) : EIO LeanPq.Error Nat

@[extern "lean_pq_test_element_refcount"]
private opaque elementRefcount {α : Type} (values : @& Array α) (index : USize) :
    EIO LeanPq.Error Nat

private def check (label : String) (condition : Bool) : EIO LeanPq.Error Unit :=
  unless condition do throw (.otherError label)

private def sameRefs (label : String) (before after : List Nat) : EIO LeanPq.Error Unit :=
  check s!"{label}: references changed from {before} to {after}" (before == after)

private def queryRefs (conn : @& Handle) (sql : @& String) (types : @& Array Oid)
    (values : @& Array String) (lengths formats : @& Array Int) : EIO LeanPq.Error (List Nat) := do
  pure [← refcount conn, ← refcount sql, ← refcount types, ← refcount values,
    ← elementRefcount values 0, ← elementRefcount values 1,
    ← refcount lengths, ← refcount formats]

def connection (conninfo : String) : EIO LeanPq.Error Unit := do
  let stamp ← IO.monoNanosNow
  let info := s!"{conninfo} application_name=ownership-{stamp}"
  let before ← refcount info
  let conn ← PqConnectDb info
  sameRefs "connection string" [before] [← refcount info]
  let command := s!"SELECT 1 AS payload /* {stamp} */"
  let parameter := ("server_version" ++ toString stamp).take 14 |>.toString
  let escaped := s!"ownership'{stamp}"
  let before := [← refcount conn, ← refcount command, ← refcount parameter, ← refcount escaped]
  for _ in [:16] do
    check "connection status" ((← PqStatus conn) == .connectionOk)
    let _ ← PqDb conn
    let _ ← PqUser conn
    let _ ← PqPass conn
    let _ ← PqHost conn
    let _ ← PqHostAddr conn
    let _ ← PqPort conn
    let _ ← PqTty conn
    let _ ← PqOptions conn
    let _ ← PqTransactionStatus conn
    let _ ← PqParameterStatus conn parameter
    let _ ← PqProtocolVersion conn
    let _ ← PqServerVersion conn
    let _ ← PqErrorMessage conn
    let _ ← PqSocket conn
    let result ← PqExec conn command
    check "simple query" ((← PqResultStatus result) == .tuplesOk)
    let _ ← PqEscapeLiteral conn escaped
    let _ ← PqEscapeIdentifier conn escaped
    let _ ← PqEscapeStringConn conn escaped
    let encoded ← PqEscapeByteaConn conn escaped
    let encodedBefore ← refcount encoded
    let _ ← PqUnescapeBytea encoded
    sameRefs "unescape input" [encodedBefore] [← refcount encoded]
  PqReset conn
  sameRefs "connection, query and escape inputs" before
    [← refcount conn, ← refcount command, ← refcount parameter, ← refcount escaped]
  -- Invalid connection options fail before opening a network connection.
  let invalid := s!"{info} invalid_ownership_option=value"
  let invalidBefore ← refcount invalid
  let failed ← try
    let _ ← PqConnectDb invalid
    pure false
  catch _ => pure true
  check "invalid connection must fail" failed
  sameRefs "failed connection input" [invalidBefore] [← refcount invalid]

def connectionParameters (conninfo : String) : EIO LeanPq.Error Unit := do
  let stamp ← IO.monoNanosNow
  for reject in [false, true] do
    let keyword := if reject then s!"invalid_ownership_{stamp}"
      else ("dbname" ++ toString stamp).take 6 |>.toString
    let keywords := #[keyword]
    let values := #[s!"{conninfo} application_name=ownership-{stamp}"]
    let before := [← refcount keywords, ← elementRefcount keywords 0,
      ← refcount values, ← elementRefcount values 0]
    for _ in [:8] do
      let connected ← try
        let conn ← PqConnectDbParams keywords values 1
        check "connection parameter status" ((← PqStatus conn) == .connectionOk)
        pure true
      catch _ => pure false
      check "connection parameter outcome" (connected != reject)
    sameRefs s!"connection parameter arrays (reject={reject})" before
      [← refcount keywords, ← elementRefcount keywords 0,
        ← refcount values, ← elementRefcount values 0]
  let mismatch ← try
    let _ ← PqConnectDbParams #["dbname"] #[]
    pure false
  catch _ => pure true
  check "mismatched connection arrays" mismatch

def parameters (conninfo : String) : EIO LeanPq.Error Unit := do
  let conn ← PqConnectDb conninfo
  let stamp ← IO.monoNanosNow
  let values := #[s!"{stamp}:" ++ String.ofList (List.replicate 65536 'x'), s!"second:{stamp}"]
  let types := values.map fun _ => (25 : Oid)
  let lengths := values.map fun value => (value.utf8ByteSize : Int)
  let formats := values.map fun _ => (0 : Int)
  for reject in [false, true] do
    let sql := s!"SELECT $1::text, $2::text{if reject then ", 1/0" else ""} /* {stamp} */"
    let before ← queryRefs conn sql types values lengths formats
    for _ in [:32] do
      let result ← PqExecParams conn sql 2 types values lengths formats 0
      if reject then
        check "division by zero SQLSTATE" ((← PqResultErrorField result 67) == "22012")
      else
        check "parameter query status" ((← PqResultStatus result) == .tuplesOk)
        check "first parameter round trip" ((← PqGetvalue result 0 0) == values[0]!)
        check "second parameter round trip" ((← PqGetvalue result 0 1) == values[1]!)
    sameRefs s!"execParams (reject={reject})" before
      (← queryRefs conn sql types values lengths formats)

def prepared (conninfo : String) : EIO LeanPq.Error Unit := do
  let conn ← PqConnectDb conninfo
  let stamp ← IO.monoNanosNow
  let values := #[s!"{stamp}:" ++ String.ofList (List.replicate 65536 'x'), s!"second:{stamp}"]
  let types := values.map fun _ => (25 : Oid)
  let lengths := values.map fun value => (value.utf8ByteSize : Int)
  let formats := values.map fun _ => (0 : Int)
  let name := s!"ownership_{stamp}"
  let sql := s!"SELECT $1::text, $2::text /* {stamp} */"
  let before := [← refcount conn, ← refcount name, ← refcount sql, ← refcount types]
  let result ← PqPrepare conn name sql 2 types
  check "prepare status" ((← PqResultStatus result) == .commandOk)
  sameRefs "prepare inputs" before
    [← refcount conn, ← refcount name, ← refcount sql, ← refcount types]
  let fieldName := ("text" ++ toString stamp).take 4 |>.toString
  let fieldRefs ← refcount fieldName
  let result ← PqExec conn (sql.replace "$1::text, $2::text" "'value'::text")
  let _ ← PqFnumber result fieldName
  sameRefs "field name input" [fieldRefs] [← refcount fieldName]
  for reject in [false, true] do
    let statement := if reject then s!"missing_{stamp}" else name
    let before ← queryRefs conn statement types values lengths formats
    for _ in [:32] do
      let result ← PqExecPrepared conn statement 2 values lengths formats 0
      if reject then
        check "missing prepared statement SQLSTATE" ((← PqResultErrorField result 67) == "26000")
      else
        check "prepared query status" ((← PqResultStatus result) == .tuplesOk)
        check "prepared first parameter" ((← PqGetvalue result 0 0) == values[0]!)
        check "prepared second parameter" ((← PqGetvalue result 0 1) == values[1]!)
    sameRefs s!"execPrepared (reject={reject})" before
      (← queryRefs conn statement types values lengths formats)

end Tests.Ownership
