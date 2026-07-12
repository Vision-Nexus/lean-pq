/-
Permission-tracking monad for PostgreSQL operations.

`PqM perm α` is a reader monad carrying a `Handle` that tracks the permission
level at the type level. Operations are gated by permission:
- `readOnly`:     SELECT queries
- `dataAltering`: INSERT, UPDATE, DELETE
- `admin`:        CREATE, DROP, ALTER, etc.

Lower-permission computations automatically lift into higher-permission contexts
via `MonadLift`, but the reverse is a compile-time error.
-/
import LeanPq.Error
import LeanPq.Extern

namespace LeanPq

/-- Permission levels for database operations, ordered from least to most privileged. -/
inductive Permission where
  | readOnly
  | dataAltering
  | admin
  deriving BEq, DecidableEq, Repr, Inhabited

namespace Permission

/-- Total order: readOnly ≤ dataAltering ≤ admin -/
def le : Permission → Permission → Bool
  | .readOnly, _ => true
  | .dataAltering, .dataAltering | .dataAltering, .admin => true
  | .admin, .admin => true
  | _, _ => false

instance : LE Permission where
  le p1 p2 := p1.le p2 = true

instance (p1 p2 : Permission) : Decidable (p1 ≤ p2) :=
  inferInstanceAs (Decidable (p1.le p2 = true))

end Permission

/-- Typeclass witnessing that permission `p1` is at most `p2`.
    Instances are defined for all 6 valid pairs so typeclass resolution works. -/
class PermLE (p1 p2 : Permission) where
  proof : p1.le p2 = true

instance : PermLE .readOnly .readOnly where proof := rfl
instance : PermLE .readOnly .dataAltering where proof := rfl
instance : PermLE .readOnly .admin where proof := rfl
instance : PermLE .dataAltering .dataAltering where proof := rfl
instance : PermLE .dataAltering .admin where proof := rfl
instance : PermLE .admin .admin where proof := rfl

/-- A reader-like monad carrying a database `Handle` with compile-time permission tracking. -/
structure PqM (perm : Permission) (α : Type) where
  run : Handle → EIO LeanPq.Error α

namespace PqM

/-- Successful `PGresult` shape required by a public execution API. -/
inductive ResultExpectation where
  | tuples
  | command
  | commandOrTuples
  deriving BEq, DecidableEq, Repr

instance : ToString ResultExpectation where
  toString
    | .tuples => "TUPLES_OK"
    | .command => "COMMAND_OK"
    | .commandOrTuples => "COMMAND_OK|TUPLES_OK"

private def ResultExpectation.accepts : ResultExpectation → Extern.ExecStatus → Bool
  | .tuples, .tuplesOk => true
  | .command, .commandOk => true
  | .commandOrTuples, .commandOk | .commandOrTuples, .tuplesOk => true
  | _, _ => false

/-- Reject an unsuccessful server result immediately. `PG_DIAG_SQLSTATE` is libpq field code `C`
(ASCII 67). Zero affected rows remain successful; cardinality belongs to the caller's CAS contract. -/
def checkedResult (operation : String) (expected : ResultExpectation)
    (result : Extern.PGresult) : EIO LeanPq.Error Extern.PGresult := do
  let actual ← Extern.PqResultStatus result
  if expected.accepts actual then pure result
  else
    let sqlStateRaw ← Extern.PqResultErrorField result 67
    let sqlState := if sqlStateRaw.isEmpty then "unavailable" else sqlStateRaw
    let detail ← Extern.PqResultErrorMessage result
    throw (.postgresError operation (toString expected) (toString actual) sqlState
      detail.trimAscii.toString)

@[inline] def pure' (a : α) : PqM perm α :=
  ⟨fun _ => Pure.pure a⟩

@[inline] def bind' (m : PqM perm α) (f : α → PqM perm β) : PqM perm β :=
  ⟨fun conn => do
    let a ← m.run conn
    (f a).run conn⟩

instance : Monad (PqM perm) where
  pure := PqM.pure'
  bind := PqM.bind'

instance : MonadLift (EIO LeanPq.Error) (PqM perm) where
  monadLift action := ⟨fun _ => action⟩

/-- Lift a readOnly computation into a dataAltering context. -/
instance : MonadLift (PqM .readOnly) (PqM .dataAltering) where
  monadLift m := ⟨fun conn => m.run conn⟩

/-- Lift a readOnly computation into an admin context. -/
instance : MonadLift (PqM .readOnly) (PqM .admin) where
  monadLift m := ⟨fun conn => m.run conn⟩

/-- Lift a dataAltering computation into an admin context. -/
instance : MonadLift (PqM .dataAltering) (PqM .admin) where
  monadLift m := ⟨fun conn => m.run conn⟩

instance : MonadExceptOf LeanPq.Error (PqM perm) where
  throw e := ⟨fun _ => throw e⟩
  tryCatch m handler := ⟨fun conn =>
    tryCatch (m.run conn) (fun e => (handler e).run conn)⟩

/-- Lift an IO action into PqM, converting IO errors to LeanPq.Error. -/
def liftIO (action : IO α) : PqM perm α :=
  ⟨fun _ => action.toEIO (fun e => LeanPq.Error.otherError (toString e))⟩

/-- Get the underlying connection handle. -/
def getConn : PqM perm Handle :=
  ⟨fun conn => Pure.pure conn⟩

-- Permission-gated query execution

/-- Execute a read-only SQL query (SELECT). -/
def execSelect (sql : String) : PqM .readOnly Extern.PGresult :=
  ⟨fun conn => do checkedResult "execSelect" .tuples (← Extern.PqExec conn sql)⟩

/-- Execute a data-modifying SQL query (INSERT, UPDATE, DELETE). -/
def execModify (sql : String) : PqM .dataAltering Extern.PGresult :=
  ⟨fun conn => do checkedResult "execModify" .commandOrTuples (← Extern.PqExec conn sql)⟩

/-- Execute an administrative SQL query (CREATE, DROP, ALTER, etc.). -/
def execAdmin (sql : String) : PqM .admin Extern.PGresult :=
  ⟨fun conn => do checkedResult "execAdmin" .commandOrTuples (← Extern.PqExec conn sql)⟩

/-- Execute a parameterized query (read-only). -/
def execParamsSelect (sql : String) (paramTypes : Array Oid) (paramValues : Array String)
    : PqM .readOnly Extern.PGresult :=
  ⟨fun conn => do
    checkedResult "execParamsSelect" .tuples <| ← Extern.PqExecParams conn sql
      (Int.ofNat paramValues.size) paramTypes paramValues (paramValues.map (fun _ => 0))
      (paramValues.map (fun _ => 0)) 0⟩

/-- Execute a parameterized query (data-modifying). -/
def execParamsModify (sql : String) (paramTypes : Array Oid) (paramValues : Array String)
    : PqM .dataAltering Extern.PGresult :=
  ⟨fun conn => do
    checkedResult "execParamsModify" .commandOrTuples <| ← Extern.PqExecParams conn sql
      (Int.ofNat paramValues.size) paramTypes paramValues (paramValues.map (fun _ => 0))
      (paramValues.map (fun _ => 0)) 0⟩

private def rollbackAndThrow (conn : Handle) (cause : LeanPq.Error) : EIO LeanPq.Error α := do
  try
    let _ ← checkedResult "ROLLBACK" .command (← Extern.PqExec conn "ROLLBACK")
    pure ()
  catch rollbackError =>
    throw (.otherError s!"transaction failed ({toString cause}); ROLLBACK also failed \
      ({toString rollbackError})")
  throw cause

/-- Run a computation inside a transaction. -/
def withTransaction (body : PqM perm α) : PqM perm α := ⟨fun conn => do
  try
    let _ ← checkedResult "BEGIN" .command (← Extern.PqExec conn "BEGIN")
    pure ()
  catch e => rollbackAndThrow conn e
  let value ← try body.run conn catch e => rollbackAndThrow conn e
  try
    let _ ← checkedResult "COMMIT" .command (← Extern.PqExec conn "COMMIT")
    pure value
  catch e => rollbackAndThrow conn e⟩

/-- Fetch all results from a PGresult as a list of rows (each row is a list of strings). -/
def fetchAll (result : Extern.PGresult) : PqM perm (List (List String)) := ⟨fun _ => do
  let nrows ← Extern.PqNtuples result
  let ncols ← Extern.PqNfields result
  let mut rows : List (List String) := []
  for row in [0:nrows.toNat] do
    let mut cols : List String := []
    for col in [0:ncols.toNat] do
      let value ← Extern.PqGetvalue result (Int.ofNat row) (Int.ofNat col)
      cols := cols ++ [value]
    rows := rows ++ [cols]
  Pure.pure rows⟩

/-- Connect and run a PqM computation. -/
def withConnection (conninfo : String) (body : PqM perm α) : EIO LeanPq.Error α := do
  let conn ← Extern.PqConnectDb conninfo
  body.run conn

/-- Connect and run a PqM computation, converting to IO. -/
def withConnectionIO (conninfo : String) (body : PqM perm α) : IO α := do
  let result ← (withConnection conninfo body).toIO (fun e => IO.Error.otherError 0 (toString e))
  Pure.pure result

end PqM
end LeanPq
