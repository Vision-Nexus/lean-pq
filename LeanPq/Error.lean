namespace LeanPq

inductive Error where
  | connectionError (code : UInt32)
  | otherError (msg: String)
  /-- PostgreSQL returned an unsuccessful `PGresult`. Keeping the operation, expected/actual status,
  and SQLSTATE as separate fields lets callers classify server failures without parsing prose. -/
  | postgresError (operation expectedStatus actualStatus sqlState detail : String)
  deriving BEq, DecidableEq, Repr, Inhabited


instance : ToString Error where
  toString := fun
  | .connectionError code => s!"Connection error: {code}."
  | .otherError msg => s!"Other error: {msg}."
  | .postgresError operation expected actual sqlState detail =>
    s!"PostgreSQL error during {operation}: status={actual}, expected={expected}, \
SQLSTATE={sqlState}; {detail}"

end LeanPq
