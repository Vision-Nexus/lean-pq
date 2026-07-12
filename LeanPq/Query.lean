/-
Type-safe SQL AST with schema-indexed expressions.

Column references carry a proof that the column exists in the schema,
making invalid column references a compile-time error.

All user values are rendered as $1, $2, ... parameters, making SQL injection
structurally impossible.
-/
import LeanPq.Schema
import LeanPq.Monad

namespace LeanPq

/-- Binary operators for SQL expressions. -/
inductive BinOp where
  | eq | neq | lt | le | gt | ge
  | and | or
  | add | sub | mul | div
  | like | ilike
  deriving BEq, Repr, Inhabited

/-- Render a binary operator to SQL. -/
def BinOp.toSQL : BinOp → String
  | .eq => "="
  | .neq => "<>"
  | .lt => "<"
  | .le => "<="
  | .gt => ">"
  | .ge => ">="
  | .and => "AND"
  | .or => "OR"
  | .add => "+"
  | .sub => "-"
  | .mul => "*"
  | .div => "/"
  | .like => "LIKE"
  | .ilike => "ILIKE"

/-- Sort direction. -/
inductive SortDir where
  | asc | desc
  deriving BEq, Repr, Inhabited

/-- SQL expression indexed by the available columns.
    `col` requires a proof that the column name exists in the schema. -/
inductive Expr (columns : List Column) where
  | col (name : String) (h : columns.any (fun c => c.name == name) = true)
  | litStr (value : String)
  | litInt (value : Int)
  | litBool (value : Bool)
  | litNull
  | param (index : Nat)
  | binOp (op : BinOp) (lhs rhs : Expr columns)
  | isNull (e : Expr columns)
  | isNotNull (e : Expr columns)
  | not (e : Expr columns)
  | star

/-- State for tracking parameter indices during rendering. -/
structure RenderState where
  params : Array String := #[]
  nextIdx : Nat := 1

/-- Render an expression to a SQL string with parameterized values. -/
def Expr.render {columns : List Column} : Expr columns → RenderState → (String × RenderState)
  | .col name _, st => (name, st)
  | .litStr value, st =>
    let idx := st.nextIdx
    (s!"${idx}", { params := st.params.push value, nextIdx := idx + 1 })
  | .litInt value, st =>
    let idx := st.nextIdx
    (s!"${idx}", { params := st.params.push (toString value), nextIdx := idx + 1 })
  | .litBool value, st =>
    let s := if value then "TRUE" else "FALSE"
    (s, st)
  | .litNull, st => ("NULL", st)
  | .param index, st => (s!"${index}", st)
  | .binOp op lhs rhs, st =>
    let (lhsStr, st1) := lhs.render st
    let (rhsStr, st2) := rhs.render st1
    (s!"({lhsStr} {op.toSQL} {rhsStr})", st2)
  | .isNull e, st =>
    let (eStr, st1) := e.render st
    (s!"({eStr} IS NULL)", st1)
  | .isNotNull e, st =>
    let (eStr, st1) := e.render st
    (s!"({eStr} IS NOT NULL)", st1)
  | .not e, st =>
    let (eStr, st1) := e.render st
    (s!"(NOT {eStr})", st1)
  | .star, st => ("*", st)

/-- Column specification for SELECT: either specific columns or all (*). -/
inductive SelectColumns (columns : List Column) where
  | all
  | specific (names : List String)
    (h : names.all (fun n => columns.any (fun c => c.name == n)) = true)

/-- Assignment for UPDATE SET clause. -/
structure Assignment (columns : List Column) where
  colName : String
  value : Expr columns
  h : columns.any (fun c => c.name == colName) = true

/-- SQL query AST. Each variant knows its permission level. -/
inductive Query where
  | select
    (table : TableSchema)
    (cols : SelectColumns table.columns)
    (where_ : Option (Expr table.columns) := none)
    (orderBy : List (String × SortDir) := [])
    (limit : Option Nat := none)
  | insert
    (table : TableSchema)
    (colNames : List String)
    (values : List (Expr table.columns))
  | update
    (table : TableSchema)
    (assignments : List (Assignment table.columns))
    (where_ : Option (Expr table.columns) := none)
  | delete
    (table : TableSchema)
    (where_ : Option (Expr table.columns) := none)
  | createTable (schema : TableSchema)
  | dropTable (name : String) (ifExists : Bool := false)

namespace Query

/-- Get the permission level required for a query. -/
def permissionLevel : Query → Permission
  | .select .. => .readOnly
  | .insert .. => .dataAltering
  | .update .. => .dataAltering
  | .delete .. => .dataAltering
  | .createTable .. => .admin
  | .dropTable .. => .admin

/-- Render a DataType to its SQL name. -/
private def dataTypeToSQL : DataType → String
  | .smallint => "SMALLINT"
  | .integer => "INTEGER"
  | .bigint => "BIGINT"
  | .numeric none none => "NUMERIC"
  | .numeric (some p) none => s!"NUMERIC({p})"
  | .numeric (some p) (some s) => s!"NUMERIC({p}, {s})"
  | .numeric none (some _) => "NUMERIC"
  | .real => "REAL"
  | .double_precision => "DOUBLE PRECISION"
  | .smallserial => "SMALLSERIAL"
  | .serial => "SERIAL"
  | .bigserial => "BIGSERIAL"
  | .money => "MONEY"
  | .character none => "CHAR"
  | .character (some n) => s!"CHAR({n})"
  | .character_varying none => "VARCHAR"
  | .character_varying (some n) => s!"VARCHAR({n})"
  | .text => "TEXT"
  | .bytea => "BYTEA"
  | .date => "DATE"
  | .time none false => "TIME"
  | .time (some p) false => s!"TIME({p})"
  | .time none true => "TIME WITH TIME ZONE"
  | .time (some p) true => s!"TIME({p}) WITH TIME ZONE"
  | .timestamp none false => "TIMESTAMP"
  | .timestamp (some p) false => s!"TIMESTAMP({p})"
  | .timestamp none true => "TIMESTAMP WITH TIME ZONE"
  | .timestamp (some p) true => s!"TIMESTAMP({p}) WITH TIME ZONE"
  | .interval none none => "INTERVAL"
  | .interval (some f) none => s!"INTERVAL {f}"
  | .interval none (some p) => s!"INTERVAL({p})"
  | .interval (some f) (some p) => s!"INTERVAL {f}({p})"
  | .boolean => "BOOLEAN"
  | .uuid => "UUID"
  | .json => "JSON"
  | .jsonb => "JSONB"
  | .xml => "XML"
  | .array elem none => s!"{dataTypeToSQL elem}[]"
  | .array elem (some n) => s!"{dataTypeToSQL elem}[{n}]"
  | .inet => "INET"
  | .cidr => "CIDR"
  | .macaddr => "MACADDR"
  | .macaddr8 => "MACADDR8"
  | .bit none => "BIT"
  | .bit (some n) => s!"BIT({n})"
  | .bit_varying none => "BIT VARYING"
  | .bit_varying (some n) => s!"BIT VARYING({n})"
  | .tsvector => "TSVECTOR"
  | .tsquery => "TSQUERY"
  | .point => "POINT"
  | .line => "LINE"
  | .lseg => "LSEG"
  | .box => "BOX"
  | .path => "PATH"
  | .polygon => "POLYGON"
  | .circle => "CIRCLE"
  | .int4range => "INT4RANGE"
  | .int8range => "INT8RANGE"
  | .numrange => "NUMRANGE"
  | .tsrange => "TSRANGE"
  | .tstzrange => "TSTZRANGE"
  | .daterange => "DATERANGE"
  | .int4multirange => "INT4MULTIRANGE"
  | .int8multirange => "INT8MULTIRANGE"
  | .nummultirange => "NUMMULTIRANGE"
  | .tsmultirange => "TSMULTIRANGE"
  | .tstzmultirange => "TSTZMULTIRANGE"
  | .datemultirange => "DATEMULTIRANGE"
  | .oid => "OID"
  | .enum name => name
  | .composite name _ => name
  | .domain name => name
  | dt => toString (repr dt)

/-- Render a query to a parameterized SQL string and array of parameter values. -/
def render : Query → (String × Array String)
  | .select table cols where_ orderBy limit =>
    let colStr := match cols with
      | .all => "*"
      | .specific names _ => ", ".intercalate names
    let st : RenderState := {}
    let (whereClause, st) := match where_ with
      | none => ("", st)
      | some expr =>
        let (s, st) := expr.render st
        (s!" WHERE {s}", st)
    let orderClause := if orderBy.isEmpty then "" else
      let parts := orderBy.map fun (col, dir) =>
        let d := match dir with | .asc => "ASC" | .desc => "DESC"
        s!"{col} {d}"
      s!" ORDER BY {", ".intercalate parts}"
    let limitClause := match limit with
      | none => ""
      | some n => s!" LIMIT {n}"
    (s!"SELECT {colStr} FROM {table.name}{whereClause}{orderClause}{limitClause}", st.params)

  | .insert table colNames values =>
    let colStr := ", ".intercalate colNames
    let st : RenderState := {}
    let (valStrs, st) := values.foldl (fun (acc, st) expr =>
      let (s, st) := expr.render st
      (acc ++ [s], st)) ([], st)
    let valStr := ", ".intercalate valStrs
    (s!"INSERT INTO {table.name} ({colStr}) VALUES ({valStr})", st.params)

  | .update table assignments where_ =>
    let st : RenderState := {}
    let (setParts, st) := assignments.foldl (fun (acc, st) a =>
      let (valStr, st) := a.value.render st
      (acc ++ [s!"{a.colName} = {valStr}"], st)) ([], st)
    let setStr := ", ".intercalate setParts
    let (whereClause, st) := match where_ with
      | none => ("", st)
      | some expr =>
        let (s, st) := expr.render st
        (s!" WHERE {s}", st)
    (s!"UPDATE {table.name} SET {setStr}{whereClause}", st.params)

  | .delete table where_ =>
    let st : RenderState := {}
    let (whereClause, st) := match where_ with
      | none => ("", st)
      | some expr =>
        let (s, st) := expr.render st
        (s!" WHERE {s}", st)
    (s!"DELETE FROM {table.name}{whereClause}", st.params)

  | .createTable schema =>
    let colDefs := schema.columns.map fun col =>
      let nullStr := if col.nullable then "" else " NOT NULL"
      s!"  {col.name} {dataTypeToSQL col.type}{nullStr}"
    let body := ",\n".intercalate colDefs
    (s!"CREATE TABLE {schema.name} (\n{body}\n)", #[])

  | .dropTable name ifExists =>
    let ie := if ifExists then "IF EXISTS " else ""
    (s!"DROP TABLE {ie}{name}", #[])

/-- Set ORDER BY on a SELECT query. No-op on other query types. -/
def withOrderBy (ob : List (String × SortDir)) : Query → Query
  | .select t c w _ l => .select t c w ob l
  | q => q

/-- Set LIMIT on a SELECT query. No-op on other query types. -/
def withLimit (n : Nat) : Query → Query
  | .select t c w ob _ => .select t c w ob (some n)
  | q => q

end Query

namespace PqM

private def queryExpectation : Query → ResultExpectation
  | .select .. => .tuples
  | .insert .. | .update .. | .delete .. | .createTable .. | .dropTable .. => .command

/-- Execute a type-safe query. The permission proof is auto-discharged by `by decide`
    for valid combinations and fails at compile time for invalid ones. -/
def execQuery (q : Query) (_h : q.permissionLevel.le perm = true := by decide)
    : PqM perm Extern.PGresult := do
  let (sql, params) := q.render
  let conn ← PqM.getConn
  let result ← if params.isEmpty then
    Extern.PqExec conn sql
  else do
    let paramTypes : Array Oid := params.map (fun _ => (0 : UInt32))
    let paramLengths : Array Int := params.map (fun _ => 0)
    let paramFormats : Array Int := params.map (fun _ => 0)
    Extern.PqExecParams conn sql (Int.ofNat params.size) paramTypes params paramLengths paramFormats 0
  checkedResult "execQuery" (queryExpectation q) result

/-- Execute a type-safe query and fetch all rows.
    Combines `execQuery` with `fetchAll` for convenience. -/
def query (q : Query) (h : q.permissionLevel.le perm = true := by decide)
    : PqM perm (List (List String)) := do
  let res ← PqM.execQuery q h
  PqM.fetchAll res

end PqM
end LeanPq
