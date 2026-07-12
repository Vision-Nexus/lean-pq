#include <lean/lean.h>
#include <libpq-fe.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/*
LibPQ documentation:
https://www.postgresql.org/docs/current/libpq.html

https://gist.github.com/ydewit/7ab62be1bd0fea5bd53b48d23914dd6b#4-scalar-values-in-lean-s-ffi
*/


/* Production builds must not emit raw connection/result addresses for every SQL operation.
 * A developer may opt in explicitly with -DLEAN_PQ_DEBUG=1. */
#ifndef LEAN_PQ_DEBUG
#define LEAN_PQ_DEBUG 0
#endif

#define LEAN_PQ_CONNECTION_FAILED_INIT 100

// [Database Connection Control Functions](https://www.postgresql.org/docs/current/libpq-connect.html)

struct connection {
  // The libpq connection handle.
  PGconn *pg_conn;
};

typedef struct connection Connection;

static lean_external_class *pq_connection_external_class = NULL;

static void pq_connection_finalizer(void *h) {
  Connection *connection = (Connection *)h;
#if LEAN_PQ_DEBUG
  fprintf(stderr, "pq_connection_finalizer %p\n", connection->pg_conn);
#endif
  // Closes the connection to the server. Also frees memory used by the PGconn
  // object.
  PQfinish(connection->pg_conn);
  free(connection);
}

static void pq_connection_foreach(void *mod, b_lean_obj_arg fn) {}

lean_obj_res pq_connection_wrap_handle(Connection *hconn) {
  return lean_alloc_external(pq_connection_external_class, hconn);
}

static Connection *pq_connection_get_handle(lean_object *conn) {
  return (Connection *)lean_get_external_data(conn);
}

static void initialize_pq_connection_external_class() {
  if (pq_connection_external_class == NULL) {
    pq_connection_external_class = lean_register_external_class(
        pq_connection_finalizer, pq_connection_foreach);
  }
}

// Error management

static lean_object* pq_connection_error(const uint32_t code) {
  lean_object* code_obj = lean_box_uint32(code);
  lean_object* connect_err = lean_alloc_ctor(0, 1, 0); // connectionError constructor
  lean_ctor_set(connect_err, 0, code_obj);
  return connect_err;
}

static lean_object* pq_other_error(const char* msg) {
  lean_object* msg_obj = lean_mk_string(msg);
  lean_object* other_err = lean_alloc_ctor(1, 1, 0); // otherError constructor
  lean_ctor_set(other_err, 0, msg_obj);
  return other_err;
}

// PQconnectdbParams - Makes a new connection to the database server using parameter arrays
// Documentation: https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTDBPARAMS
LEAN_EXPORT lean_obj_res lean_pq_connect_db_params(b_lean_obj_arg keywords, b_lean_obj_arg values, b_lean_obj_arg expand_dbname) {
  // Initialize the external class for connections
  initialize_pq_connection_external_class();
  size_t size = lean_array_size(keywords);
  const char **keywords_cstr = (const char **)malloc(size * sizeof(const char *));
  const char **values_cstr = (const char **)malloc(size * sizeof(const char *));
  for (size_t i = 0; i < size; i++) {
    keywords_cstr[i] = lean_string_cstr(lean_array_uget(keywords, i));
  }
  for (size_t i = 0; i < size; i++) {
    values_cstr[i] = lean_string_cstr(lean_array_uget(values, i));
  }
  int expand_dbname_int = lean_unbox(expand_dbname);
  PGconn *pg_conn = PQconnectdbParams(keywords_cstr, values_cstr, expand_dbname_int); // Create the libpq handle
  free(keywords_cstr);
  free(values_cstr);
  ConnStatusType status = PQstatus(pg_conn);
  // If the connection is not successful, return an error
  if (status != CONNECTION_OK)
    return lean_io_result_mk_error(pq_connection_error((uint32_t)status));
  Connection *connection = (Connection *)malloc(sizeof *connection); // Allocate our wrapper
  if (!connection)
    return lean_io_result_mk_error(pq_other_error("Memory allocation for connection failed"));
  // Initialize all fields to safe defaults
  connection->pg_conn = pg_conn;
#if LEAN_PQ_DEBUG
  fprintf(stderr, "Connection %p\n", pg_conn);
#endif
  return lean_io_result_mk_ok(pq_connection_wrap_handle(connection));
}

// PQconnectdb - Makes a new connection to the database server using a connection string
// Documentation: https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTDB
LEAN_EXPORT lean_obj_res lean_pq_connect_db(b_lean_obj_arg conninfo) {
  // Initialize the external class for connections
  initialize_pq_connection_external_class();
  const char *conninfo_cstr = lean_string_cstr(conninfo); // Convert Lean string to C string
  PGconn *pg_conn = PQconnectdb(conninfo_cstr); // Create the libpq handle
  ConnStatusType status = PQstatus(pg_conn);
  // If the connection is not successful, return an error
  if (status != CONNECTION_OK)
    return lean_io_result_mk_error(pq_connection_error((uint32_t)status));
  Connection *connection = (Connection *)malloc(sizeof *connection); // Allocate our wrapper
  if (!connection)
    return lean_io_result_mk_error(pq_other_error("Memory allocation for connection failed"));
  // Initialize all fields to safe defaults
  connection->pg_conn = pg_conn;
#if LEAN_PQ_DEBUG
  fprintf(stderr, "Connection %p\n", pg_conn);
#endif
  return lean_io_result_mk_ok(pq_connection_wrap_handle(connection));
}

// PQreset - Resets the communication channel with the server
// Documentation: https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQRESET
LEAN_EXPORT lean_obj_res lean_pq_reset(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  PQreset(connection->pg_conn);
  return lean_io_result_mk_ok(lean_box(0));
}

// [Connection Status Functions](https://www.postgresql.org/docs/current/libpq-status.html)

// Phase 0a: Macro for connection string getter functions
// 8 functions share the same 4-line body pattern
#define LEAN_PQ_CONN_STRING_GETTER(lean_name, pq_func) \
  LEAN_EXPORT lean_obj_res lean_name(b_lean_obj_arg conn) { \
    Connection *connection = pq_connection_get_handle(conn); \
    const char *val = pq_func(connection->pg_conn); \
    return lean_io_result_mk_ok(lean_mk_string(val)); \
  }

LEAN_PQ_CONN_STRING_GETTER(lean_pq_db, PQdb)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_user, PQuser)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_pass, PQpass)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_host, PQhost)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_host_addr, PQhostaddr)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_port, PQport)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_tty, PQtty)
LEAN_PQ_CONN_STRING_GETTER(lean_pq_options, PQoptions)

// PQstatus - Returns the status of the connection
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSTATUS
LEAN_EXPORT lean_obj_res lean_pq_status(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  ConnStatusType status = PQstatus(connection->pg_conn);
  lean_object * status_obj = lean_box_uint32((uint32_t)status);
  return lean_io_result_mk_ok(status_obj);
}

// PQtransactionStatus - Returns the current in-transaction status of the server
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQTRANSACTIONSTATUS
LEAN_EXPORT lean_obj_res lean_pq_transaction_status(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  PGTransactionStatusType transaction_status = PQtransactionStatus(connection->pg_conn);
  lean_object * transaction_status_obj = lean_box_uint32((uint32_t)transaction_status);
  return lean_io_result_mk_ok(transaction_status_obj);
}

// PQparameterStatus - Looks up a current parameter setting of the server
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQPARAMETERSTATUS
LEAN_EXPORT lean_obj_res lean_pq_parameter_status(b_lean_obj_arg conn, b_lean_obj_arg param_name) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * param_name_cstr = lean_string_cstr(param_name);
  const char * param_value = PQparameterStatus(connection->pg_conn, param_name_cstr);
  return lean_io_result_mk_ok(lean_mk_string(param_value));
}

// PQprotocolVersion - Returns the version of the protocol used to communicate with the server
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQPROTOCOLVERSION
LEAN_EXPORT lean_obj_res lean_pq_protocol_version(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  int protocol_version = PQprotocolVersion(connection->pg_conn);
  lean_object * protocol_version_boxed = lean_box_uint32((uint32_t)protocol_version);
  return lean_io_result_mk_ok(protocol_version_boxed);
}

// PQserverVersion - Returns the server version number
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSERVERVERSION
LEAN_EXPORT lean_obj_res lean_pq_server_version(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  int server_version = PQserverVersion(connection->pg_conn);
  lean_object * server_version_boxed = lean_box_uint32((uint32_t)server_version);
  return lean_io_result_mk_ok(server_version_boxed);
}

// PQerrorMessage - Returns the error message most recently generated by an operation on the connection
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQERRORMESSAGE
LEAN_EXPORT lean_obj_res lean_pq_error_message(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * error_message = PQerrorMessage(connection->pg_conn);
  return lean_io_result_mk_ok(lean_mk_string(error_message));
}

// PQsocket - Returns the file descriptor number of the connection socket to the server
// Documentation: https://www.postgresql.org/docs/current/libpq-status.html#LIBPQ-PQSOCKET
LEAN_EXPORT lean_obj_res lean_pq_socket(b_lean_obj_arg conn) {
  Connection *connection = pq_connection_get_handle(conn);
  int socket = PQsocket(connection->pg_conn);
  lean_object * socket_boxed = lean_box_uint32((uint32_t)socket);
  return lean_io_result_mk_ok(socket_boxed);
}

// [Command Execution Functions](https://www.postgresql.org/docs/current/libpq-exec.html)

struct result {
  /**
   * A class for results
   */
  PGresult *pg_result;
};

typedef struct result Result;

static lean_external_class *pq_result_external_class = NULL;

static void pq_result_finalizer(void *h) {
  Result *result = (Result *)h;
#if LEAN_PQ_DEBUG
  fprintf(stderr, "pq_result_finalizer %p\n", result->pg_result);
#endif
  PQclear(result->pg_result);
  free(result);
}

static void pq_result_foreach(void *mod, b_lean_obj_arg fn) {}

lean_obj_res pq_result_wrap_handle(Result *hresult) {
  return lean_alloc_external(pq_result_external_class, hresult);
}

static Result *pq_result_get_handle(lean_object *hresult) {
  return (Result *)lean_get_external_data(hresult);
}

static void initialize_pq_result_external_class() {
  if (pq_result_external_class == NULL) {
    pq_result_external_class = lean_register_external_class(
        pq_result_finalizer, pq_result_foreach);
  }
}

// Phase 0a: Helper to wrap a PGresult into a Lean external object
// Avoids repeating the malloc/check/wrap pattern in every exec function
static lean_obj_res wrap_pg_result(PGresult *pg_result) {
  initialize_pq_result_external_class();
  Result *result = (Result *)malloc(sizeof *result);
  if (!result)
    return lean_io_result_mk_error(pq_other_error("Memory allocation for result failed"));
  result->pg_result = pg_result;
#if LEAN_PQ_DEBUG
  fprintf(stderr, "Result %p\n", pg_result);
#endif
  return lean_io_result_mk_ok(pq_result_wrap_handle(result));
}

// PQexec - Submits a command to the server and waits for the result
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXEC
LEAN_EXPORT lean_obj_res lean_pq_exec(b_lean_obj_arg conn, b_lean_obj_arg cmd) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * cmd_cstr = lean_string_cstr(cmd);
  PGresult * pg_result = PQexec(connection->pg_conn, cmd_cstr);
  return wrap_pg_result(pg_result);
}

// PQexecParams - Submits a command to the server and waits for the result, with the ability to pass parameters separately
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPARAMS
//
// Phase 1a: Fixed array marshalling — uses lean_array_uget iteration instead of broken lean_unbox casts
LEAN_EXPORT lean_obj_res lean_pq_exec_params(
  b_lean_obj_arg conn,
  b_lean_obj_arg cmd,
  b_lean_obj_arg nParams,
  b_lean_obj_arg paramTypes,
  b_lean_obj_arg paramValues,
  b_lean_obj_arg paramLengths,
  b_lean_obj_arg paramFormats,
  b_lean_obj_arg resultFormat) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * cmd_cstr = lean_string_cstr(cmd);
  int nParams_int = lean_unbox(nParams);
  int resultFormat_int = lean_unbox(resultFormat);

  // Marshal paramTypes: Array UInt32 -> Oid*
  Oid *paramTypes_c = NULL;
  if (nParams_int > 0) {
    paramTypes_c = (Oid *)malloc(nParams_int * sizeof(Oid));
    if (!paramTypes_c)
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramTypes failed"));
    for (int i = 0; i < nParams_int; i++) {
      paramTypes_c[i] = (Oid)lean_unbox_uint32(lean_array_uget(paramTypes, (size_t)i));
    }
  }

  // Marshal paramValues: Array String -> const char**
  const char **paramValues_c = NULL;
  if (nParams_int > 0) {
    paramValues_c = (const char **)malloc(nParams_int * sizeof(const char *));
    if (!paramValues_c) {
      free(paramTypes_c);
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramValues failed"));
    }
    for (int i = 0; i < nParams_int; i++) {
      paramValues_c[i] = lean_string_cstr(lean_array_uget(paramValues, (size_t)i));
    }
  }

  // Marshal paramLengths: Array Int -> int*
  int *paramLengths_c = NULL;
  if (nParams_int > 0) {
    paramLengths_c = (int *)malloc(nParams_int * sizeof(int));
    if (!paramLengths_c) {
      free(paramTypes_c); free(paramValues_c);
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramLengths failed"));
    }
    for (int i = 0; i < nParams_int; i++) {
      paramLengths_c[i] = (int)lean_unbox(lean_array_uget(paramLengths, (size_t)i));
    }
  }

  // Marshal paramFormats: Array Int -> int*
  int *paramFormats_c = NULL;
  if (nParams_int > 0) {
    paramFormats_c = (int *)malloc(nParams_int * sizeof(int));
    if (!paramFormats_c) {
      free(paramTypes_c); free(paramValues_c); free(paramLengths_c);
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramFormats failed"));
    }
    for (int i = 0; i < nParams_int; i++) {
      paramFormats_c[i] = (int)lean_unbox(lean_array_uget(paramFormats, (size_t)i));
    }
  }

  PGresult * pg_result = PQexecParams(connection->pg_conn, cmd_cstr, nParams_int,
    paramTypes_c, paramValues_c, paramLengths_c, paramFormats_c, resultFormat_int);

  free(paramTypes_c);
  free(paramValues_c);
  free(paramLengths_c);
  free(paramFormats_c);

  return wrap_pg_result(pg_result);
}

// PQprepare - Submits a request to create a prepared statement with the given parameters
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQPREPARE
//
// Phase 1a: Fixed array marshalling for paramTypes
LEAN_EXPORT lean_obj_res lean_pq_prepare(b_lean_obj_arg conn, b_lean_obj_arg stmtName, b_lean_obj_arg query, b_lean_obj_arg nParams, b_lean_obj_arg paramTypes) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * stmtName_cstr = lean_string_cstr(stmtName);
  const char * query_cstr = lean_string_cstr(query);
  const int nParams_int = lean_unbox(nParams);

  // Marshal paramTypes: Array UInt32 -> Oid*
  Oid *paramTypes_c = NULL;
  if (nParams_int > 0) {
    paramTypes_c = (Oid *)malloc(nParams_int * sizeof(Oid));
    if (!paramTypes_c)
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramTypes failed"));
    for (int i = 0; i < nParams_int; i++) {
      paramTypes_c[i] = (Oid)lean_unbox_uint32(lean_array_uget(paramTypes, (size_t)i));
    }
  }

  PGresult * pg_result = PQprepare(connection->pg_conn, stmtName_cstr, query_cstr, nParams_int, paramTypes_c);
  free(paramTypes_c);

  return wrap_pg_result(pg_result);
}

// PQexecPrepared - Sends a request to execute a prepared statement with given parameters
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQEXECPREPARED
//
// Phase 1a: Fixed array marshalling for paramValues, paramLengths, paramFormats
LEAN_EXPORT lean_obj_res lean_pq_exec_prepared(b_lean_obj_arg conn, b_lean_obj_arg stmtName, b_lean_obj_arg nParams, b_lean_obj_arg paramValues, b_lean_obj_arg paramLengths, b_lean_obj_arg paramFormats, b_lean_obj_arg resultFormat) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * stmtName_cstr = lean_string_cstr(stmtName);
  const int nParams_int = lean_unbox(nParams);
  int resultFormat_int = lean_unbox(resultFormat);

  // Marshal paramValues: Array String -> const char**
  const char **paramValues_c = NULL;
  if (nParams_int > 0) {
    paramValues_c = (const char **)malloc(nParams_int * sizeof(const char *));
    if (!paramValues_c)
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramValues failed"));
    for (int i = 0; i < nParams_int; i++) {
      paramValues_c[i] = lean_string_cstr(lean_array_uget(paramValues, (size_t)i));
    }
  }

  // Marshal paramLengths: Array Int -> int*
  int *paramLengths_c = NULL;
  if (nParams_int > 0) {
    paramLengths_c = (int *)malloc(nParams_int * sizeof(int));
    if (!paramLengths_c) {
      free(paramValues_c);
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramLengths failed"));
    }
    for (int i = 0; i < nParams_int; i++) {
      paramLengths_c[i] = (int)lean_unbox(lean_array_uget(paramLengths, (size_t)i));
    }
  }

  // Marshal paramFormats: Array Int -> int*
  int *paramFormats_c = NULL;
  if (nParams_int > 0) {
    paramFormats_c = (int *)malloc(nParams_int * sizeof(int));
    if (!paramFormats_c) {
      free(paramValues_c); free(paramLengths_c);
      return lean_io_result_mk_error(pq_other_error("Memory allocation for paramFormats failed"));
    }
    for (int i = 0; i < nParams_int; i++) {
      paramFormats_c[i] = (int)lean_unbox(lean_array_uget(paramFormats, (size_t)i));
    }
  }

  PGresult * pg_result = PQexecPrepared(connection->pg_conn, stmtName_cstr, nParams_int,
    paramValues_c, paramLengths_c, paramFormats_c, resultFormat_int);

  free(paramValues_c);
  free(paramLengths_c);
  free(paramFormats_c);

  return wrap_pg_result(pg_result);
}

// [Result Functions](https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-EXEC-SELECT-INFO)

// Result Status Functions
// PQresultStatus - Returns the result status of the command
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTSTATUS
LEAN_EXPORT lean_obj_res lean_pq_result_status(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  ExecStatusType status = PQresultStatus(result->pg_result);
  lean_object * status_boxed = lean_box_uint32((uint32_t)status);
  return lean_io_result_mk_ok(status_boxed);
}

// PQresStatus - Converts the enumerated type returned by PQresultStatus into a string constant
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESSTATUS
//
// Phase 0b: Fixed — was identical to lean_pq_result_status. Now correctly calls PQresStatus.
LEAN_EXPORT lean_obj_res lean_pq_res_status(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  ExecStatusType status = PQresultStatus(result->pg_result);
  const char *status_str = PQresStatus(status);
  return lean_io_result_mk_ok(lean_mk_string(status_str));
}

// PQresultErrorMessage - Returns the error message associated with the command
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTERRORMESSAGE
LEAN_EXPORT lean_obj_res lean_pq_result_error_message(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  const char * error_message = PQresultErrorMessage(result->pg_result);
  if (error_message == NULL)
    return lean_io_result_mk_error(
        pq_other_error("PQresultErrorMessage returned NULL"));
  return lean_io_result_mk_ok(lean_mk_string(error_message));
}

// PQresultErrorField - Returns an individual field of an error report
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQRESULTERRORFIELD
LEAN_EXPORT lean_obj_res lean_pq_result_error_field(b_lean_obj_arg res, b_lean_obj_arg fieldcode) {
  Result *result = pq_result_get_handle(res);
  int fieldcode_int = lean_unbox(fieldcode);
  const char * error_field = PQresultErrorField(result->pg_result, fieldcode_int);
  /* libpq returns NULL when this diagnostic field is absent.  Lean's string constructor requires a
   * valid C string; mapping the absent field to the empty string lets checkedResult report
   * SQLSTATE=unavailable while still rejecting the unsuccessful PGresult. */
  return lean_io_result_mk_ok(lean_mk_string(error_field == NULL ? "" : error_field));
}

// Retrieving Query Result Information
// PQntuples - Returns the number of rows (tuples) in the query result
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQNTUPLES
LEAN_EXPORT lean_obj_res lean_pq_ntuples(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  int ntuples = PQntuples(result->pg_result);
  return lean_io_result_mk_ok(lean_box(ntuples));
}

// PQnfields - Returns the number of columns (fields) in each row of the query result
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQNFIELDS
LEAN_EXPORT lean_obj_res lean_pq_nfields(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  int nfields = PQnfields(result->pg_result);
  return lean_io_result_mk_ok(lean_box(nfields));
}

// PQfname - Returns the column name associated with the given column number
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFNAME
LEAN_EXPORT lean_obj_res lean_pq_fname(b_lean_obj_arg res, b_lean_obj_arg field_num) {
  Result *result = pq_result_get_handle(res);
  int field_num_int = lean_unbox(field_num);
  const char * fname = PQfname(result->pg_result, field_num_int);
  return lean_io_result_mk_ok(lean_mk_string(fname));
}

// PQfnumber - Returns the column number associated with the given column name
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFNUMBER
LEAN_EXPORT lean_obj_res lean_pq_fnumber(b_lean_obj_arg res, b_lean_obj_arg field_name) {
  Result *result = pq_result_get_handle(res);
  const char * field_name_cstr = lean_string_cstr(field_name);
  int fnumber = PQfnumber(result->pg_result, field_name_cstr);
  return lean_io_result_mk_ok(lean_box(fnumber));
}

// Phase 0a: Macro for result-with-field-num getters that return int
#define LEAN_PQ_RESULT_INT_FIELD(lean_name, pq_func) \
  LEAN_EXPORT lean_obj_res lean_name(b_lean_obj_arg res, b_lean_obj_arg field_num) { \
    Result *result = pq_result_get_handle(res); \
    int field_num_int = lean_unbox(field_num); \
    return lean_io_result_mk_ok(lean_box(pq_func(result->pg_result, field_num_int))); \
  }

// PQftablecol - Returns the column number (within its table) of the column making up the specified query result column
LEAN_PQ_RESULT_INT_FIELD(lean_pq_ftablecol, PQftablecol)

// PQfformat - Returns the format code indicating the format of the given column
LEAN_PQ_RESULT_INT_FIELD(lean_pq_fformat, PQfformat)

// PQfsize - Returns the size in bytes of the type associated with the given column number
LEAN_PQ_RESULT_INT_FIELD(lean_pq_fsize, PQfsize)

// PQfmod - Returns the type modifier of the type associated with the given column number
LEAN_PQ_RESULT_INT_FIELD(lean_pq_fmod, PQfmod)

// PQftable - Returns the OID of the table from which the given column was fetched
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFTABLE
// Phase 1b: Changed from lean_box_usize to lean_box_uint32 — OIDs are 32-bit
LEAN_EXPORT lean_obj_res lean_pq_ftable(b_lean_obj_arg res, b_lean_obj_arg field_num) {
  Result *result = pq_result_get_handle(res);
  int field_num_int = lean_unbox(field_num);
  Oid ftable = PQftable(result->pg_result, field_num_int);
  lean_object * ftable_obj = lean_box_uint32((uint32_t)ftable);
  return lean_io_result_mk_ok(ftable_obj);
}

// PQftype - Returns the data type associated with the given column number
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQFTYPE
// Phase 1b: Changed from lean_box_usize to lean_box_uint32 — OIDs are 32-bit
LEAN_EXPORT lean_obj_res lean_pq_ftype(b_lean_obj_arg res, b_lean_obj_arg field_num) {
  Result *result = pq_result_get_handle(res);
  int field_num_int = lean_unbox(field_num);
  Oid ftype = PQftype(result->pg_result, field_num_int);
  lean_object * ftype_obj = lean_box_uint32((uint32_t)ftype);
  return lean_io_result_mk_ok(ftype_obj);
}

// PQbinaryTuples - Returns 1 if the PGresult contains binary tuple data, 0 if it contains text data
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQBINARYTUPLES
LEAN_EXPORT lean_obj_res lean_pq_binary_tuples(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  int binary_tuples = PQbinaryTuples(result->pg_result);
  return lean_io_result_mk_ok(lean_box(binary_tuples));
}

// Retrieving Other Result Information
// PQcmdStatus - Returns the command status tag from the last SQL command executed
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQCMDSTATUS
LEAN_EXPORT lean_obj_res lean_pq_cmd_status(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  const char * cmd_status = PQcmdStatus(result->pg_result);
  return lean_io_result_mk_ok(lean_mk_string(cmd_status));
}

// PQcmdTuples - Returns the number of rows affected by the SQL command
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQCMDTUPLES
LEAN_EXPORT lean_obj_res lean_pq_cmd_tuples(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  const char * cmd_tuples = PQcmdTuples(result->pg_result);
  return lean_io_result_mk_ok(lean_mk_string(cmd_tuples));
}

// PQoidValue - Returns the OID of the inserted row, if the SQL command was an INSERT that inserted exactly one row into a table that has OIDs
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQOIDVALUE
// Phase 1b: Changed from lean_box_usize to lean_box_uint32 — OIDs are 32-bit
LEAN_EXPORT lean_obj_res lean_pq_oid_value(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  Oid oid_value = PQoidValue(result->pg_result);
  lean_object * oid_value_obj = lean_box_uint32((uint32_t)oid_value);
  return lean_io_result_mk_ok(oid_value_obj);
}

// PQoidStatus - Returns a string with the OID of the inserted row, if the SQL command was an INSERT that inserted exactly one row into a table that has OIDs
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQOIDSTATUS
LEAN_EXPORT lean_obj_res lean_pq_oid_status(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  const char * oid_status = PQoidStatus(result->pg_result);
  return lean_io_result_mk_ok(lean_mk_string(oid_status));
}

// Retrieving Row Values
// PQgetvalue - Returns a single field value of one row of a PGresult
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQGETVALUE
LEAN_EXPORT lean_obj_res lean_pq_getvalue(b_lean_obj_arg res, b_lean_obj_arg row_num, b_lean_obj_arg field_num) {
  Result *result = pq_result_get_handle(res);
  int row_num_int = lean_unbox(row_num);
  int field_num_int = lean_unbox(field_num);
  const char * value = PQgetvalue(result->pg_result, row_num_int, field_num_int);
  return lean_io_result_mk_ok(lean_mk_string(value));
}

// PQgetisnull - Tests a field for a null value
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQGETISNULL
LEAN_EXPORT lean_obj_res lean_pq_getisnull(b_lean_obj_arg res, b_lean_obj_arg row_num, b_lean_obj_arg field_num) {
  Result *result = pq_result_get_handle(res);
  int row_num_int = lean_unbox(row_num);
  int field_num_int = lean_unbox(field_num);
  int is_null = PQgetisnull(result->pg_result, row_num_int, field_num_int);
  return lean_io_result_mk_ok(lean_box(is_null));
}

// PQgetlength - Returns the actual length of a field value in bytes
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQGETLENGTH
LEAN_EXPORT lean_obj_res lean_pq_getlength(b_lean_obj_arg res, b_lean_obj_arg row_num, b_lean_obj_arg field_num) {
  Result *result = pq_result_get_handle(res);
  int row_num_int = lean_unbox(row_num);
  int field_num_int = lean_unbox(field_num);
  int length = PQgetlength(result->pg_result, row_num_int, field_num_int);
  return lean_io_result_mk_ok(lean_box(length));
}

// PQnparams - Returns the number of parameters of a prepared statement
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQNPARAMS
LEAN_EXPORT lean_obj_res lean_pq_nparams(b_lean_obj_arg res) {
  Result *result = pq_result_get_handle(res);
  int nparams = PQnparams(result->pg_result);
  return lean_io_result_mk_ok(lean_box(nparams));
}

// PQparamtype - Returns the data type of the indicated statement parameter
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQPARAMTYPE
// Phase 1b: Changed from lean_box_usize to lean_box_uint32 — OIDs are 32-bit
LEAN_EXPORT lean_obj_res lean_pq_paramtype(b_lean_obj_arg res, b_lean_obj_arg param_num) {
  Result *result = pq_result_get_handle(res);
  int param_num_int = lean_unbox(param_num);
  Oid param_type = PQparamtype(result->pg_result, param_num_int);
  lean_object * param_type_obj = lean_box_uint32((uint32_t)param_type);
  return lean_io_result_mk_ok(param_type_obj);
}

// Escaping Strings for Inclusion in SQL Commands
// PQescapeLiteral - Escapes a string for use as an SQL string literal on the given connection
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQESCAPELITERAL
LEAN_EXPORT lean_obj_res lean_pq_escape_literal(b_lean_obj_arg conn, b_lean_obj_arg str) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * str_cstr = lean_string_cstr(str);
  size_t str_length = strlen(str_cstr);
  char * escaped = PQescapeLiteral(connection->pg_conn, str_cstr, str_length);
  if (escaped == NULL) {
    return lean_io_result_mk_error(pq_other_error("PQescapeLiteral failed"));
  }
  lean_object * result = lean_mk_string(escaped);
  PQfreemem(escaped);
  return lean_io_result_mk_ok(result);
}

// PQescapeIdentifier - Escapes a string for use as an SQL identifier on the given connection
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQESCAPEIDENTIFIER
LEAN_EXPORT lean_obj_res lean_pq_escape_identifier(b_lean_obj_arg conn, b_lean_obj_arg str) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * str_cstr = lean_string_cstr(str);
  size_t str_length = strlen(str_cstr);
  char * escaped = PQescapeIdentifier(connection->pg_conn, str_cstr, str_length);
  if (escaped == NULL) {
    return lean_io_result_mk_error(pq_other_error("PQescapeIdentifier failed"));
  }
  lean_object * result = lean_mk_string(escaped);
  PQfreemem(escaped);
  return lean_io_result_mk_ok(result);
}

// PQescapeStringConn - Escapes string literals, much like PQescapeLiteral, but the caller is responsible for providing an appropriately sized buffer
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQESCAPESTRINGCONN
LEAN_EXPORT lean_obj_res lean_pq_escape_string_conn(b_lean_obj_arg conn, b_lean_obj_arg from) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * from_cstr = lean_string_cstr(from);
  size_t from_length = strlen(from_cstr);
  // Allocate buffer for escaped string (worst case: 2x original length + 1)
  size_t to_length = 2 * from_length + 1;
  char * to = (char *)malloc(to_length);
  if (to == NULL) {
    return lean_io_result_mk_error(pq_other_error("Memory allocation failed"));
  }
  int error = 0;
  size_t escaped_length = PQescapeStringConn(connection->pg_conn, to, from_cstr, from_length, &error);
  (void)escaped_length;
  if (error != 0) {
    free(to);
    return lean_io_result_mk_error(pq_other_error("PQescapeStringConn failed"));
  }
  lean_object * result = lean_mk_string(to);
  free(to);
  return lean_io_result_mk_ok(result);
}

// PQescapeByteaConn - Escapes binary data for use within an SQL command with the type bytea
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQESCAPEBYTEACONN
LEAN_EXPORT lean_obj_res lean_pq_escape_bytea_conn(b_lean_obj_arg conn, b_lean_obj_arg from) {
  Connection *connection = pq_connection_get_handle(conn);
  const char * from_cstr = lean_string_cstr(from);
  size_t from_length = strlen(from_cstr);
  size_t to_length = 0;
  unsigned char * escaped = PQescapeByteaConn(connection->pg_conn, (const unsigned char *)from_cstr, from_length, &to_length);
  if (escaped == NULL) {
    return lean_io_result_mk_error(pq_other_error("PQescapeByteaConn failed"));
  }
  lean_object * result = lean_mk_string((const char *)escaped);
  PQfreemem(escaped);
  return lean_io_result_mk_ok(result);
}

// PQunescapeBytea - Converts a string representation of binary data into binary data — the reverse of PQescapeBytea
// Documentation: https://www.postgresql.org/docs/current/libpq-exec.html#LIBPQ-PQUNESCAPEBYTEA
LEAN_EXPORT lean_obj_res lean_pq_unescape_bytea(b_lean_obj_arg str) {
  const char * str_cstr = lean_string_cstr(str);
  size_t to_length = 0;
  unsigned char * unescaped = PQunescapeBytea((const unsigned char *)str_cstr, &to_length);
  if (unescaped == NULL) {
    return lean_io_result_mk_error(pq_other_error("PQunescapeBytea failed"));
  }
  lean_object * result = lean_mk_string((const char *)unescaped);
  PQfreemem(unescaped);
  return lean_io_result_mk_ok(result);
}
