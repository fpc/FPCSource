{$mode objfpc}

{$ifdef BSD}
  {$linklib c}
  {$linklib pthread}
{$endif}

interface

{$ifdef LOAD_DYNAMICALLY}
uses 
  SysUtils, DynLibs;
{$endif}

{
  Automatically converted by H2Pas 0.99.16 from sqlite3.h
  The following command line parameters were used:
    -D
    -c
    sqlite3.h

  Manual corrections made by Luiz Américo - 2005
}

{$PACKRECORDS C}

const
{$IFDEF WINDOWS}
  Sqlite3Lib = 'sqlite3.dll';
{$else}
  Sqlite3Lib = 'libsqlite3.so';
{$endif}

  SQLITE_INTEGER = 1;   
  SQLITE_FLOAT = 2;   
{ #define SQLITE_TEXT  3  // See below  }
  SQLITE_BLOB = 4;   
  SQLITE_NULL = 5;   
  SQLITE_TEXT = 3;   
  SQLITE3_TEXT = 3;   
  SQLITE_UTF8 = 1;       
  SQLITE_UTF16LE = 2;       
  SQLITE_UTF16BE = 3;       
{ Use native byte order  }
  SQLITE_UTF16 = 4;       
{ sqlite3_create_function only  }
  SQLITE_ANY = 5;  
   
   //sqlite_exec return values
  SQLITE_OK = 0;   
  SQLITE_ERROR = 1;{ SQL error or missing database  }
  SQLITE_INTERNAL = 2;{ An internal logic error in SQLite  }
  SQLITE_PERM = 3;   { Access permission denied  }
  SQLITE_ABORT = 4; { Callback routine requested an abort  }
  SQLITE_BUSY = 5;  { The database file is locked  }
  SQLITE_LOCKED = 6;{ A table in the database is locked  }
  SQLITE_NOMEM = 7; { A malloc() failed  }
  SQLITE_READONLY = 8;{ Attempt to write a readonly database  }
  SQLITE_INTERRUPT = 9;{ Operation terminated by sqlite3_interrupt() }
  SQLITE_IOERR = 10;   { Some kind of disk I/O error occurred  }
  SQLITE_CORRUPT = 11;   { The database disk image is malformed  }
  SQLITE_NOTFOUND = 12;   { (Internal Only) Table or record not found  }
  SQLITE_FULL = 13;   { Insertion failed because database is full  }
  SQLITE_CANTOPEN = 14;   { Unable to open the database file  }
  SQLITE_PROTOCOL = 15;   { Database lock protocol error  }
  SQLITE_EMPTY = 16;   { Database is empty  }
  SQLITE_SCHEMA = 17;   { The database schema changed  }
  SQLITE_TOOBIG = 18;   { Too much data for one row of a table  }
  SQLITE_CONSTRAINT = 19;   { Abort due to contraint violation  }
  SQLITE_MISMATCH = 20;   { Data type mismatch  }
  SQLITE_MISUSE = 21;   { Library used incorrectly  }
  SQLITE_NOLFS = 22;   { Uses OS features not supported on host  }
  SQLITE_AUTH = 23;   { Authorization denied  }
  SQLITE_FORMAT = 24;   { Auxiliary database format error  }
  SQLITE_RANGE = 25;   { 2nd parameter to sqlite3_bind out of range  }
  SQLITE_NOTADB = 26;   { File opened that is not a database file  }
  SQLITE_ROW = 100;   { sqlite3_step() has another row ready  }
  SQLITE_DONE = 101;   { sqlite3_step() has finished executing  }

   SQLITE_COPY = 0;   
   SQLITE_CREATE_INDEX = 1;   
   SQLITE_CREATE_TABLE = 2;   
   SQLITE_CREATE_TEMP_INDEX = 3;   
   SQLITE_CREATE_TEMP_TABLE = 4;   
   SQLITE_CREATE_TEMP_TRIGGER = 5;   
   SQLITE_CREATE_TEMP_VIEW = 6;   
   SQLITE_CREATE_TRIGGER = 7;   
   SQLITE_CREATE_VIEW = 8;   
   SQLITE_DELETE = 9;   
   SQLITE_DROP_INDEX = 10;   
   SQLITE_DROP_TABLE = 11;   
   SQLITE_DROP_TEMP_INDEX = 12;   
   SQLITE_DROP_TEMP_TABLE = 13;   
   SQLITE_DROP_TEMP_TRIGGER = 14;   
   SQLITE_DROP_TEMP_VIEW = 15;   
   SQLITE_DROP_TRIGGER = 16;   
   SQLITE_DROP_VIEW = 17;   
   SQLITE_INSERT = 18;   
   SQLITE_PRAGMA = 19;   
   SQLITE_READ = 20;   
   SQLITE_SELECT = 21;   
   SQLITE_TRANSACTION = 22;   
   SQLITE_UPDATE = 23;   
   SQLITE_ATTACH = 24;   
   SQLITE_DETACH = 25;   
   SQLITE_ALTER_TABLE = 26;   
   SQLITE_REINDEX = 27;   

  SQLITE_DENY = 1;   
  SQLITE_IGNORE = 2;   

// Original from sqlite3.h: 
//#define SQLITE_STATIC      ((void(*)(void *))0)
//#define SQLITE_TRANSIENT   ((void(*)(void *))-1)
Const
  SQLITE_STATIC    =  0;
  SQLITE_TRANSIENT =  -1;

type
  sqlite_int64 = int64;
  sqlite_uint64 = qword;
  PPPChar = ^PPChar;
  Psqlite3  = Pointer;
  PPSqlite3 = ^PSqlite3;
  Psqlite3_context  = Pointer;
  Psqlite3_stmt  = Pointer;
  PPsqlite3_stmt = ^Psqlite3_stmt;
  Psqlite3_value  = Pointer;
  PPsqlite3_value  = ^Psqlite3_value;

//Callback function types
//Notice that most functions were named using as prefix the function name that uses them,
//rather than describing their functions  

  sqlite3_callback = function (_para1:pointer; _para2:longint; _para3:PPchar; _para4:PPchar):longint;cdecl;
  busy_handler_func = function (_para1:pointer; _para2:longint):longint;cdecl;
  sqlite3_set_authorizer_func = function (_para1:pointer; _para2:longint; _para3:Pchar; _para4:Pchar; _para5:Pchar; _para6:Pchar):longint;cdecl;
  sqlite3_trace_func = procedure (_para1:pointer; _para2:Pchar);cdecl;
  sqlite3_progress_handler_func = function (_para1:pointer):longint;cdecl;
  sqlite3_commit_hook_func = function (_para1:pointer):longint;cdecl;
  bind_destructor_func = procedure (_para1:pointer);cdecl;
  create_function_step_func = procedure (_para1:Psqlite3_context; _para2:longint; _para3:PPsqlite3_value);cdecl;
  create_function_func_func = procedure (_para1:Psqlite3_context; _para2:longint; _para3:PPsqlite3_value);cdecl;
  create_function_final_func = procedure (_para1:Psqlite3_context);cdecl;
  sqlite3_set_auxdata_func = procedure (_para1:pointer);cdecl;
  sqlite3_result_func = procedure (_para1:pointer);cdecl;
  sqlite3_create_collation_func = function (_para1:pointer; _para2:longint; _para3:pointer; _para4:longint; _para5:pointer):longint;cdecl;
  sqlite3_collation_needed_func = procedure (_para1:pointer; _para2:Psqlite3; eTextRep:longint; _para4:Pchar);cdecl;

{$ifndef win32}
var
  //This is not working under windows. Any clues?
  sqlite3_temp_directory : Pchar;cvar; external;
{$endif}

{$IFNDEF LOAD_DYNAMICALLY}

function sqlite3_close(_para1:Psqlite3):longint;cdecl; external Sqlite3Lib name 'sqlite3_close';
function sqlite3_exec(_para1:Psqlite3; sql:Pchar; _para3:sqlite3_callback; _para4:pointer; errmsg:PPchar):longint;cdecl; external Sqlite3Lib name 'sqlite3_exec';
function sqlite3_last_insert_rowid(_para1:Psqlite3):sqlite_int64;cdecl; external Sqlite3Lib name 'sqlite3_last_insert_rowid';
function sqlite3_changes(_para1:Psqlite3):longint;cdecl; external Sqlite3Lib name 'sqlite3_changes';
function sqlite3_total_changes(_para1:Psqlite3):longint;cdecl; external Sqlite3Lib name 'sqlite3_total_changes';
procedure sqlite3_interrupt(_para1:Psqlite3);cdecl; external Sqlite3Lib name 'sqlite3_interrupt';
function sqlite3_complete(sql:Pchar):longint;cdecl; external Sqlite3Lib name 'sqlite3_complete';
function sqlite3_complete16(sql:pointer):longint;cdecl; external Sqlite3Lib name 'sqlite3_complete16';
function sqlite3_busy_handler(_para1:Psqlite3; _para2:busy_handler_func; _para3:pointer):longint;cdecl; external Sqlite3Lib name 'sqlite3_busy_handler';
function sqlite3_busy_timeout(_para1:Psqlite3; ms:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_busy_timeout';
function sqlite3_get_table(_para1:Psqlite3; sql:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint; errmsg:PPchar):longint;cdecl; external Sqlite3Lib name 'sqlite3_get_table';
procedure sqlite3_free_table(result:PPchar);cdecl; external Sqlite3Lib name 'sqlite3_free_table';
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
// function sqlite3_mprintf(_para1:Pchar; args:array of const):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_mprintf';
function sqlite3_mprintf(_para1:Pchar):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_mprintf';
//function sqlite3_vmprintf(_para1:Pchar; _para2:va_list):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_vmprintf';
procedure sqlite3_free(z:Pchar);cdecl; external Sqlite3Lib name 'sqlite3_free';
//function sqlite3_snprintf(_para1:longint; _para2:Pchar; _para3:Pchar; args:array of const):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_snprintf';
function sqlite3_snprintf(_para1:longint; _para2:Pchar; _para3:Pchar):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_snprintf';
function sqlite3_set_authorizer(_para1:Psqlite3; xAuth:sqlite3_set_authorizer_func; pUserData:pointer):longint;cdecl; external Sqlite3Lib name 'sqlite3_set_authorizer';
function sqlite3_trace(_para1:Psqlite3; xTrace:sqlite3_trace_func; _para3:pointer):pointer;cdecl; external Sqlite3Lib name 'sqlite3_trace';
procedure sqlite3_progress_handler(_para1:Psqlite3; _para2:longint; _para3:sqlite3_progress_handler_func; _para4:pointer);cdecl; external Sqlite3Lib name 'sqlite3_progress_handler';
function sqlite3_commit_hook(_para1:Psqlite3; _para2:sqlite3_commit_hook_func; _para3:pointer):pointer;cdecl; external Sqlite3Lib name 'sqlite3_commit_hook';
function sqlite3_open(filename:Pchar; ppDb:PPsqlite3):longint;cdecl; external Sqlite3Lib name 'sqlite3_open';
function sqlite3_open16(filename:pointer; ppDb:PPsqlite3):longint;cdecl; external Sqlite3Lib name 'sqlite3_open16';
function sqlite3_errcode(db:Psqlite3):longint;cdecl; external Sqlite3Lib name 'sqlite3_errcode';
function sqlite3_errmsg(_para1:Psqlite3):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_errmsg';
function sqlite3_errmsg16(_para1:Psqlite3):pointer;cdecl; external Sqlite3Lib name 'sqlite3_errmsg16';
function sqlite3_prepare(db:Psqlite3; zSql:Pchar; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:PPchar):longint;cdecl; external Sqlite3Lib name 'sqlite3_prepare';
function sqlite3_prepare16(db:Psqlite3; zSql:pointer; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:Ppointer):longint;cdecl; external Sqlite3Lib name 'sqlite3_prepare16';
function sqlite3_bind_blob(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:bind_destructor_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_blob';
function sqlite3_bind_double(_para1:Psqlite3_stmt; _para2:longint; _para3:double):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_double';
function sqlite3_bind_int(_para1:Psqlite3_stmt; _para2:longint; _para3:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_int';
function sqlite3_bind_int64(_para1:Psqlite3_stmt; _para2:longint; _para3:sqlite_int64):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_int64';
function sqlite3_bind_null(_para1:Psqlite3_stmt; _para2:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_null';
function sqlite3_bind_text(_para1:Psqlite3_stmt; _para2:longint; _para3:Pchar; n:longint; _para5:bind_destructor_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_text';
function sqlite3_bind_text16(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:bind_destructor_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_text16';
//function sqlite3_bind_value(_para1:Psqlite3_stmt; _para2:longint; _para3:Psqlite3_value):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_value';
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
function sqlite3_bind_blob(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_blob';
function sqlite3_bind_text(_para1:Psqlite3_stmt; _para2:longint; _para3:Pchar; n:longint; _para5:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_text';
function sqlite3_bind_text16(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_text16';

function sqlite3_bind_parameter_count(_para1:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_parameter_count';
function sqlite3_bind_parameter_name(_para1:Psqlite3_stmt; _para2:longint):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_bind_parameter_name';
function sqlite3_bind_parameter_index(_para1:Psqlite3_stmt; zName:Pchar):longint;cdecl; external Sqlite3Lib name 'sqlite3_bind_parameter_index';
//function sqlite3_clear_bindings(_para1:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_clear_bindings';
function sqlite3_column_count(pStmt:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_column_count';
function sqlite3_column_name(_para1:Psqlite3_stmt; _para2:longint):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_column_name';
function sqlite3_column_name16(_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl; external Sqlite3Lib name 'sqlite3_column_name16';
function sqlite3_column_decltype(_para1:Psqlite3_stmt; i:longint):Pchar;cdecl; external Sqlite3Lib name 'sqlite3_column_decltype';
function sqlite3_column_decltype16(_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl; external Sqlite3Lib name 'sqlite3_column_decltype16';
function sqlite3_step(_para1:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_step';
function sqlite3_data_count(pStmt:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_data_count';
function sqlite3_column_blob(_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl; external Sqlite3Lib name 'sqlite3_column_blob';
function sqlite3_column_bytes(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_column_bytes';
function sqlite3_column_bytes16(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_column_bytes16';
function sqlite3_column_double(_para1:Psqlite3_stmt; iCol:longint):double;cdecl; external Sqlite3Lib name 'sqlite3_column_double';
function sqlite3_column_int(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_column_int';
function sqlite3_column_int64(_para1:Psqlite3_stmt; iCol:longint):sqlite_int64;cdecl; external Sqlite3Lib name 'sqlite3_column_int64';
function sqlite3_column_text(_para1:Psqlite3_stmt; iCol:longint):PChar;cdecl; external Sqlite3Lib name 'sqlite3_column_text';
function sqlite3_column_text16(_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl; external Sqlite3Lib name 'sqlite3_column_text16';
function sqlite3_column_type(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_column_type';
function sqlite3_finalize(pStmt:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_finalize';
function sqlite3_reset(pStmt:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_reset';
function sqlite3_create_function(_para1:Psqlite3; zFunctionName:Pchar; nArg:longint; eTextRep:longint; _para5:pointer;        xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_create_function';
function sqlite3_create_function16(_para1:Psqlite3; zFunctionName:pointer; nArg:longint; eTextRep:longint; _para5:pointer;            xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_create_function16';
function sqlite3_aggregate_count(_para1:Psqlite3_context):longint;cdecl; external Sqlite3Lib name 'sqlite3_aggregate_count';
function sqlite3_value_blob(_para1:Psqlite3_value):pointer;cdecl; external Sqlite3Lib name 'sqlite3_value_blob';
function sqlite3_value_bytes(_para1:Psqlite3_value):longint;cdecl; external Sqlite3Lib name 'sqlite3_value_bytes';
function sqlite3_value_bytes16(_para1:Psqlite3_value):longint;cdecl; external Sqlite3Lib name 'sqlite3_value_bytes16';
function sqlite3_value_double(_para1:Psqlite3_value):double;cdecl; external Sqlite3Lib name 'sqlite3_value_double';
function sqlite3_value_int(_para1:Psqlite3_value):longint;cdecl; external Sqlite3Lib name 'sqlite3_value_int';
function sqlite3_value_int64(_para1:Psqlite3_value):sqlite_int64;cdecl; external Sqlite3Lib name 'sqlite3_value_int64';
function sqlite3_value_text(_para1:Psqlite3_value):PChar;cdecl; external Sqlite3Lib name 'sqlite3_value_text';
function sqlite3_value_text16(_para1:Psqlite3_value):pointer;cdecl; external Sqlite3Lib name 'sqlite3_value_text16';
function sqlite3_value_text16le(_para1:Psqlite3_value):pointer;cdecl; external Sqlite3Lib name 'sqlite3_value_text16le';
function sqlite3_value_text16be(_para1:Psqlite3_value):pointer;cdecl; external Sqlite3Lib name 'sqlite3_value_text16be';
function sqlite3_value_type(_para1:Psqlite3_value):longint;cdecl; external Sqlite3Lib name 'sqlite3_value_type';
function sqlite3_aggregate_context(_para1:Psqlite3_context; nBytes:longint):pointer;cdecl; external Sqlite3Lib name 'sqlite3_aggregate_context';
function sqlite3_user_data(_para1:Psqlite3_context):pointer;cdecl; external Sqlite3Lib name 'sqlite3_user_data';
function sqlite3_get_auxdata(_para1:Psqlite3_context; _para2:longint):pointer;cdecl; external Sqlite3Lib name 'sqlite3_get_auxdata';
procedure sqlite3_set_auxdata(_para1:Psqlite3_context; _para2:longint; _para3:pointer; _para4:sqlite3_set_auxdata_func);cdecl; external Sqlite3Lib name 'sqlite3_set_auxdata';
procedure sqlite3_result_blob(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl; external Sqlite3Lib name 'sqlite3_result_blob';
procedure sqlite3_result_double(_para1:Psqlite3_context; _para2:double);cdecl; external Sqlite3Lib name 'sqlite3_result_double';
procedure sqlite3_result_error(_para1:Psqlite3_context; _para2:Pchar; _para3:longint);cdecl; external Sqlite3Lib name 'sqlite3_result_error';
procedure sqlite3_result_error16(_para1:Psqlite3_context; _para2:pointer; _para3:longint);cdecl; external Sqlite3Lib name 'sqlite3_result_error16';
procedure sqlite3_result_int(_para1:Psqlite3_context; _para2:longint);cdecl; external Sqlite3Lib name 'sqlite3_result_int';
procedure sqlite3_result_int64(_para1:Psqlite3_context; _para2:sqlite_int64);cdecl; external Sqlite3Lib name 'sqlite3_result_int64';
procedure sqlite3_result_null(_para1:Psqlite3_context);cdecl; external Sqlite3Lib name 'sqlite3_result_null';
procedure sqlite3_result_text(_para1:Psqlite3_context; _para2:Pchar; _para3:longint; _para4:sqlite3_result_func);cdecl; external Sqlite3Lib name 'sqlite3_result_text';
procedure sqlite3_result_text16(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl; external Sqlite3Lib name 'sqlite3_result_text16';
procedure sqlite3_result_text16le(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl; external Sqlite3Lib name 'sqlite3_result_text16le';
procedure sqlite3_result_text16be(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl; external Sqlite3Lib name 'sqlite3_result_text16be';
procedure sqlite3_result_value(_para1:Psqlite3_context; _para2:Psqlite3_value);cdecl; external Sqlite3Lib name 'sqlite3_result_value';     
function sqlite3_create_collation(_para1:Psqlite3; zName:Pchar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_create_collation';
function sqlite3_create_collation16(_para1:Psqlite3; zName:Pchar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_create_collation16';
function sqlite3_collation_needed(_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_collation_needed';
function sqlite3_collation_needed16(_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl; external Sqlite3Lib name 'sqlite3_collation_needed16';
function sqlite3_libversion:PChar;cdecl; external Sqlite3Lib name 'sqlite3_libversion';
//Alias for allowing better code portability (win32 is not working with external variables) 
function sqlite3_version:PChar;cdecl; external Sqlite3Lib name 'sqlite3_libversion';

// Not published functions
function sqlite3_libversion_number:longint;cdecl; external Sqlite3Lib name 'sqlite3_libversion_number';
//function sqlite3_key(db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_key';
//function sqlite3_rekey(db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_rekey';
//function sqlite3_sleep(_para1:longint):longint;cdecl; external Sqlite3Lib name 'sqlite3_sleep';
//function sqlite3_expired(_para1:Psqlite3_stmt):longint;cdecl; external Sqlite3Lib name 'sqlite3_expired';
//function sqlite3_global_recover:longint;cdecl; external Sqlite3Lib name 'sqlite3_global_recover';

{$ELSE LOAD_DYNAMICALLY}

Var
  sqlite3_close : function (_para1:Psqlite3):longint;cdecl;
  sqlite3_exec : function (_para1:Psqlite3; sql:Pchar; _para3:sqlite3_callback; _para4:pointer; errmsg:PPchar):longint;cdecl;
  sqlite3_last_insert_rowid : function (_para1:Psqlite3):sqlite_int64;cdecl;
  sqlite3_changes : function (_para1:Psqlite3):longint;cdecl;
  sqlite3_total_changes : function (_para1:Psqlite3):longint;cdecl;
  sqlite3_interrupt : procedure (_para1:Psqlite3);cdecl;
  sqlite3_complete : function (sql:Pchar):longint;cdecl;
  sqlite3_complete16 : function (sql:pointer):longint;cdecl;
  sqlite3_busy_handler : function (_para1:Psqlite3; _para2:busy_handler_func; _para3:pointer):longint;cdecl;
  sqlite3_busy_timeout : function (_para1:Psqlite3; ms:longint):longint;cdecl;
  sqlite3_get_table : function (_para1:Psqlite3; sql:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint; errmsg:PPchar):longint;cdecl;
  sqlite3_free_table : procedure (result:PPchar);cdecl;
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   sqlite3_mprintf : function (_para1:Pchar; args:array of const):Pchar;cdecl;
  sqlite3_mprintf : function (_para1:Pchar):Pchar;cdecl;
//  sqlite3_vmprintf : function (_para1:Pchar; _para2:va_list):Pchar;cdecl;
  sqlite3_free : procedure (z:Pchar);cdecl;
//  sqlite3_snprintf : function (_para1:longint; _para2:Pchar; _para3:Pchar; args:array of const):Pchar;cdecl;
  sqlite3_snprintf : function (_para1:longint; _para2:Pchar; _para3:Pchar):Pchar;cdecl;
  sqlite3_set_authorizer : function (_para1:Psqlite3; xAuth:sqlite3_set_authorizer_func; pUserData:pointer):longint;cdecl;
  sqlite3_trace : function (_para1:Psqlite3; xTrace:sqlite3_trace_func; _para3:pointer):pointer;cdecl;
  sqlite3_progress_handler : procedure (_para1:Psqlite3; _para2:longint; _para3:sqlite3_progress_handler_func; _para4:pointer);cdecl;
  sqlite3_commit_hook : function (_para1:Psqlite3; _para2:sqlite3_commit_hook_func; _para3:pointer):pointer;cdecl;
  sqlite3_open : function (filename:Pchar; ppDb:PPsqlite3):longint;cdecl;
  sqlite3_open16 : function (filename:pointer; ppDb:PPsqlite3):longint;cdecl;
  sqlite3_errcode : function (db:Psqlite3):longint;cdecl;
  sqlite3_errmsg : function (_para1:Psqlite3):Pchar;cdecl;
  sqlite3_errmsg16 : function (_para1:Psqlite3):pointer;cdecl;
  sqlite3_prepare : function (db:Psqlite3; zSql:Pchar; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:PPchar):longint;cdecl;
  sqlite3_prepare16 : function (db:Psqlite3; zSql:pointer; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:Ppointer):longint;cdecl;
  sqlite3_bind_blob : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:bind_destructor_func):longint;cdecl;
  sqlite3_bind_double : function (_para1:Psqlite3_stmt; _para2:longint; _para3:double):longint;cdecl;
  sqlite3_bind_int : function (_para1:Psqlite3_stmt; _para2:longint; _para3:longint):longint;cdecl;
  sqlite3_bind_int64 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:sqlite_int64):longint;cdecl;
  sqlite3_bind_null : function (_para1:Psqlite3_stmt; _para2:longint):longint;cdecl;
  sqlite3_bind_text : function (_para1:Psqlite3_stmt; _para2:longint; _para3:Pchar; n:longint; _para5:bind_destructor_func):longint;cdecl;
  sqlite3_bind_text16 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:bind_destructor_func):longint;cdecl;
//  sqlite3_bind_value : function (_para1:Psqlite3_stmt; _para2:longint; _para3:Psqlite3_value):longint;cdecl;
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  sqlite3_bind_blob1 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:longint):longint;cdecl;
  sqlite3_bind_text1 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:Pchar; n:longint; _para5:longint):longint;cdecl;
  sqlite3_bind_text161 : function (_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:longint):longint;cdecl;

  sqlite3_bind_parameter_count : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_bind_parameter_name : function (_para1:Psqlite3_stmt; _para2:longint):Pchar;cdecl;
  sqlite3_bind_parameter_index : function (_para1:Psqlite3_stmt; zName:Pchar):longint;cdecl;
//  sqlite3_clear_bindings : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_count : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_name : function (_para1:Psqlite3_stmt; _para2:longint):Pchar;cdecl;
  sqlite3_column_name16 : function (_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl;
  sqlite3_column_decltype : function (_para1:Psqlite3_stmt; i:longint):Pchar;cdecl;
  sqlite3_column_decltype16 : function (_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl;
  sqlite3_step : function (_para1:Psqlite3_stmt):longint;cdecl;
  sqlite3_data_count : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_column_blob : function (_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl;
  sqlite3_column_bytes : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_column_bytes16 : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_column_double : function (_para1:Psqlite3_stmt; iCol:longint):double;cdecl;
  sqlite3_column_int : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_column_int64 : function (_para1:Psqlite3_stmt; iCol:longint):sqlite_int64;cdecl;
  sqlite3_column_text : function (_para1:Psqlite3_stmt; iCol:longint):PChar;cdecl;
  sqlite3_column_text16 : function (_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl;
  sqlite3_column_type : function (_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;
  sqlite3_finalize : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_reset : function (pStmt:Psqlite3_stmt):longint;cdecl;
  sqlite3_create_function : function (_para1:Psqlite3; zFunctionName:Pchar; nArg:longint; eTextRep:longint; _para5:pointer;        xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl;
  sqlite3_create_function16 : function (_para1:Psqlite3; zFunctionName:pointer; nArg:longint; eTextRep:longint; _para5:pointer;            xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl;
  sqlite3_aggregate_count : function (_para1:Psqlite3_context):longint;cdecl;
  sqlite3_value_blob : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_bytes : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_value_bytes16 : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_value_double : function (_para1:Psqlite3_value):double;cdecl;
  sqlite3_value_int : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_value_int64 : function (_para1:Psqlite3_value):sqlite_int64;cdecl;
  sqlite3_value_text : function (_para1:Psqlite3_value):PChar;cdecl;
  sqlite3_value_text16 : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_text16le : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_text16be : function (_para1:Psqlite3_value):pointer;cdecl;
  sqlite3_value_type : function (_para1:Psqlite3_value):longint;cdecl;
  sqlite3_aggregate_context : function (_para1:Psqlite3_context; nBytes:longint):pointer;cdecl;
  sqlite3_user_data : function (_para1:Psqlite3_context):pointer;cdecl;
  sqlite3_get_auxdata : function (_para1:Psqlite3_context; _para2:longint):pointer;cdecl;
  sqlite3_set_auxdata : procedure (_para1:Psqlite3_context; _para2:longint; _para3:pointer; _para4:sqlite3_set_auxdata_func);cdecl;
  sqlite3_result_blob : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_double : procedure (_para1:Psqlite3_context; _para2:double);cdecl;
  sqlite3_result_error : procedure (_para1:Psqlite3_context; _para2:Pchar; _para3:longint);cdecl;
  sqlite3_result_error16 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint);cdecl;
  sqlite3_result_int : procedure (_para1:Psqlite3_context; _para2:longint);cdecl;
  sqlite3_result_int64 : procedure (_para1:Psqlite3_context; _para2:sqlite_int64);cdecl;
  sqlite3_result_null : procedure (_para1:Psqlite3_context);cdecl;
  sqlite3_result_text : procedure (_para1:Psqlite3_context; _para2:Pchar; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_text16 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_text16le : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_text16be : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;
  sqlite3_result_value : procedure (_para1:Psqlite3_context; _para2:Psqlite3_value);cdecl;     
  sqlite3_create_collation : function (_para1:Psqlite3; zName:Pchar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl;
  sqlite3_create_collation16 : function (_para1:Psqlite3; zName:Pchar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl;
  sqlite3_collation_needed : function (_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl;
  sqlite3_collation_needed16 : function (_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl;
  sqlite3_libversion: function : PChar;cdecl;
//Alias for allowing better code portability (win32 is not working with external variables) 
  sqlite3_version: function: PChar;cdecl;

// Not published functions
  sqlite3_libversion_number : Function :longint;cdecl;
//  sqlite3_key : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_rekey : function (db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;
//  sqlite3_sleep : function (_para1:longint):longint;cdecl;
//  sqlite3_expired : function (_para1:Psqlite3_stmt):longint;cdecl;
//function sqlite3_global_recover:longint;cdecl;

Procedure InitialiseSQLite;
Procedure InitialiseSQLite(LibraryName : String);
Procedure ReleaseSQLite;

var 
  SQLiteLibraryHandle : TLibHandle;
  DefaultLibrary : String = Sqlite3Lib;

{$ENDIF LOAD_DYNAMICALLY}

implementation

{$IFDEF LOAD_DYNAMICALLY}

ResourceString
  SErrLoadFailed     = 'Can not load SQLite client library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'SQLIte interface already initialized from library %s.';

Procedure LoadAddresses(LibHandle : TLibHandle);

begin
  pointer(sqlite3_close) := GetProcedureAddress(LibHandle,'sqlite3_close');
  pointer(sqlite3_exec) := GetProcedureAddress(LibHandle,'sqlite3_exec');
  pointer(sqlite3_last_insert_rowid) := GetProcedureAddress(LibHandle,'sqlite3_last_insert_rowid');
  pointer(sqlite3_changes) := GetProcedureAddress(LibHandle,'sqlite3_changes');
  pointer(sqlite3_total_changes) := GetProcedureAddress(LibHandle,'sqlite3_total_changes');
  pointer(sqlite3_interrupt) := GetProcedureAddress(LibHandle,'sqlite3_interrupt');
  pointer(sqlite3_complete) := GetProcedureAddress(LibHandle,'sqlite3_complete');
  pointer(sqlite3_complete16) := GetProcedureAddress(LibHandle,'sqlite3_complete16');
  pointer(sqlite3_busy_handler) := GetProcedureAddress(LibHandle,'sqlite3_busy_handler');
  pointer(sqlite3_busy_timeout) := GetProcedureAddress(LibHandle,'sqlite3_busy_timeout');
  pointer(sqlite3_get_table) := GetProcedureAddress(LibHandle,'sqlite3_get_table');
  pointer(sqlite3_free_table) := GetProcedureAddress(LibHandle,'sqlite3_free_table');
// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
//   pointer(sqlite3_mprintf) := GetProcedureAddress(LibHandle,'sqlite3_mprintf');
  pointer(sqlite3_mprintf) := GetProcedureAddress(LibHandle,'sqlite3_mprintf');
//  pointer(sqlite3_vmprintf) := GetProcedureAddress(LibHandle,'sqlite3_vmprintf');
  pointer(sqlite3_free) := GetProcedureAddress(LibHandle,'sqlite3_free');
//  pointer(sqlite3_snprintf) := GetProcedureAddress(LibHandle,'sqlite3_snprintf');
  pointer(sqlite3_snprintf) := GetProcedureAddress(LibHandle,'sqlite3_snprintf');
  pointer(sqlite3_set_authorizer) := GetProcedureAddress(LibHandle,'sqlite3_set_authorizer');
  pointer(sqlite3_trace) := GetProcedureAddress(LibHandle,'sqlite3_trace');
  pointer(sqlite3_progress_handler) := GetProcedureAddress(LibHandle,'sqlite3_progress_handler');
  pointer(sqlite3_commit_hook) := GetProcedureAddress(LibHandle,'sqlite3_commit_hook');
  pointer(sqlite3_open) := GetProcedureAddress(LibHandle,'sqlite3_open');
  pointer(sqlite3_open16) := GetProcedureAddress(LibHandle,'sqlite3_open16');
  pointer(sqlite3_errcode) := GetProcedureAddress(LibHandle,'sqlite3_errcode');
  pointer(sqlite3_errmsg) := GetProcedureAddress(LibHandle,'sqlite3_errmsg');
  pointer(sqlite3_errmsg16) := GetProcedureAddress(LibHandle,'sqlite3_errmsg16');
  pointer(sqlite3_prepare) := GetProcedureAddress(LibHandle,'sqlite3_prepare');
  pointer(sqlite3_prepare16) := GetProcedureAddress(LibHandle,'sqlite3_prepare16');
  pointer(sqlite3_bind_blob) := GetProcedureAddress(LibHandle,'sqlite3_bind_blob');
  pointer(sqlite3_bind_double) := GetProcedureAddress(LibHandle,'sqlite3_bind_double');
  pointer(sqlite3_bind_int) := GetProcedureAddress(LibHandle,'sqlite3_bind_int');
  pointer(sqlite3_bind_int64) := GetProcedureAddress(LibHandle,'sqlite3_bind_int64');
  pointer(sqlite3_bind_null) := GetProcedureAddress(LibHandle,'sqlite3_bind_null');
  pointer(sqlite3_bind_text) := GetProcedureAddress(LibHandle,'sqlite3_bind_text');
  pointer(sqlite3_bind_text16) := GetProcedureAddress(LibHandle,'sqlite3_bind_text16');
//  pointer(sqlite3_bind_value) := GetProcedureAddress(LibHandle,'sqlite3_bind_value');
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
  pointer(sqlite3_bind_blob1) := GetProcedureAddress(LibHandle,'sqlite3_bind_blob');
  pointer(sqlite3_bind_text1) := GetProcedureAddress(LibHandle,'sqlite3_bind_text');
  pointer(sqlite3_bind_text161) := GetProcedureAddress(LibHandle,'sqlite3_bind_text16');

  pointer(sqlite3_bind_parameter_count) := GetProcedureAddress(LibHandle,'sqlite3_bind_parameter_count');
  pointer(sqlite3_bind_parameter_name) := GetProcedureAddress(LibHandle,'sqlite3_bind_parameter_name');
  pointer(sqlite3_bind_parameter_index) := GetProcedureAddress(LibHandle,'sqlite3_bind_parameter_index');
//  pointer(sqlite3_clear_bindings) := GetProcedureAddress(LibHandle,'sqlite3_clear_bindings');
  pointer(sqlite3_column_count) := GetProcedureAddress(LibHandle,'sqlite3_column_count');
  pointer(sqlite3_column_name) := GetProcedureAddress(LibHandle,'sqlite3_column_name');
  pointer(sqlite3_column_name16) := GetProcedureAddress(LibHandle,'sqlite3_column_name16');
  pointer(sqlite3_column_decltype) := GetProcedureAddress(LibHandle,'sqlite3_column_decltype');
  pointer(sqlite3_column_decltype16) := GetProcedureAddress(LibHandle,'sqlite3_column_decltype16');
  pointer(sqlite3_step) := GetProcedureAddress(LibHandle,'sqlite3_step');
  pointer(sqlite3_data_count) := GetProcedureAddress(LibHandle,'sqlite3_data_count');
  pointer(sqlite3_column_blob) := GetProcedureAddress(LibHandle,'sqlite3_column_blob');
  pointer(sqlite3_column_bytes) := GetProcedureAddress(LibHandle,'sqlite3_column_bytes');
  pointer(sqlite3_column_bytes16) := GetProcedureAddress(LibHandle,'sqlite3_column_bytes16');
  pointer(sqlite3_column_double) := GetProcedureAddress(LibHandle,'sqlite3_column_double');
  pointer(sqlite3_column_int) := GetProcedureAddress(LibHandle,'sqlite3_column_int');
  pointer(sqlite3_column_int64) := GetProcedureAddress(LibHandle,'sqlite3_column_int64');
  pointer(sqlite3_column_text) := GetProcedureAddress(LibHandle,'sqlite3_column_text');
  pointer(sqlite3_column_text16) := GetProcedureAddress(LibHandle,'sqlite3_column_text16');
  pointer(sqlite3_column_type) := GetProcedureAddress(LibHandle,'sqlite3_column_type');
  pointer(sqlite3_finalize) := GetProcedureAddress(LibHandle,'sqlite3_finalize');
  pointer(sqlite3_reset) := GetProcedureAddress(LibHandle,'sqlite3_reset');
  pointer(sqlite3_create_function) := GetProcedureAddress(LibHandle,'sqlite3_create_function');
  pointer(sqlite3_create_function16) := GetProcedureAddress(LibHandle,'sqlite3_create_function16');
  pointer(sqlite3_aggregate_count) := GetProcedureAddress(LibHandle,'sqlite3_aggregate_count');
  pointer(sqlite3_value_blob) := GetProcedureAddress(LibHandle,'sqlite3_value_blob');
  pointer(sqlite3_value_bytes) := GetProcedureAddress(LibHandle,'sqlite3_value_bytes');
  pointer(sqlite3_value_bytes16) := GetProcedureAddress(LibHandle,'sqlite3_value_bytes16');
  pointer(sqlite3_value_double) := GetProcedureAddress(LibHandle,'sqlite3_value_double');
  pointer(sqlite3_value_int) := GetProcedureAddress(LibHandle,'sqlite3_value_int');
  pointer(sqlite3_value_int64) := GetProcedureAddress(LibHandle,'sqlite3_value_int64');
  pointer(sqlite3_value_text) := GetProcedureAddress(LibHandle,'sqlite3_value_text');
  pointer(sqlite3_value_text16) := GetProcedureAddress(LibHandle,'sqlite3_value_text16');
  pointer(sqlite3_value_text16le) := GetProcedureAddress(LibHandle,'sqlite3_value_text16le');
  pointer(sqlite3_value_text16be) := GetProcedureAddress(LibHandle,'sqlite3_value_text16be');
  pointer(sqlite3_value_type) := GetProcedureAddress(LibHandle,'sqlite3_value_type');
  pointer(sqlite3_aggregate_context) := GetProcedureAddress(LibHandle,'sqlite3_aggregate_context');
  pointer(sqlite3_user_data) := GetProcedureAddress(LibHandle,'sqlite3_user_data');
  pointer(sqlite3_get_auxdata) := GetProcedureAddress(LibHandle,'sqlite3_get_auxdata');
  pointer(sqlite3_set_auxdata) := GetProcedureAddress(LibHandle,'sqlite3_set_auxdata');
  pointer(sqlite3_result_blob) := GetProcedureAddress(LibHandle,'sqlite3_result_blob');
  pointer(sqlite3_result_double) := GetProcedureAddress(LibHandle,'sqlite3_result_double');
  pointer(sqlite3_result_error) := GetProcedureAddress(LibHandle,'sqlite3_result_error');
  pointer(sqlite3_result_error16) := GetProcedureAddress(LibHandle,'sqlite3_result_error16');
  pointer(sqlite3_result_int) := GetProcedureAddress(LibHandle,'sqlite3_result_int');
  pointer(sqlite3_result_int64) := GetProcedureAddress(LibHandle,'sqlite3_result_int64');
  pointer(sqlite3_result_null) := GetProcedureAddress(LibHandle,'sqlite3_result_null');
  pointer(sqlite3_result_text) := GetProcedureAddress(LibHandle,'sqlite3_result_text');
  pointer(sqlite3_result_text16) := GetProcedureAddress(LibHandle,'sqlite3_result_text16');
  pointer(sqlite3_result_text16le) := GetProcedureAddress(LibHandle,'sqlite3_result_text16le');
  pointer(sqlite3_result_text16be) := GetProcedureAddress(LibHandle,'sqlite3_result_text16be');
  pointer(sqlite3_result_value) := GetProcedureAddress(LibHandle,'sqlite3_result_value');
  pointer(sqlite3_create_collation) := GetProcedureAddress(LibHandle,'sqlite3_create_collation');
  pointer(sqlite3_create_collation16) := GetProcedureAddress(LibHandle,'sqlite3_create_collation16');
  pointer(sqlite3_collation_needed) := GetProcedureAddress(LibHandle,'sqlite3_collation_needed');
  pointer(sqlite3_collation_needed16) := GetProcedureAddress(LibHandle,'sqlite3_collation_needed16');
  pointer(sqlite3_libversion):=GetProcedureAddress(LibHandle,'sqlite3_libversion');
//Alias for allowing better code portability (win32 is not working with external variables) 
  pointer(sqlite3_version):=GetProcedureAddress(LibHandle,'sqlite3_libversion');

// Not published functions
  pointer(sqlite3_libversion_number):=GetProcedureAddress(LibHandle,'sqlite3_libversion_number');
//  pointer(sqlite3_key) := GetProcedureAddress(LibHandle,'sqlite3_key');
//  pointer(sqlite3_rekey) := GetProcedureAddress(LibHandle,'sqlite3_rekey');
//  pointer(sqlite3_sleep) := GetProcedureAddress(LibHandle,'sqlite3_sleep');
//  pointer(sqlite3_expired) := GetProcedureAddress(LibHandle,'sqlite3_expired');
// function sqlite3_global_recover:longint;cdecl;
end;
        
var
  RefCount : integer;
  LoadedLibrary : String;
            

Function TryInitialiseSqlite(Const LibraryName : String) : Boolean;
            
begin
  Result:=False;
  if (RefCount=0) then
    begin
    SQLiteLibraryHandle:=LoadLibrary(LibraryName);
    Result:=(SQLiteLibraryHandle<>nilhandle);
    If not Result then
      Exit;
    inc(RefCount);
    LoadedLibrary:=LibraryName;
    LoadAddresses(SQLiteLibraryHandle);
    end
  else
    begin
    If (LoadedLibrary<>LibraryName) then
      Raise EInoutError.CreateFmt(SErrAlreadyLoaded,[LoadedLibrary]);
    inc(RefCount);
    Result:=True;
    end;
end;

Procedure InitialiseSQLite;

begin
  InitialiseSQLite(DefaultLibrary);
end;


Procedure InitialiseSQLite(LibraryName : String);

begin
  If Not TryInitialiseSQLIte(LibraryName) then
    Raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName]);
end;

Procedure ReleaseSQLITE;

begin
  if RefCount>1 then
    Dec(RefCount)
  else if UnloadLibrary(SQLITELibraryHandle) then
    begin
    Dec(RefCount);
    SQLITELibraryHandle := NilHandle;
    LoadedLibrary:='';
    end;
end;

{$ENDIF}

end.
