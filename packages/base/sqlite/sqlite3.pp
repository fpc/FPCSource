{$mode objfpc}
unit sqlite3;

interface

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
  External_library='sqlite3';

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
  sqlite3_temp_directory : Pchar;cvar;external;
{$endif}

function sqlite3_close(_para1:Psqlite3):longint;cdecl;external External_library name 'sqlite3_close';
function sqlite3_exec(_para1:Psqlite3; sql:Pchar; _para3:sqlite3_callback; _para4:pointer; errmsg:PPchar):longint;cdecl;external External_library name 'sqlite3_exec';
function sqlite3_last_insert_rowid(_para1:Psqlite3):sqlite_int64;cdecl;external External_library name 'sqlite3_last_insert_rowid';
function sqlite3_changes(_para1:Psqlite3):longint;cdecl;external External_library name 'sqlite3_changes';
function sqlite3_total_changes(_para1:Psqlite3):longint;cdecl;external External_library name 'sqlite3_total_changes';
procedure sqlite3_interrupt(_para1:Psqlite3);cdecl;external External_library name 'sqlite3_interrupt';
function sqlite3_complete(sql:Pchar):longint;cdecl;external External_library name 'sqlite3_complete';
function sqlite3_complete16(sql:pointer):longint;cdecl;external External_library name 'sqlite3_complete16';
function sqlite3_busy_handler(_para1:Psqlite3; _para2:busy_handler_func; _para3:pointer):longint;cdecl;external External_library name 'sqlite3_busy_handler';
function sqlite3_busy_timeout(_para1:Psqlite3; ms:longint):longint;cdecl;external External_library name 'sqlite3_busy_timeout';
function sqlite3_get_table(_para1:Psqlite3; sql:Pchar; resultp:PPPchar; nrow:Plongint; ncolumn:Plongint; 
           errmsg:PPchar):longint;cdecl;external External_library name 'sqlite3_get_table';
procedure sqlite3_free_table(result:PPchar);cdecl;external External_library name 'sqlite3_free_table';

// Todo: see how translate sqlite3_mprintf, sqlite3_vmprintf, sqlite3_snprintf
// function sqlite3_mprintf(_para1:Pchar; args:array of const):Pchar;cdecl;external External_library name 'sqlite3_mprintf';
function sqlite3_mprintf(_para1:Pchar):Pchar;cdecl;external External_library name 'sqlite3_mprintf';
//function sqlite3_vmprintf(_para1:Pchar; _para2:va_list):Pchar;cdecl;external External_library name 'sqlite3_vmprintf';
procedure sqlite3_free(z:Pchar);cdecl;external External_library name 'sqlite3_free';
//function sqlite3_snprintf(_para1:longint; _para2:Pchar; _para3:Pchar; args:array of const):Pchar;cdecl;external External_library name 'sqlite3_snprintf';
function sqlite3_snprintf(_para1:longint; _para2:Pchar; _para3:Pchar):Pchar;cdecl;external External_library name 'sqlite3_snprintf';

function sqlite3_set_authorizer(_para1:Psqlite3; xAuth:sqlite3_set_authorizer_func; pUserData:pointer):longint;cdecl;external External_library name 'sqlite3_set_authorizer';

const
   SQLITE_COPY = 0;   
{ Index Name      Table Name       }
   SQLITE_CREATE_INDEX = 1;   
{ Table Name      NULL             }
   SQLITE_CREATE_TABLE = 2;   
{ Index Name      Table Name       }
   SQLITE_CREATE_TEMP_INDEX = 3;   
{ Table Name      NULL             }
   SQLITE_CREATE_TEMP_TABLE = 4;   
{ Trigger Name    Table Name       }
   SQLITE_CREATE_TEMP_TRIGGER = 5;   
{ View Name       NULL             }
   SQLITE_CREATE_TEMP_VIEW = 6;   
{ Trigger Name    Table Name       }
   SQLITE_CREATE_TRIGGER = 7;   
{ View Name       NULL             }
   SQLITE_CREATE_VIEW = 8;   
{ Table Name      NULL             }
   SQLITE_DELETE = 9;   
{ Index Name      Table Name       }
   SQLITE_DROP_INDEX = 10;   
{ Table Name      NULL             }
   SQLITE_DROP_TABLE = 11;   
{ Index Name      Table Name       }
   SQLITE_DROP_TEMP_INDEX = 12;   
{ Table Name      NULL             }
   SQLITE_DROP_TEMP_TABLE = 13;   
{ Trigger Name    Table Name       }
   SQLITE_DROP_TEMP_TRIGGER = 14;   
{ View Name       NULL             }
   SQLITE_DROP_TEMP_VIEW = 15;   
{ Trigger Name    Table Name       }
   SQLITE_DROP_TRIGGER = 16;   
{ View Name       NULL             }
   SQLITE_DROP_VIEW = 17;   
{ Table Name      NULL             }
   SQLITE_INSERT = 18;   
{ Pragma Name     1st arg or NULL  }
   SQLITE_PRAGMA = 19;   
{ Table Name      Column Name      }
   SQLITE_READ = 20;   
{ NULL            NULL             }
   SQLITE_SELECT = 21;   
{ NULL            NULL             }
   SQLITE_TRANSACTION = 22;   
{ Table Name      Column Name      }
   SQLITE_UPDATE = 23;   
{ Filename        NULL             }
   SQLITE_ATTACH = 24;   
{ Database Name   NULL             }
   SQLITE_DETACH = 25;   
{ Database Name   Table Name       }
   SQLITE_ALTER_TABLE = 26;   
{ Index Name      NULL             }
   SQLITE_REINDEX = 27;   

{ #define SQLITE_OK  0   // Allow access (This is actually defined above)  }
{ Abort the SQL statement with an error  }
  SQLITE_DENY = 1;   
{ Don't allow access, but don't generate an error  }
  SQLITE_IGNORE = 2;   

function sqlite3_trace(_para1:Psqlite3; xTrace:sqlite3_trace_func; _para3:pointer):pointer;cdecl;external External_library name 'sqlite3_trace';
procedure sqlite3_progress_handler(_para1:Psqlite3; _para2:longint; _para3:sqlite3_progress_handler_func; _para4:pointer);cdecl;external External_library name 'sqlite3_progress_handler';
function sqlite3_commit_hook(_para1:Psqlite3; _para2:sqlite3_commit_hook_func; _para3:pointer):pointer;cdecl;external External_library name 'sqlite3_commit_hook';
function sqlite3_open(filename:Pchar; ppDb:PPsqlite3):longint;cdecl;external External_library name 'sqlite3_open';
function sqlite3_open16(filename:pointer; ppDb:PPsqlite3):longint;cdecl;external External_library name 'sqlite3_open16';
function sqlite3_errcode(db:Psqlite3):longint;cdecl;external External_library name 'sqlite3_errcode';
function sqlite3_errmsg(_para1:Psqlite3):Pchar;cdecl;external External_library name 'sqlite3_errmsg';
function sqlite3_errmsg16(_para1:Psqlite3):pointer;cdecl;external External_library name 'sqlite3_errmsg16';
function sqlite3_prepare(db:Psqlite3; zSql:Pchar; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:PPchar):longint;cdecl;external External_library name 'sqlite3_prepare';
function sqlite3_prepare16(db:Psqlite3; zSql:pointer; nBytes:longint; ppStmt:PPsqlite3_stmt; pzTail:Ppointer):longint;cdecl;external External_library name 'sqlite3_prepare16';
function sqlite3_bind_blob(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:bind_destructor_func):longint;cdecl;external External_library name 'sqlite3_bind_blob';
function sqlite3_bind_double(_para1:Psqlite3_stmt; _para2:longint; _para3:double):longint;cdecl;external External_library name 'sqlite3_bind_double';
function sqlite3_bind_int(_para1:Psqlite3_stmt; _para2:longint; _para3:longint):longint;cdecl;external External_library name 'sqlite3_bind_int';
function sqlite3_bind_int64(_para1:Psqlite3_stmt; _para2:longint; _para3:sqlite_int64):longint;cdecl;external External_library name 'sqlite3_bind_int64';
function sqlite3_bind_null(_para1:Psqlite3_stmt; _para2:longint):longint;cdecl;external External_library name 'sqlite3_bind_null';
function sqlite3_bind_text(_para1:Psqlite3_stmt; _para2:longint; _para3:Pchar; n:longint; _para5:bind_destructor_func):longint;cdecl;external External_library name 'sqlite3_bind_text';
function sqlite3_bind_text16(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:bind_destructor_func):longint;cdecl;external External_library name 'sqlite3_bind_text16';
//function sqlite3_bind_value(_para1:Psqlite3_stmt; _para2:longint; _para3:Psqlite3_value):longint;cdecl;external External_library name 'sqlite3_bind_value';

// Original from sqlite3.h: 
//#define SQLITE_STATIC      ((void(*)(void *))0)
//#define SQLITE_TRANSIENT   ((void(*)(void *))-1)
Const
  SQLITE_STATIC    =  0;
  SQLITE_TRANSIENT =  -1;
  
//These overloaded functions were introduced to allow the use of SQLITE_STATIC and SQLITE_TRANSIENT
//It's the c world man ;-)
function sqlite3_bind_blob(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; n:longint; _para5:longint):longint;cdecl;external External_library name 'sqlite3_bind_blob';
function sqlite3_bind_text(_para1:Psqlite3_stmt; _para2:longint; _para3:Pchar; n:longint; _para5:longint):longint;cdecl;external External_library name 'sqlite3_bind_text';
function sqlite3_bind_text16(_para1:Psqlite3_stmt; _para2:longint; _para3:pointer; _para4:longint; _para5:longint):longint;cdecl;external External_library name 'sqlite3_bind_text16';

function sqlite3_bind_parameter_count(_para1:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_bind_parameter_count';
function sqlite3_bind_parameter_name(_para1:Psqlite3_stmt; _para2:longint):Pchar;cdecl;external External_library name 'sqlite3_bind_parameter_name';
function sqlite3_bind_parameter_index(_para1:Psqlite3_stmt; zName:Pchar):longint;cdecl;external External_library name 'sqlite3_bind_parameter_index';
//function sqlite3_clear_bindings(_para1:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_clear_bindings';
function sqlite3_column_count(pStmt:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_column_count';
function sqlite3_column_name(_para1:Psqlite3_stmt; _para2:longint):Pchar;cdecl;external External_library name 'sqlite3_column_name';
function sqlite3_column_name16(_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl;external External_library name 'sqlite3_column_name16';
function sqlite3_column_decltype(_para1:Psqlite3_stmt; i:longint):Pchar;cdecl;external External_library name 'sqlite3_column_decltype';
function sqlite3_column_decltype16(_para1:Psqlite3_stmt; _para2:longint):pointer;cdecl;external External_library name 'sqlite3_column_decltype16';
function sqlite3_step(_para1:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_step';
function sqlite3_data_count(pStmt:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_data_count';
function sqlite3_column_blob(_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl;external External_library name 'sqlite3_column_blob';
function sqlite3_column_bytes(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;external External_library name 'sqlite3_column_bytes';
function sqlite3_column_bytes16(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;external External_library name 'sqlite3_column_bytes16';
function sqlite3_column_double(_para1:Psqlite3_stmt; iCol:longint):double;cdecl;external External_library name 'sqlite3_column_double';
function sqlite3_column_int(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;external External_library name 'sqlite3_column_int';
function sqlite3_column_int64(_para1:Psqlite3_stmt; iCol:longint):sqlite_int64;cdecl;external External_library name 'sqlite3_column_int64';
function sqlite3_column_text(_para1:Psqlite3_stmt; iCol:longint):PChar;cdecl;external External_library name 'sqlite3_column_text';
function sqlite3_column_text16(_para1:Psqlite3_stmt; iCol:longint):pointer;cdecl;external External_library name 'sqlite3_column_text16';
function sqlite3_column_type(_para1:Psqlite3_stmt; iCol:longint):longint;cdecl;external External_library name 'sqlite3_column_type';
function sqlite3_finalize(pStmt:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_finalize';
function sqlite3_reset(pStmt:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_reset';
function sqlite3_create_function(_para1:Psqlite3; zFunctionName:Pchar; nArg:longint; eTextRep:longint; _para5:pointer; 
           xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl;external External_library name 'sqlite3_create_function';
function sqlite3_create_function16(_para1:Psqlite3; zFunctionName:pointer; nArg:longint; eTextRep:longint; _para5:pointer; 
           xFunc:create_function_func_func; xStep:create_function_step_func; xFinal:create_function_final_func):longint;cdecl;external External_library name 'sqlite3_create_function16';
function sqlite3_aggregate_count(_para1:Psqlite3_context):longint;cdecl;external External_library name 'sqlite3_aggregate_count';
function sqlite3_value_blob(_para1:Psqlite3_value):pointer;cdecl;external External_library name 'sqlite3_value_blob';
function sqlite3_value_bytes(_para1:Psqlite3_value):longint;cdecl;external External_library name 'sqlite3_value_bytes';
function sqlite3_value_bytes16(_para1:Psqlite3_value):longint;cdecl;external External_library name 'sqlite3_value_bytes16';
function sqlite3_value_double(_para1:Psqlite3_value):double;cdecl;external External_library name 'sqlite3_value_double';
function sqlite3_value_int(_para1:Psqlite3_value):longint;cdecl;external External_library name 'sqlite3_value_int';
function sqlite3_value_int64(_para1:Psqlite3_value):sqlite_int64;cdecl;external External_library name 'sqlite3_value_int64';
function sqlite3_value_text(_para1:Psqlite3_value):PChar;cdecl;external External_library name 'sqlite3_value_text';
function sqlite3_value_text16(_para1:Psqlite3_value):pointer;cdecl;external External_library name 'sqlite3_value_text16';
function sqlite3_value_text16le(_para1:Psqlite3_value):pointer;cdecl;external External_library name 'sqlite3_value_text16le';
function sqlite3_value_text16be(_para1:Psqlite3_value):pointer;cdecl;external External_library name 'sqlite3_value_text16be';
function sqlite3_value_type(_para1:Psqlite3_value):longint;cdecl;external External_library name 'sqlite3_value_type';
function sqlite3_aggregate_context(_para1:Psqlite3_context; nBytes:longint):pointer;cdecl;external External_library name 'sqlite3_aggregate_context';
function sqlite3_user_data(_para1:Psqlite3_context):pointer;cdecl;external External_library name 'sqlite3_user_data';
function sqlite3_get_auxdata(_para1:Psqlite3_context; _para2:longint):pointer;cdecl;external External_library name 'sqlite3_get_auxdata';
procedure sqlite3_set_auxdata(_para1:Psqlite3_context; _para2:longint; _para3:pointer; _para4:sqlite3_set_auxdata_func);cdecl;external External_library name 'sqlite3_set_auxdata';
procedure sqlite3_result_blob(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;external External_library name 'sqlite3_result_blob';
procedure sqlite3_result_double(_para1:Psqlite3_context; _para2:double);cdecl;external External_library name 'sqlite3_result_double';
procedure sqlite3_result_error(_para1:Psqlite3_context; _para2:Pchar; _para3:longint);cdecl;external External_library name 'sqlite3_result_error';
procedure sqlite3_result_error16(_para1:Psqlite3_context; _para2:pointer; _para3:longint);cdecl;external External_library name 'sqlite3_result_error16';
procedure sqlite3_result_int(_para1:Psqlite3_context; _para2:longint);cdecl;external External_library name 'sqlite3_result_int';
procedure sqlite3_result_int64(_para1:Psqlite3_context; _para2:sqlite_int64);cdecl;external External_library name 'sqlite3_result_int64';
procedure sqlite3_result_null(_para1:Psqlite3_context);cdecl;external External_library name 'sqlite3_result_null';
procedure sqlite3_result_text(_para1:Psqlite3_context; _para2:Pchar; _para3:longint; _para4:sqlite3_result_func);cdecl;external External_library name 'sqlite3_result_text';
procedure sqlite3_result_text16(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;external External_library name 'sqlite3_result_text16';
procedure sqlite3_result_text16le(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;external External_library name 'sqlite3_result_text16le';
procedure sqlite3_result_text16be(_para1:Psqlite3_context; _para2:pointer; _para3:longint; _para4:sqlite3_result_func);cdecl;external External_library name 'sqlite3_result_text16be';
procedure sqlite3_result_value(_para1:Psqlite3_context; _para2:Psqlite3_value);cdecl;external External_library name 'sqlite3_result_value';     
    
function sqlite3_create_collation(_para1:Psqlite3; zName:Pchar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl;external External_library name 'sqlite3_create_collation';
function sqlite3_create_collation16(_para1:Psqlite3; zName:Pchar; eTextRep:longint; _para4:pointer; xCompare:sqlite3_create_collation_func):longint;cdecl;external External_library name 'sqlite3_create_collation16';
 
function sqlite3_collation_needed(_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl;external External_library name 'sqlite3_collation_needed';
function sqlite3_collation_needed16(_para1:Psqlite3; _para2:pointer; _para3:sqlite3_collation_needed_func):longint;cdecl;external External_library name 'sqlite3_collation_needed16';

function sqlite3_libversion:PChar;cdecl;external External_library name 'sqlite3_libversion';
//Alias for allowing better code portability (win32 is not working with external variables) 
function sqlite3_version:PChar;cdecl;external External_library name 'sqlite3_libversion';

// Not published functions
function sqlite3_libversion_number:longint;cdecl;external External_library name 'sqlite3_libversion_number';
//function sqlite3_key(db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;external External_library name 'sqlite3_key';
//function sqlite3_rekey(db:Psqlite3; pKey:pointer; nKey:longint):longint;cdecl;external External_library name 'sqlite3_rekey';
//function sqlite3_sleep(_para1:longint):longint;cdecl;external External_library name 'sqlite3_sleep';
//function sqlite3_expired(_para1:Psqlite3_stmt):longint;cdecl;external External_library name 'sqlite3_expired';
//function sqlite3_global_recover:longint;cdecl;external External_library name 'sqlite3_global_recover';

implementation

end.
