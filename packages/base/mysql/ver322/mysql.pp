
unit mysql;

{
  Import unit for the mysql header files.
  Translated form the original mysql.h by Michael Van Canneyt
  (michael@tfdec1.fys.kuleuven.ac.be)

}

interface

uses mysql_com;
{$ifdef win32}
Const mysqllib = 'libmysql';
{$else}
Const mysqllib = 'mysqlclient';
{$endif}

{$ifndef win32}
{$linklib c}
{$linklib m}
{$endif}

{ All is 4-byte aligned on my machine. }
{$packrecords 4}

type
   my_bool = byte;
   gptr   = pchar;
   Socket = Longint;
   PCardinal = ^Cardinal;

{$ifdef Unix}
Var
  mysql_port : cardinal; external name 'mysql_port';
  mysql_unix_port : pchar; external name 'mysql_unix_port';
{$endif}

{
#define IS_PRI_KEY(n)   ((n) & PRI_KEY_FLAG)
#define IS_NOT_NULL(n)  ((n) & NOT_NULL_FLAG)
#define IS_BLOB(n)      ((n) & BLOB_FLAG)
#define IS_NUM(t)       ((t) <= FIELD_TYPE_INT24)
}
Type
st_mysql_field = record
  name : Pchar;                 { Name of column }
  table : pchar;                { Table of column if column was a field }
  def: pchar;                   { Default value (set by mysql_list_fields) }
  ftype : enum_field_types;     { Type of field. See mysql_com.h for types }
  length : cardinal;            { Width of column }
  max_length : cardinal;        { Max width of selected set }
  flags : cardinal;             { Div flags }
  decimals : cardinal;  { Number of decimals in field }
end;
TMYSQL_FIELD = st_mysql_field;
PMYSQL_FIELD = ^TMYSQL_FIELD;

TMYSQL_ROW = PPchar;             { return data as array of strings }
TMYSQL_FIELD_OFFSET = cardinal;  { offset to current field }

{$ifndef oldmysql}
  my_ulonglong=qword;
{$else}
  my_longlong=cardinal;
{$endif}

PST_MYSQL_Rows = ^st_mysql_rows;
st_mysql_rows = Record
  next : pst_mysql_rows;                { list of rows }
  Data : TMYSQL_ROW;
end;
TMYSQL_ROWS = st_mysql_rows;
PMYSQL_ROWS = ^TMYSQL_ROWS;


TMYSQL_ROW_OFFSET = PMYSQL_ROWS;        { offset to current row }

st_mysql_data  = record
  rows   : my_ulonglong;
  fields : cardinal;
  data : PMYSQL_ROWS;
  alloc : TMEM_ROOT;
end;

TMYSQL_DATA = st_mysql_data;
PMYSQL_DATA = ^TMYSQL_DATA;

st_mysql_options = record
  connect_timeout,client_flag : cardinal;
  compress,named_pipe : my_bool;
  port : cardinal;
  host,init_command,user,password,unix_socket,db : pchar;
  my_cnf_file,my_cnf_group : pchar;
end;

mysql_status = (MYSQL_STATUS_READY,
                MYSQL_STATUS_GET_RESULT,
                MYSQL_STATUS_USE_RESULT);


st_mysql = Record
  NET : TNET;                   { Communication parameters }
  host,user,passwd,unix_socket,server_version,host_info,
  info,db : pchar;
  port,client_flag,server_capabilities : cardinal;
  protocol_version : cardinal;
  field_count : cardinal;
  thread_id : cardinal;         { Id for connection in server }
  affected_rows : my_ulonglong;
  insert_id : my_ulonglong;             { id if insert on table with NEXTNR }
  extra_info : my_ulonglong;            { Used by mysqlshow }
  packet_length : cardinal;
  status : mysql_status;
  fields : PMYSQL_FIELD;
  field_alloc : TMEM_ROOT;
  free_me : my_bool;            { If free in mysql_close }
  reconnect : my_bool;          { set to 1 if automatic reconnect }
  options : st_mysql_options;
end;
TMYSQL = st_mysql;
PMYSQL = ^TMYSQL;


st_mysql_res = record
  row_count : my_ulonglong;
  field_count, current_field : cardinal;
  fields :         PMYSQL_FIELD;
  data :           PMYSQL_DATA;
  data_cursor :    PMYSQL_ROWS;
  field_alloc :    TMEM_ROOT;
  row :            TMYSQL_ROW;                  { If unbuffered read }
  current_row :    TMYSQL_ROW;          { buffer to current row }
  lengths :        pcardinal;           { column lengths of current row }
  handle :         PMYSQL;              { for unbuffered reads }
  eof :            my_bool;                     { Used my mysql_fetch_row }
end;
TMYSQL_RES  = st_mysql_res;
PMYSQL_RES  = ^TMYSQL_RES;


{ Translated Macros }

Function mysql_num_rows (res : PMYSQL_RES) : my_ulonglong;
Function mysql_num_fields(res : PMYSQL_RES) : Cardinal;
Function mysql_eof(res : PMYSQL_RES) : my_bool;
Function mysql_fetch_field_direct(res : PMYSQL_RES; fieldnr : Cardinal) : TMYSQL_FIELD;
Function mysql_fetch_fields(res : PMYSQL_RES) : PMYSQL_FIELD;
Function mysql_row_tell(res : PMYSQL_RES) : PMYSQL_ROWS;
Function mysql_field_tell(res : PMYSQL_RES) : Cardinal;
Function mysql_affected_rows(mysql : PMYSQL): my_ulonglong;
Function mysql_insert_id(mysql : PMYSQL): my_ulonglong;
Function mysql_errno(mysql : PMYSQL) : Cardinal;
Function mysql_info(mysql : PMYSQL): Pchar;
Function mysql_reload(mysql : PMYSQL) : Longint;
Function mysql_thread_id(mysql : PMYSQL) : Cardinal;
Function mysql_error(mysql : PMYSQL) : pchar;

{ Original functions }

Function mysql_connect (mysql : PMYSQL; host,user,passwd: pchar) : PMYSQL; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_real_connect (mysql : PMYSQL; const host,user,passwd : pchar;
                                   port : cardinal;
                                   unix_socket : pchar;
                                   clientflag : cardinal) : PMYSQL;{$ifdef win32} stdcall {$else} cdecl {$endif};

Function mysql_close(sock : PMYSQL) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_select_db(MYSQL : PMYSQL; db : Pchar) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_query(mysql : PMYSQL; q : pchar) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_real_query(mysql : PMYSQL; q : Pchar; length : longint) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_create_db(mysql : PMYSQL; db : pchar) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_drop_db(mysql : PMYSQL; DB : Pchar) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_shutdown(mysql : PMYSQL) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_dump_debug_info(mysql : PMYSQL) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_refresh(mysql : PMYSQL; refresh_options : cardinal) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_kill(mysql : PMYSQL; pid : Cardinal) : longint; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_stat(mysql : PMYSQL) : Pchar; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_get_server_info(mysql : PMYSQL) : pchar; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_get_client_info : pchar; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_get_host_info(mysql : PMYSQL) : pchar; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_get_proto_info(mysql : PMYSQL) : Cardinal; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_list_dbs(mysql : PMYSQL;wild : Pchar) : PMYSQL_RES; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function  mysql_list_tables(mysql : PMYSQL;Wild : Pchar) : PMYSQL_RES; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function  mysql_list_fields(mysql : PMYSQL; table,wild : pchar) : PMYSQL_RES; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function  mysql_list_processes(mysql : PMYSQL) : PMYSQL_RES; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function  mysql_store_result(mysql : PMYSQL) : PMYSQL_RES; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function  mysql_use_result(mysql : PMYSQL) : PMYSQL_RES; {$ifdef win32} stdcall {$else} cdecl {$endif};
Procedure mysql_free_result(res : PMYSQL_RES);{$ifdef win32} stdcall {$else} cdecl {$endif};
Procedure mysql_data_seek(mysql : PMYSQL_RES; offs : cardinal);{$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_row_seek(mysql : PMYSQL_RES; Offs: TMYSQL_ROW_OFFSET): TMYSQL_ROW_OFFSET; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_field_seek(musql : PMYSQL_RES;offs : TMYSQL_FIELD_OFFSET): TMYSQL_FIELD_OFFSET; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_fetch_row(mysql : PMYSQL_RES) : TMYSQL_ROW; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_fetch_lengths(mysql : PMYSQL_RES) : PCardinal; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_fetch_field(handle : PMYSQL_RES) : PMYSQL_FIELD; {$ifdef win32} stdcall {$else} cdecl {$endif};
Function mysql_escape_string(escto,escfrom : pchar; length : Cardinal) : cardinal; {$ifdef win32} stdcall {$else} cdecl {$endif};
Procedure mysql_debug(debug : pchar);{$ifdef win32} stdcall {$else} cdecl {$endif};

implementation


function mysql_connect (mysql : PMYSQL; host,user,passwd: pchar) : PMYSQL;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_connect';
function mysql_real_connect (mysql : PMYSQL; const host,user,passwd : pchar;
                                   port : cardinal;
                                   unix_socket : pchar;
                                   clientflag : cardinal) : PMYSQL;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib;

function mysql_close(sock : PMYSQL) : longint ;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_close';
function mysql_select_db(MYSQL : PMYSQL; db : Pchar) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_select_db';
function mysql_query(mysql : PMYSQL; q : pchar) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_query';
function mysql_real_query(mysql : PMYSQL; q : Pchar; length : longint) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_real_query';
function mysql_create_db(mysql : PMYSQL; db : pchar) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_create_db';
Function mysql_drop_db(mysql : PMYSQL; DB : Pchar) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_drop_db';
Function mysql_shutdown(mysql : PMYSQL) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_shutdown';
Function mysql_dump_debug_info(mysql : PMYSQL) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_dump_debug_info';
Function mysql_refresh(mysql : PMYSQL; refresh_options : cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_refresh';
Function mysql_kill(mysql : PMYSQL; pid : Cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_kill';
Function mysql_stat(mysql : PMYSQL) : Pchar;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_stat';
Function mysql_get_server_info(mysql : PMYSQL) : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_get_server_info';
Function mysql_get_client_info : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib;
Function mysql_get_host_info(mysql : PMYSQL) : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_get_host_info';
Function mysql_get_proto_info(mysql : PMYSQL) : Cardinal;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_get_proto_info';
Function mysql_list_dbs(mysql : PMYSQL;wild : Pchar) : PMYSQL_RES;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_list_dbs';
Function mysql_list_tables(mysql : PMYSQL;Wild : Pchar) : PMYSQL_RES;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_list_tables';
Function mysql_list_fields(mysql : PMYSQL; table,wild : pchar) : PMYSQL_RES;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_list_fields';
Function mysql_list_processes(mysql : PMYSQL) : PMYSQL_RES;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_list_processes';
Function mysql_store_result(mysql : PMYSQL) : PMYSQL_RES;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_store_result';
Function mysql_use_result(mysql : PMYSQL) : PMYSQL_RES;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_use_result';
Procedure mysql_free_result(res : PMYSQL_RES);{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_free_result';
Procedure mysql_data_seek(mysql : PMYSQL_RES; offs : cardinal);{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_data_seek';
Function mysql_row_seek(mysql : PMYSQL_RES; Offs: TMYSQL_ROW_OFFSET): TMYSQL_ROW_OFFSET;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_row_seek';
Function mysql_field_seek(musql : PMYSQL_RES;offs : TMYSQL_FIELD_OFFSET): TMYSQL_FIELD_OFFSET;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_field_seek';
function mysql_fetch_row(mysql : PMYSQL_RES) : TMYSQL_ROW;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_fetch_row';
function mysql_fetch_lengths(mysql : PMYSQL_RES) : PCardinal;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_fetch_lengths';
function mysql_fetch_field(handle : PMYSQL_RES) : PMYSQL_FIELD;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_fetch_field';
Function mysql_escape_string(escto,escfrom : pchar; length : Cardinal) : cardinal;{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_escape_string';
Procedure mysql_debug(debug : pchar);{$ifdef win32} stdcall {$else} cdecl {$endif}; external mysqllib name 'mysql_debug';

Function  mysql_error(mysql : PMYSQL) : pchar;

begin
 mysql_error:=mysql^.net.last_error
end;

Function mysql_num_rows (res : PMYSQL_RES) : my_ulonglong;

begin
  mysql_num_rows:=res^.row_count
end;

Function mysql_num_fields(res : PMYSQL_RES) : Cardinal;

begin
  mysql_num_fields:=res^.field_count
end;

Function mysql_eof(res : PMYSQL_RES) : my_bool;

begin
  mysql_eof:=res^.eof
end;

Function mysql_fetch_field_direct(res : PMYSQL_RES; fieldnr : Cardinal) : TMYSQL_FIELD;

begin
  mysql_fetch_field_direct:=res^.fields[fieldnr];
end;

Function mysql_fetch_fields(res : PMYSQL_RES) : PMYSQL_FIELD;

begin
 mysql_fetch_fields:=res^.fields
end;

Function mysql_row_tell(res : PMYSQL_RES) : PMYSQL_ROWS;

begin
  mysql_row_tell:=res^.data_cursor
end;

Function mysql_field_tell(res : PMYSQL_RES) : Cardinal;

begin
  mysql_field_tell:=res^.current_field
end;

Function mysql_affected_rows(mysql : PMYSQL): my_ulonglong;

begin
  mysql_affected_rows:=mysql^.affected_rows
end;

Function mysql_insert_id(mysql : PMYSQL): my_ulonglong;

begin
  mysql_insert_id:=mysql^.insert_id
end;

Function mysql_errno(mysql : PMYSQL) : Cardinal;

begin
  mysql_errno:=mysql^.net.last_errno
end;

Function mysql_info(mysql : PMYSQL): Pchar;

begin
  mysql_info:=mysql^.info
end;

Function mysql_reload(mysql : PMYSQL) : Longint;

begin
   mysql_reload:=mysql_refresh(mysql,REFRESH_GRANT)
end;

Function mysql_thread_id(mysql : PMysql) : Cardinal;

begin
  mysql_thread_id:=mysql^.thread_id
end;

end.
