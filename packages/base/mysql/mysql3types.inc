{
  Contains the types needed for use with MySQL v3
}

{
  Adapted from mysql3.pp by Bram Kuijvenhoven (Hexis BV, The Netherlands)
}

type
   my_bool = byte;
   gptr   = pchar;
   Socket = Longint;
   PCardinal = ^Cardinal;

{$ifdef linux}
Var
  mysql_port : cardinal; external name 'mysql_port';
  mysql_unix_port : pchar; external name 'mysql_unix_port';
{$endif}

{$ifdef darwin}
Var
  mysql_port : cardinal; external mysqllib name 'mysql_port';
  mysql_unix_port : pchar; external mysqllib name 'mysql_unix_port';
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
{$ifndef use_mysql_321}
  charset_dir, charset_name : pchar;
  use_ssl : my_bool;
  ssl_key, ssl_cert, ssl_ca, ssl_capath : pchar;
{$endif}
end;

{$ifndef use_mysql_321}
mysql_option = (MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS,
                    MYSQL_OPT_NAMED_PIPE, MYSQL_INIT_COMMAND,
                    MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
                    MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME);
{$endif}

mysql_status = (MYSQL_STATUS_READY,
                MYSQL_STATUS_GET_RESULT,
                MYSQL_STATUS_USE_RESULT);

{$ifndef use_mysql_321}
(*
charset_info_st = Record
        number : cardinal;
        name : pchar;
        ctype : pointer {uchar*};
        to_lower : pointer {uchar*};
        to_upper : pointer {uchar*};
        sort_order : pointer {uchar*};
        strxfrm_multiply : cardinal;

        { einige nicht näher definierte Felder }
        a, strxfrm, strnncoll, strnxfrm, like_range : pointer;
        mbmaxlen : cardinal;
        ismbchar, ismbhead, mbcharlen : pointer;
end;
*)
{$endif}

st_mysql = Record
  NET : TNET;                   { Communication parameters }
{$ifndef use_mysql_321}
  connector_fd : gptr;
{$endif}
  host,user,passwd,unix_socket,server_version,host_info,
  info,db : pchar;
  port,client_flag,server_capabilities : cardinal;
  protocol_version : cardinal;
  field_count : cardinal;
{$ifndef use_mysql_321}
  server_status : cardinal;
{$endif}
  thread_id : ptruint;         { Id for connection in server }
  affected_rows : my_ulonglong;
  insert_id : my_ulonglong;             { id if insert on table with NEXTNR }
  extra_info : my_ulonglong;            { Used by mysqlshow }
  packet_length : sizeint;
  status : mysql_status;
  fields : PMYSQL_FIELD;
  field_alloc : TMEM_ROOT;
  free_me : my_bool;            { If free in mysql_close }
  reconnect : my_bool;          { set to 1 if automatic reconnect }
  options : st_mysql_options;
{$ifndef use_mysql_321}
  scramble_buf : array[0..8] of char;
  charset : pointer { struct charset_info_st};
  server_language : cardinal;
{$endif}
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
  lengths :        psizeint;           { column lengths of current row }
  handle :         PMYSQL;              { for unbuffered reads }
  eof :            my_bool;                     { Used my mysql_fetch_row }
end;
TMYSQL_RES  = st_mysql_res;
PMYSQL_RES  = ^TMYSQL_RES;

{ Translated macros }
Function mysql_reload(mysql : PMYSQL) : Longint;


