unit mysql;
interface

uses mysql_com;

{
  Automatically converted by H2Pas 0.99.15 from mysql.ph
  The following command line parameters were used:
    -p
    -D
    -l
    mysqlclient
    mysql.ph
}

  const
    External_library='mysqlclient'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

{$PACKRECORDS C}

  { Copyright (C) 2000 MySQL AB

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  }

type

  Pmy_bool = ^my_bool;
  my_bool = char;

  Pgptr = ^gptr;
  gptr = char;

  Pmy_socket = ^my_socket;
  my_socket = longint;

var
  mysql_port : dword;cvar;external;
  mysql_unix_port : Pchar;cvar;external;

type
  Pst_mysql_field = ^st_mysql_field;
  st_mysql_field = record
       name : Pchar;
       table : Pchar;
       org_table : Pchar;
       db : Pchar;
       def : Pchar;
       length : dword;
       max_length : dword;
       flags : dword;
       decimals : dword;
       ftype : enum_field_types;
    end;
  MYSQL_FIELD = st_mysql_field;
  TMYSQL_FIELD = MYSQL_FIELD;
  PMYSQL_FIELD = ^MYSQL_FIELD;

  function IS_PRI_KEY(n : longint) : Boolean;
  function IS_NOT_NULL(n : longint) :  Boolean;
  function IS_BLOB(n : longint) : boolean;

type
  MYSQL_ROW = ppchar;
  PMYSQL_ROW = ^MYSQL_ROW;
  TMYSQL_ROW = MYSQL_ROW;

  PMYSQL_FIELD_OFFSET = ^MYSQL_FIELD_OFFSET;
  MYSQL_FIELD_OFFSET = dword;

  Pmy_ulonglong = ^my_ulonglong;
  my_ulonglong = qword;

  function MYSQL_COUNT_ERROR : longint;

type
  Pst_mysql_rows = ^st_mysql_rows;
  st_mysql_rows = record
     next : Pst_mysql_rows;
     data : MYSQL_ROW;
  end;
  MYSQL_ROWS = st_mysql_rows;
  TMYSQL_ROWS = MYSQL_ROWS;
  PMYSQL_ROWS = ^MYSQL_ROWS;

  MYSQL_ROW_OFFSET = MYSQL_ROWS;
  PMYSQL_ROW_OFFSET = ^MYSQL_ROW_OFFSET;

  Pst_used_mem = ^st_used_mem;
  st_used_mem = record
    next : Pst_used_mem;
    left : dword;
    size : dword;
  end;

  USED_MEM  = st_used_mem;
  TUSED_MEM = USED_MEM;
  PUSED_MEM = ^USED_MEM;

  Pst_mem_root = ^st_mem_root;
  st_mem_root = record
       free : PUSED_MEM;
       used : PUSED_MEM;
       pre_alloc : PUSED_MEM;
       min_malloc : dword;
       block_size : dword;
       block_num : dword;
       first_block_usage : dword;
       error_handler : procedure ;cdecl;
    end;
  MEM_ROOT = st_mem_root;
  TMEM_ROOT = MEM_ROOT;
  PMEM_ROOT = ^MEM_ROOT;

  Pst_mysql_data = ^st_mysql_data;
  st_mysql_data = record
       rows : my_ulonglong;
       fields : dword;
       data : PMYSQL_ROWS;
       alloc : MEM_ROOT;
    end;
  MYSQL_DATA = st_mysql_data;
  TMYSQL_DATA = MYSQL_DATA;
  PMYSQL_DATA = ^MYSQL_DATA;

  Pst_mysql_options = ^st_mysql_options;
  st_mysql_options = record
       connect_timeout : dword;
       client_flag : dword;
       port : dword;
       host : Pchar;
       init_command : Pchar;
       user : Pchar;
       password : Pchar;
       unix_socket : Pchar;
       db : Pchar;
       my_cnf_file : Pchar;
       my_cnf_group : Pchar;
       charset_dir : Pchar;
       charset_name : Pchar;
       ssl_key : Pchar;
       ssl_cert : Pchar;
       ssl_ca : Pchar;
       ssl_capath : Pchar;
       ssl_cipher : Pchar;
       max_allowed_packet : Cardinal;
       use_ssl : my_bool;
       compress : my_bool;
       named_pipe : my_bool;
       rpl_probe : my_bool;
       rpl_parse : my_bool;
       no_master_reads : my_bool;
    end;
  TMYSQL_OPTIONS = st_mysql_options;
  PTMYSQL_OPTIONS = ^TMYSQL_OPTIONS;

  mysql_option = (MYSQL_OPT_CONNECT_TIMEOUT,MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,MYSQL_SET_CHARSET_NAME
    );

  mysql_status = (MYSQL_STATUS_READY,MYSQL_STATUS_GET_RESULT,
    MYSQL_STATUS_USE_RESULT);

  mysql_rpl_type = (MYSQL_RPL_MASTER,MYSQL_RPL_SLAVE,MYSQL_RPL_ADMIN );

  Pst_mysql = ^st_mysql;
  st_mysql = record
       net : NET;
       connector_fd : gptr;
       host : Pchar;
       user : Pchar;
       passwd : Pchar;
       unix_socket : Pchar;
       server_version : Pchar;
       host_info : Pchar;
       info : Pchar;
       db : Pchar;
       charset : Pointer;  //!! Was Pcharset_info_st;
       fields : PMYSQL_FIELD;
       field_alloc : MEM_ROOT;
       affected_rows : my_ulonglong;
       insert_id : my_ulonglong;
       extra_info : my_ulonglong;
       thread_id : dword;
       packet_length : dword;
       port : dword;
       client_flag : dword;
       server_capabilities : dword;
       protocol_version : dword;
       field_count : dword;
       server_status : dword;
       server_language : dword;
       options : st_mysql_options;
       status : mysql_status;
       free_me : my_bool;
       reconnect : my_bool;
       scramble_buff : array[0..8] of char;
       rpl_pivot : my_bool;
       master : Pst_mysql;
       next_slave : Pst_mysql;
       last_used_slave : Pst_mysql;
       last_used_con : Pst_mysql;
    end;
  TMYSQL = st_mysql;
  PMYSQL = ^TMYSQL;

  Pst_mysql_res = ^st_mysql_res;
  st_mysql_res = record
       row_count : my_ulonglong;
       fields : PMYSQL_FIELD;
       data : PMYSQL_DATA;
       data_cursor : PMYSQL_ROWS;
       lengths : Pdword;
       handle : PMYSQL;
       field_alloc : MEM_ROOT;
       field_count : dword;
       current_field : dword;
       row : MYSQL_ROW;
       current_row : MYSQL_ROW;
       eof : my_bool;
    end;
  MYSQL_RES = st_mysql_res;
  TMYSQL_RES = MYSQL_RES;
  PMYSQL_RES = ^MYSQL_RES;

const
  MAX_MYSQL_MANAGER_ERR = 256;
  MAX_MYSQL_MANAGER_MSG = 256;
  MANAGER_OK = 200;
  MANAGER_INFO = 250;
  MANAGER_ACCESS = 401;
  MANAGER_CLIENT_ERR = 450;
  MANAGER_INTERNAL_ERR = 500;

type

  Pst_mysql_manager = ^st_mysql_manager;
  st_mysql_manager = record
       net : NET;
       host : Pchar;
       user : Pchar;
       passwd : Pchar;
       port : dword;
       free_me : my_bool;
       eof : my_bool;
       cmd_status : longint;
       last_errno : longint;
       net_buf : Pchar;
       net_buf_pos : Pchar;
       net_data_end : Pchar;
       net_buf_size : longint;
       last_error : array[0..(MAX_MYSQL_MANAGER_ERR)-1] of char;
    end;
  MYSQL_MANAGER = st_mysql_manager;
  PMYSQL_MANAGER = ^MYSQL_MANAGER;

function mysql_server_init(argc:longint; argv:PPchar; groups:PPchar):longint;cdecl;external External_library name 'mysql_server_init';
procedure mysql_server_end;cdecl;external External_library name 'mysql_server_end';
function mysql_thread_init:my_bool;cdecl;external External_library name 'mysql_thread_init';
procedure mysql_thread_end;cdecl;external External_library name 'mysql_thread_end';
function mysql_num_rows(res:PMYSQL_RES):my_ulonglong;cdecl;external External_library name 'mysql_num_rows';
function mysql_num_fields(res:PMYSQL_RES):dword;cdecl;external External_library name 'mysql_num_fields';
function mysql_eof(res:PMYSQL_RES):my_bool;cdecl;external External_library name 'mysql_eof';
function mysql_fetch_field_direct(res:PMYSQL_RES; fieldnr:dword):PMYSQL_FIELD;cdecl;external External_library name 'mysql_fetch_field_direct';
function mysql_fetch_fields(res:PMYSQL_RES):PMYSQL_FIELD;cdecl;external External_library name 'mysql_fetch_fields';
function mysql_row_tell(res:PMYSQL_RES):PMYSQL_ROWS;cdecl;external External_library name 'mysql_row_tell';
function mysql_field_tell(res:PMYSQL_RES):dword;cdecl;external External_library name 'mysql_field_tell';
function mysql_field_count(mysql:PMYSQL):dword;cdecl;external External_library name 'mysql_field_count';
function mysql_affected_rows(mysql:PMYSQL):my_ulonglong;cdecl;external External_library name 'mysql_affected_rows';
function mysql_insert_id(mysql:PMYSQL):my_ulonglong;cdecl;external External_library name 'mysql_insert_id';
function mysql_errno(mysql:PMYSQL):dword;cdecl;external External_library name 'mysql_errno';
function mysql_error(mysql:PMYSQL):Pchar;cdecl;external External_library name 'mysql_error';
function mysql_info(mysql:PMYSQL):Pchar;cdecl;external External_library name 'mysql_info';
function mysql_thread_id(mysql:PMYSQL):dword;cdecl;external External_library name 'mysql_thread_id';
function mysql_character_set_name(mysql:PMYSQL):Pchar;cdecl;external External_library name 'mysql_character_set_name';
function mysql_init(mysql:PMYSQL):PMYSQL;cdecl;external External_library name 'mysql_init';
function mysql_ssl_set(mysql:PMYSQL; key:Pchar; cert:Pchar; ca:Pchar; capath:Pchar;
           cipher:Pchar):longint;cdecl;external External_library name 'mysql_ssl_set';
function mysql_ssl_clear(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_ssl_clear';
function mysql_change_user(mysql:PMYSQL; user:Pchar; passwd:Pchar; db:Pchar):my_bool;cdecl;external External_library name 'mysql_change_user';
function mysql_real_connect(mysql:PMYSQL; host:Pchar; user:Pchar; passwd:Pchar; db:Pchar;
           port:dword; unix_socket:Pchar; clientflag:dword):PMYSQL;cdecl;external External_library name 'mysql_real_connect';
procedure mysql_close(sock:PMYSQL);cdecl;external External_library name 'mysql_close';
function mysql_select_db(mysql:PMYSQL; db:Pchar):longint;cdecl;external External_library name 'mysql_select_db';
function mysql_query(mysql:PMYSQL; q:Pchar):longint;cdecl;external External_library name 'mysql_query';
function mysql_send_query(mysql:PMYSQL; q:Pchar; length:dword):longint;cdecl;external External_library name 'mysql_send_query';
function mysql_read_query_result(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_read_query_result';
function mysql_real_query(mysql:PMYSQL; q:Pchar; length:dword):longint;cdecl;external External_library name 'mysql_real_query';
function mysql_master_query(mysql:PMYSQL; q:Pchar; length:dword):longint;cdecl;external External_library name 'mysql_master_query';
function mysql_master_send_query(mysql:PMYSQL; q:Pchar; length:dword):longint;cdecl;external External_library name 'mysql_master_send_query';
function mysql_slave_query(mysql:PMYSQL; q:Pchar; length:dword):longint;cdecl;external External_library name 'mysql_slave_query';
function mysql_slave_send_query(mysql:PMYSQL; q:Pchar; length:dword):longint;cdecl;external External_library name 'mysql_slave_send_query';

procedure mysql_enable_rpl_parse(mysql:PMYSQL);cdecl;external External_library name 'mysql_enable_rpl_parse';
procedure mysql_disable_rpl_parse(mysql:PMYSQL);cdecl;external External_library name 'mysql_disable_rpl_parse';
function mysql_rpl_parse_enabled(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_rpl_parse_enabled';
procedure mysql_enable_reads_from_master(mysql:PMYSQL);cdecl;external External_library name 'mysql_enable_reads_from_master';
procedure mysql_disable_reads_from_master(mysql:PMYSQL);cdecl;external External_library name 'mysql_disable_reads_from_master';
function mysql_reads_from_master_enabled(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_reads_from_master_enabled';
(* error
enum mysql_rpl_type      mysql_rpl_query_type(const char* q, int len);
in declaration at line 291 *)

function mysql_rpl_probe(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_rpl_probe';
function mysql_set_master(mysql:PMYSQL; host:Pchar; port:dword; user:Pchar; passwd:Pchar):longint;cdecl;external External_library name 'mysql_set_master';
function mysql_add_slave(mysql:PMYSQL; host:Pchar; port:dword; user:Pchar; passwd:Pchar):longint;cdecl;external External_library name 'mysql_add_slave';
function mysql_shutdown(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_shutdown';
function mysql_dump_debug_info(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_dump_debug_info';
function mysql_refresh(mysql:PMYSQL; refresh_options:dword):longint;cdecl;external External_library name 'mysql_refresh';
function mysql_kill(mysql:PMYSQL; pid:dword):longint;cdecl;external External_library name 'mysql_kill';
function mysql_ping(mysql:PMYSQL):longint;cdecl;external External_library name 'mysql_ping';
function mysql_stat(mysql:PMYSQL):Pchar;cdecl;external External_library name 'mysql_stat';
function mysql_get_server_info(mysql:PMYSQL):Pchar;cdecl;external External_library name 'mysql_get_server_info';
function mysql_get_client_info:Pchar;cdecl;external External_library name 'mysql_get_client_info';
function mysql_get_host_info(mysql:PMYSQL):Pchar;cdecl;external External_library name 'mysql_get_host_info';
function mysql_get_proto_info(mysql:PMYSQL):dword;cdecl;external External_library name 'mysql_get_proto_info';
function mysql_list_dbs(mysql:PMYSQL; wild:Pchar):PMYSQL_RES;cdecl;external External_library name 'mysql_list_dbs';
function mysql_list_tables(mysql:PMYSQL; wild:Pchar):PMYSQL_RES;cdecl;external External_library name 'mysql_list_tables';
function mysql_list_fields(mysql:PMYSQL; table:Pchar; wild:Pchar):PMYSQL_RES;cdecl;external External_library name 'mysql_list_fields';
function mysql_list_processes(mysql:PMYSQL):PMYSQL_RES;cdecl;external External_library name 'mysql_list_processes';
function mysql_store_result(mysql:PMYSQL):PMYSQL_RES;cdecl;external External_library name 'mysql_store_result';
function mysql_use_result(mysql:PMYSQL):PMYSQL_RES;cdecl;external External_library name 'mysql_use_result';
function mysql_options(mysql:PMYSQL; option:mysql_option; arg:Pchar):longint;cdecl;external External_library name 'mysql_options';
procedure mysql_free_result(result:PMYSQL_RES);cdecl;external External_library name 'mysql_free_result';
procedure mysql_data_seek(result:PMYSQL_RES; offset:my_ulonglong);cdecl;external External_library name 'mysql_data_seek';
function mysql_row_seek(result:PMYSQL_RES; _para2:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;cdecl;external External_library name 'mysql_row_seek';
function mysql_field_seek(result:PMYSQL_RES; offset:MYSQL_FIELD_OFFSET):MYSQL_FIELD_OFFSET;cdecl;external External_library name 'mysql_field_seek';
function mysql_fetch_row(result:PMYSQL_RES):MYSQL_ROW;cdecl;external External_library name 'mysql_fetch_row';
function mysql_fetch_lengths(result:PMYSQL_RES):Pdword;cdecl;external External_library name 'mysql_fetch_lengths';
function mysql_fetch_field(result:PMYSQL_RES):PMYSQL_FIELD;cdecl;external External_library name 'mysql_fetch_field';
function mysql_escape_string(_to:Pchar; from:Pchar; from_length:dword):dword;cdecl;external External_library name 'mysql_escape_string';
function mysql_real_escape_string(mysql:PMYSQL; _to:Pchar; from:Pchar; length:dword):dword;cdecl;external External_library name 'mysql_real_escape_string';
procedure mysql_debug(debug:Pchar);cdecl;external External_library name 'mysql_debug';

Type
  TExdendBuffer = function (_para1:pointer; _to:Pchar; length:Pdword):Pchar;
    function mysql_odbc_escape_string(mysql:PMYSQL; _to:Pchar; to_length:dword; from:Pchar; from_length:dword;
               param:pointer; extend_buffer: TExdendBuffer):Pchar;cdecl;external External_library name 'mysql_odbc_escape_string';
    procedure myodbc_remove_escape(mysql:PMYSQL; name:Pchar);cdecl;external External_library name 'myodbc_remove_escape';
    function mysql_thread_safe:dword;cdecl;external External_library name 'mysql_thread_safe';
    function mysql_manager_init(con:PMYSQL_MANAGER):PMYSQL_MANAGER;cdecl;external External_library name 'mysql_manager_init';
    function mysql_manager_connect(con:PMYSQL_MANAGER; host:Pchar; user:Pchar; passwd:Pchar; port:dword):PMYSQL_MANAGER;cdecl;external External_library name 'mysql_manager_connect';
    procedure mysql_manager_close(con:PMYSQL_MANAGER);cdecl;external External_library name 'mysql_manager_close';
    function mysql_manager_command(con:PMYSQL_MANAGER; cmd:Pchar; cmd_len:longint):longint;cdecl;external External_library name 'mysql_manager_command';
    function mysql_manager_fetch_line(con:PMYSQL_MANAGER; res_buf:Pchar; res_buf_size:longint):longint;cdecl;external External_library name 'mysql_manager_fetch_line';
    function mysql_reload(mysql : pmysql) : longint;
    function simple_command(mysql:PMYSQL; command:enum_server_command; arg:Pchar; length:dword; skipp_check:my_bool):longint;cdecl;external External_library name 'simple_command';
    function net_safe_read(mysql:PMYSQL):dword;cdecl;external External_library name 'net_safe_read';

implementation

function IS_PRI_KEY(n : longint) : Boolean;
begin
   IS_PRI_KEY:=(n and PRI_KEY_FLAG)<>0;
end;


function IS_NOT_NULL(n : longint) : Boolean;
begin
   IS_NOT_NULL:=(n and NOT_NULL_FLAG)<>0;
end;


function IS_BLOB(n : longint) : Boolean;
begin
   IS_BLOB:=(n and BLOB_FLAG)<>0;
end;


function IS_NUM_FIELD(f : Pst_mysql_field) : Boolean;
begin
   IS_NUM_FIELD:=((f^.flags) and NUM_FLAG)<>0;
end;


function MYSQL_COUNT_ERROR : longint;
begin
  MYSQL_COUNT_ERROR:= not (my_ulonglong(0));
end;


function mysql_reload(mysql : pmysql) : longint;
begin
  mysql_reload:=mysql_refresh(mysql,REFRESH_GRANT);
end;

end.
