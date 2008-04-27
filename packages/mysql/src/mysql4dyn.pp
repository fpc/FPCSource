{
  Contains the MySQL 4 functions calls

  Call InitialiseMysql4 before using any of the calls, and call ReleaseMysql4
  when finished.
}
unit mysql4dyn;

{$mode objfpc}{$H+}
{$MACRO on}

interface

uses ctypes,dynlibs, classes, sysutils, my4_sys, mysql4_comdyn;


{
  Automatically converted by H2Pas 0.99.15 from mysql.ph
  The following command line parameters were used:
    -p
    -D
    -l
    mysqlclient
    mysql.ph
}


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


{$IFDEF Unix}
  {$DEFINE extdecl:=cdecl}
  const
    Mysqllib = 'libmysqlclient.'+sharedsuffix;
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
  const
    Mysqllib = 'libmysql.dll';
{$ENDIF}

{$PACKRECORDS C}

{$i mysql4types.inc}

type tpcharfunction = function : pchar; extdecl;

var
  mysql_server_init : function (argc:longint; argv:PPchar; groups:PPchar):longint;extdecl;
  mysql_server_end : procedure;extdecl;
  mysql_thread_init : function :my_bool;extdecl;
  mysql_thread_end : procedure;extdecl;
  mysql_num_rows : function (res:PMYSQL_RES):my_ulonglong;extdecl;
  mysql_num_fields : function (res:PMYSQL_RES):dword;extdecl;
  mysql_eof : function (res:PMYSQL_RES):my_bool;extdecl;
  mysql_fetch_field_direct : function (res:PMYSQL_RES; fieldnr:dword):PMYSQL_FIELD;extdecl;
  mysql_fetch_fields : function (res:PMYSQL_RES):PMYSQL_FIELD;extdecl;
  mysql_row_tell : function (res:PMYSQL_RES):PMYSQL_ROWS;extdecl;
  mysql_field_tell : function (res:PMYSQL_RES):dword;extdecl;
  mysql_field_count : function (mysql:PMYSQL):dword;extdecl;
  mysql_affected_rows : function (mysql:PMYSQL):my_ulonglong;extdecl;
  mysql_insert_id : function (mysql:PMYSQL):my_ulonglong;extdecl;
  mysql_errno : function (mysql:PMYSQL):dword;extdecl;
  mysql_error : function (mysql:PMYSQL):Pchar;extdecl;
  mysql_info : function (mysql:PMYSQL):Pchar;extdecl;
  mysql_thread_id : function (mysql:PMYSQL):dword;extdecl;
  mysql_character_set_name : function (mysql:PMYSQL):Pchar;extdecl;
  mysql_init : function (mysql:PMYSQL):PMYSQL;extdecl;
  mysql_ssl_set : function (mysql:PMYSQL; key:Pchar; cert:Pchar; ca:Pchar; capath:Pchar;cipher:Pchar):longint;extdecl;
  mysql_ssl_clear : function (mysql:PMYSQL):longint;extdecl;
  mysql_change_user : function (mysql:PMYSQL; user:Pchar; passwd:Pchar; db:Pchar):my_bool;extdecl;
  mysql_real_connect : function (mysql:PMYSQL; host:Pchar; user:Pchar; passwd:Pchar; db:Pchar;port:dword; unix_socket:Pchar; clientflag:dword):PMYSQL;extdecl;
  mysql_close : procedure (sock:PMYSQL);extdecl;
  mysql_select_db : function (mysql:PMYSQL; db:Pchar):longint;extdecl;
  mysql_query : function (mysql:PMYSQL; q:Pchar):longint;extdecl;
  mysql_send_query : function (mysql:PMYSQL; q:Pchar; length:dword):longint;extdecl;
  mysql_read_query_result : function (mysql:PMYSQL):longint;extdecl;
  mysql_real_query : function (mysql:PMYSQL; q:Pchar; length:dword):longint;extdecl;
  mysql_master_query : function (mysql:PMYSQL; q:Pchar; length:dword):longint;extdecl;
  mysql_master_send_query : function (mysql:PMYSQL; q:Pchar; length:dword):longint;extdecl;
  mysql_slave_query : function (mysql:PMYSQL; q:Pchar; length:dword):longint;extdecl;
  mysql_slave_send_query : function (mysql:PMYSQL; q:Pchar; length:dword):longint;extdecl;
  mysql_enable_rpl_parse : procedure (mysql:PMYSQL);extdecl;
  mysql_disable_rpl_parse : procedure (mysql:PMYSQL);extdecl;
  mysql_rpl_parse_enabled : function (mysql:PMYSQL):longint;extdecl;
  mysql_enable_reads_from_master : procedure (mysql:PMYSQL);extdecl;
  mysql_disable_reads_from_master : procedure (mysql:PMYSQL);extdecl;
  mysql_reads_from_master_enabled : function (mysql:PMYSQL):longint;extdecl;
(* error
enum mysql_rpl_type      mysql_rpl_query_type(const char* q, int len);
in declaration at line 291 *)
  mysql_rpl_probe : function (mysql:PMYSQL):longint;extdecl;
  mysql_set_master : function (mysql:PMYSQL; host:Pchar; port:dword; user:Pchar; passwd:Pchar):longint;extdecl;
  mysql_add_slave : function (mysql:PMYSQL; host:Pchar; port:dword; user:Pchar; passwd:Pchar):longint;extdecl;
  mysql_shutdown : function (mysql:PMYSQL):longint;extdecl;
  mysql_dump_debug_info : function (mysql:PMYSQL):longint;extdecl;
  mysql_refresh : function (mysql:PMYSQL; refresh_options:dword):longint;extdecl;
  mysql_kill : function (mysql:PMYSQL; pid:dword):longint;extdecl;
  mysql_ping : function (mysql:PMYSQL):longint;extdecl;
  mysql_stat : function (mysql:PMYSQL):Pchar;extdecl;
  mysql_get_server_info : function (mysql:PMYSQL):Pchar;extdecl;
  mysql_get_client_info : tpcharfunction; //function:Pchar; extdecl;
  mysql_get_host_info : function (mysql:PMYSQL):Pchar;extdecl;
  mysql_get_proto_info : function (mysql:PMYSQL):dword;extdecl;
  mysql_list_dbs : function (mysql:PMYSQL; wild:Pchar):PMYSQL_RES;extdecl;
  mysql_list_tables : function (mysql:PMYSQL; wild:Pchar):PMYSQL_RES;extdecl;
  mysql_list_fields : function (mysql:PMYSQL; table:Pchar; wild:Pchar):PMYSQL_RES;extdecl;
  mysql_list_processes : function (mysql:PMYSQL):PMYSQL_RES;extdecl;
  mysql_store_result : function (mysql:PMYSQL):PMYSQL_RES;extdecl;
  mysql_use_result : function (mysql:PMYSQL):PMYSQL_RES;extdecl;
  mysql_options : function (mysql:PMYSQL; option:mysql_option; arg:Pchar):longint;extdecl;
  mysql_free_result : procedure (result:PMYSQL_RES);extdecl;
  mysql_data_seek : procedure (result:PMYSQL_RES; offset:my_ulonglong);extdecl;
  mysql_row_seek : function (result:PMYSQL_RES; _para2:MYSQL_ROW_OFFSET):MYSQL_ROW_OFFSET;extdecl;
  mysql_field_seek : function (result:PMYSQL_RES; offset:MYSQL_FIELD_OFFSET):MYSQL_FIELD_OFFSET;extdecl;
  mysql_fetch_row : function (result:PMYSQL_RES):MYSQL_ROW;extdecl;
  mysql_fetch_lengths : function (result:PMYSQL_RES):Pdword;extdecl;
  mysql_fetch_field : function (result:PMYSQL_RES):PMYSQL_FIELD;extdecl;
  mysql_escape_string : function (_to:Pchar; from:Pchar; from_length:dword):dword;extdecl;
  mysql_real_escape_string : function (mysql:PMYSQL; _to:Pchar; from:Pchar; length:dword):dword;extdecl;
  mysql_debug : procedure (debug:Pchar);extdecl;
  mysql_odbc_escape_string : function (mysql:PMYSQL; _to:Pchar; to_length:dword; from:Pchar; from_length:dword;param:pointer; extend_buffer: TExdendBuffer):Pchar;extdecl;
  myodbc_remove_escape : procedure (mysql:PMYSQL; name:Pchar);extdecl;
  mysql_thread_safe : function :dword;extdecl;
  mysql_manager_init : function (con:PMYSQL_MANAGER):PMYSQL_MANAGER;extdecl;
  mysql_manager_connect : function (con:PMYSQL_MANAGER; host:Pchar; user:Pchar; passwd:Pchar; port:dword):PMYSQL_MANAGER;extdecl;
  mysql_manager_close : procedure (con:PMYSQL_MANAGER);extdecl;
  mysql_manager_command : function (con:PMYSQL_MANAGER; cmd:Pchar; cmd_len:longint):longint;extdecl;
  mysql_manager_fetch_line : function (con:PMYSQL_MANAGER; res_buf:Pchar; res_buf_size:longint):longint;extdecl;
  simple_command : function (mysql:PMYSQL; command:enum_server_command; arg:Pchar; length:dword; skipp_check:my_bool):longint;extdecl;
  net_safe_read : function (mysql:PMYSQL):dword;extdecl;


function IS_PRI_KEY(n : longint) : Boolean;
function IS_NOT_NULL(n : longint) :  Boolean;
function IS_BLOB(n : longint) : boolean;
function MYSQL_COUNT_ERROR : longint;
function mysql_reload(mysql : pmysql) : longint;

Function InitialiseMysql4 : Integer;
Function InitialiseMysql4(Const LibraryName : String) : Integer;
Procedure ReleaseMysql4;

var Mysql4LibraryHandle : TLibHandle;

implementation

ResourceString
  SErrAlreadyLoaded = 'MySQL 4.1 already initialized from library %s';
  SLoadFailed       = 'Can not load MySQL 4.1 library "%s". Please check your installation.';
  
var 
  RefCount : integer;
  LoadedLibrary : String;
  
Function InitialiseMysql4 : Integer;

begin
  // Use Default library
  Result:=InitialiseMySQL4(Mysqllib);
end;

Function InitialiseMysql4(Const LibraryName : String) : Integer;


begin
  inc(RefCount);
  if RefCount = 1 then
    begin
    Mysql4LibraryHandle := loadlibrary(LibraryName);
    if Mysql4LibraryHandle = nilhandle then
      begin
      RefCount := 0;
      Raise EInOutError.CreateFmt(SLoadFailed,[LibraryName]);
      end;
    LoadedLibrary:=LibraryName;  
    pointer(mysql_get_client_info) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_get_client_info');

    // To avoid the strangest problems for ppl using other client-libs
    if copy(strpas(mysql_get_client_info()),1,3) <> '4.0' then
      Raise EInOutError.Create('This program can only work with the MySQL client version 4.0.x. Please use the right version of '+Mysqllib+'.');

    pointer(mysql_server_init) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_server_init');
    pointer(mysql_server_end) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_server_end');
    pointer(mysql_thread_init) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_thread_init');
    pointer(mysql_thread_end) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_thread_end');
    pointer(mysql_num_rows) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_num_rows');
    pointer(mysql_num_fields) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_num_fields');
    pointer(mysql_eof) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_eof');
    pointer(mysql_fetch_field_direct) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_fetch_field_direct');
    pointer(mysql_fetch_fields) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_fetch_fields');
    pointer(mysql_row_tell) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_row_tell');
    pointer(mysql_field_tell) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_field_tell');
    pointer(mysql_field_count) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_field_count');
    pointer(mysql_affected_rows) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_affected_rows');
    pointer(mysql_insert_id) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_insert_id');
    pointer(mysql_errno) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_errno');
    pointer(mysql_error) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_error');
    pointer(mysql_info) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_info');
    pointer(mysql_thread_id) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_thread_id');
    pointer(mysql_character_set_name) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_character_set_name');
    pointer(mysql_init) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_init');
    pointer(mysql_ssl_set) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_ssl_set');
    pointer(mysql_ssl_clear) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_ssl_clear');
    pointer(mysql_change_user) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_change_user');
    pointer(mysql_real_connect) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_real_connect');
    pointer(mysql_close) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_close');
    pointer(mysql_select_db) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_select_db');
    pointer(mysql_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_query');
    pointer(mysql_send_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_send_query');
    pointer(mysql_read_query_result) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_read_query_result');
    pointer(mysql_real_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_real_query');
    pointer(mysql_master_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_master_query');
    pointer(mysql_master_send_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_master_send_query');
    pointer(mysql_slave_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_slave_query');
    pointer(mysql_slave_send_query) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_slave_send_query');
    pointer(mysql_enable_rpl_parse) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_enable_rpl_parse');
    pointer(mysql_disable_rpl_parse) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_disable_rpl_parse');
    pointer(mysql_rpl_parse_enabled) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_rpl_parse_enabled');
    pointer(mysql_enable_reads_from_master) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_enable_reads_from_master');
    pointer(mysql_disable_reads_from_master) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_disable_reads_from_master');
    pointer(mysql_reads_from_master_enabled) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_reads_from_master_enabled');
    pointer(mysql_rpl_probe) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_rpl_probe');
    pointer(mysql_set_master) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_set_master');
    pointer(mysql_add_slave) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_add_slave');
    pointer(mysql_shutdown) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_shutdown');
    pointer(mysql_dump_debug_info) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_dump_debug_info');
    pointer(mysql_refresh) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_refresh');
    pointer(mysql_kill) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_kill');
    pointer(mysql_ping) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_ping');
    pointer(mysql_stat) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_stat');
    pointer(mysql_get_server_info) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_get_server_info');
    pointer(mysql_get_host_info) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_get_host_info');
    pointer(mysql_get_proto_info) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_get_proto_info');
    pointer(mysql_list_dbs) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_list_dbs');
    pointer(mysql_list_tables) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_list_tables');
    pointer(mysql_list_fields) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_list_fields');
    pointer(mysql_list_processes) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_list_processes');
    pointer(mysql_store_result) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_store_result');
    pointer(mysql_use_result) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_use_result');
    pointer(mysql_options) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_options');
    pointer(mysql_free_result) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_free_result');
    pointer(mysql_data_seek) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_data_seek');
    pointer(mysql_row_seek) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_row_seek');
    pointer(mysql_field_seek) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_field_seek');
    pointer(mysql_fetch_row) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_fetch_row');
    pointer(mysql_fetch_lengths) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_fetch_lengths');
    pointer(mysql_fetch_field) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_fetch_field');
    pointer(mysql_escape_string) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_escape_string');
    pointer(mysql_real_escape_string) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_real_escape_string');
    pointer(mysql_debug) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_debug');
    pointer(mysql_odbc_escape_string) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_odbc_escape_string');
    pointer(myodbc_remove_escape) := GetProcedureAddress(Mysql4LibraryHandle,'myodbc_remove_escape');
    pointer(mysql_thread_safe) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_thread_safe');
    pointer(mysql_manager_init) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_manager_init');
    pointer(mysql_manager_connect) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_manager_connect');
    pointer(mysql_manager_close) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_manager_close');
    pointer(mysql_manager_command) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_manager_command');
    pointer(mysql_manager_fetch_line) := GetProcedureAddress(Mysql4LibraryHandle,'mysql_manager_fetch_line');
    pointer(simple_command) := GetProcedureAddress(Mysql4LibraryHandle,'simple_command');
    pointer(net_safe_read) := GetProcedureAddress(Mysql4LibraryHandle,'net_safe_read');

    InitialiseMysql4_com;
    end
  else
    If (LibraryName<>LoadedLibrary) then
      Raise EInOUtError.CreateFmt(SErrAlreadyLoaded,[LoadedLibrary]);
  Result:=RefCount;  
end;

Procedure ReleaseMysql4;

begin
  if RefCount > 0 then dec(RefCount);
  if RefCount = 0 then
    begin
    if not UnloadLibrary(Mysql4LibraryHandle) then 
      inc(RefCount)
    else
      LoadedLibrary:='';
    ReleaseMysql4_com;
    end;
end;

{$i mysql4impl.inc}

end.
