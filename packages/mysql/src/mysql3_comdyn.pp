{
  Contains the MySQL_com functions calls

  Call InitialiseMysql3_com before using any of the calls, and call ReleaseMysql3_com
  when finished.
}
unit mysql3_comdyn;

{
  Adapted from mysql4_comdyn by Bram Kuijvenhoven (Hexis BV, The Netherlands)
}

{$mode objfpc}{$H+}
{$MACRO on}

interface

uses dynlibs, sysutils;

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

{$i mysql3_comtypes.inc}

var
  sql_free : procedure(root : PMEM_ROOT);extdecl;
  init_alloc_root : procedure(root: PMEM_ROOT;block_size : Cardinal);extdecl;
  sql_alloc_first_block : function(root : PMEM_ROOT) : my_bool;extdecl;
  sql_alloc_root : function(mem_root : PMEM_ROOT;len : Cardinal) : longint;extdecl;
  sql_strdup_root : function(root : PMEM_ROOT;st : pchar) : pchar;extdecl;
  sql_memdup_root : function(root: PMEM_ROOT;st : pchar; len : Cardinal) : longint;extdecl;
  my_net_init : function(net :PNET; fd : Socket) : Longint;extdecl;
  net_end : procedure(net : PNET);extdecl;
  net_clear : procedure(net : PNET);extdecl;
  net_flush : function(net : PNET) : longint;extdecl;
  my_net_write : function(net : PNET;packet : pbyte;len : cardinal) : longint;extdecl;
  net_write_command : function(net : PNET; command : char;packet : pbyte;len : cardinal) : longint;extdecl;
  net_real_write : function(net : PNET;packet : pbyte; len : Cardinal) : longint;extdecl;
  my_net_read : function(net : PNET) : Cardinal;extdecl;
  randominit : procedure(rand : Prand_struct; seed1,seed2 : Cardinal);extdecl;
  rnd : function(rand : Prand_struct) : double;extdecl;
  make_scrambled_password : procedure(toarg, passwd : Pchar);extdecl;
  get_salt_from_password : procedure(res : pcardinal; password : pchar);extdecl;
  scramble : procedure(toarg,message,password : pchar; old_ver : my_bool);extdecl;
  check_scramble : function(scramble,message : pchar; salt : cardinal;old_ver:my_bool) : my_bool;extdecl;
  get_tty_password : function(opt_message:  pchar) : pchar;extdecl;

Procedure InitialiseMysql3_com;
Procedure ReleaseMysql3_com;

var Mysql3_comLibraryHandle : TLibHandle;

implementation

var RefCount : integer;

Procedure InitialiseMysql3_com;

begin
  inc(RefCount);
  if RefCount = 1 then
    begin
    Mysql3_comLibraryHandle := loadlibrary(Mysqllib);
    if Mysql3_comLibraryHandle = nilhandle then
      begin
      RefCount := 0;
      Raise EInOutError.Create('Can not load MySQL client. Is it installed? ('+Mysqllib+')');
      end;

    pointer(sql_free) := GetProcedureAddress(Mysql3_comLibraryHandle,'sql_free');
    pointer(init_alloc_root) := GetProcedureAddress(Mysql3_comLibraryHandle,'init_alloc_root');
    pointer(sql_alloc_first_block) := GetProcedureAddress(Mysql3_comLibraryHandle,'sql_alloc_first_block');
    pointer(sql_alloc_root) := GetProcedureAddress(Mysql3_comLibraryHandle,'sql_alloc_root');
    pointer(sql_strdup_root) := GetProcedureAddress(Mysql3_comLibraryHandle,'sql_strdup_root');
    pointer(sql_memdup_root) := GetProcedureAddress(Mysql3_comLibraryHandle,'sql_memdup_root');
    pointer(my_net_init) := GetProcedureAddress(Mysql3_comLibraryHandle,'my_net_init');
    pointer(net_end) := GetProcedureAddress(Mysql3_comLibraryHandle,'net_end');
    pointer(net_clear) := GetProcedureAddress(Mysql3_comLibraryHandle,'net_clear');
    pointer(net_flush) := GetProcedureAddress(Mysql3_comLibraryHandle,'net_flush');
    pointer(my_net_write) := GetProcedureAddress(Mysql3_comLibraryHandle,'my_net_write');
    pointer(net_write_command) := GetProcedureAddress(Mysql3_comLibraryHandle,'net_write_command');
    pointer(net_real_write) := GetProcedureAddress(Mysql3_comLibraryHandle,'net_real_write');
    pointer(my_net_read) := GetProcedureAddress(Mysql3_comLibraryHandle,'my_net_read');
    pointer(randominit) := GetProcedureAddress(Mysql3_comLibraryHandle,'randominit');
    pointer(rnd) := GetProcedureAddress(Mysql3_comLibraryHandle,'rnd');
    pointer(make_scrambled_password) := GetProcedureAddress(Mysql3_comLibraryHandle,'make_scrambled_password');
    pointer(get_salt_from_password) := GetProcedureAddress(Mysql3_comLibraryHandle,'get_salt_from_password');
    pointer(scramble) := GetProcedureAddress(Mysql3_comLibraryHandle,'scramble');
    pointer(check_scramble) := GetProcedureAddress(Mysql3_comLibraryHandle,'check_scramble');
    pointer(get_tty_password) := GetProcedureAddress(Mysql3_comLibraryHandle,'get_tty_password');
    end;
end;

Procedure ReleaseMysql3_com;

begin
  if RefCount > 0 then dec(RefCount);
  if RefCount = 0 then
    begin
    if not UnloadLibrary(Mysql3_comLibraryHandle) then inc(RefCount);
    end;
end;

end.
