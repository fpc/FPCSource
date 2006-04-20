unit mysql3_com;

{$undef use_mysql_321} { if undefined, use mysql 3.23 interface }

{ updated to match version 3.23 header files of mysql by Bernhard Steffen
  (bernhard.steffen@gmx.net)

  split into mysql/mysqldyn libraries by Bram Kuijvenhoven (Hexis BV, The Netherlands)
}

{$mode objfpc}{$h+}
{$macro on}

interface

uses
  mysql3_version;

{$IFDEF Unix}
  {$DEFINE extdecl:=cdecl}
{$ENDIF}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
{$ENDIF}

{$ifndef Windows}
{$linklib c}
{$linklib m}
{$linklib mysqlclient}
{$endif}
{$r+,i+,o+}

{
 Common definition between mysql server & client
}

{$packrecords c}

{$i mysql3_comtypes.inc}

  { Prototypes to password functions }

Procedure sql_free (root : PMEM_ROOT);extdecl;external;
Procedure init_alloc_root (root: PMEM_ROOT;block_size : Cardinal);extdecl;external;
Function sql_alloc_first_block(root : PMEM_ROOT) : my_bool;extdecl;external;
Function sql_alloc_root(mem_root : PMEM_ROOT;len : Cardinal) : longint;extdecl;external;
Function sql_strdup_root(root : PMEM_ROOT;st : pchar) : pchar;extdecl;external;
Function sql_memdup_root(root: PMEM_ROOT;st : pchar; len : Cardinal) : longint;extdecl;external;
Function  my_net_init(net :PNET; fd : Socket) : Longint;extdecl;external;
procedure net_end(net : PNET);extdecl;external;
Procedure net_clear(net : PNET);extdecl;external;
Function  net_flush(net : PNET) : longint;extdecl;external;
Function  my_net_write(net : PNET;packet : pbyte;len : cardinal) : longint;extdecl;external;
Function  net_write_command(net : PNET; command : char;packet : pbyte;len : cardinal) : longint;extdecl;external;
Function  net_real_write(net : PNET;packet : pbyte; len : Cardinal) : longint;extdecl;external;
Function  my_net_read(net : PNET) : Cardinal;extdecl;external;
procedure randominit(rand : Prand_struct; seed1,seed2 : Cardinal);extdecl;external;
Function  rnd(rand : Prand_struct) : double;extdecl;external;
procedure make_scrambled_password(toarg, passwd : Pchar);extdecl;external;
procedure get_salt_from_password(res : pcardinal; password : pchar);extdecl;external;
procedure scramble(toarg,message,password : pchar; old_ver : my_bool);extdecl;external;
function  check_scramble(scramble,message : pchar; salt : cardinal;old_ver:my_bool) : my_bool;extdecl;external;
function  get_tty_password(opt_message:  pchar) : pchar;extdecl;external;

(*
#define NULL_LENGTH ((unsigned long) ~0) { For net_store_length }
*)

implementation


end.
