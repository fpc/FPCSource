unit mysql_com;

interface

{$ifndef win32}
  {$linklib mysqlclient}
  {$linklib m}
  {$linklib c}
{$endif}

{
 Common definition between mysql server & client
}

{$packrecords 4}
{ Extra types introduced for pascal }
Type
  pbyte = ^byte;
  pcardinal = ^cardinal;
  Socket = longint;
  my_bool = byte;

Const
 NAME_LEN  = 64 ;               { Field/table name length }
 LOCAL_HOST : pchar = 'localhost' ;

 MYSQL_PORT = 3306;             { Alloced by ISI for MySQL }
 MYSQL_UNIX_ADDR  : pchar = '/tmp/mysql.sock';

Type
 enum_server_command = ( COM_SLEEP,COM_QUIT,COM_INIT_DB,COM_QUERY,
                          COM_FIELD_LIST,COM_CREATE_DB,COM_DROP_DB,COM_REFRESH,
                          COM_SHUTDOWN,COM_STATISTICS,
                          COM_PROCESS_INFO,COM_CONNECT,COM_PROCESS_KILL,
                          COM_DEBUG);

Const
 NOT_NULL_FLAG       = 1;               { Field can't be NULL }
 PRI_KEY_FLAG        = 2;               { Field is part of a primary key }
 UNIQUE_KEY_FLAG     = 4;               { Field is part of a unique key }
 MULTIPLE_KEY_FLAG   = 8;               { Field is part of a key }
 BLOB_FLAG           = 16;              { Field is a blob }
 UNSIGNED_FLAG       = 32;              { Field is unsigned }
 ZEROFILL_FLAG       = 64;              { Field is zerofill }
 BINARY_FLAG         = 128;
{ The following are only sent to new clients }
 ENUM_FLAG           = 256;             { field is an enum }
 AUTO_INCREMENT_FLAG = 512;             { field is a autoincrement field }
 TIMESTAMP_FLAG      = 1024;            { Field is a timestamp }
 PART_KEY_FLAG       = 16384;           { Intern; Part of some key }
 GROUP_FLAG          = 32768;           { Intern group field }

 REFRESH_GRANT          = 1;    { Refresh grant tables }
 REFRESH_LOG            = 2;    { Start on new log file }
 REFRESH_TABLES         = 4;    { close all tables }

 CLIENT_LONG_PASSWORD   = 1;    { new more secure passwords }
 CLIENT_FOUND_ROWS      = 2;    { Found instead of affected rows }
 CLIENT_LONG_FLAG       = 4;    { Get all column flags }

Type
pst_used_mem = ^st_used_mem;
st_used_mem  = record                           { struct for once_alloc }
  next : pst_used_mem;                          { Next block in use }
  left : cardinal;                              { memory left in block  }
  size : cardinal;                              { size of block }
end;

TUSED_MEM = st_used_mem;
PUSED_MEM = ^TUSED_MEM;

TError_handler = Procedure;

st_mem_root =  record
  free : PUSED_MEM;
  used : PUSED_MEM;
  min_malloc : cardinal;
  block_size : cardinal;
  error_handler : TERROR_Handler;
end;
TMEM_ROOT = st_mem_root;
PMEM_ROOT = ^TMEM_ROOT;

Const
 MYSQL_ERRMSG_SIZE = 200;

Type
net_type = (NET_TYPE_TCPIP, NET_TYPE_SOCKET, NETTYPE_NAMEDPIPE);
st_net  = record
  nettype : net_type; //DT
  fd : Socket;
  fcntl : Longint;
  buff,buff_end,write_pos,read_pos : Pchar;//DT
  last_error : array [0..MYSQL_ERRMSG_SIZE-1] of char;
  last_errno,max_packet,timeout,pkt_nr : Cardinal;
  error,return_errno : my_bool;
  compress : my_bool; //DT

  remain_in_buf,r_length, buf_length, where_b : cardinal; //DT
  more : my_bool;//DT
  save_char : char; //DT
end;
TNET = st_net;
PNET = ^TNET;

Const
  packet_error : longint = -1;

Type
 enum_field_types = ( FIELD_TYPE_DECIMAL, FIELD_TYPE_TINY,
                        FIELD_TYPE_SHORT,  FIELD_TYPE_LONG,
                        FIELD_TYPE_FLOAT,  FIELD_TYPE_DOUBLE,
                        FIELD_TYPE_NULL,   FIELD_TYPE_TIMESTAMP,
                        FIELD_TYPE_LONGLONG,FIELD_TYPE_INT24,
                        FIELD_TYPE_DATE,   FIELD_TYPE_TIME,
                        FIELD_TYPE_DATETIME,
                        FIELD_TYPE_ENUM := 247,
                        FIELD_TYPE_SET := 248,
                        FIELD_TYPE_TINY_BLOB := 249,
                        FIELD_TYPE_MEDIUM_BLOB := 250,
                        FIELD_TYPE_LONG_BLOB :=251,
                        FIELD_TYPE_BLOB :=252,
                        FIELD_TYPE_VAR_STRING :=253,
                        FIELD_TYPE_STRING:=254);

Const
FIELD_TYPE_CHAR = FIELD_TYPE_TINY;              { For compability }
FIELD_TYPE_INTERVAL = FIELD_TYPE_ENUM;          { For compability }

Procedure sql_free (root : PMEM_ROOT);{$ifdef win32} stdcall {$else} cdecl {$endif};
Procedure init_alloc_root (root: PMEM_ROOT;block_size : Cardinal);{$ifdef win32} stdcall {$else} cdecl {$endif};
Function sql_alloc_first_block(root : PMEM_ROOT) : my_bool;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function sql_alloc_root(mem_root : PMEM_ROOT;len : Cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function sql_strdup_root(root : PMEM_ROOT;st : pchar) : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function sql_memdup_root(root: PMEM_ROOT;st : pchar; len : Cardinal): longint;{$ifdef win32} stdcall {$else} cdecl {$endif};

{
extern unsigned long max_allowed_packet;
extern unsigned long net_buffer_length;
}

{
#define net_new_transaction(net) ((net)->pkt_nr=0)
}

Function  my_net_init(net :PNET; fd : Socket) : Longint;{$ifdef win32} stdcall {$else} cdecl {$endif};
procedure net_end(net : PNET);{$ifdef win32} stdcall {$else} cdecl {$endif};
Procedure net_clear(net : PNET);{$ifdef win32} stdcall {$else} cdecl {$endif};
Function  net_flush(net : PNET) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function  my_net_write(net : PNET;packet : pbyte;len : cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function  net_write_command(net : PNET; command : char;packet : pbyte;len : cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function  net_real_write(net : PNET;packet : pbyte; len : Cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};
Function  my_net_read(net : PNET) : Cardinal;{$ifdef win32} stdcall {$else} cdecl {$endif};

Type
TRand_struct  = record
  seed,seed2,max_value : Cardinal;
  max_value_dbl : double;
end;
PRand_struct = ^TRand_struct;

{ The following is for user defined functions }

Item_result = (STRING_RESULT,REAL_RESULT,INT_RESULT);

st_udf_args = record
  arg_count : cardinal;                 { Number of arguments }
  arg_type : ^Item_result;              { Pointer to item_results }
  args : ppchar;                        { Pointer to argument }
  lengths : PCardinal;                  { Length of string arguments }
end;
TUDF_ARGS = st_udf_args;
PUDPF_ARGS = ^TUDF_ARGS;

  { This holds information about the result }

st_udf_init = record
  maybe_null : my_bool;                 { 1 if function can return NULL }
  decimals : cardinal;                  { for real functions }
  max_length : Cardinal;                { For string functions }
  ptr : PChar;                          { free pointer for function data }
end;
TUDF_INIT = st_udf_init;
PUDF_INIT = TUDF_INIT;

  { Prototypes to password functions }

procedure randominit(rand : Prand_struct; seed1,seed2 : Cardinal);{$ifdef win32} stdcall {$else} cdecl {$endif};
Function  rnd(rand : Prand_struct) : double;{$ifdef win32} stdcall {$else} cdecl {$endif};
procedure make_scrambled_password(toarg, passwd : Pchar);{$ifdef win32} stdcall {$else} cdecl {$endif};
procedure get_salt_from_password(res : pcardinal; password : pchar);{$ifdef win32} stdcall {$else} cdecl {$endif};
procedure scramble(toarg,message,password : pchar; old_ver : my_bool);{$ifdef win32} stdcall {$else} cdecl {$endif};
function  check_scramble(scramble,message : pchar; salt : cardinal;old_ver:my_bool) : my_bool;{$ifdef win32} stdcall {$else} cdecl {$endif};
function  get_tty_password(opt_message:  pchar) : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif};

{
#define NULL_LENGTH ((unsigned long) ~0) { For net_store_length }
}

implementation

Procedure sql_free (root : PMEM_ROOT);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Procedure init_alloc_root (root: PMEM_ROOT;block_size : Cardinal);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function sql_alloc_first_block(root : PMEM_ROOT) : my_bool;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function sql_alloc_root(mem_root : PMEM_ROOT;len : Cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function sql_strdup_root(root : PMEM_ROOT;st : pchar) : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function sql_memdup_root(root: PMEM_ROOT;st : pchar; len : Cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  my_net_init(net :PNET; fd : Socket) : Longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
procedure net_end(net : PNET);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Procedure net_clear(net : PNET);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  net_flush(net : PNET) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  my_net_write(net : PNET;packet : pbyte;len : cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  net_write_command(net : PNET; command : char;packet : pbyte;len : cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  net_real_write(net : PNET;packet : pbyte; len : Cardinal) : longint;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  my_net_read(net : PNET) : Cardinal;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
procedure randominit(rand : Prand_struct; seed1,seed2 : Cardinal);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
Function  rnd(rand : Prand_struct) : double;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
procedure make_scrambled_password(toarg, passwd : Pchar);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
procedure get_salt_from_password(res : pcardinal; password : pchar);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
procedure scramble(toarg,message,password : pchar; old_ver : my_bool);{$ifdef win32} stdcall {$else} cdecl {$endif};external;
function  check_scramble(scramble,message : pchar; salt : cardinal;old_ver:my_bool) : my_bool;{$ifdef win32} stdcall {$else} cdecl {$endif};external;
function  get_tty_password(opt_message:  pchar) : pchar;{$ifdef win32} stdcall {$else} cdecl {$endif};external;

end.
