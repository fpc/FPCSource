{
  Contains the Interbase/Firebird-functions calls
  In this stage only the calls needed for IBConnection are implemented
  Other calls could be simply implemented, using copy-paste from ibase60

  Call InitialiseIbase60 before using any of the calls, and call ReleaseIbase60
  when finished.
}

unit ibase60dyn;

{$mode objfpc}{$H+}
{$MACRO on}

interface

uses
  dynlibs,sysutils;

{$IFDEF Unix}
  {$LINKLIB c}
  {$LINKLIB crypt}
  {$DEFINE extdecl:=cdecl}
  const
    gdslib = 'libgds.so';
    fbclib = 'libfbclient.so';
{$ENDIF}
{$IFDEF Win32}
  {$DEFINE extdecl:=stdcall}
  const
    gdslib = 'gds32.dll';
    fbclib = 'fbclient.dll';
{$ENDIF}

{$i ibase60types.inc}

var

{                          }
{ OSRI database functions  }
{                          }
  isc_attach_database : function (_para1:PISC_STATUS; _para2:smallint; _para3:Pchar; _para4:Pisc_db_handle; _para5:smallint;_para6:Pchar):ISC_STATUS; extdecl;
  isc_array_gen_sdl : function (_para1:PISC_STATUS; _para2:PISC_ARRAY_DESC; _para3:Psmallint; _para4:Pchar; _para5:Psmallint):ISC_STATUS; extdecl;
  isc_array_get_slice : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:PISC_ARRAY_DESC;_para6:pointer; _para7:PISC_LONG):ISC_STATUS; extdecl;
  isc_array_lookup_bounds : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:Pchar;_para6:PISC_ARRAY_DESC):ISC_STATUS; extdecl;
  isc_array_lookup_desc : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:Pchar;_para6:PISC_ARRAY_DESC):ISC_STATUS; extdecl;
  isc_array_set_desc : function (_para1:PISC_STATUS; _para2:Pchar; _para3:Pchar; _para4:Psmallint; _para5:Psmallint;_para6:Psmallint; _para7:PISC_ARRAY_DESC):ISC_STATUS; extdecl;
  isc_array_put_slice : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:PISC_ARRAY_DESC;_para6:pointer; _para7:PISC_LONG):ISC_STATUS; extdecl;
  isc_blob_default_desc : procedure (_para1:PISC_BLOB_DESC; _para2:Pbyte; _para3:Pbyte); extdecl;
  isc_blob_gen_bpb : function (_para1:PISC_STATUS; _para2:PISC_BLOB_DESC; _para3:PISC_BLOB_DESC; _para4:word; _para5:Pbyte;_para6:Pword):ISC_STATUS; extdecl;
  isc_blob_info : function (_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:smallint; _para4:Pchar; _para5:smallint;_para6:Pchar):ISC_STATUS; extdecl;
  isc_blob_lookup_desc : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pbyte; _para5:Pbyte;_para6:PISC_BLOB_DESC; _para7:Pbyte):ISC_STATUS; extdecl;
  isc_blob_set_desc : function (_para1:PISC_STATUS; _para2:Pbyte; _para3:Pbyte; _para4:smallint; _para5:smallint;_para6:smallint; _para7:PISC_BLOB_DESC):ISC_STATUS; extdecl;
  isc_cancel_blob : function (_para1:PISC_STATUS; _para2:Pisc_blob_handle):ISC_STATUS; extdecl;
  isc_cancel_events : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:PISC_LONG):ISC_STATUS; extdecl;
  isc_close_blob : function (_para1:PISC_STATUS; _para2:Pisc_blob_handle):ISC_STATUS; extdecl;
  isc_commit_retaining : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl;
  isc_commit_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl;
  isc_create_blob : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD):ISC_STATUS; extdecl;
  isc_create_blob2 : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD;_para6:smallint; _para7:Pchar):ISC_STATUS; extdecl;
  isc_create_database : function (_para1:PISC_STATUS; _para2:smallint; _para3:Pchar; _para4:Pisc_db_handle; _para5:smallint;_para6:Pchar; _para7:smallint):ISC_STATUS; extdecl;
  isc_database_info : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:smallint; _para4:Pchar; _para5:smallint;_para6:Pchar):ISC_STATUS; extdecl;
  isc_decode_date : procedure (_para1:PISC_QUAD; _para2:pointer); extdecl;
  isc_decode_sql_date : procedure (_para1:PISC_DATE; _para2:pointer); extdecl;
  isc_decode_sql_time : procedure (_para1:PISC_TIME; _para2:pointer); extdecl;
  isc_decode_timestamp : procedure (_para1:PISC_TIMESTAMP; _para2:pointer); extdecl;
  isc_detach_database : function (_para1:PISC_STATUS; _para2:Pisc_db_handle):ISC_STATUS; extdecl;
  isc_drop_database : function (_para1:PISC_STATUS; _para2:Pisc_db_handle):ISC_STATUS; extdecl;
  isc_dsql_allocate_statement : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_stmt_handle):ISC_STATUS; extdecl;
  isc_dsql_alloc_statement2 : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_stmt_handle):ISC_STATUS; extdecl;
  isc_dsql_describe : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_describe_bind : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_exec_immed2 : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;_para6:word; _para7:PXSQLDA; _para8:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_execute : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_execute2 : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:PXSQLDA;_para6:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_execute_immediate : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;_para6:word; _para7:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_fetch : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_finish : function (_para1:Pisc_db_handle):ISC_STATUS; extdecl;
  isc_dsql_free_statement : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word):ISC_STATUS; extdecl;
  isc_dsql_insert : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_prepare : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;_para6:word; _para7:PXSQLDA):ISC_STATUS; extdecl;
  isc_dsql_set_cursor_name : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:Pchar; _para4:word):ISC_STATUS; extdecl;
  isc_dsql_sql_info : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:smallint; _para4:Pchar; _para5:smallint;_para6:Pchar):ISC_STATUS; extdecl;
  isc_encode_date : procedure (_para1:pointer; _para2:PISC_QUAD); extdecl;
  isc_encode_sql_date : procedure (_para1:pointer; _para2:PISC_DATE); extdecl;
  isc_encode_sql_time : procedure (_para1:pointer; _para2:PISC_TIME); extdecl;
  isc_encode_timestamp : procedure (_para1:pointer; _para2:PISC_TIMESTAMP); extdecl;
  isc_event_block : function (_para1:PPchar; _para2:PPchar; _para3:word; args:array of const):ISC_LONG; cdecl;
{!!MVC
  void         isc_event_counts (unsigned ISC_LONG   ,
  short,
  char   ,
  char   ); extdecl; external gdslib;
!!MVC }
  isc_expand_dpb : procedure (_para1:PPchar; _para2:Psmallint; args:array of const); cdecl;
  isc_modify_dpb : function (_para1:PPchar; _para2:Psmallint; _para3:word; _para4:Pchar; _para5:smallint):longint; extdecl;
  isc_free : function (_para1:Pchar):ISC_LONG; extdecl;
  isc_get_segment : function (_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:Pword; _para4:word; _para5:Pchar):ISC_STATUS; extdecl;
  isc_get_slice : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:smallint;_para6:Pchar; _para7:smallint; _para8:PISC_LONG; _para9:ISC_LONG; _para10:pointer;_para11:PISC_LONG):ISC_STATUS; extdecl;
  isc_interprete : function (_para1:Pchar; _para2:PPISC_STATUS):ISC_STATUS; extdecl;
  isc_open_blob : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD):ISC_STATUS; extdecl;
  isc_open_blob2 : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD;_para6:smallint; _para7:Pchar):ISC_STATUS; extdecl;
  isc_prepare_transaction2 : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; _para4:Pchar):ISC_STATUS; extdecl;
  isc_print_sqlerror : procedure (_para1:smallint; _para2:PISC_STATUS); extdecl;
  isc_print_status : function (_para1:PISC_STATUS):ISC_STATUS; extdecl;
  isc_put_segment : function (_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:word; _para4:Pchar):ISC_STATUS; extdecl;
  isc_put_slice : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:smallint;_para6:Pchar; _para7:smallint; _para8:PISC_LONG; _para9:ISC_LONG; _para10:pointer):ISC_STATUS; extdecl;
  isc_que_events : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:PISC_LONG; _para4:smallint; _para5:Pchar;_para6:isc_callback; _para7:pointer):ISC_STATUS; extdecl;
  isc_rollback_retaining : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl;
  isc_rollback_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl;
  isc_start_multiple : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; _para4:pointer):ISC_STATUS; extdecl;
  isc_start_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; args:array of const):ISC_STATUS; cdecl;
  isc_sqlcode : function (_para1:PISC_STATUS):ISC_LONG; extdecl;
  isc_sql_interprete : procedure (_para1:smallint; _para2:Pchar; _para3:smallint); extdecl;
  isc_transaction_info : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; _para4:Pchar; _para5:smallint;_para6:Pchar):ISC_STATUS; extdecl;
  isc_transact_request : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;_para6:word; _para7:Pchar; _para8:word; _para9:Pchar):ISC_STATUS; extdecl;
  isc_vax_integer : function (_para1:Pchar; _para2:smallint):ISC_LONG; extdecl;
  isc_portable_integer : function (_para1:Pbyte; _para2:smallint):ISC_INT64; extdecl;
{                                    }
{ Security Functions                 }
{                                    }
  isc_add_user : function (_para1:PISC_STATUS; _para2:PUSER_SEC_DATA):longint; extdecl;
  isc_delete_user : function (_para1:PISC_STATUS; _para2:PUSER_SEC_DATA):longint; extdecl;
  isc_modify_user : function (_para1:PISC_STATUS; _para2:PUSER_SEC_DATA):longint; extdecl;
{                                 }
{  Other OSRI functions           }
{                                 }
  isc_compile_request : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_req_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_compile_request2 : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_req_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_ddl : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_prepare_transaction : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl;
  isc_receive : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:smallint; _para4:smallint; _para5:pointer;_para6:smallint):ISC_STATUS; extdecl;
  isc_reconnect_transaction : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_release_request : function (_para1:PISC_STATUS; _para2:Pisc_req_handle):ISC_STATUS; extdecl;
  isc_request_info : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:smallint; _para4:smallint; _para5:Pchar;_para6:smallint; _para7:Pchar):ISC_STATUS; extdecl;
  isc_seek_blob : function (_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:smallint; _para4:ISC_LONG; _para5:PISC_LONG):ISC_STATUS; extdecl;
  isc_send : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:smallint; _para4:smallint; _para5:pointer;_para6:smallint):ISC_STATUS; extdecl;
  isc_start_and_send : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Pisc_tr_handle; _para4:smallint; _para5:smallint;_para6:pointer; _para7:smallint):ISC_STATUS; extdecl;
  isc_start_request : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Pisc_tr_handle; _para4:smallint):ISC_STATUS; extdecl;
  isc_unwind_request : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint):ISC_STATUS; extdecl;
  isc_wait_for_event : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:smallint; _para4:Pchar; _para5:Pchar):ISC_STATUS; extdecl;
{                            }
{ Other Sql functions        }
{                            }
  isc_close : function (_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl;
  isc_declare : function (_para1:PISC_STATUS; _para2:Pchar; _para3:Pchar):ISC_STATUS; extdecl;
  isc_describe : function (_para1:PISC_STATUS; _para2:Pchar; _para3:PXSQLDA):ISC_STATUS; extdecl;
  isc_describe_bind : function (_para1:PISC_STATUS; _para2:Pchar; _para3:PXSQLDA):ISC_STATUS; extdecl;
  isc_execute : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_execute_immediate : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_fetch : function (_para1:PISC_STATUS; _para2:Pchar; _para3:PXSQLDA):ISC_STATUS; extdecl;
  isc_open : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_prepare : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:Psmallint;_para6:Pchar; _para7:PXSQLDA):ISC_STATUS; extdecl;
{                                    }
{ Other Dynamic sql functions        }
{                                    }
  isc_dsql_execute_m : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;_para6:word; _para7:word; _para8:Pchar):ISC_STATUS; extdecl;
  isc_dsql_execute2_m : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;_para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:Pchar;_para11:word; _para12:word; _para13:Pchar):ISC_STATUS; extdecl;
  isc_dsql_execute_immediate_m : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;_para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:word;_para11:Pchar):ISC_STATUS; extdecl;
  isc_dsql_exec_immed3_m : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;_para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:word;_para11:Pchar; _para12:word; _para13:Pchar; _para14:word; _para15:word;_para16:Pchar):ISC_STATUS; extdecl;
  isc_dsql_fetch_m : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:Pchar; _para5:word;_para6:word; _para7:Pchar):ISC_STATUS; extdecl;
  isc_dsql_insert_m : function (_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:Pchar; _para5:word;_para6:word; _para7:Pchar):ISC_STATUS; extdecl;
  isc_dsql_prepare_m : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;_para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:Pchar):ISC_STATUS; extdecl;
  isc_dsql_release : function (_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl;
  isc_embed_dsql_close : function (_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl;
  isc_embed_dsql_declare : function (_para1:PISC_STATUS; _para2:Pchar; _para3:Pchar):ISC_STATUS; extdecl;
  isc_embed_dsql_describe : function (_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_describe_bind : function (_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_execute : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_execute2 : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA;_para6:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_execute_immed : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;_para6:word; _para7:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_fetch : function (_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_open : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_open2 : function (_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA;_para6:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_insert : function (_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_prepare : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:word;_para6:Pchar; _para7:word; _para8:PXSQLDA):ISC_STATUS; extdecl;
  isc_embed_dsql_release : function (_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl;
{                             }
{ Other Blob functions        }
{                             }
  BLOB_open : function (_para1:isc_blob_handle; _para2:Pchar; _para3:longint):PBSTREAM; extdecl;
  BLOB_put : function (_para1:char; _para2:PBSTREAM):longint; extdecl;
  BLOB_close : function (_para1:PBSTREAM):longint; extdecl;
  BLOB_get : function (_para1:PBSTREAM):longint; extdecl;
  BLOB_display : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl;
  BLOB_dump : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl;
  BLOB_edit : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl;
  BLOB_load : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl;
  BLOB_text_dump : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl;
  BLOB_text_load : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl;
  Bopen : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):PBSTREAM; extdecl;
{$IFDEF Unix}
  Bopen2 : function (_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar; _para5:word):PBSTREAM; extdecl;
{$ENDIF}
{                             }
{ Other Misc functions        }
{                             }
  isc_ftof : function (_para1:Pchar; _para2:word; _para3:Pchar; _para4:word):ISC_LONG; extdecl;
  isc_print_blr : function (_para1:Pchar; _para2:isc_callback; _para3:pointer; _para4:smallint):ISC_STATUS; extdecl;
  isc_set_debug : procedure (_para1:longint); extdecl;
  isc_qtoq : procedure (_para1:PISC_QUAD; _para2:PISC_QUAD); extdecl;
  isc_vtof : procedure (_para1:Pchar; _para2:Pchar; _para3:word); extdecl;
  isc_vtov : procedure (_para1:Pchar; _para2:Pchar; _para3:smallint); extdecl;
  isc_version : function (_para1:Pisc_db_handle; _para2:isc_callback; _para3:pointer):longint; extdecl;
{$IFDEF Unix}
  isc_reset_fpe : function (_para1:word):ISC_LONG; extdecl;
{$ENDIF}
{                                        }
{ Service manager functions              }
{                                        }
(*!!MVC
  #define ADD_SPB_LENGTH(p, length)     { (p)++ = (length); \
  (p)++ = (length) >> 8;}

#define ADD_SPB_NUMERIC(p, data)      { (p)++ = (data); \
  (p)++ = (data) >> 8; \
  (p)++ = (data) >> 16; \
  (p)++ = (data) >> 24;}
!!MVC *)
  isc_service_attach : function (_para1:PISC_STATUS; _para2:word; _para3:Pchar; _para4:Pisc_svc_handle; _para5:word;_para6:Pchar):ISC_STATUS; extdecl;
  isc_service_detach : function (_para1:PISC_STATUS; _para2:Pisc_svc_handle):ISC_STATUS; extdecl;
  isc_service_query : function (_para1:PISC_STATUS; _para2:Pisc_svc_handle; _para3:Pisc_resv_handle; _para4:word; _para5:Pchar;_para6:word; _para7:Pchar; _para8:word; _para9:Pchar):ISC_STATUS; extdecl;
  isc_service_start : function (_para1:PISC_STATUS; _para2:Pisc_svc_handle; _para3:Pisc_resv_handle; _para4:word; _para5:Pchar):ISC_STATUS; extdecl;
{                              }
{ Forms functions              }
{                              }
{$IFDEF Unix}
  isc_compile_map : function (_para1:PISC_STATUS; _para2:Pisc_form_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_compile_menu : function (_para1:PISC_STATUS; _para2:Pisc_form_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_compile_sub_map : function (_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_create_window : function (_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Psmallint; _para4:Pchar; _para5:Psmallint;_para6:Psmallint):ISC_STATUS; extdecl;
  isc_delete_window : function (_para1:PISC_STATUS; _para2:Pisc_win_handle):ISC_STATUS; extdecl;
  isc_drive_form : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_win_handle; _para5:Pisc_req_handle;_para6:Pbyte; _para7:Pbyte):ISC_STATUS; extdecl;
  isc_drive_menu : function (_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar;_para6:Psmallint; _para7:Pchar; _para8:Psmallint; _para9:Psmallint; _para10:Pchar;_para11:PISC_LONG):ISC_STATUS; extdecl;
  isc_form_delete : function (_para1:PISC_STATUS; _para2:Pisc_form_handle):ISC_STATUS; extdecl;
  isc_form_fetch : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_req_handle; _para5:Pbyte):ISC_STATUS; extdecl;
  isc_form_insert : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_req_handle; _para5:Pbyte):ISC_STATUS; extdecl;
  isc_get_entree : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Psmallint; _para4:Pchar; _para5:PISC_LONG;_para6:Psmallint):ISC_STATUS; extdecl;
  isc_initialize_menu : function (_para1:PISC_STATUS; _para2:Pisc_req_handle):ISC_STATUS; extdecl;
  isc_menu : function (_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl;
  isc_load_form : function (_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_form_handle; _para5:Psmallint;_para6:Pchar):ISC_STATUS; extdecl;
  isc_pop_window : function (_para1:PISC_STATUS; _para2:Pisc_win_handle):ISC_STATUS; extdecl;
  isc_put_entree : function (_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Psmallint; _para4:Pchar; _para5:PISC_LONG):ISC_STATUS; extdecl;
  isc_reset_form : function (_para1:PISC_STATUS; _para2:Pisc_req_handle):ISC_STATUS; extdecl;
  isc_suspend_window : function (_para1:PISC_STATUS; _para2:Pisc_win_handle):ISC_STATUS; extdecl;
{$ENDIF}

Procedure InitialiseIBase60;
Procedure ReleaseIBase60;

var IBaseLibraryHandle : TLibHandle;

implementation

var RefCount : integer;

Procedure InitialiseIBase60;

begin
  inc(RefCount);
  if RefCount = 1 then
    begin
    IBaseLibraryHandle := loadlibrary(fbclib);
    if IBaseLibraryHandle = nilhandle then
      begin
      IBaseLibraryHandle := loadlibrary(gdslib);
      if loadlibrary(gdslib) = nilhandle then
        begin
        RefCount := 0;
        Raise EInOutError.Create('Can not load Firebird or Interbase client. Is it installed? ('+gdslib+' or '+fbclib+')');
        end;
      end;

    pointer(isc_attach_database) := GetProcedureAddress(IBaseLibraryHandle,'isc_attach_database');
    pointer(isc_array_gen_sdl) := GetProcedureAddress(IBaseLibraryHandle,'isc_array_gen_sdl');
    pointer(isc_array_get_slice) := GetProcedureAddress(IBaseLibraryHandle,'isc_array_get_slice');
    pointer(isc_array_lookup_bounds) := GetProcedureAddress(IBaseLibraryHandle,'isc_array_lookup_bounds');
    pointer(isc_array_lookup_desc) := GetProcedureAddress(IBaseLibraryHandle,'isc_array_lookup_desc');
    pointer(isc_array_set_desc) := GetProcedureAddress(IBaseLibraryHandle,'isc_array_set_desc');
    pointer(isc_array_put_slice) := GetProcedureAddress(IBaseLibraryHandle,'isc_array_put_slice');
    pointer(isc_blob_default_desc) := GetProcedureAddress(IBaseLibraryHandle,'isc_blob_default_desc');
    pointer(isc_blob_gen_bpb) := GetProcedureAddress(IBaseLibraryHandle,'isc_blob_gen_bpb');
    pointer(isc_blob_info) := GetProcedureAddress(IBaseLibraryHandle,'isc_blob_info');
    pointer(isc_blob_lookup_desc) := GetProcedureAddress(IBaseLibraryHandle,'isc_blob_lookup_desc');
    pointer(isc_blob_set_desc) := GetProcedureAddress(IBaseLibraryHandle,'isc_blob_set_desc');
    pointer(isc_cancel_blob) := GetProcedureAddress(IBaseLibraryHandle,'isc_cancel_blob');
    pointer(isc_cancel_events) := GetProcedureAddress(IBaseLibraryHandle,'isc_cancel_events');
    pointer(isc_close_blob) := GetProcedureAddress(IBaseLibraryHandle,'isc_close_blob');
    pointer(isc_commit_retaining) := GetProcedureAddress(IBaseLibraryHandle,'isc_commit_retaining');
    pointer(isc_commit_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_commit_transaction');
    pointer(isc_create_blob) := GetProcedureAddress(IBaseLibraryHandle,'isc_create_blob');
    pointer(isc_create_blob2) := GetProcedureAddress(IBaseLibraryHandle,'isc_create_blob2');
    pointer(isc_create_database) := GetProcedureAddress(IBaseLibraryHandle,'isc_create_database');
    pointer(isc_database_info) := GetProcedureAddress(IBaseLibraryHandle,'isc_database_info');
    pointer(isc_decode_date) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_date');
    pointer(isc_decode_sql_date) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_sql_date');
    pointer(isc_decode_sql_time) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_sql_time');
    pointer(isc_decode_timestamp) := GetProcedureAddress(IBaseLibraryHandle,'isc_decode_timestamp');
    pointer(isc_detach_database) := GetProcedureAddress(IBaseLibraryHandle,'isc_detach_database');
    pointer(isc_drop_database) := GetProcedureAddress(IBaseLibraryHandle,'isc_drop_database');
    pointer(isc_dsql_allocate_statement) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_allocate_statement');
    pointer(isc_dsql_alloc_statement2) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_alloc_statement2');
    pointer(isc_dsql_describe) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_describe');
    pointer(isc_dsql_describe_bind) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_describe_bind');
    pointer(isc_dsql_exec_immed2) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_exec_immed2');
    pointer(isc_dsql_execute) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute');
    pointer(isc_dsql_execute2) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute2');
    pointer(isc_dsql_execute_immediate) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute_immediate');
    pointer(isc_dsql_fetch) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_fetch');
    pointer(isc_dsql_finish) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_finish');
    pointer(isc_dsql_free_statement) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_free_statement');
    pointer(isc_dsql_insert) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_insert');
    pointer(isc_dsql_prepare) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_prepare');
    pointer(isc_dsql_set_cursor_name) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_set_cursor_name');
    pointer(isc_dsql_sql_info) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_sql_info');
    pointer(isc_encode_date) := GetProcedureAddress(IBaseLibraryHandle,'isc_encode_date');
    pointer(isc_encode_sql_date) := GetProcedureAddress(IBaseLibraryHandle,'isc_encode_sql_date');
    pointer(isc_encode_sql_time) := GetProcedureAddress(IBaseLibraryHandle,'isc_encode_sql_time');
    pointer(isc_encode_timestamp) := GetProcedureAddress(IBaseLibraryHandle,'isc_encode_timestamp');
    pointer(isc_event_block) := GetProcedureAddress(IBaseLibraryHandle,'isc_event_block');
    pointer(isc_expand_dpb) := GetProcedureAddress(IBaseLibraryHandle,'isc_expand_dpb');
    pointer(isc_modify_dpb) := GetProcedureAddress(IBaseLibraryHandle,'isc_modify_dpb');
    pointer(isc_free) := GetProcedureAddress(IBaseLibraryHandle,'isc_free');
    pointer(isc_get_segment) := GetProcedureAddress(IBaseLibraryHandle,'isc_get_segment');
    pointer(isc_get_slice) := GetProcedureAddress(IBaseLibraryHandle,'isc_get_slice');
    pointer(isc_interprete) := GetProcedureAddress(IBaseLibraryHandle,'isc_interprete');
    pointer(isc_open_blob) := GetProcedureAddress(IBaseLibraryHandle,'isc_open_blob');
    pointer(isc_open_blob2) := GetProcedureAddress(IBaseLibraryHandle,'isc_open_blob2');
    pointer(isc_prepare_transaction2) := GetProcedureAddress(IBaseLibraryHandle,'isc_prepare_transaction2');
    pointer(isc_print_sqlerror) := GetProcedureAddress(IBaseLibraryHandle,'isc_print_sqlerror');
    pointer(isc_print_status) := GetProcedureAddress(IBaseLibraryHandle,'isc_print_status');
    pointer(isc_put_segment) := GetProcedureAddress(IBaseLibraryHandle,'isc_put_segment');
    pointer(isc_put_slice) := GetProcedureAddress(IBaseLibraryHandle,'isc_put_slice');
    pointer(isc_que_events) := GetProcedureAddress(IBaseLibraryHandle,'isc_que_events');
    pointer(isc_rollback_retaining) := GetProcedureAddress(IBaseLibraryHandle,'isc_rollback_retaining');
    pointer(isc_rollback_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_rollback_transaction');
    pointer(isc_start_multiple) := GetProcedureAddress(IBaseLibraryHandle,'isc_start_multiple');
    pointer(isc_start_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_start_transaction');
    pointer(isc_sqlcode) := GetProcedureAddress(IBaseLibraryHandle,'isc_sqlcode');
    pointer(isc_sql_interprete) := GetProcedureAddress(IBaseLibraryHandle,'isc_sql_interprete');
    pointer(isc_transaction_info) := GetProcedureAddress(IBaseLibraryHandle,'isc_transaction_info');
    pointer(isc_transact_request) := GetProcedureAddress(IBaseLibraryHandle,'isc_transact_request');
    pointer(isc_vax_integer) := GetProcedureAddress(IBaseLibraryHandle,'isc_vax_integer');
    pointer(isc_portable_integer) := GetProcedureAddress(IBaseLibraryHandle,'isc_portable_integer');
    pointer(isc_add_user) := GetProcedureAddress(IBaseLibraryHandle,'isc_add_user');
    pointer(isc_delete_user) := GetProcedureAddress(IBaseLibraryHandle,'isc_delete_user');
    pointer(isc_modify_user) := GetProcedureAddress(IBaseLibraryHandle,'isc_modify_user');
    pointer(isc_compile_request) := GetProcedureAddress(IBaseLibraryHandle,'isc_compile_request');
    pointer(isc_compile_request2) := GetProcedureAddress(IBaseLibraryHandle,'isc_compile_request2');
    pointer(isc_ddl) := GetProcedureAddress(IBaseLibraryHandle,'isc_ddl');
    pointer(isc_prepare_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_prepare_transaction');
    pointer(isc_receive) := GetProcedureAddress(IBaseLibraryHandle,'isc_receive');
    pointer(isc_reconnect_transaction) := GetProcedureAddress(IBaseLibraryHandle,'isc_reconnect_transaction');
    pointer(isc_release_request) := GetProcedureAddress(IBaseLibraryHandle,'isc_release_request');
    pointer(isc_request_info) := GetProcedureAddress(IBaseLibraryHandle,'isc_request_info');
    pointer(isc_seek_blob) := GetProcedureAddress(IBaseLibraryHandle,'isc_seek_blob');
    pointer(isc_send) := GetProcedureAddress(IBaseLibraryHandle,'isc_send');
    pointer(isc_start_and_send) := GetProcedureAddress(IBaseLibraryHandle,'isc_start_and_send');
    pointer(isc_start_request) := GetProcedureAddress(IBaseLibraryHandle,'isc_start_request');
    pointer(isc_unwind_request) := GetProcedureAddress(IBaseLibraryHandle,'isc_unwind_request');
    pointer(isc_wait_for_event) := GetProcedureAddress(IBaseLibraryHandle,'isc_wait_for_event');
    pointer(isc_close) := GetProcedureAddress(IBaseLibraryHandle,'isc_close');
    pointer(isc_declare) := GetProcedureAddress(IBaseLibraryHandle,'isc_declare');
    pointer(isc_describe) := GetProcedureAddress(IBaseLibraryHandle,'isc_describe');
    pointer(isc_describe_bind) := GetProcedureAddress(IBaseLibraryHandle,'isc_describe_bind');
    pointer(isc_execute) := GetProcedureAddress(IBaseLibraryHandle,'isc_execute');
    pointer(isc_execute_immediate) := GetProcedureAddress(IBaseLibraryHandle,'isc_execute_immediate');
    pointer(isc_fetch) := GetProcedureAddress(IBaseLibraryHandle,'isc_fetch');
    pointer(isc_open) := GetProcedureAddress(IBaseLibraryHandle,'isc_open');
    pointer(isc_prepare) := GetProcedureAddress(IBaseLibraryHandle,'isc_prepare');
    pointer(isc_dsql_execute_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute_m');
    pointer(isc_dsql_execute2_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute2_m');
    pointer(isc_dsql_execute_immediate_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_execute_immediate_m');
    pointer(isc_dsql_exec_immed3_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_exec_immed3_m');
    pointer(isc_dsql_fetch_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_fetch_m');
    pointer(isc_dsql_insert_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_insert_m');
    pointer(isc_dsql_prepare_m) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_prepare_m');
    pointer(isc_dsql_release) := GetProcedureAddress(IBaseLibraryHandle,'isc_dsql_release');
    pointer(isc_embed_dsql_close) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_close');
    pointer(isc_embed_dsql_declare) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_declare');
    pointer(isc_embed_dsql_describe) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_describe');
    pointer(isc_embed_dsql_describe_bind) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_describe_bind');
    pointer(isc_embed_dsql_execute) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_execute');
    pointer(isc_embed_dsql_execute2) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_execute2');
    pointer(isc_embed_dsql_execute_immed) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_execute_immed');
    pointer(isc_embed_dsql_fetch) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_fetch');
    pointer(isc_embed_dsql_open) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_open');
    pointer(isc_embed_dsql_open2) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_open2');
    pointer(isc_embed_dsql_insert) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_insert');
    pointer(isc_embed_dsql_prepare) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_prepare');
    pointer(isc_embed_dsql_release) := GetProcedureAddress(IBaseLibraryHandle,'isc_embed_dsql_release');
    pointer(BLOB_open) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_open');
    pointer(BLOB_put) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_put');
    pointer(BLOB_close) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_close');
    pointer(BLOB_get) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_get');
    pointer(BLOB_display) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_display');
    pointer(BLOB_dump) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_dump');
    pointer(BLOB_edit) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_edit');
    pointer(BLOB_load) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_load');
    pointer(BLOB_text_dump) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_text_dump');
    pointer(BLOB_text_load) := GetProcedureAddress(IBaseLibraryHandle,'BLOB_text_load');
    pointer(Bopen) := GetProcedureAddress(IBaseLibraryHandle,'Bopen');
{$IFDEF Unix}
    pointer(Bopen2) := GetProcedureAddress(IBaseLibraryHandle,'Bopen2');
{$ENDIF}
    pointer(isc_ftof) := GetProcedureAddress(IBaseLibraryHandle,'isc_ftof');
    pointer(isc_print_blr) := GetProcedureAddress(IBaseLibraryHandle,'isc_print_blr');
    pointer(isc_set_debug) := GetProcedureAddress(IBaseLibraryHandle,'isc_set_debug');
    pointer(isc_qtoq) := GetProcedureAddress(IBaseLibraryHandle,'isc_qtoq');
    pointer(isc_vtof) := GetProcedureAddress(IBaseLibraryHandle,'isc_vtof');
    pointer(isc_vtov) := GetProcedureAddress(IBaseLibraryHandle,'isc_vtov');
    pointer(isc_version) := GetProcedureAddress(IBaseLibraryHandle,'isc_version');
{$IFDEF Unix}
    pointer(isc_reset_fpe) := GetProcedureAddress(IBaseLibraryHandle,'isc_reset_fpe');
{$ENDIF}
    pointer(isc_service_attach) := GetProcedureAddress(IBaseLibraryHandle,'isc_service_attach');
    pointer(isc_service_detach) := GetProcedureAddress(IBaseLibraryHandle,'isc_service_detach');
    pointer(isc_service_query) := GetProcedureAddress(IBaseLibraryHandle,'isc_service_query');
    pointer(isc_service_start) := GetProcedureAddress(IBaseLibraryHandle,'isc_service_start');
{$IFDEF Unix}
    pointer(isc_compile_map) := GetProcedureAddress(IBaseLibraryHandle,'isc_compile_map');
    pointer(isc_compile_menu) := GetProcedureAddress(IBaseLibraryHandle,'isc_compile_menu');
    pointer(isc_compile_sub_map) := GetProcedureAddress(IBaseLibraryHandle,'isc_compile_sub_map');
    pointer(isc_create_window) := GetProcedureAddress(IBaseLibraryHandle,'isc_create_window');
    pointer(isc_delete_window) := GetProcedureAddress(IBaseLibraryHandle,'isc_delete_window');
    pointer(isc_drive_form) := GetProcedureAddress(IBaseLibraryHandle,'isc_drive_form');
    pointer(isc_drive_menu) := GetProcedureAddress(IBaseLibraryHandle,'isc_drive_menu');
    pointer(isc_form_delete) := GetProcedureAddress(IBaseLibraryHandle,'isc_form_delete');
    pointer(isc_form_fetch) := GetProcedureAddress(IBaseLibraryHandle,'isc_form_fetch');
    pointer(isc_form_insert) := GetProcedureAddress(IBaseLibraryHandle,'isc_form_insert');
    pointer(isc_get_entree) := GetProcedureAddress(IBaseLibraryHandle,'isc_get_entree');
    pointer(isc_initialize_menu) := GetProcedureAddress(IBaseLibraryHandle,'isc_initialize_menu');
    pointer(isc_menu) := GetProcedureAddress(IBaseLibraryHandle,'isc_menu');
    pointer(isc_load_form) := GetProcedureAddress(IBaseLibraryHandle,'isc_load_form');
    pointer(isc_pop_window) := GetProcedureAddress(IBaseLibraryHandle,'isc_pop_window');
    pointer(isc_put_entree) := GetProcedureAddress(IBaseLibraryHandle,'isc_put_entree');
    pointer(isc_reset_form) := GetProcedureAddress(IBaseLibraryHandle,'isc_reset_form');
    pointer(isc_suspend_window) := GetProcedureAddress(IBaseLibraryHandle,'isc_suspend_window');
{$ENDIF}
    end;
end;

Procedure ReleaseIBase60;

begin
  if RefCount > 0 then dec(RefCount);
  if RefCount = 0 then
    begin
    if not UnloadLibrary(IBaseLibraryHandle) then inc(RefCount);
    end;
end;

// This function is also defined in ibase60!
function XSQLDA_LENGTH(n: Integer): Integer;
begin
  Result := SizeOf(XSQLDA) + (n - 1) * SizeOf(XSQLVAR);
end;

end.

