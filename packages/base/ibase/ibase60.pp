{
  $Id$
}
unit ibase60;

{$MODE objfpc}
{$MACRO on}

interface

{$IFDEF Unix}
  {$LINKLIB c}
  {$LINKLIB crypt}
  {$DEFINE extdecl:=cdecl}
  const
    gdslib = 'gds';
{$ENDIF}
{$IFDEF Win32}
  {$DEFINE extdecl:=stdcall}
  const
    gdslib = 'gds32.dll';
{$ENDIF}

{$i ibase60types.inc}

  {                          }
  { OSRI database functions  }
  {                          }

  function isc_attach_database(_para1:PISC_STATUS; _para2:smallint; _para3:Pchar; _para4:Pisc_db_handle; _para5:smallint;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_array_gen_sdl(_para1:PISC_STATUS; _para2:PISC_ARRAY_DESC; _para3:Psmallint; _para4:Pchar; _para5:Psmallint):ISC_STATUS; extdecl; external gdslib;

  function isc_array_get_slice(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:PISC_ARRAY_DESC;
             _para6:pointer; _para7:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  function isc_array_lookup_bounds(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:Pchar;
             _para6:PISC_ARRAY_DESC):ISC_STATUS; extdecl; external gdslib;

  function isc_array_lookup_desc(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:Pchar;
             _para6:PISC_ARRAY_DESC):ISC_STATUS; extdecl; external gdslib;

  function isc_array_set_desc(_para1:PISC_STATUS; _para2:Pchar; _para3:Pchar; _para4:Psmallint; _para5:Psmallint;
             _para6:Psmallint; _para7:PISC_ARRAY_DESC):ISC_STATUS; extdecl; external gdslib;

  function isc_array_put_slice(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:PISC_ARRAY_DESC;
             _para6:pointer; _para7:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  procedure isc_blob_default_desc(_para1:PISC_BLOB_DESC; _para2:Pbyte; _para3:Pbyte); extdecl; external gdslib;

  function isc_blob_gen_bpb(_para1:PISC_STATUS; _para2:PISC_BLOB_DESC; _para3:PISC_BLOB_DESC; _para4:word; _para5:Pbyte;
             _para6:Pword):ISC_STATUS; extdecl; external gdslib;

  function isc_blob_info(_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:smallint; _para4:Pchar; _para5:smallint;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_blob_lookup_desc(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pbyte; _para5:Pbyte;
             _para6:PISC_BLOB_DESC; _para7:Pbyte):ISC_STATUS; extdecl; external gdslib;

  function isc_blob_set_desc(_para1:PISC_STATUS; _para2:Pbyte; _para3:Pbyte; _para4:smallint; _para5:smallint;
             _para6:smallint; _para7:PISC_BLOB_DESC):ISC_STATUS; extdecl; external gdslib;

  function isc_cancel_blob(_para1:PISC_STATUS; _para2:Pisc_blob_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_cancel_events(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  function isc_close_blob(_para1:PISC_STATUS; _para2:Pisc_blob_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_commit_retaining(_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_commit_transaction(_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_create_blob(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD):ISC_STATUS; extdecl; external gdslib;

  function isc_create_blob2(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD;
             _para6:smallint; _para7:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_create_database(_para1:PISC_STATUS; _para2:smallint; _para3:Pchar; _para4:Pisc_db_handle; _para5:smallint;
             _para6:Pchar; _para7:smallint):ISC_STATUS; extdecl; external gdslib;

  function isc_database_info(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:smallint; _para4:Pchar; _para5:smallint;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  procedure isc_decode_date(_para1:PISC_QUAD; _para2:pointer); extdecl; external gdslib;

  procedure isc_decode_sql_date(_para1:PISC_DATE; _para2:pointer); extdecl; external gdslib;

  procedure isc_decode_sql_time(_para1:PISC_TIME; _para2:pointer); extdecl; external gdslib;

  procedure isc_decode_timestamp(_para1:PISC_TIMESTAMP; _para2:pointer); extdecl; external gdslib;

  function isc_detach_database(_para1:PISC_STATUS; _para2:Pisc_db_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_drop_database(_para1:PISC_STATUS; _para2:Pisc_db_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_allocate_statement(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_stmt_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_alloc_statement2(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_stmt_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_describe(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_describe_bind(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_exec_immed2(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:PXSQLDA; _para8:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_execute(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_execute2(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:PXSQLDA;
             _para6:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_execute_immediate(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_fetch(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_finish(_para1:Pisc_db_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_free_statement(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_insert(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_prepare(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_set_cursor_name(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:Pchar; _para4:word):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_sql_info(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:smallint; _para4:Pchar; _para5:smallint;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  procedure isc_encode_date(_para1:pointer; _para2:PISC_QUAD); extdecl; external gdslib;

  procedure isc_encode_sql_date(_para1:pointer; _para2:PISC_DATE); extdecl; external gdslib;

  procedure isc_encode_sql_time(_para1:pointer; _para2:PISC_TIME); extdecl; external gdslib;

  procedure isc_encode_timestamp(_para1:pointer; _para2:PISC_TIMESTAMP); extdecl; external gdslib;

  function isc_event_block(_para1:PPchar; _para2:PPchar; _para3:word; args:array of const):ISC_LONG; cdecl; external gdslib;

  {!!MVC
  void         isc_event_counts (unsigned ISC_LONG   ,
                                         short,
                                         char   ,
                                         char   ); extdecl; external gdslib;
  !!MVC }
  procedure isc_expand_dpb(_para1:PPchar; _para2:Psmallint; args:array of const); cdecl; external gdslib;

  function isc_modify_dpb(_para1:PPchar; _para2:Psmallint; _para3:word; _para4:Pchar; _para5:smallint):longint; extdecl; external gdslib;

  function isc_free(_para1:Pchar):ISC_LONG; extdecl; external gdslib;

  function isc_get_segment(_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:Pword; _para4:word; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_get_slice(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:smallint;
             _para6:Pchar; _para7:smallint; _para8:PISC_LONG; _para9:ISC_LONG; _para10:pointer;
             _para11:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  function isc_interprete(_para1:Pchar; _para2:PPISC_STATUS):ISC_STATUS; extdecl; external gdslib;

  function isc_open_blob(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD):ISC_STATUS; extdecl; external gdslib;

  function isc_open_blob2(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_blob_handle; _para5:PISC_QUAD;
             _para6:smallint; _para7:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_prepare_transaction2(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; _para4:Pchar):ISC_STATUS; extdecl; external gdslib;

  procedure isc_print_sqlerror(_para1:smallint; _para2:PISC_STATUS); extdecl; external gdslib;

  function isc_print_status(_para1:PISC_STATUS):ISC_STATUS; extdecl; external gdslib;

  function isc_put_segment(_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:word; _para4:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_put_slice(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:PISC_QUAD; _para5:smallint;
             _para6:Pchar; _para7:smallint; _para8:PISC_LONG; _para9:ISC_LONG; _para10:pointer):ISC_STATUS; extdecl; external gdslib;

  function isc_que_events(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:PISC_LONG; _para4:smallint; _para5:Pchar;
             _para6:isc_callback; _para7:pointer):ISC_STATUS; extdecl; external gdslib;

  function isc_rollback_retaining(_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_rollback_transaction(_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_start_multiple(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; _para4:pointer):ISC_STATUS; extdecl; external gdslib;

  function isc_start_transaction(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; args:array of const):ISC_STATUS; cdecl; external gdslib;

  function isc_sqlcode(_para1:PISC_STATUS):ISC_LONG; extdecl; external gdslib;

  procedure isc_sql_interprete(_para1:smallint; _para2:Pchar; _para3:smallint); extdecl; external gdslib;

  function isc_transaction_info(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint; _para4:Pchar; _para5:smallint;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_transact_request(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:Pchar; _para8:word; _para9:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_vax_integer(_para1:Pchar; _para2:smallint):ISC_LONG; extdecl; external gdslib;

  function isc_portable_integer(_para1:Pbyte; _para2:smallint):ISC_INT64; extdecl; external gdslib;

  {                                    }
  { Security Functions                 }
  {                                    }


  function isc_add_user(_para1:PISC_STATUS; _para2:PUSER_SEC_DATA):longint; extdecl; external gdslib;

  function isc_delete_user(_para1:PISC_STATUS; _para2:PUSER_SEC_DATA):longint; extdecl; external gdslib;

  function isc_modify_user(_para1:PISC_STATUS; _para2:PUSER_SEC_DATA):longint; extdecl; external gdslib;

  {                                 }
  {  Other OSRI functions           }
  {                                 }
  function isc_compile_request(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_req_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_compile_request2(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_req_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_ddl(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_prepare_transaction(_para1:PISC_STATUS; _para2:Pisc_tr_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_receive(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:smallint; _para4:smallint; _para5:pointer;
             _para6:smallint):ISC_STATUS; extdecl; external gdslib;

  function isc_reconnect_transaction(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:smallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_release_request(_para1:PISC_STATUS; _para2:Pisc_req_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_request_info(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:smallint; _para4:smallint; _para5:Pchar;
             _para6:smallint; _para7:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_seek_blob(_para1:PISC_STATUS; _para2:Pisc_blob_handle; _para3:smallint; _para4:ISC_LONG; _para5:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  function isc_send(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:smallint; _para4:smallint; _para5:pointer;
             _para6:smallint):ISC_STATUS; extdecl; external gdslib;

  function isc_start_and_send(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Pisc_tr_handle; _para4:smallint; _para5:smallint;
             _para6:pointer; _para7:smallint):ISC_STATUS; extdecl; external gdslib;

  function isc_start_request(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Pisc_tr_handle; _para4:smallint):ISC_STATUS; extdecl; external gdslib;

  function isc_unwind_request(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:smallint):ISC_STATUS; extdecl; external gdslib;

  function isc_wait_for_event(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:smallint; _para4:Pchar; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  {                            }
  { Other Sql functions        }
  {                            }
  function isc_close(_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_declare(_para1:PISC_STATUS; _para2:Pchar; _para3:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_describe(_para1:PISC_STATUS; _para2:Pchar; _para3:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_describe_bind(_para1:PISC_STATUS; _para2:Pchar; _para3:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_execute(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_execute_immediate(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_fetch(_para1:PISC_STATUS; _para2:Pchar; _para3:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_open(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_prepare(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:Psmallint;
             _para6:Pchar; _para7:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  {                                    }
  { Other Dynamic sql functions        }
  {                                    }
  function isc_dsql_execute_m(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:word; _para8:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_execute2_m(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:Pchar;
             _para11:word; _para12:word; _para13:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_execute_immediate_m(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:word;
             _para11:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_exec_immed3_m(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:word;
             _para11:Pchar; _para12:word; _para13:Pchar; _para14:word; _para15:word;
             _para16:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_fetch_m(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:Pchar; _para5:word;
             _para6:word; _para7:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_insert_m(_para1:PISC_STATUS; _para2:Pisc_stmt_handle; _para3:word; _para4:Pchar; _para5:word;
             _para6:word; _para7:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_prepare_m(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pisc_stmt_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:word; _para8:Pchar; _para9:word; _para10:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_dsql_release(_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_close(_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_declare(_para1:PISC_STATUS; _para2:Pchar; _para3:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_describe(_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_describe_bind(_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_execute(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_execute2(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA;
             _para6:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_execute_immed(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_fetch(_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_open(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_open2(_para1:PISC_STATUS; _para2:Pisc_tr_handle; _para3:Pchar; _para4:word; _para5:PXSQLDA;
             _para6:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_insert(_para1:PISC_STATUS; _para2:Pchar; _para3:word; _para4:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_prepare(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pchar; _para5:word;
             _para6:Pchar; _para7:word; _para8:PXSQLDA):ISC_STATUS; extdecl; external gdslib;

  function isc_embed_dsql_release(_para1:PISC_STATUS; _para2:Pchar):ISC_STATUS; extdecl; external gdslib;

  {                             }
  { Other Blob functions        }
  {                             }
  function BLOB_open(_para1:isc_blob_handle; _para2:Pchar; _para3:longint):PBSTREAM; extdecl; external gdslib;

  function BLOB_put(_para1:char; _para2:PBSTREAM):longint; extdecl; external gdslib;

  function BLOB_close(_para1:PBSTREAM):longint; extdecl; external gdslib;

  function BLOB_get(_para1:PBSTREAM):longint; extdecl; external gdslib;

  function BLOB_display(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl; external gdslib;

  function BLOB_dump(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl; external gdslib;

  function BLOB_edit(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl; external gdslib;

  function BLOB_load(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl; external gdslib;

  function BLOB_text_dump(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl; external gdslib;

  function BLOB_text_load(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):longint; extdecl; external gdslib;

  function Bopen(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar):PBSTREAM; extdecl; external gdslib;

{$IFDEF Unix}
  function Bopen2(_para1:PISC_QUAD; _para2:isc_db_handle; _para3:isc_tr_handle; _para4:Pchar; _para5:word):PBSTREAM; extdecl; external gdslib;
{$ENDIF}

  {                             }
  { Other Misc functions        }
  {                             }
  function isc_ftof(_para1:Pchar; _para2:word; _para3:Pchar; _para4:word):ISC_LONG; extdecl; external gdslib;

  function isc_print_blr(_para1:Pchar; _para2:isc_callback; _para3:pointer; _para4:smallint):ISC_STATUS; extdecl; external gdslib;

  procedure isc_set_debug(_para1:longint); extdecl; external gdslib;

  procedure isc_qtoq(_para1:PISC_QUAD; _para2:PISC_QUAD); extdecl; external gdslib;

  procedure isc_vtof(_para1:Pchar; _para2:Pchar; _para3:word); extdecl; external gdslib;

  procedure isc_vtov(_para1:Pchar; _para2:Pchar; _para3:smallint); extdecl; external gdslib;

  function isc_version(_para1:Pisc_db_handle; _para2:isc_callback; _para3:pointer):longint; extdecl; external gdslib;

{$IFDEF Unix}
  function isc_reset_fpe(_para1:word):ISC_LONG; extdecl; external gdslib;
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

  function isc_service_attach(_para1:PISC_STATUS; _para2:word; _para3:Pchar; _para4:Pisc_svc_handle; _para5:word;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_service_detach(_para1:PISC_STATUS; _para2:Pisc_svc_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_service_query(_para1:PISC_STATUS; _para2:Pisc_svc_handle; _para3:Pisc_resv_handle; _para4:word; _para5:Pchar;
             _para6:word; _para7:Pchar; _para8:word; _para9:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_service_start(_para1:PISC_STATUS; _para2:Pisc_svc_handle; _para3:Pisc_resv_handle; _para4:word; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  {                              }
  { Forms functions              }
  {                              }
{$IFDEF Unix}
  function isc_compile_map(_para1:PISC_STATUS; _para2:Pisc_form_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_compile_menu(_para1:PISC_STATUS; _para2:Pisc_form_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_compile_sub_map(_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_create_window(_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Psmallint; _para4:Pchar; _para5:Psmallint;
             _para6:Psmallint):ISC_STATUS; extdecl; external gdslib;

  function isc_delete_window(_para1:PISC_STATUS; _para2:Pisc_win_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_drive_form(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_win_handle; _para5:Pisc_req_handle;
             _para6:Pbyte; _para7:Pbyte):ISC_STATUS; extdecl; external gdslib;

  function isc_drive_menu(_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar;
             _para6:Psmallint; _para7:Pchar; _para8:Psmallint; _para9:Psmallint; _para10:Pchar;
             _para11:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  function isc_form_delete(_para1:PISC_STATUS; _para2:Pisc_form_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_form_fetch(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_req_handle; _para5:Pbyte):ISC_STATUS; extdecl; external gdslib;

  function isc_form_insert(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_req_handle; _para5:Pbyte):ISC_STATUS; extdecl; external gdslib;

  function isc_get_entree(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Psmallint; _para4:Pchar; _para5:PISC_LONG;
             _para6:Psmallint):ISC_STATUS; extdecl; external gdslib;

  function isc_initialize_menu(_para1:PISC_STATUS; _para2:Pisc_req_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_menu(_para1:PISC_STATUS; _para2:Pisc_win_handle; _para3:Pisc_req_handle; _para4:Psmallint; _para5:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_load_form(_para1:PISC_STATUS; _para2:Pisc_db_handle; _para3:Pisc_tr_handle; _para4:Pisc_form_handle; _para5:Psmallint;
             _para6:Pchar):ISC_STATUS; extdecl; external gdslib;

  function isc_pop_window(_para1:PISC_STATUS; _para2:Pisc_win_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_put_entree(_para1:PISC_STATUS; _para2:Pisc_req_handle; _para3:Psmallint; _para4:Pchar; _para5:PISC_LONG):ISC_STATUS; extdecl; external gdslib;

  function isc_reset_form(_para1:PISC_STATUS; _para2:Pisc_req_handle):ISC_STATUS; extdecl; external gdslib;

  function isc_suspend_window(_para1:PISC_STATUS; _para2:Pisc_win_handle):ISC_STATUS; extdecl; external gdslib;
{$ENDIF}

  function isc_attach_database:ISC_STATUS; extdecl; external gdslib;

  function isc_array_gen_sdl:ISC_STATUS; extdecl; external gdslib;

  function isc_array_get_slice:ISC_STATUS; extdecl; external gdslib;

  function isc_array_lookup_bounds:ISC_STATUS; extdecl; external gdslib;

  function isc_array_lookup_desc:ISC_STATUS; extdecl; external gdslib;

  function isc_array_set_desc:ISC_STATUS; extdecl; external gdslib;

  function isc_array_put_slice:ISC_STATUS; extdecl; external gdslib;

  function isc_blob_gen_bpb:ISC_STATUS; extdecl; external gdslib;

  function isc_blob_info:ISC_STATUS; extdecl; external gdslib;

  function isc_blob_lookup_desc:ISC_STATUS; extdecl; external gdslib;

  function isc_blob_set_desc:ISC_STATUS; extdecl; external gdslib;

  function isc_cancel_blob:ISC_STATUS; extdecl; external gdslib;

  function isc_cancel_events:ISC_STATUS; extdecl; external gdslib;

  function isc_close_blob:ISC_STATUS; extdecl; external gdslib;

  function isc_commit_retaining:ISC_STATUS; extdecl; external gdslib;

  function isc_commit_transaction:ISC_STATUS; extdecl; external gdslib;

  function isc_compile_request:ISC_STATUS; extdecl; external gdslib;

  function isc_compile_request2:ISC_STATUS; extdecl; external gdslib;

  function isc_create_blob:ISC_STATUS; extdecl; external gdslib;

  function isc_create_blob2:ISC_STATUS; extdecl; external gdslib;

  function isc_create_database:ISC_STATUS; extdecl; external gdslib;

  function isc_database_info:ISC_STATUS; extdecl; external gdslib;

  function isc_ddl:ISC_STATUS; extdecl; external gdslib;

  procedure isc_decode_date; extdecl; external gdslib;

  procedure isc_decode_sql_date; extdecl; external gdslib;

  procedure isc_decode_sql_time; extdecl; external gdslib;

  procedure isc_decode_timestamp; extdecl; external gdslib;

  function isc_detach_database:ISC_STATUS; extdecl; external gdslib;

  function isc_drop_database:ISC_STATUS; extdecl; external gdslib;

  procedure isc_encode_date; extdecl; external gdslib;

  procedure isc_encode_sql_date; extdecl; external gdslib;

  procedure isc_encode_sql_time; extdecl; external gdslib;

  procedure isc_encode_timestamp; extdecl; external gdslib;

  function isc_event_block:ISC_LONG; cdecl; external gdslib;

  procedure isc_event_counts; extdecl; external gdslib;

  procedure isc_expand_dpb; cdecl; external gdslib;

  function isc_modify_dpb:longint; extdecl; external gdslib;

  function isc_free:ISC_LONG; extdecl; external gdslib;

  function isc_get_segment:ISC_STATUS; extdecl; external gdslib;

  function isc_get_slice:ISC_STATUS; extdecl; external gdslib;

  function isc_interprete:ISC_STATUS; extdecl; external gdslib;

  function isc_open_blob:ISC_STATUS; extdecl; external gdslib;

  function isc_open_blob2:ISC_STATUS; extdecl; external gdslib;

  function isc_prepare_transaction:ISC_STATUS; extdecl; external gdslib;

  function isc_prepare_transaction2:ISC_STATUS; extdecl; external gdslib;

  procedure isc_print_sqlerror; extdecl; external gdslib;

  function isc_print_status:ISC_STATUS; extdecl; external gdslib;

  function isc_put_segment:ISC_STATUS; extdecl; external gdslib;

  function isc_put_slice:ISC_STATUS; extdecl; external gdslib;

  function isc_que_events:ISC_STATUS; extdecl; external gdslib;

  function isc_receive:ISC_STATUS; extdecl; external gdslib;

  function isc_reconnect_transaction:ISC_STATUS; extdecl; external gdslib;

  function isc_release_request:ISC_STATUS; extdecl; external gdslib;

  function isc_request_info:ISC_STATUS; extdecl; external gdslib;

{$IFDEF Unix}
  function isc_reset_fpe:ISC_LONG; extdecl; external gdslib;
{$ENDIF}

  function isc_rollback_transaction:ISC_STATUS; extdecl; external gdslib;

  function isc_rollback_retaining:ISC_STATUS; extdecl; external gdslib;

  function isc_seek_blob:ISC_STATUS; extdecl; external gdslib;

  function isc_send:ISC_STATUS; extdecl; external gdslib;

  function isc_service_attach:ISC_STATUS; extdecl; external gdslib;

  function isc_service_detach:ISC_STATUS; extdecl; external gdslib;

  function isc_service_query:ISC_STATUS; extdecl; external gdslib;

  function isc_service_start:ISC_STATUS; extdecl; external gdslib;

  function isc_start_and_send:ISC_STATUS; extdecl; external gdslib;

  function isc_start_multiple:ISC_STATUS; extdecl; external gdslib;

  function isc_start_request:ISC_STATUS; extdecl; external gdslib;

  function isc_start_transaction:ISC_STATUS; cdecl; external gdslib;

  function isc_sqlcode:ISC_LONG; extdecl; external gdslib;

  function isc_transaction_info:ISC_STATUS; extdecl; external gdslib;

  function isc_transact_request:ISC_STATUS; extdecl; external gdslib;

  function isc_unwind_request:ISC_STATUS; extdecl; external gdslib;

  function isc_wait_for_event:ISC_STATUS; extdecl; external gdslib;

  function isc_ftof:ISC_LONG; extdecl; external gdslib;

  function isc_print_blr:ISC_STATUS; extdecl; external gdslib;

  procedure isc_set_debug; extdecl; external gdslib;

  procedure isc_qtoq; extdecl; external gdslib;

  function isc_vax_integer:ISC_LONG; extdecl; external gdslib;

  procedure isc_vtof; extdecl; external gdslib;

  procedure isc_vtov; extdecl; external gdslib;

  function isc_version:longint; extdecl; external gdslib;

  {                 }
  { Blob functions  }
  {                 }

  function Bopen:PBSTREAM; extdecl; external gdslib;

  function BLOB_open:PBSTREAM; extdecl; external gdslib;

{$IFDEF Unix}
  function Bopen2:PBSTREAM; extdecl; external gdslib;
{$ENDIF}

  implementation

// This function is also defined in ibase60dyn!

function XSQLDA_LENGTH(n: Integer): Integer;
begin
  Result := SizeOf(XSQLDA) + (n - 1) * SizeOf(XSQLVAR);
end;


end.
{
  $Log$
  Revision 1.5  2005-02-04 18:14:22  joost
  - replaced gdsdecl by extdecl for the codetools
  - cleanup of double declarations

  Revision 1.4  2005/01/12 10:23:34  michael
  * Changes from Joost van der Sluis to enable dynamic loading of the Interbase library

  Revision 1.3  2003/04/13 10:26:34  michael
  + Added crypt library

  Revision 1.2  2002/09/07 15:42:52  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:54:51  peter
    * splitted to base and extra

}
