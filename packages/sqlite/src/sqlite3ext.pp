{
  This file is part of the Free Pascal Classes Library (FCL).
  Copyright (C) 2018 Silvio Clecio (silvioprog) member of
  the Free Pascal development team.

  This unit file defines the SQLite interface for use by
  shared libraries that want to be imported as extensions
  into a SQLite instance.

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit SQLite3Ext;

{$mode objfpc}{$h+}

interface

uses
  ctypes,
  sqlite3;

{$packrecords c}

type
  Ppcchar = ^pcchar;
  PPpcchar = ^Ppcchar;
  va_list = type Pointer;

  xCallback = function (_para1:cunsigned; _para2:pointer; _para3:pointer; _para4:pointer):cint;cdecl;

  Psqlite3_api_routines = ^sqlite3_api_routines;
  (*
  ** The following structure holds pointers to all of the SQLite API
  ** routines.
  **
  ** WARNING:  In order to maintain backwards compatibility, add new
  ** interfaces to the end of this structure only.  If you insert new
  ** interfaces in the middle of this structure, then older different
  ** versions of SQLite will not be able to load each other's shared
  ** libraries!
  *)
  sqlite3_api_routines = record
      aggregate_context : function (_para1:Psqlite3_context; nBytes:cint):pointer;cdecl;
      aggregate_count : function (_para1:Psqlite3_context):cint;cdecl;
      bind_blob : function (_para1:Psqlite3_stmt; _para2:cint; _para3:pointer; n:cint; _para5:sqlite3_destructor_type):cint;cdecl;
      bind_double : function (_para1:Psqlite3_stmt; _para2:cint; _para3:double):cint;cdecl;
      bind_int : function (_para1:Psqlite3_stmt; _para2:cint; _para3:cint):cint;cdecl;
      bind_int64 : function (_para1:Psqlite3_stmt; _para2:cint; _para3:sqlite_int64):cint;cdecl;
      bind_null : function (_para1:Psqlite3_stmt; _para2:cint):cint;cdecl;
      bind_parameter_count : function (_para1:Psqlite3_stmt):cint;cdecl;
      bind_parameter_index : function (_para1:Psqlite3_stmt; zName:pcchar):cint;cdecl;
      bind_parameter_name : function (_para1:Psqlite3_stmt; _para2:cint):pcchar;cdecl;
      bind_text : function (_para1:Psqlite3_stmt; _para2:cint; _para3:pcchar; n:cint; _para5:sqlite3_destructor_type):cint;cdecl;
      bind_text16 : function (_para1:Psqlite3_stmt; _para2:cint; _para3:pointer; _para4:cint; _para5:sqlite3_destructor_type):cint;cdecl;
      bind_value : function (_para1:Psqlite3_stmt; _para2:cint; _para3:Psqlite3_value):cint;cdecl;
      busy_handler : function (_para1:Psqlite3; _para2:busyhandler_callback; _para3:pointer):cint;cdecl;
      busy_timeout : function (_para1:Psqlite3; ms:cint):cint;cdecl;
      changes : function (_para1:Psqlite3):cint;cdecl;
      close : function (_para1:Psqlite3):cint;cdecl;
      collation_needed : function (_para1:Psqlite3; _para2:pointer; _para3:collation_needed_cb):cint;cdecl;
      collation_needed16 : function (_para1:Psqlite3; _para2:pointer; _para3:collation_needed_cb):cint;cdecl;
      column_blob : function (_para1:Psqlite3_stmt; iCol:cint):pointer;cdecl;
      column_bytes : function (_para1:Psqlite3_stmt; iCol:cint):cint;cdecl;
      column_bytes16 : function (_para1:Psqlite3_stmt; iCol:cint):cint;cdecl;
      column_count : function (pStmt:Psqlite3_stmt):cint;cdecl;
      column_database_name : function (_para1:Psqlite3_stmt; _para2:cint):pcchar;cdecl;
      column_database_name16 : function (_para1:Psqlite3_stmt; _para2:cint):pointer;cdecl;
      column_decltype : function (_para1:Psqlite3_stmt; i:cint):pcchar;cdecl;
      column_decltype16 : function (_para1:Psqlite3_stmt; _para2:cint):pointer;cdecl;
      column_double : function (_para1:Psqlite3_stmt; iCol:cint):double;cdecl;
      column_int : function (_para1:Psqlite3_stmt; iCol:cint):cint;cdecl;
      column_int64 : function (_para1:Psqlite3_stmt; iCol:cint):sqlite_int64;cdecl;
      column_name : function (_para1:Psqlite3_stmt; _para2:cint):pcchar;cdecl;
      column_name16 : function (_para1:Psqlite3_stmt; _para2:cint):pointer;cdecl;
      column_origin_name : function (_para1:Psqlite3_stmt; _para2:cint):pcchar;cdecl;
      column_origin_name16 : function (_para1:Psqlite3_stmt; _para2:cint):pointer;cdecl;
      column_table_name : function (_para1:Psqlite3_stmt; _para2:cint):pcchar;cdecl;
      column_table_name16 : function (_para1:Psqlite3_stmt; _para2:cint):pointer;cdecl;
      column_text : function (_para1:Psqlite3_stmt; iCol:cint):pcuchar;cdecl;
      column_text16 : function (_para1:Psqlite3_stmt; iCol:cint):pointer;cdecl;
      column_type : function (_para1:Psqlite3_stmt; iCol:cint):cint;cdecl;
      column_value : function (_para1:Psqlite3_stmt; iCol:cint):Psqlite3_value;cdecl;
      commit_hook : function (_para1:Psqlite3; _para2:commit_callback; _para3:pointer):pointer;cdecl;
      complete : function (sql:pcchar):cint;cdecl;
      complete16 : function (sql:pointer):cint;cdecl;
      create_collation : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:pointer; _para5:xCompare):cint;cdecl;
      create_collation16 : function (_para1:Psqlite3; _para2:pointer; _para3:cint; _para4:pointer; _para5:xCompare):cint;cdecl;
      create_function : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:cint; _para5:pointer; 
                   xFunc:xFunc; xStep:xStep; xFinal:xFinal):cint;cdecl;
      create_function16 : function (_para1:Psqlite3; _para2:pointer; _para3:cint; _para4:cint; _para5:pointer; 
                   xFunc:xFunc; xStep:xStep; xFinal:xFinal):cint;cdecl;
      create_module : function (_para1:Psqlite3; _para2:pcchar; _para3:Psqlite3_module; _para4:pointer):cint;cdecl;
      data_count : function (pStmt:Psqlite3_stmt):cint;cdecl;
      db_handle : function (_para1:Psqlite3_stmt):Psqlite3;cdecl;
      declare_vtab : function (_para1:Psqlite3; _para2:pcchar):cint;cdecl;
      enable_shared_cache : function (_para1:cint):cint;cdecl;
      errcode : function (db:Psqlite3):cint;cdecl;
      errmsg : function (_para1:Psqlite3):pcchar;cdecl;
      errmsg16 : function (_para1:Psqlite3):pointer;cdecl;
      exec : function (_para1:Psqlite3; _para2:pcchar; _para3:sqlite3_callback; _para4:pointer; _para5:Ppcchar):cint;cdecl;
      expired : function (_para1:Psqlite3_stmt):cint;cdecl;
      finalize : function (pStmt:Psqlite3_stmt):cint;cdecl;
      free : procedure;cdecl;
      free_table : procedure (result:Ppcchar);cdecl;
      get_autocommit : function (_para1:Psqlite3):cint;cdecl;
      get_auxdata : function (_para1:Psqlite3_context; _para2:cint):pointer;cdecl;
      get_table : function (_para1:Psqlite3; _para2:pcchar; _para3:PPpcchar; _para4:pcint; _para5:pcint; 
                   _para6:Ppcchar):cint;cdecl;
      global_recover : function :cint;cdecl;
      interruptx : procedure (_para1:Psqlite3);cdecl;
      last_insert_rowid : function (_para1:Psqlite3):sqlite_int64;cdecl;
      libversion : function :pcchar;cdecl;
      libversion_number : function :cint;cdecl;
      malloc : function (_para1:cint):pointer;cdecl;
      mprintf : function (_para1:pcchar; args:array of const):pcchar;cdecl;
      open : function (_para1:pcchar; _para2:PPsqlite3):cint;cdecl;
      open16 : function (_para1:pointer; _para2:PPsqlite3):cint;cdecl;
      prepare : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:PPsqlite3_stmt; _para5:Ppcchar):cint;cdecl;
      prepare16 : function (_para1:Psqlite3; _para2:pointer; _para3:cint; _para4:PPsqlite3_stmt; _para5:Ppointer):cint;cdecl;
      profile : function (_para1:Psqlite3; _para2:xProfile; _para3:pointer):pointer;cdecl;
      progress_handler : procedure (_para1:Psqlite3; _para2:cint; _para3:commit_callback; _para4:pointer);cdecl;
      realloc : function:pointer;cdecl;
      reset : function (pStmt:Psqlite3_stmt):cint;cdecl;
      result_blob : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:cint; _para4:sqlite3_destructor_type);cdecl;
      result_double : procedure (_para1:Psqlite3_context; _para2:double);cdecl;
      result_error : procedure (_para1:Psqlite3_context; _para2:pcchar; _para3:cint);cdecl;
      result_error16 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:cint);cdecl;
      result_int : procedure (_para1:Psqlite3_context; _para2:cint);cdecl;
      result_int64 : procedure (_para1:Psqlite3_context; _para2:sqlite_int64);cdecl;
      result_null : procedure (_para1:Psqlite3_context);cdecl;
      result_text : procedure (_para1:Psqlite3_context; _para2:pcchar; _para3:cint; _para4:sqlite3_destructor_type);cdecl;
      result_text16 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:cint; _para4:sqlite3_destructor_type);cdecl;
      result_text16be : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:cint; _para4:sqlite3_destructor_type);cdecl;
      result_text16le : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:cint; _para4:sqlite3_destructor_type);cdecl;
      result_value : procedure (_para1:Psqlite3_context; _para2:Psqlite3_value);cdecl;
      rollback_hook : function (_para1:Psqlite3; _para2:sqlite3_destructor_type; _para3:pointer):pointer;cdecl;
      set_authorizer : function (_para1:Psqlite3; _para2:xAuth; _para3:pointer):cint;cdecl;
      set_auxdata : procedure (_para1:Psqlite3_context; _para2:cint; _para3:pointer; _para4:sqlite3_destructor_type);cdecl;
      xsnprintf : function (_para1:cint; _para2:pcchar; _para3:pcchar; args:array of const):pcchar;cdecl;
      step : function (_para1:Psqlite3_stmt):cint;cdecl;
      table_column_metadata : function (_para1:Psqlite3; _para2:pcchar; _para3:pcchar; _para4:pcchar; _para5:Ppcchar; 
                   _para6:Ppcchar; _para7:pcint; _para8:pcint; _para9:pcint):cint;cdecl;
      thread_cleanup : procedure ;cdecl;
      total_changes : function (_para1:Psqlite3):cint;cdecl;
      trace : function (_para1:Psqlite3; xTrace:xTrace; _para3:pointer):pointer;cdecl;
      transfer_bindings : function (_para1:Psqlite3_stmt; _para2:Psqlite3_stmt):cint;cdecl;
      update_hook : function (_para1:Psqlite3; _para2:update_callback; _para3:pointer):pointer;cdecl;
      user_data : function (_para1:Psqlite3_context):pointer;cdecl;
      value_blob : function (_para1:Psqlite3_value):pointer;cdecl;
      value_bytes : function (_para1:Psqlite3_value):cint;cdecl;
      value_bytes16 : function (_para1:Psqlite3_value):cint;cdecl;
      value_double : function (_para1:Psqlite3_value):double;cdecl;
      value_int : function (_para1:Psqlite3_value):cint;cdecl;
      value_int64 : function (_para1:Psqlite3_value):sqlite_int64;cdecl;
      value_numeric_type : function (_para1:Psqlite3_value):cint;cdecl;
      value_text : function (_para1:Psqlite3_value):pcuchar;cdecl;
      value_text16 : function (_para1:Psqlite3_value):pointer;cdecl;
      value_text16be : function (_para1:Psqlite3_value):pointer;cdecl;
      value_text16le : function (_para1:Psqlite3_value):pointer;cdecl;
      value_type : function (_para1:Psqlite3_value):cint;cdecl;
      vmprintf : function (_para1:pcchar; _para2:va_list):pcchar;cdecl;
      overload_function : function (_para1:Psqlite3; zFuncName:pcchar; nArg:cint):cint;cdecl;
      prepare_v2 : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:PPsqlite3_stmt; _para5:Ppcchar):cint;cdecl;
      prepare16_v2 : function (_para1:Psqlite3; _para2:pointer; _para3:cint; _para4:PPsqlite3_stmt; _para5:Ppointer):cint;cdecl;
      clear_bindings : function (_para1:Psqlite3_stmt):cint;cdecl;
      create_module_v2 : function (_para1:Psqlite3; _para2:pcchar; _para3:Psqlite3_module; _para4:pointer; xDestroy:sqlite3_destructor_type):cint;cdecl;
      bind_zeroblob : function (_para1:Psqlite3_stmt; _para2:cint; _para3:cint):cint;cdecl;
      blob_bytes : function (_para1:Psqlite3_blob):cint;cdecl;
      blob_close : function (_para1:Psqlite3_blob):cint;cdecl;
      blob_open : function (_para1:Psqlite3; _para2:pcchar; _para3:pcchar; _para4:pcchar; _para5:sqlite3_int64; 
                   _para6:cint; _para7:PPsqlite3_blob):cint;cdecl;
      blob_read : function (_para1:Psqlite3_blob; _para2:pointer; _para3:cint; _para4:cint):cint;cdecl;
      blob_write : function (_para1:Psqlite3_blob; _para2:pointer; _para3:cint; _para4:cint):cint;cdecl;
      create_collation_v2 : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:pointer; _para5:xCompare; 
                   _para6:sqlite3_destructor_type):cint;cdecl;
      file_control : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:pointer):cint;cdecl;
      memory_highwater : function (_para1:cint):sqlite3_int64;cdecl;
      memory_used : function :sqlite3_int64;cdecl;
      mutex_alloc : function (_para1:cint):Psqlite3_mutex;cdecl;
      mutex_enter : procedure (_para1:Psqlite3_mutex);cdecl;
      mutex_free : procedure (_para1:Psqlite3_mutex);cdecl;
      mutex_leave : procedure (_para1:Psqlite3_mutex);cdecl;
      mutex_try : function (_para1:Psqlite3_mutex):cint;cdecl;
      open_v2 : function (_para1:pcchar; _para2:PPsqlite3; _para3:cint; _para4:pcchar):cint;cdecl;
      release_memory : function (_para1:cint):cint;cdecl;
      result_error_nomem : procedure (_para1:Psqlite3_context);cdecl;
      result_error_toobig : procedure (_para1:Psqlite3_context);cdecl;
      sleep : function (_para1:cint):cint;cdecl;
      soft_heap_limit : procedure (_para1:cint);cdecl;
      vfs_find : function (_para1:pcchar):Psqlite3_vfs;cdecl;
      vfs_register : function (_para1:Psqlite3_vfs; _para2:cint):cint;cdecl;
      vfs_unregister : function (_para1:Psqlite3_vfs):cint;cdecl;
      xthreadsafe : function :cint;cdecl;
      result_zeroblob : procedure (_para1:Psqlite3_context; _para2:cint);cdecl;
      result_error_code : procedure (_para1:Psqlite3_context; _para2:cint);cdecl;
      test_control : function (_para1:cint; args:array of const):cint;cdecl;
      randomness : procedure (_para1:cint; _para2:pointer);cdecl;
      context_db_handle : function (_para1:Psqlite3_context):Psqlite3;cdecl;
      extended_result_codes : function (_para1:Psqlite3; _para2:cint):cint;cdecl;
      limit : function (_para1:Psqlite3; _para2:cint; _para3:cint):cint;cdecl;
      next_stmt : function (_para1:Psqlite3; _para2:Psqlite3_stmt):Psqlite3_stmt;cdecl;
      sql : function (_para1:Psqlite3_stmt):pcchar;cdecl;
      status : function (_para1:cint; _para2:pcint; _para3:pcint; _para4:cint):cint;cdecl;
      backup_finish : function (_para1:Psqlite3backup):cint;cdecl;
      backup_init : function (_para1:Psqlite3; _para2:pcchar; _para3:Psqlite3; _para4:pcchar):Psqlite3backup;cdecl;
      backup_pagecount : function (_para1:Psqlite3backup):cint;cdecl;
      backup_remaining : function (_para1:Psqlite3backup):cint;cdecl;
      backup_step : function (_para1:Psqlite3backup; _para2:cint):cint;cdecl;
      compileoption_get : function (_para1:cint):pcchar;cdecl;
      compileoption_used : function (_para1:pcchar):cint;cdecl;
      create_function_v2 : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:cint; _para5:pointer; 
                   xFunc:xFunc; xStep:xStep; xFinal:xFinal; xDestroy:sqlite3_destructor_type):cint;cdecl;
      db_config : function (_para1:Psqlite3; _para2:cint; args:array of const):cint;cdecl;
      db_mutex : function (_para1:Psqlite3):Psqlite3_mutex;cdecl;
      db_status : function (_para1:Psqlite3; _para2:cint; _para3:pcint; _para4:pcint; _para5:cint):cint;cdecl;
      extended_errcode : function (_para1:Psqlite3):cint;cdecl;
      log : procedure (_para1:cint; _para2:pcchar; args:array of const);cdecl;
      soft_heap_limit64 : function (_para1:sqlite3_int64):sqlite3_int64;cdecl;
      sourceid : function :pcchar;cdecl;
      stmt_status : function (_para1:Psqlite3_stmt; _para2:cint; _para3:cint):cint;cdecl;
      strnicmp : function (_para1:pcchar; _para2:pcchar; _para3:cint):cint;cdecl;
      unlock_notify : function (_para1:Psqlite3; _para2:xNotifycb; _para3:pointer):cint;cdecl;
      wal_autocheckpoint : function (_para1:Psqlite3; _para2:cint):cint;cdecl;
      wal_checkpoint : function (_para1:Psqlite3; _para2:pcchar):cint;cdecl;
      wal_hook : function (_para1:Psqlite3; _para2:wal_hook_cb; _para3:pointer):pointer;cdecl;
      blob_reopen : function (_para1:Psqlite3_blob; _para2:sqlite3_int64):cint;cdecl;
      vtab_config : function (_para1:Psqlite3; op:cint; args:array of const):cint;cdecl;
      vtab_on_conflict : function (_para1:Psqlite3):cint;cdecl;
      close_v2 : function (_para1:Psqlite3):cint;cdecl;
      db_filename : function (_para1:Psqlite3; _para2:pcchar):pcchar;cdecl;
      db_readonly : function (_para1:Psqlite3; _para2:pcchar):cint;cdecl;
      db_release_memory : function (_para1:Psqlite3):cint;cdecl;
      errstr : function (_para1:cint):pcchar;cdecl;
      stmt_busy : function (_para1:Psqlite3_stmt):cint;cdecl;
      stmt_readonly : function (_para1:Psqlite3_stmt):cint;cdecl;
      stricmp : function (_para1:pcchar; _para2:pcchar):cint;cdecl;
      uri_boolean : function (_para1:pcchar; _para2:pcchar; _para3:cint):cint;cdecl;
      uri_int64 : function (_para1:pcchar; _para2:pcchar; _para3:sqlite3_int64):sqlite3_int64;cdecl;
      uri_parameter : function (_para1:pcchar; _para2:pcchar):pcchar;cdecl;
      xvsnprintf : function (_para1:cint; _para2:pcchar; _para3:pcchar; _para4:va_list):pcchar;cdecl;
      wal_checkpoint_v2 : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:pcint; _para5:pcint):cint;cdecl;
      auto_extension : function (_para1:pointer ):cint;cdecl;
      bind_blob64 : function (_para1:Psqlite3_stmt; _para2:cint; _para3:pointer; _para4:sqlite3_uint64; _para5:sqlite3_destructor_type):cint;cdecl;
      bind_text64 : function (_para1:Psqlite3_stmt; _para2:cint; _para3:pcchar; _para4:sqlite3_uint64; _para5:sqlite3_destructor_type; 
                   _para6:cuchar):cint;cdecl;
      cancel_auto_extension : function (_para1:pointer ):cint;cdecl;
      load_extension : function (_para1:Psqlite3; _para2:pcchar; _para3:pcchar; _para4:Ppcchar):cint;cdecl;
      malloc64 : function (_para1:sqlite3_uint64):pointer;cdecl;
      msize : function (_para1:pointer):sqlite3_uint64;cdecl;
      realloc64 : function (_para1:pointer; _para2:sqlite3_uint64):pointer;cdecl;
      reset_auto_extension : procedure ;cdecl;
      result_blob64 : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:sqlite3_uint64; _para4:sqlite3_destructor_type);cdecl;
      result_text64 : procedure (_para1:Psqlite3_context; _para2:pcchar; _para3:sqlite3_uint64; _para4:sqlite3_destructor_type; _para5:cuchar);cdecl;
      strglob : function (_para1:pcchar; _para2:pcchar):cint;cdecl;
      value_dup : function (_para1:Psqlite3_value):Psqlite3_value;cdecl;
      value_free : procedure (_para1:Psqlite3_value);cdecl;
      result_zeroblob64 : function (_para1:Psqlite3_context; _para2:sqlite3_uint64):cint;cdecl;
      bind_zeroblob64 : function (_para1:Psqlite3_stmt; _para2:cint; _para3:sqlite3_uint64):cint;cdecl;
      value_subtype : function (_para1:Psqlite3_value):cuint;cdecl;
      result_subtype : procedure (_para1:Psqlite3_context; _para2:cuint);cdecl;
      status64 : function (_para1:cint; _para2:Psqlite3_int64; _para3:Psqlite3_int64; _para4:cint):cint;cdecl;
      strlike : function (_para1:pcchar; _para2:pcchar; _para3:cuint):cint;cdecl;
      db_cacheflush : function (_para1:Psqlite3):cint;cdecl;
      system_errno : function (_para1:Psqlite3):cint;cdecl;
      trace_v2 : function (_para1:Psqlite3; _para2:cunsigned; _para3:xCallback; _para4:pointer):cint;cdecl;
      expanded_sql : function (_para1:Psqlite3_stmt):pcchar;cdecl;
      set_last_insert_rowid : procedure (_para1:Psqlite3; _para2:sqlite3_int64);cdecl;
      prepare_v3 : function (_para1:Psqlite3; _para2:pcchar; _para3:cint; _para4:cuint; _para5:PPsqlite3_stmt; 
                   _para6:Ppcchar):cint;cdecl;
      prepare16_v3 : function (_para1:Psqlite3; _para2:pointer; _para3:cint; _para4:cuint; _para5:PPsqlite3_stmt; 
                   _para6:Ppointer):cint;cdecl;
      bind_pointer : function (_para1:Psqlite3_stmt; _para2:cint; _para3:pointer; _para4:pcchar; _para5:sqlite3_destructor_type):cint;cdecl;
      result_pointer : procedure (_para1:Psqlite3_context; _para2:pointer; _para3:pcchar; _para4:sqlite3_destructor_type);cdecl;
      value_pointer : function (_para1:Psqlite3_value; _para2:pcchar):pointer;cdecl;
      vtab_nochange : function (_para1:Psqlite3_context):cint;cdecl;
      value_nochange : function (_para1:Psqlite3_value):cint;cdecl;
      vtab_collation : function (_para1:Psqlite3_index_info; _para2:cint):pcchar;cdecl;
    end;

// These are no-ops.
procedure SQLITE_EXTENSION_INIT1;
procedure SQLITE_EXTENSION_INIT3;

// This is actually unnecessary, but is provided for compatibility with sqlite3ext tutorial.

Var
  sqlite3_api : Psqlite3_api_routines;
  
procedure SQLITE_EXTENSION_INIT2(v: Psqlite3_api_routines);

implementation

procedure SQLITE_EXTENSION_INIT1;
begin
end;

procedure SQLITE_EXTENSION_INIT2(v: Psqlite3_api_routines);
begin
  sqlite3_api:=v;
end;

procedure SQLITE_EXTENSION_INIT3;
begin

end;

end.
