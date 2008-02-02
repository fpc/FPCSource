{
  Contains the types needed for use with MySQL v4.0
}

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
       org_name : pchar;
       table : Pchar;
       org_table : Pchar;
       db : Pchar;
       catalog : pchar;
       def : Pchar;
       length : culong;
       max_length : culong;
       name_length : cuint;
       org_name_length : cuint;
       table_length : cuint;
       org_table_length : cuint;
       db_length : cuint;
       catalog_length : cuint;
       def_length : cuint;
       flags : cuint;
       decimals : cuint;
       charsetnr : cuint;
       ftype : enum_field_types;
    end;
  MYSQL_FIELD = st_mysql_field;
  TMYSQL_FIELD = MYSQL_FIELD;
  PMYSQL_FIELD = ^MYSQL_FIELD;

type
  MYSQL_ROW = ppchar;
  PMYSQL_ROW = ^MYSQL_ROW;
  TMYSQL_ROW = MYSQL_ROW;

  PMYSQL_FIELD_OFFSET = ^MYSQL_FIELD_OFFSET;
  MYSQL_FIELD_OFFSET = dword;

  Pmy_ulonglong = ^my_ulonglong;
  my_ulonglong = qword;

type
  Pst_mysql_rows = ^st_mysql_rows;
  st_mysql_rows = record
     next : Pst_mysql_rows;
     data : MYSQL_ROW;
     length : culong;
  end;
  MYSQL_ROWS = st_mysql_rows;
  TMYSQL_ROWS = MYSQL_ROWS;
  PMYSQL_ROWS = ^MYSQL_ROWS;

  MYSQL_ROW_OFFSET = ^MYSQL_ROWS;
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
       fields : cuint;
       data : PMYSQL_ROWS;
       alloc : MEM_ROOT;
       prev_ptr : ^MYSQL_ROWS;
    end;
  MYSQL_DATA = st_mysql_data;
  TMYSQL_DATA = MYSQL_DATA;
  PMYSQL_DATA = ^MYSQL_DATA;

  mysql_option = (MYSQL_OPT_CONNECT_TIMEOUT,MYSQL_OPT_COMPRESS,
    MYSQL_OPT_NAMED_PIPE,MYSQL_INIT_COMMAND,
    MYSQL_READ_DEFAULT_FILE,MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR,MYSQL_SET_CHARSET_NAME
    );

  Pst_mysql_options = ^st_mysql_options;
  st_mysql_options = record
       connect_timeout,
       read_timeout,
       write_timeout: cuint;
       port,protocol : cuint;
       client_flag : culong;
       host,
       user,
       password,
       unix_socket,
       db : pchar;
       init_commands : pst_dynamic_array;
       my_cnf_file : Pchar;
       my_cnf_group : Pchar;
       charset_dir : Pchar;
       charset_name : Pchar;
       ssl_key : Pchar;
       ssl_cert : Pchar;
       ssl_ca : Pchar;
       ssl_capath : Pchar;
       ssl_cipher : Pchar;
       shared_memory_base_name : pchar;
       max_allowed_packet : culong;
       use_ssl : my_bool;
       compress : my_bool;
       named_pipe : my_bool;
       rpl_probe : my_bool;
       rpl_parse : my_bool;
       no_master_reads : my_bool;
       methods_to_use : mysql_option;
       client_ip : pchar;
       secure_auth : my_bool;
       local_infile_init : pointer;
       local_infile_read : pointer;
       local_infile_end : pointer;
       local_infile_error : pointer;
       local_infile_userdata : pointer;
    end;
  TMYSQL_OPTIONS = st_mysql_options;
  PTMYSQL_OPTIONS = ^TMYSQL_OPTIONS;

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
       thread_id : culong;
       packet_length : culong;
       port : cuint;
       client_flag : culong;
       server_capabilities : culong;
       protocol_version : cuint;
       field_count : cuint;
       server_status : cuint;
       server_language : cuint;
       warning_count : cuint;
       options : st_mysql_options;
       status : mysql_status;
       free_me : my_bool;
       reconnect : my_bool;
       scramble_buff : array[0..20] of char;
       rpl_pivot : my_bool;
       master : Pst_mysql;
       next_slave : Pst_mysql;
       last_used_slave : Pst_mysql;
       last_used_con : Pst_mysql;

       stmts : pointer; // LIST
       methods : pointer; // Pst_mysql_methods
       thd : pointer;
       unbuffered_fetch_owner : pmy_bool;
    end;
  TMYSQL = st_mysql;
  PMYSQL = ^TMYSQL;

  Pst_mysql_res = ^st_mysql_res;
  st_mysql_res = record
       row_count : my_ulonglong;
       fields : PMYSQL_FIELD;
       data : PMYSQL_DATA;
       data_cursor : PMYSQL_ROWS;
       lengths : pculong;
       handle : PMYSQL;
       field_alloc : MEM_ROOT;
       field_count : dword;
       current_field : cuint;
       row : MYSQL_ROW;
       current_row : MYSQL_ROW;
       eof : my_bool;
       unbuffered_fetch_cancelled : my_bool;
       methods : pointer; { was ^my_sql_methods }
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

Type
  TExdendBuffer = function (_para1:pointer; _to:Pchar; length:Pdword):Pchar;


