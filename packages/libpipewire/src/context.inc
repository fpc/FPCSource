
type
  Ppw_context = ^Tpw_context;
  Ppw_context_events = ^Tpw_context_events;
  Ppw_core = ^Tpw_core;
  Ppw_export_type = ^Tpw_export_type;
  Ppw_global = ^Tpw_global;
  Ppw_impl_client = ^Tpw_impl_client;
  Ppw_proxy = ^Tpw_proxy;



  Tpw_context_events = record
    version: Tuint32_t;
    Destroy: procedure(Data: pointer); cdecl;
    Free: procedure(Data: pointer); cdecl;
    check_access: procedure(Data: pointer; client: Ppw_impl_client); cdecl;
    global_added: procedure(Data: pointer; global: Ppw_global); cdecl;
    global_removed: procedure(Data: pointer; global: Ppw_global); cdecl;
  end;

  Tpw_export_type = record
    link: Tspa_list;
    _type: PChar;
    func: function(core: Ppw_core; _type: PChar; props: Pspa_dict; object_: pointer; user_data_size: Tsize_t) : Ppw_proxy; cdecl;
  end;


  tpw_context_conf_section_for_eachcallback = function(Data: pointer; location: PChar; section: PChar; str: PChar; len: Tsize_t) : longint; cdecl;

  tpw_context_conf_section_match_rulescallback = function(Data: pointer; location: PChar; action: PChar; str: PChar; len: Tsize_t) : longint; cdecl;

  tpw_context_for_each_globalcallback = function(Data: pointer; global: Ppw_global) : longint; cdecl;

function pw_context_new(main_loop: Ppw_loop; props: Ppw_properties; user_data_size: Tsize_t) : Ppw_context; cdecl; external libpipewirename Name 'pw_context_new';

procedure pw_context_destroy(context: Ppw_context); cdecl; external libpipewirename Name 'pw_context_destroy';

function pw_context_get_user_data(context: Ppw_context) : pointer; cdecl; external libpipewirename Name 'pw_context_get_user_data';

procedure pw_context_add_listener(context: Ppw_context; listener: Pspa_hook; events: Ppw_context_events; Data: pointer); cdecl; external libpipewirename Name 'pw_context_add_listener';

function pw_context_get_properties(context: Ppw_context) : Ppw_properties; cdecl; external libpipewirename Name 'pw_context_get_properties';

function pw_context_update_properties(context: Ppw_context; dict: Pspa_dict) : longint; cdecl; external libpipewirename Name 'pw_context_update_properties';

function pw_context_get_conf_section(context: Ppw_context; section: PChar) : PChar; cdecl; external libpipewirename Name 'pw_context_get_conf_section';

function pw_context_parse_conf_section(context: Ppw_context; conf: Ppw_properties; section: PChar) : longint; cdecl; external libpipewirename Name 'pw_context_parse_conf_section';

function pw_context_conf_update_props(context: Ppw_context; section: PChar; props: Ppw_properties) : longint; cdecl; external libpipewirename Name 'pw_context_conf_update_props';

function pw_context_conf_section_for_each(context: Ppw_context; section: PChar; callback: tpw_context_conf_section_for_eachcallback; Data: pointer) : longint;
  cdecl; external libpipewirename Name 'pw_context_conf_section_for_each';

function pw_context_conf_section_match_rules(context: Ppw_context; section: PChar; props: Pspa_dict; callback: tpw_context_conf_section_match_rulescallback;
  Data: pointer) : longint; cdecl; external libpipewirename Name 'pw_context_conf_section_match_rules';

function pw_context_get_support(context: Ppw_context; n_support: Puint32_t) : Pspa_support; cdecl; external libpipewirename Name 'pw_context_get_support';

function pw_context_get_main_loop(context: Ppw_context) : Ppw_loop; cdecl; external libpipewirename Name 'pw_context_get_main_loop';

function pw_context_get_work_queue(context: Ppw_context) : Ppw_work_queue; cdecl; external libpipewirename Name 'pw_context_get_work_queue';

function pw_context_for_each_global(context: Ppw_context; callback: tpw_context_for_each_globalcallback; Data: pointer) : longint; cdecl; external libpipewirename Name 'pw_context_for_each_global';

function pw_context_find_global(context: Ppw_context; id: Tuint32_t) : Ppw_global; cdecl; external libpipewirename Name 'pw_context_find_global';

function pw_context_add_spa_lib(context: Ppw_context; factory_regex: PChar; lib: PChar) : longint; cdecl; external libpipewirename Name 'pw_context_add_spa_lib';

function pw_context_find_spa_lib(context: Ppw_context; factory_name: PChar) : PChar; cdecl; external libpipewirename Name 'pw_context_find_spa_lib';

function pw_context_load_spa_handle(context: Ppw_context; factory_name: PChar; info: Pspa_dict) : Pspa_handle; cdecl; external libpipewirename Name 'pw_context_load_spa_handle';



function pw_context_register_export_type(context: Ppw_context; _type: Ppw_export_type) : longint; cdecl; external libpipewirename Name 'pw_context_register_export_type';

function pw_context_find_export_type(context: Ppw_context; _type: PChar) : Ppw_export_type; cdecl; external libpipewirename Name 'pw_context_find_export_type';

function pw_context_set_object(context: Ppw_context; _type: PChar; Value: pointer) : longint; cdecl; external libpipewirename Name 'pw_context_set_object';

function pw_context_get_object(context: Ppw_context; _type: PChar) : pointer; cdecl; external libpipewirename Name 'pw_context_get_object';


