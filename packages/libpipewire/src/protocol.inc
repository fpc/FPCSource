
type
  Tpw_impl_core = record
  end;

  Ppw_impl_core = ^Tpw_impl_core;
  Ppw_protocol = ^Tpw_protocol;
  Ppw_protocol_client = ^Tpw_protocol_client;
  Ppw_protocol_events = ^Tpw_protocol_events;
  Ppw_protocol_implementation = ^Tpw_protocol_implementation;
  Ppw_protocol_marshal = ^Tpw_protocol_marshal;
  Ppw_protocol_server = ^Tpw_protocol_server;

  Tpw_protocol_clientconnectdonecallback = procedure(Data: pointer; Result: longint); cdecl;

  Tpw_protocol_client = record
    link: Tspa_list;
    protocol: Ppw_protocol;
    core: Ppw_core;
    connect: function(client: Ppw_protocol_client; props: Pspa_dict; done_callback: Tpw_protocol_clientconnectdonecallback; Data: pointer) : longint; cdecl;
    connect_fd: function(client: Ppw_protocol_client; fd: longint; Close: T_Bool) : longint; cdecl;
    steal_fd: function(client: Ppw_protocol_client) : longint; cdecl;
    disconnect: procedure(client: Ppw_protocol_client); cdecl;
    Destroy: procedure(client: Ppw_protocol_client); cdecl;
    set_paused: function(client: Ppw_protocol_client; paused: T_Bool) : longint; cdecl;
  end;

  Tpw_protocol_server = record
    link: Tspa_list;
    protocol: Ppw_protocol;
    core: Ppw_impl_core;
    client_list: Tspa_list;
    Destroy: procedure(listen: Ppw_protocol_server); cdecl;
  end;

  Tpw_protocol_marshal = record
    _type: PChar;
    version: Tuint32_t;
    flags: Tuint32_t;
    n_client_methods: Tuint32_t;
    n_server_methods: Tuint32_t;
    client_marshal: pointer;
    server_demarshal: pointer;
    server_marshal: pointer;
    client_demarshal: pointer;
  end;

  Tpw_protocol_implementation = record
    version: Tuint32_t;
    new_client: function(protocol: Ppw_protocol; core: Ppw_core; props: Pspa_dict) : Ppw_protocol_client; cdecl;
    add_server: function(protocol: Ppw_protocol; core: Ppw_impl_core; props: Pspa_dict) : Ppw_protocol_server; cdecl;
  end;

  Tpw_protocol_events = record
    version: Tuint32_t;
    Destroy: procedure(Data: pointer); cdecl;
  end;


function pw_protocol_new(context: Ppw_context; Name: PChar; user_data_size: Tsize_t) : Ppw_protocol; cdecl; external libpipewirename Name 'pw_protocol_new';

procedure pw_protocol_destroy(protocol: Ppw_protocol); cdecl; external libpipewirename Name 'pw_protocol_destroy';

function pw_protocol_get_context(protocol: Ppw_protocol) : Ppw_context; cdecl; external libpipewirename Name 'pw_protocol_get_context';

function pw_protocol_get_user_data(protocol: Ppw_protocol) : pointer; cdecl; external libpipewirename Name 'pw_protocol_get_user_data';

function pw_protocol_get_implementation(protocol: Ppw_protocol) : Ppw_protocol_implementation; cdecl; external libpipewirename Name 'pw_protocol_get_implementation';

function pw_protocol_get_extension(protocol: Ppw_protocol) : pointer; cdecl; external libpipewirename Name 'pw_protocol_get_extension';

procedure pw_protocol_add_listener(protocol: Ppw_protocol; listener: Pspa_hook; events: Ppw_protocol_events; Data: pointer); cdecl; external libpipewirename Name 'pw_protocol_add_listener';

function pw_protocol_add_marshal(protocol: Ppw_protocol; marshal: Ppw_protocol_marshal) : longint; cdecl; external libpipewirename Name 'pw_protocol_add_marshal';

function pw_protocol_get_marshal(protocol: Ppw_protocol; _type: PChar; version: Tuint32_t; flags: Tuint32_t) : Ppw_protocol_marshal; cdecl; external libpipewirename Name 'pw_protocol_get_marshal';

function pw_context_find_protocol(context: Ppw_context; Name: PChar) : Ppw_protocol; cdecl; external libpipewirename Name 'pw_context_find_protocol';


