
type
  Tpw_filter = record
  end;

  Ppw_filter = ^Tpw_filter;
  Ppw_filter_events = ^Tpw_filter_events;
  Ppw_filter_flags = ^Tpw_filter_flags;
  Ppw_filter_port_flags = ^Tpw_filter_port_flags;
  Ppw_filter_state = ^Tpw_filter_state;

  Tpw_filter_state = (
    PW_FILTER_STATE_ERROR := -(1),
    PW_FILTER_STATE_UNCONNECTED := 0,
    PW_FILTER_STATE_CONNECTING := 1,
    PW_FILTER_STATE_PAUSED := 2,
    PW_FILTER_STATE_STREAMING := 3);

  Tpw_filter_events = record
    version: Tuint32_t;
    Destroy: procedure(Data: pointer); cdecl;
    state_changed: procedure(Data: pointer; old: Tpw_filter_state; state: Tpw_filter_state; error: PChar); cdecl;
    io_changed: procedure(Data: pointer; port_data: pointer; id: Tuint32_t; area: pointer; size: Tuint32_t); cdecl;
    param_changed: procedure(Data: pointer; port_data: pointer; id: Tuint32_t; param: Pspa_pod); cdecl;
    add_buffer: procedure(Data: pointer; port_data: pointer; buffer: Ppw_buffer); cdecl;
    remove_buffer: procedure(Data: pointer; port_data: pointer; buffer: Ppw_buffer); cdecl;
    process: procedure(Data: pointer; position: Pspa_io_position); cdecl;
    drained: procedure(Data: pointer); cdecl;
    command: procedure(Data: pointer; command: Pspa_command); cdecl;
  end;

  Tpw_filter_flags = (
    PW_FILTER_FLAG_NONE := 0,
    PW_FILTER_FLAG_INACTIVE := 1 shl 0,
    PW_FILTER_FLAG_DRIVER := 1 shl 1,
    PW_FILTER_FLAG_RT_PROCESS := 1 shl 2,
    PW_FILTER_FLAG_CUSTOM_LATENCY := 1 shl 3
  );

  Tpw_filter_port_flags = (
    PW_FILTER_PORT_FLAG_NONE := 0,
    PW_FILTER_PORT_FLAG_MAP_BUFFERS := 1 shl 0,
    PW_FILTER_PORT_FLAG_ALLOC_BUFFERS := 1 shl 1
  );


function pw_filter_state_as_string(state: Tpw_filter_state) : PChar; cdecl; external libpipewirename Name 'pw_filter_state_as_string';


function pw_filter_new(core: Ppw_core; Name: PChar; props: Ppw_properties) : Ppw_filter; cdecl; external libpipewirename Name 'pw_filter_new';

function pw_filter_new_simple(loop: Ppw_loop; Name: PChar; props: Ppw_properties; events: Ppw_filter_events; Data: pointer) : Ppw_filter; cdecl; external libpipewirename Name 'pw_filter_new_simple';

procedure pw_filter_destroy(filter: Ppw_filter); cdecl; external libpipewirename Name 'pw_filter_destroy';

procedure pw_filter_add_listener(filter: Ppw_filter; listener: Pspa_hook; events: Ppw_filter_events; Data: pointer); cdecl; external libpipewirename Name 'pw_filter_add_listener';

function pw_filter_get_state(filter: Ppw_filter; error: PPchar) : Tpw_filter_state; cdecl; external libpipewirename Name 'pw_filter_get_state';

function pw_filter_get_name(filter: Ppw_filter) : PChar; cdecl; external libpipewirename Name 'pw_filter_get_name';

function pw_filter_get_core(filter: Ppw_filter) : Ppw_core; cdecl; external libpipewirename Name 'pw_filter_get_core';

function pw_filter_connect(filter: Ppw_filter; flags: Tpw_filter_flags; params: PPspa_pod; n_params: Tuint32_t) : longint; cdecl; external libpipewirename Name 'pw_filter_connect';

function pw_filter_get_node_id(filter: Ppw_filter) : Tuint32_t; cdecl; external libpipewirename Name 'pw_filter_get_node_id';

function pw_filter_disconnect(filter: Ppw_filter) : longint; cdecl; external libpipewirename Name 'pw_filter_disconnect';

function pw_filter_add_port(filter: Ppw_filter; direction: Tspa_direction; flags: Tpw_filter_port_flags; port_data_size: Tsize_t; props: Ppw_properties;
  params: PPspa_pod; n_params: Tuint32_t) : pointer; cdecl; external libpipewirename Name 'pw_filter_add_port';

function pw_filter_remove_port(port_data: pointer) : longint; cdecl; external libpipewirename Name 'pw_filter_remove_port';

function pw_filter_get_properties(filter: Ppw_filter; port_data: pointer) : Ppw_properties; cdecl; external libpipewirename Name 'pw_filter_get_properties';

function pw_filter_update_properties(filter: Ppw_filter; port_data: pointer; dict: Pspa_dict) : longint; cdecl; external libpipewirename Name 'pw_filter_update_properties';

function pw_filter_set_error(filter: Ppw_filter; res: longint; error: PChar; args: array of const) : longint; cdecl; external libpipewirename Name 'pw_filter_set_error';

function pw_filter_set_error(filter: Ppw_filter; res: longint; error: PChar) : longint; cdecl; external libpipewirename Name 'pw_filter_set_error';

function pw_filter_update_params(filter: Ppw_filter; port_data: pointer; params: PPspa_pod; n_params: Tuint32_t) : longint; cdecl; external libpipewirename Name 'pw_filter_update_params';

function pw_filter_get_time(filter: Ppw_filter; time: Ppw_time) : longint; cdecl; external libpipewirename Name 'pw_filter_get_time';

function pw_filter_dequeue_buffer(port_data: pointer) : Ppw_buffer; cdecl; external libpipewirename Name 'pw_filter_dequeue_buffer';

function pw_filter_queue_buffer(port_data: pointer; buffer: Ppw_buffer) : longint; cdecl; external libpipewirename Name 'pw_filter_queue_buffer';

function pw_filter_get_dsp_buffer(port_data: pointer; n_samples: Tuint32_t) : pointer; cdecl; external libpipewirename Name 'pw_filter_get_dsp_buffer';

function pw_filter_set_active(filter: Ppw_filter; active: T_Bool) : longint; cdecl; external libpipewirename Name 'pw_filter_set_active';

function pw_filter_flush(filter: Ppw_filter; drain: T_Bool) : longint; cdecl; external libpipewirename Name 'pw_filter_flush';


