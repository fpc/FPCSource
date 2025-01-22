
type
  Ppw_node = ^Tpw_node;
  Ppw_node_events = ^Tpw_node_events;
  Ppw_node_info = ^Tpw_node_info;
  Ppw_node_methods = ^Tpw_node_methods;
  Ppw_node_state = ^Tpw_node_state;

  Tpw_node = record
    {undefined structure}
  end;

  Tpw_node_state = (
    PW_NODE_STATE_ERROR := -(1),
    PW_NODE_STATE_CREATING := 0,
    PW_NODE_STATE_SUSPENDED := 1,
    PW_NODE_STATE_IDLE := 2,
    PW_NODE_STATE_RUNNING := 3);




  Tpw_node_info = record
    id: Tuint32_t;
    max_input_ports: Tuint32_t;
    max_output_ports: Tuint32_t;
    change_mask: Tuint64_t;
    n_input_ports: Tuint32_t;
    n_output_ports: Tuint32_t;
    state: Tpw_node_state;
    error: PChar;
    props: Pspa_dict;
    params: Pspa_param_info;
    n_params: Tuint32_t;
  end;

  Tpw_node_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_node_info); cdecl;
    param: procedure(object_: pointer; seq: longint; id: Tuint32_t; index: Tuint32_t; Next: Tuint32_t; param: Pspa_pod); cdecl;
  end;

  Tpw_node_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_node_events; Data: pointer) : longint; cdecl;
    subscribe_params: function(object_: pointer; ids: Puint32_t; n_ids: Tuint32_t) : longint; cdecl;
    enum_params: function(object_: pointer; seq: longint; id: Tuint32_t; start: Tuint32_t; num: Tuint32_t; filter: Pspa_pod) : longint; cdecl;
    set_param: function(object_: pointer; id: Tuint32_t; flags: Tuint32_t; param: Pspa_pod) : longint; cdecl;
    send_command: function(object_: pointer; command: Pspa_command) : longint; cdecl;
  end;



function pw_node_state_as_string(state: Tpw_node_state) : PChar; cdecl; external libpipewirename Name 'pw_node_state_as_string';
function pw_node_info_update(info: Ppw_node_info; update: Ppw_node_info) : Ppw_node_info; cdecl; external libpipewirename Name 'pw_node_info_update';

function pw_node_info_merge(info: Ppw_node_info; update: Ppw_node_info; reset: T_Bool) : Ppw_node_info; cdecl; external libpipewirename Name 'pw_node_info_merge';

procedure pw_node_info_free(info: Ppw_node_info); cdecl; external libpipewirename Name 'pw_node_info_free';


