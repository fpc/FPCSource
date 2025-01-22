
type
  Ppw_link = ^Tpw_link;
  Ppw_link_events = ^Tpw_link_events;
  Ppw_link_info = ^Tpw_link_info;
  Ppw_link_methods = ^Tpw_link_methods;
  Ppw_link_state = ^Tpw_link_state;

  Tpw_link = record
    {undefined structure}
  end;

  Tpw_link_state = (
    PW_LINK_STATE_ERROR := -(2),
    PW_LINK_STATE_UNLINKED := -(1),
    PW_LINK_STATE_INIT := 0,
    PW_LINK_STATE_NEGOTIATING := 1,
    PW_LINK_STATE_ALLOCATING := 2,
    PW_LINK_STATE_PAUSED := 3,
    PW_LINK_STATE_ACTIVE := 4);




  Tpw_link_info = record
    id: Tuint32_t;
    output_node_id: Tuint32_t;
    output_port_id: Tuint32_t;
    input_node_id: Tuint32_t;
    input_port_id: Tuint32_t;
    change_mask: Tuint64_t;
    state: Tpw_link_state;
    error: PChar;
    format: Pspa_pod;
    props: Pspa_dict;
  end;

  Tpw_link_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_link_info); cdecl;
  end;

  Tpw_link_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_link_events; Data: pointer) : longint; cdecl;
  end;



function pw_link_state_as_string(state: Tpw_link_state) : PChar; cdecl; external libpipewirename Name 'pw_link_state_as_string';

function pw_link_info_update(info: Ppw_link_info; update: Ppw_link_info) : Ppw_link_info; cdecl; external libpipewirename Name 'pw_link_info_update';

function pw_link_info_merge(info: Ppw_link_info; update: Ppw_link_info; reset: T_Bool) : Ppw_link_info; cdecl; external libpipewirename Name 'pw_link_info_merge';

procedure pw_link_info_free(info: Ppw_link_info); cdecl; external libpipewirename Name 'pw_link_info_free';



