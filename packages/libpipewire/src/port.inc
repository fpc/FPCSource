
const
  PW_DIRECTION_INPUT = SPA_DIRECTION_INPUT;

type
  Ppw_port = ^Tpw_port;
  Ppw_port_events = ^Tpw_port_events;
  Ppw_port_info = ^Tpw_port_info;
  Ppw_port_methods = ^Tpw_port_methods;

  Tpw_port = record
    {undefined structure}
  end;


  Tpw_port_info = record
    id: Tuint32_t;
    direction: Tspa_direction;
    change_mask: Tuint64_t;
    props: Pspa_dict;
    params: Pspa_param_info;
    n_params: Tuint32_t;
  end;

  Tpw_port_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_port_info); cdecl;
    param: procedure(object_: pointer; seq: longint; id: Tuint32_t; index: Tuint32_t; Next: Tuint32_t; param: Pspa_pod); cdecl;
  end;

  Tpw_port_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_port_events; Data: pointer) : longint; cdecl;
    subscribe_params: function(object_: pointer; ids: Puint32_t; n_ids: Tuint32_t) : longint; cdecl;
    enum_params: function(object_: pointer; seq: longint; id: Tuint32_t; start: Tuint32_t; num: Tuint32_t; filter: Pspa_pod) : longint; cdecl;
  end;



function pw_direction_as_string(direction: Tspa_direction) : PChar; cdecl; external libpipewirename Name 'pw_direction_as_string';

function pw_port_info_update(info: Ppw_port_info; update: Ppw_port_info) : Ppw_port_info; cdecl; external libpipewirename Name 'pw_port_info_update';

function pw_port_info_merge(info: Ppw_port_info; update: Ppw_port_info; reset: T_Bool) : Ppw_port_info; cdecl; external libpipewirename Name 'pw_port_info_merge';

procedure pw_port_info_free(info: Ppw_port_info); cdecl; external libpipewirename Name 'pw_port_info_free';


