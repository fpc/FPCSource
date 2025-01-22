
type
  Ppw_device = ^Tpw_device;
  Ppw_device_events = ^Tpw_device_events;
  Ppw_device_info = ^Tpw_device_info;
  Ppw_device_methods = ^Tpw_device_methods;


  Tpw_device = record
    {undefined structure}
  end;

  Tpw_device_info = record
    id: Tuint32_t;
    change_mask: Tuint64_t;
    props: Pspa_dict;
    params: Pspa_param_info;
    n_params: Tuint32_t;
  end;

  Tpw_device_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_device_info); cdecl;
    param: procedure(object_: pointer; seq: longint; id: Tuint32_t; index: Tuint32_t; Next: Tuint32_t; param: Pspa_pod); cdecl;
  end;


  Tpw_device_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_device_events; Data: pointer) : longint; cdecl;
    subscribe_params: function(object_: pointer; ids: Puint32_t; n_ids: Tuint32_t) : longint; cdecl;
    enum_params: function(object_: pointer; seq: longint; id: Tuint32_t; start: Tuint32_t; num: Tuint32_t; filter: Pspa_pod) : longint; cdecl;
    set_param: function(object_: pointer; id: Tuint32_t; flags: Tuint32_t; param: Pspa_pod) : longint; cdecl;
  end;


function pw_device_info_update(info: Ppw_device_info; update: Ppw_device_info) : Ppw_device_info; cdecl; external libpipewirename Name 'pw_device_info_update';

function pw_device_info_merge(info: Ppw_device_info; update: Ppw_device_info; reset: T_Bool) : Ppw_device_info; cdecl; external libpipewirename Name 'pw_device_info_merge';

procedure pw_device_info_free(info: Ppw_device_info); cdecl; external libpipewirename Name 'pw_device_info_free';

