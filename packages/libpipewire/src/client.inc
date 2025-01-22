
type
  Ppw_client = ^Tpw_client;
  Ppw_client_events = ^Tpw_client_events;
  Ppw_client_info = ^Tpw_client_info;
  Ppw_client_methods = ^Tpw_client_methods;

  Tpw_client = record
    {undefined structure}
  end;

  Tpw_client_info = record
    id: Tuint32_t;
    change_mask: Tuint64_t;
    props: Pspa_dict;
  end;

  Tpw_client_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_client_info); cdecl;
    permissions: procedure(object_: pointer; index: Tuint32_t; n_permissions: Tuint32_t; permissions: Ppw_permission); cdecl;
  end;

  Tpw_client_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_client_events; Data: pointer) : longint; cdecl;
    error: function(object_: pointer; id: Tuint32_t; res: longint; message: PChar) : longint; cdecl;
    update_properties: function(object_: pointer; props: Pspa_dict) : longint; cdecl;
    get_permissions: function(object_: pointer; index: Tuint32_t; num: Tuint32_t) : longint; cdecl;
    update_permissions: function(object_: pointer; n_permissions: Tuint32_t; permissions: Ppw_permission) : longint; cdecl;
  end;


function pw_client_info_update(info: Ppw_client_info; update: Ppw_client_info) : Ppw_client_info; cdecl; external libpipewirename Name 'pw_client_info_update';

function pw_client_info_merge(info: Ppw_client_info; update: Ppw_client_info; reset: T_Bool) : Ppw_client_info; cdecl; external libpipewirename Name 'pw_client_info_merge';

procedure pw_client_info_free(info: Ppw_client_info); cdecl; external libpipewirename Name 'pw_client_info_free';


