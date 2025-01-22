
type
  Ppw_factory = ^Tpw_factory;
  Ppw_factory_events = ^Tpw_factory_events;
  Ppw_factory_info = ^Tpw_factory_info;
  Ppw_factory_methods = ^Tpw_factory_methods;

  Tpw_factory = record
    {undefined structure}
  end;

  Tpw_factory_info = record
    id: Tuint32_t;
    Name: PChar;
    _type: PChar;
    version: Tuint32_t;
    change_mask: Tuint64_t;
    props: Pspa_dict;
  end;

  Tpw_factory_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_factory_info); cdecl;
  end;

  Tpw_factory_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_factory_events; Data: pointer) : longint; cdecl;
  end;


function pw_factory_info_update(info: Ppw_factory_info; update: Ppw_factory_info) : Ppw_factory_info; cdecl; external libpipewirename Name 'pw_factory_info_update';

function pw_factory_info_merge(info: Ppw_factory_info; update: Ppw_factory_info; reset: T_Bool) : Ppw_factory_info; cdecl; external libpipewirename Name 'pw_factory_info_merge';

procedure pw_factory_info_free(info: Ppw_factory_info); cdecl; external libpipewirename Name 'pw_factory_info_free';



