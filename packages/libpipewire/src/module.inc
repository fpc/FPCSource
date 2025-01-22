

Type
  Ppw_module  = ^Tpw_module;
  Ppw_module_events  = ^Tpw_module_events;
  Ppw_module_info  = ^Tpw_module_info;
  Ppw_module_methods  = ^Tpw_module_methods;

  Tpw_module = record
    {undefined structure}
  end;

  Tpw_module_info = record
    id : Tuint32_t;
    name : Pchar;
    filename : Pchar;
    args : Pchar;
    change_mask : Tuint64_t;
    props : Pspa_dict;
  end;

  Tpw_module_events = record
    version : Tuint32_t;
    info : procedure (object_:pointer; info:Ppw_module_info);cdecl;
  end;

  Tpw_module_methods = record
    version : Tuint32_t;
    add_listener : function (object_:pointer; listener:Pspa_hook; events:Ppw_module_events; data:pointer):longint;cdecl;
  end;


function pw_module_info_update(info:Ppw_module_info; update:Ppw_module_info):Ppw_module_info;cdecl;external libpipewirename name 'pw_module_info_update';

function pw_module_info_merge(info:Ppw_module_info; update:Ppw_module_info; reset:T_Bool):Ppw_module_info;cdecl;external libpipewirename name 'pw_module_info_merge';

procedure pw_module_info_free(info:Ppw_module_info);cdecl;external libpipewirename name 'pw_module_info_free';



