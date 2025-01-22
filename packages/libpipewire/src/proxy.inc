
Type
  Ppw_proxy_events  = ^Tpw_proxy_events;

  Tpw_proxy_events = record
    version : Tuint32_t;
    destroy : procedure (data:pointer);cdecl;
    bound : procedure (data:pointer; global_id:Tuint32_t);cdecl;
    removed : procedure (data:pointer);cdecl;
    done : procedure (data:pointer; seq:longint);cdecl;
    error : procedure (data:pointer; seq:longint; res:longint; message:Pchar);cdecl;
  end;


function pw_proxy_new(factory:Ppw_proxy; _type:Pchar; version:Tuint32_t; user_data_size:Tsize_t):Ppw_proxy;cdecl;external libpipewirename name 'pw_proxy_new';

procedure pw_proxy_add_listener(proxy:Ppw_proxy; listener:Pspa_hook; events:Ppw_proxy_events; data:pointer);cdecl;external libpipewirename name 'pw_proxy_add_listener';

procedure pw_proxy_add_object_listener(proxy:Ppw_proxy; listener:Pspa_hook; funcs:pointer; data:pointer);cdecl;external libpipewirename name 'pw_proxy_add_object_listener';

procedure pw_proxy_destroy(proxy:Ppw_proxy);cdecl;external libpipewirename name 'pw_proxy_destroy';

procedure pw_proxy_ref(proxy:Ppw_proxy);cdecl;external libpipewirename name 'pw_proxy_ref';

procedure pw_proxy_unref(proxy:Ppw_proxy);cdecl;external libpipewirename name 'pw_proxy_unref';

function pw_proxy_get_user_data(proxy:Ppw_proxy):pointer;cdecl;external libpipewirename name 'pw_proxy_get_user_data';

function pw_proxy_get_id(proxy:Ppw_proxy):Tuint32_t;cdecl;external libpipewirename name 'pw_proxy_get_id';

function pw_proxy_get_type(proxy:Ppw_proxy; version:Puint32_t):Pchar;cdecl;external libpipewirename name 'pw_proxy_get_type';

function pw_proxy_get_protocol(proxy:Ppw_proxy):Ppw_protocol;cdecl;external libpipewirename name 'pw_proxy_get_protocol';

function pw_proxy_sync(proxy:Ppw_proxy; seq:longint):longint;cdecl;external libpipewirename name 'pw_proxy_sync';

function pw_proxy_set_bound_id(proxy:Ppw_proxy; global_id:Tuint32_t):longint;cdecl;external libpipewirename name 'pw_proxy_set_bound_id';

function pw_proxy_get_bound_id(proxy:Ppw_proxy):Tuint32_t;cdecl;external libpipewirename name 'pw_proxy_get_bound_id';

function pw_proxy_error(proxy:Ppw_proxy; res:longint; error:Pchar):longint;cdecl;external libpipewirename name 'pw_proxy_error';

function pw_proxy_errorf(proxy:Ppw_proxy; res:longint; error:Pchar; args:array of const):longint;cdecl;external libpipewirename name 'pw_proxy_errorf';

function pw_proxy_errorf(proxy:Ppw_proxy; res:longint; error:Pchar):longint;cdecl;external libpipewirename name 'pw_proxy_errorf';

function pw_proxy_get_object_listeners(proxy:Ppw_proxy):Pspa_hook_list;cdecl;external libpipewirename name 'pw_proxy_get_object_listeners';

function pw_proxy_get_marshal(proxy:Ppw_proxy):Ppw_protocol_marshal;cdecl;external libpipewirename name 'pw_proxy_get_marshal';

function pw_proxy_install_marshal(proxy:Ppw_proxy; implementor:T_Bool):longint;cdecl;external libpipewirename name 'pw_proxy_install_marshal';


