
  const
    libpipewire='libpipewire-0.3'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

  Type
    Pchar  = ^Tchar;
    Ppw_impl_client  = ^Tpw_impl_client;
    Ppw_protocol  = ^Tpw_protocol;
    Ppw_protocol_marshal  = ^Tpw_protocol_marshal;
    Ppw_resource  = ^Tpw_resource;
    Ppw_resource_events  = ^Tpw_resource_events;
    Pspa_hook  = ^Tspa_hook;
    Pspa_hook_list  = ^Tspa_hook_list;
    Puint32_t  = ^Tuint32_t;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  type
    Ppw_resource_events = ^Tpw_resource_events;
    Tpw_resource_events = record
        version : Tuint32_t;
        destroy : procedure (data:pointer);cdecl;
        pong : procedure (data:pointer; seq:longint);cdecl;
        error : procedure (data:pointer; seq:longint; res:longint; message:Pchar);cdecl;
      end;


  function pw_resource_new(client:Ppw_impl_client; id:Tuint32_t; permissions:Tuint32_t; _type:Pchar; version:Tuint32_t; 
             user_data_size:Tsize_t):Ppw_resource;cdecl;external libpipewire name 'pw_resource_new';

  procedure pw_resource_destroy(resource:Ppw_resource);cdecl;external libpipewire name 'pw_resource_destroy';

  procedure pw_resource_remove(resource:Ppw_resource);cdecl;external libpipewire name 'pw_resource_remove';

  function pw_resource_get_client(resource:Ppw_resource):Ppw_impl_client;cdecl;external libpipewire name 'pw_resource_get_client';

  function pw_resource_get_id(resource:Ppw_resource):Tuint32_t;cdecl;external libpipewire name 'pw_resource_get_id';

  function pw_resource_get_permissions(resource:Ppw_resource):Tuint32_t;cdecl;external libpipewire name 'pw_resource_get_permissions';

  function pw_resource_get_type(resource:Ppw_resource; version:Puint32_t):Pchar;cdecl;external libpipewire name 'pw_resource_get_type';

  function pw_resource_get_protocol(resource:Ppw_resource):Ppw_protocol;cdecl;external libpipewire name 'pw_resource_get_protocol';

  function pw_resource_get_user_data(resource:Ppw_resource):pointer;cdecl;external libpipewire name 'pw_resource_get_user_data';

  procedure pw_resource_add_listener(resource:Ppw_resource; listener:Pspa_hook; events:Ppw_resource_events; data:pointer);cdecl;external libpipewire name 'pw_resource_add_listener';

  procedure pw_resource_add_object_listener(resource:Ppw_resource; listener:Pspa_hook; funcs:pointer; data:pointer);cdecl;external libpipewire name 'pw_resource_add_object_listener';

  function pw_resource_ping(resource:Ppw_resource; seq:longint):longint;cdecl;external libpipewire name 'pw_resource_ping';

  function pw_resource_set_bound_id(resource:Ppw_resource; global_id:Tuint32_t):longint;cdecl;external libpipewire name 'pw_resource_set_bound_id';

  function pw_resource_get_bound_id(resource:Ppw_resource):Tuint32_t;cdecl;external libpipewire name 'pw_resource_get_bound_id';

  procedure pw_resource_error(resource:Ppw_resource; res:longint; error:Pchar);cdecl;external libpipewire name 'pw_resource_error';

  procedure pw_resource_errorf(resource:Ppw_resource; res:longint; error:Pchar; args:array of const);cdecl;external libpipewire name 'pw_resource_errorf';

  procedure pw_resource_errorf(resource:Ppw_resource; res:longint; error:Pchar);cdecl;external libpipewire name 'pw_resource_errorf';

  procedure pw_resource_errorf_id(resource:Ppw_resource; id:Tuint32_t; res:longint; error:Pchar; args:array of const);cdecl;external libpipewire name 'pw_resource_errorf_id';

  procedure pw_resource_errorf_id(resource:Ppw_resource; id:Tuint32_t; res:longint; error:Pchar);cdecl;external libpipewire name 'pw_resource_errorf_id';

  function pw_resource_get_object_listeners(resource:Ppw_resource):Pspa_hook_list;cdecl;external libpipewire name 'pw_resource_get_object_listeners';

  function pw_resource_get_marshal(resource:Ppw_resource):Ppw_protocol_marshal;cdecl;external libpipewire name 'pw_resource_get_marshal';

  function pw_resource_install_marshal(resource:Ppw_resource; implementor:T_Bool):longint;cdecl;external libpipewire name 'pw_resource_install_marshal';


