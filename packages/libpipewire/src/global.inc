
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
    Ppw_context  = ^Tpw_context;
    Ppw_global  = ^Tpw_global;
    Ppw_global_events  = ^Tpw_global_events;
    Ppw_impl_client  = ^Tpw_impl_client;
    Ppw_properties  = ^Tpw_properties;
    Ppw_resource  = ^Tpw_resource;
    Pspa_dict  = ^Tspa_dict;
    Pspa_hook  = ^Tspa_hook;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  type

    Tpw_global_bind_func_t = function (object:pointer; client:Ppw_impl_client; permissions:Tuint32_t; version:Tuint32_t; id:Tuint32_t):longint;cdecl;
    Ppw_global_events = ^Tpw_global_events;
    Tpw_global_events = record
        version : Tuint32_t;cdecl;
        destroy : procedure (data:pointer);cdecl;
        free : procedure (data:pointer);cdecl;
        permissions_changed : procedure (data:pointer; client:Ppw_impl_client; old_permissions:Tuint32_t; new_permissions:Tuint32_t);cdecl;
      end;


  function pw_global_new(context:Ppw_context; _type:Pchar; version:Tuint32_t; properties:Ppw_properties; func:Tpw_global_bind_func_t;
             object:pointer):Ppw_global;cdecl;external libpipewire name 'pw_global_new';

  function pw_global_register(global:Ppw_global):longint;cdecl;external libpipewire name 'pw_global_register';

  procedure pw_global_add_listener(global:Ppw_global; listener:Pspa_hook; events:Ppw_global_events; data:pointer);cdecl;external libpipewire name 'pw_global_add_listener';

  function pw_global_get_permissions(global:Ppw_global; client:Ppw_impl_client):Tuint32_t;cdecl;external libpipewire name 'pw_global_get_permissions';

  function pw_global_get_context(global:Ppw_global):Ppw_context;cdecl;external libpipewire name 'pw_global_get_context';

  function pw_global_get_type(global:Ppw_global):Pchar;cdecl;external libpipewire name 'pw_global_get_type';

  function pw_global_is_type(global:Ppw_global; _type:Pchar):T_Bool;cdecl;external libpipewire name 'pw_global_is_type';

  function pw_global_get_version(global:Ppw_global):Tuint32_t;cdecl;external libpipewire name 'pw_global_get_version';

  function pw_global_get_properties(global:Ppw_global):Ppw_properties;cdecl;external libpipewire name 'pw_global_get_properties';

  function pw_global_update_keys(global:Ppw_global; dict:Pspa_dict; keys:PPchar):longint;cdecl;external libpipewire name 'pw_global_update_keys';

  function pw_global_get_object(global:Ppw_global):pointer;cdecl;external libpipewire name 'pw_global_get_object';

  function pw_global_get_id(global:Ppw_global):Tuint32_t;cdecl;external libpipewire name 'pw_global_get_id';

  function pw_global_get_serial(global:Ppw_global):Tuint64_t;cdecl;external libpipewire name 'pw_global_get_serial';

  function pw_global_add_resource(global:Ppw_global; resource:Ppw_resource):longint;cdecl;external libpipewire name 'pw_global_add_resource';

  function pw_global_for_each_resource(global:Ppw_global; callback:function (data:pointer; resource:Ppw_resource):longint; data:pointer):longint;cdecl;external libpipewire name 'pw_global_for_each_resource';

  function pw_global_bind(global:Ppw_global; client:Ppw_impl_client; permissions:Tuint32_t; version:Tuint32_t; id:Tuint32_t):longint;cdecl;external libpipewire name 'pw_global_bind';

  function pw_global_update_permissions(global:Ppw_global; client:Ppw_impl_client; old_permissions:Tuint32_t; new_permissions:Tuint32_t):longint;cdecl;external libpipewire name 'pw_global_update_permissions';

  procedure pw_global_destroy(global:Ppw_global);cdecl;external libpipewire name 'pw_global_destroy';


