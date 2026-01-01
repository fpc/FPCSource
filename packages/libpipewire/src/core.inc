
const
  PW_VERSION_CORE = 3;
  PW_VERSION_REGISTRY = 3;
  PW_DEFAULT_REMOTE = 'pipewire-0';
  PW_ID_CORE = 0;
  PW_ID_ANY = tuint32_t($ffffffff);

  PW_VERSION_CORE_EVENTS = 0;

type
  Ppw_core_events = ^Tpw_core_events;
  Ppw_core_info = ^Tpw_core_info;
  Ppw_core_methods = ^Tpw_core_methods;
  Ppw_registry = ^Tpw_registry;
  Ppw_registry_events = ^Tpw_registry_events;
  Ppw_registry_methods = ^Tpw_registry_methods;

  Tpw_core_events = record
    version: Tuint32_t;
    info: procedure(object_: pointer; info: Ppw_core_info); cdecl;
    done: procedure(object_: pointer; id: Tuint32_t; seq: longint); cdecl;
    ping: procedure(object_: pointer; id: Tuint32_t; seq: longint); cdecl;
    error: procedure(object_: pointer; id: Tuint32_t; seq: longint; res: longint; message: PChar); cdecl;
    remove_id: procedure(object_: pointer; id: Tuint32_t); cdecl;
    bound_id: procedure(object_: pointer; id: Tuint32_t; global_id: Tuint32_t); cdecl;
    add_mem: procedure(object_: pointer; id: Tuint32_t; _type: Tuint32_t; fd: longint; flags: Tuint32_t); cdecl;
    remove_mem: procedure(object_: pointer; id: Tuint32_t); cdecl;
  end;

  Tpw_core_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_core_events; Data: pointer) : longint; cdecl;
    hello: function(object_: pointer; version: Tuint32_t) : longint; cdecl;
    sync: function(object_: pointer; id: Tuint32_t; seq: longint) : longint; cdecl;
    pong: function(object_: pointer; id: Tuint32_t; seq: longint) : longint; cdecl;
    error: function(object_: pointer; id: Tuint32_t; seq: longint; res: longint; message: PChar) : longint; cdecl;
    get_registry: function(object_: pointer; version: Tuint32_t; user_data_size: Tsize_t) : Ppw_registry; cdecl;
    create_object: function(object_: pointer; factory_name: PChar; _type: PChar; version: Tuint32_t; props: Pspa_dict; user_data_size: Tsize_t) : pointer; cdecl;
    Destroy: function(object_: pointer; proxy: pointer) : longint; cdecl;
  end;

  Tpw_registry_events = record
    version: Tuint32_t;
    global: procedure(object_: pointer; id: Tuint32_t; permissions: Tuint32_t; _type: PChar; version: Tuint32_t; props: Pspa_dict); cdecl;
    global_remove: procedure(object_: pointer; id: Tuint32_t); cdecl;
  end;

  Tpw_registry_methods = record
    version: Tuint32_t;
    add_listener: function(object_: pointer; listener: Pspa_hook; events: Ppw_registry_events; Data: pointer) : longint; cdecl;
    bind: function(object_: pointer; id: Tuint32_t; _type: PChar; version: Tuint32_t; use_data_size: Tsize_t) : pointer; cdecl;
    Destroy: function(object_: pointer; id: Tuint32_t) : longint; cdecl;
  end;


function pw_core_info_update(info: Ppw_core_info; update: Ppw_core_info) : Ppw_core_info; cdecl; external libpipewirename Name 'pw_core_info_update';
function pw_core_info_merge(info: Ppw_core_info; update: Ppw_core_info; reset: T_Bool) : Ppw_core_info; cdecl; external libpipewirename Name 'pw_core_info_merge';
procedure pw_core_info_free(info: Ppw_core_info); cdecl; external libpipewirename Name 'pw_core_info_free';

function pw_core_sync(pw_core: Ppw_core; aID, aSync: cint) : cint;



  {
  static inline
  __attribute__((format(printf, 5, 0))) int
  pw_core_errorv(struct pw_core *core, uint32_t id, int seq,
    int res, const char *message, va_list args)

   char buffer[1024];
   vsnprintf(buffer, sizeof(buffer), message, args);
   buffer[1023] = '\0';
   return ( int _res = -
  # 342 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
         95
  # 342 "/usr/include/pipewire-0.3/pipewire/core.h"
         ; ( const struct pw_core_methods *_f = (const struct pw_core_methods *) (&((struct spa_interface*)core)->cb)->funcs; if ((__builtin_expect(!!((((_f) && ((0) == 0 || (_f)->version > (0)-1)) && (_f)->error)),1))) _res = _f->error((&((struct spa_interface*)core)->cb)->data, id, seq, res, buffer); _res; ); _res; );


  static inline
  __attribute__((format(printf, 5, 6))) int
  pw_core_errorf(struct pw_core *core, uint32_t id, int seq,
    int res, const char *message, ...)

          va_list args;
   int r;

  # 352 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
  __builtin_va_start(
  # 352 "/usr/include/pipewire-0.3/pipewire/core.h"
  args
  # 352 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
  ,
  # 352 "/usr/include/pipewire-0.3/pipewire/core.h"
  message
  # 352 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
  )
  # 352 "/usr/include/pipewire-0.3/pipewire/core.h"
                         ;
   r = pw_core_errorv(core, id, seq, res, message, args);

  # 354 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
  __builtin_va_end(
  # 354 "/usr/include/pipewire-0.3/pipewire/core.h"
  args
  # 354 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
  )
  # 354 "/usr/include/pipewire-0.3/pipewire/core.h"
              ;
   return r;


  static inline struct pw_registry *
  pw_core_get_registry(struct pw_core *core, uint32_t version, size_t user_data_size)

   struct pw_registry *res =
  # 361 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
                            ((void *)0)
  # 361 "/usr/include/pipewire-0.3/pipewire/core.h"
                                ;
   ( const struct pw_core_methods *_f = (const struct pw_core_methods *) (&((struct spa_interface*)core)->cb)->funcs; if ((__builtin_expect(!!((((_f) && ((0) == 0 || (_f)->version > (0)-1)) && (_f)->get_registry)),1))) res = _f->get_registry((&((struct spa_interface*)core)->cb)->data, version, user_data_size); res; )

                                              ;
   return res;


  static inline void *
  pw_core_create_object(struct pw_core *core,
         const char *factory_name,
         const char *type,
         uint32_t version,
         const struct spa_dict *props,
         size_t user_data_size)

   void *res =
  # 376 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
              ((void *)0)
  # 376 "/usr/include/pipewire-0.3/pipewire/core.h"
                  ;
   ( const struct pw_core_methods *_f = (const struct pw_core_methods *) (&((struct spa_interface*)core)->cb)->funcs; if ((__builtin_expect(!!((((_f) && ((0) == 0 || (_f)->version > (0)-1)) && (_f)->create_object)),1))) res = _f->create_object((&((struct spa_interface*)core)->cb)->data, factory_name, type, version, props, user_data_size); res; )


                                          ;
   return res;


   }
  {
  # 511 "/usr/include/pipewire-0.3/pipewire/core.h"


  static inline void *
  pw_registry_bind(struct pw_registry *registry,
           uint32_t id, const char *type, uint32_t version,
           size_t user_data_size)

   void *res =
  # 516 "/usr/include/pipewire-0.3/pipewire/core.h" 3 4
              ((void *)0)
  # 516 "/usr/include/pipewire-0.3/pipewire/core.h"
                  ;
   ( const struct pw_registry_methods *_f = (const struct pw_registry_methods *) (&((struct spa_interface*)registry)->cb)->funcs; if ((__builtin_expect(!!((((_f) && ((0) == 0 || (_f)->version > (0)-1)) && (_f)->bind)),1))) res = _f->bind((&((struct spa_interface*)registry)->cb)->data, id, type, version, user_data_size); res; )

                                                ;
   return res;


   }

function pw_context_connect(context: Ppw_context; properties: Ppw_properties; user_data_size: Tsize_t) : Ppw_core; cdecl; external libpipewirename Name 'pw_context_connect';

function pw_context_connect_fd(context: Ppw_context; fd: longint; properties: Ppw_properties; user_data_size: Tsize_t) : Ppw_core; cdecl; external libpipewirename Name 'pw_context_connect_fd';

function pw_context_connect_self(context: Ppw_context; properties: Ppw_properties; user_data_size: Tsize_t) : Ppw_core; cdecl; external libpipewirename Name 'pw_context_connect_self';

function pw_core_steal_fd(core: Ppw_core) : longint; cdecl; external libpipewirename Name 'pw_core_steal_fd';

function pw_core_set_paused(core: Ppw_core; paused: T_Bool) : longint; cdecl; external libpipewirename Name 'pw_core_set_paused';

function pw_core_disconnect(core: Ppw_core) : longint; cdecl; external libpipewirename Name 'pw_core_disconnect';

function pw_core_get_user_data(core: Ppw_core) : pointer; cdecl; external libpipewirename Name 'pw_core_get_user_data';

function pw_core_get_client(core: Ppw_core) : Ppw_client; cdecl; external libpipewirename Name 'pw_core_get_client';

function pw_core_get_context(core: Ppw_core) : Ppw_context; cdecl; external libpipewirename Name 'pw_core_get_context';

function pw_core_get_properties(core: Ppw_core) : Ppw_properties; cdecl; external libpipewirename Name 'pw_core_get_properties';

function pw_core_update_properties(core: Ppw_core; dict: Pspa_dict) : longint; cdecl; external libpipewirename Name 'pw_core_update_properties';

function pw_core_get_mempool(core: Ppw_core) : Ppw_mempool; cdecl; external libpipewirename Name 'pw_core_get_mempool';

function pw_core_find_proxy(core: Ppw_core; id: Tuint32_t) : Ppw_proxy; cdecl; external libpipewirename Name 'pw_core_find_proxy';

function pw_core_export(core: Ppw_core; _type: PChar; props: Pspa_dict; object_: pointer; user_data_size: Tsize_t) : Ppw_proxy; cdecl; external libpipewirename Name 'pw_core_export';

function pw_core_add_listener(pw_core: ppw_core; aListener: Pspa_hook; aEvents: Ppw_core_events; aData: Pointer) : cint;

