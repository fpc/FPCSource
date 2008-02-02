{$IfDef read_interface}

type
   PGConfListeners = ^TGConfListeners;

   TGConfListeners = Record
   end;

   TGConfListenersCallback = procedure (listeners:PGConfListeners; all_above_key:Pgchar; cnxn_id:guint; listener_data:gpointer; user_data:gpointer);cdecl;

   TGConfListenersForeach = procedure (location:Pgchar; cnxn_id:guint; listener_data:gpointer; user_data:gpointer);cdecl;

   TGConfListenersPredicate = function (location:Pgchar; cnxn_id:guint; listener_data:gpointer; user_data:gpointer):gboolean;cdecl;

function gconf_listeners_new:PGConfListeners;cdecl;external gconfclientdll name 'gconf_listeners_new';
procedure gconf_listeners_free(listeners:PGConfListeners);cdecl;external gconfclientdll name 'gconf_listeners_free';
function gconf_listeners_add(listeners:PGConfListeners; listen_point:Pgchar; listener_data:gpointer; destroy_notify:TGFreeFunc):guint;cdecl;external gconfclientdll name 'gconf_listeners_add';
procedure gconf_listeners_remove(listeners:PGConfListeners; cnxn_id:guint);cdecl;external gconfclientdll name 'gconf_listeners_remove';
procedure gconf_listeners_notify(listeners:PGConfListeners; all_above:Pgchar; callback:TGConfListenersCallback; user_data:gpointer);cdecl;external gconfclientdll name 'gconf_listeners_notify';
function gconf_listeners_count(listeners:PGConfListeners):guint;cdecl;external gconfclientdll name 'gconf_listeners_count';
procedure gconf_listeners_foreach(listeners:PGConfListeners; callback:TGConfListenersForeach; user_data:gpointer);cdecl;external gconfclientdll name 'gconf_listeners_foreach';
function gconf_listeners_get_data(listeners:PGConfListeners; cnxn_id:guint; listener_data_p:Pgpointer; location_p:PPgchar):gboolean;cdecl;external gconfclientdll name 'gconf_listeners_get_data';
procedure gconf_listeners_remove_if(listeners:PGConfListeners; predicate:TGConfListenersPredicate; user_data:gpointer);cdecl;external gconfclientdll name 'gconf_listeners_remove_if';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
