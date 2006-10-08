{$IfDef read_interface}

type
   PGConfChangeSet = ^TGConfChangeSet;
   TGConfChangeSet = Record
   end;

   TGConfChangeSetForeachFunc = procedure (cs:PGConfChangeSet; key:Pgchar; value:PGConfValue; user_data:gpointer);cdecl;

function gconf_engine_commit_change_set(conf:PGConfEngine; cs:PGConfChangeSet; remove_committed:gboolean; err:PPGError):gboolean;cdecl;external gconfclientdll name 'gconf_engine_commit_change_set';
function gconf_engine_reverse_change_set(conf:PGConfEngine; cs:PGConfChangeSet; err:PPGError):PGConfChangeSet;cdecl;external gconfclientdll name 'gconf_engine_reverse_change_set';
function gconf_engine_change_set_from_currentv(conf:PGConfEngine; keys:PPgchar; err:PPGError):PGConfChangeSet;cdecl;external gconfclientdll name 'gconf_engine_change_set_from_currentv';
function gconf_engine_change_set_from_current(conf:PGConfEngine; err:PPGError; first_key:Pgchar;
  args:array of const):PGConfChangeSet;cdecl;external gconfclientdll name 'gconf_engine_change_set_from_current';
function gconf_engine_change_set_from_current(conf:PGConfEngine; err:PPGError; first_key:Pgchar):PGConfChangeSet;cdecl;varargs;external gconfclientdll name 'gconf_engine_change_set_from_current';
function gconf_change_set_new:PGConfChangeSet;cdecl;external gconfclientdll name 'gconf_change_set_new';
procedure gconf_change_set_ref(cs:PGConfChangeSet);cdecl;external gconfclientdll name 'gconf_change_set_ref';
procedure gconf_change_set_unref(cs:PGConfChangeSet);cdecl;external gconfclientdll name 'gconf_change_set_unref';
procedure gconf_change_set_clear(cs:PGConfChangeSet);cdecl;external gconfclientdll name 'gconf_change_set_clear';
function gconf_change_set_size(cs:PGConfChangeSet):guint;cdecl;external gconfclientdll name 'gconf_change_set_size';
procedure gconf_change_set_remove(cs:PGConfChangeSet; key:Pgchar);cdecl;external gconfclientdll name 'gconf_change_set_remove';
procedure gconf_change_set_foreach(cs:PGConfChangeSet; func:TGConfChangeSetForeachFunc; user_data:gpointer);cdecl;external gconfclientdll name 'gconf_change_set_foreach';
function gconf_change_set_check_value(cs:PGConfChangeSet; key:Pgchar; value_retloc:PPGConfValue):gboolean;cdecl;external gconfclientdll name 'gconf_change_set_check_value';
procedure gconf_change_set_set(cs:PGConfChangeSet; key:Pgchar; value:PGConfValue);cdecl;external gconfclientdll name 'gconf_change_set_set';
procedure gconf_change_set_set_nocopy(cs:PGConfChangeSet; key:Pgchar; value:PGConfValue);cdecl;external gconfclientdll name 'gconf_change_set_set_nocopy';
procedure gconf_change_set_unset(cs:PGConfChangeSet; key:Pgchar);cdecl;external gconfclientdll name 'gconf_change_set_unset';
procedure gconf_change_set_set_float(cs:PGConfChangeSet; key:Pgchar; val:gdouble);cdecl;external gconfclientdll name 'gconf_change_set_set_float';
procedure gconf_change_set_set_int(cs:PGConfChangeSet; key:Pgchar; val:gint);cdecl;external gconfclientdll name 'gconf_change_set_set_int';
procedure gconf_change_set_set_string(cs:PGConfChangeSet; key:Pgchar; val:Pgchar);cdecl;external gconfclientdll name 'gconf_change_set_set_string';
procedure gconf_change_set_set_bool(cs:PGConfChangeSet; key:Pgchar; val:gboolean);cdecl;external gconfclientdll name 'gconf_change_set_set_bool';
procedure gconf_change_set_set_schema(cs:PGConfChangeSet; key:Pgchar; val:PGConfSchema);cdecl;external gconfclientdll name 'gconf_change_set_set_schema';
procedure gconf_change_set_set_list(cs:PGConfChangeSet; key:Pgchar; list_type:TGConfValueType; list:PGSList);cdecl;external gconfclientdll name 'gconf_change_set_set_list';
procedure gconf_change_set_set_pair(cs:PGConfChangeSet; key:Pgchar; car_type:TGConfValueType; cdr_type:TGConfValueType; const address_of_car:gpointer;
            const address_of_cdr:gpointer);cdecl;external gconfclientdll name 'gconf_change_set_set_pair';
procedure gconf_change_set_set_user_data(cs:PGConfChangeSet; data:gpointer; dnotify:TGDestroyNotify);cdecl;external gconfclientdll name 'gconf_change_set_set_user_data';
function gconf_change_set_get_user_data(cs:PGConfChangeSet):gpointer;cdecl;external gconfclientdll name 'gconf_change_set_get_user_data';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
