{$IfDef read_interface}

type
   PGConfEngine = ^TGConfEngine;
   TGConfEngine = Record
   end;

function gconf_engine_get_default:PGConfEngine;cdecl;external gconfdll name 'gconf_engine_get_default';
function gconf_engine_get_for_address(address:Pgchar; err:PPGError):PGConfEngine;cdecl;external gconfdll name 'gconf_engine_get_for_address';
procedure gconf_engine_unref(conf:PGConfEngine);cdecl;external gconfdll name 'gconf_engine_unref';
procedure gconf_engine_ref(conf:PGConfEngine);cdecl;external gconfdll name 'gconf_engine_ref';
procedure gconf_engine_set_user_data(engine:PGConfEngine; data:gpointer; dnotify:TGDestroyNotify);cdecl;external gconfdll name 'gconf_engine_set_user_data';
function gconf_engine_get_user_data(engine:PGConfEngine):gpointer;cdecl;external gconfdll name 'gconf_engine_get_user_data';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
