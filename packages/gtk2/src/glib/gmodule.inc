// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
 PGModule = pointer;
 TGModuleFlags =  Longint;

 TGModuleCheckInit = function (module:PGModule):Pgchar;cdecl;
 TGModuleUnload = procedure (module:PGModule);cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  G_MODULE_BIND_LAZY = 1 shl 0;
  G_MODULE_BIND_MASK = 1;

function  g_module_supported:gboolean;cdecl; external gmodulelib name 'g_module_supported';
function  g_module_open(file_name:Pgchar; flags:TGModuleFlags):PGModule;cdecl;external gmodulelib name 'g_module_open';
function  g_module_close(module:PGModule):gboolean;cdecl;external gmodulelib name 'g_module_close';
procedure g_module_make_resident(module:PGModule);cdecl;external gmodulelib name 'g_module_make_resident';
function  g_module_error: Pgchar;cdecl;external gmodulelib name 'g_module_error';
function  g_module_symbol(module:PGModule; symbol_name:Pgchar; symbol:Pgpointer):  gboolean;cdecl;external gmodulelib name 'g_module_symbol';
function  g_module_name(module:PGModule):Pgchar;cdecl;external gmodulelib name 'g_module_name';
function  g_module_build_path(directory:Pgchar; module_name:Pgchar):Pgchar;cdecl;external gmodulelib name 'g_module_build_path';

{$ENDIF read_interface_rest}
// included by glib2.pas
