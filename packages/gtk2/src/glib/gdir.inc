// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGDir   = pointer;
{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}
function  g_dir_open(path:Pgchar; flags:guint; error:PPGError):PGDir;cdecl;external gliblib name 'g_dir_open';
function  g_dir_read_name (dir:PGDir): pgchar; cdecl;external gliblib name 'g_dir_read_name';
procedure g_dir_rewind(dir:PGDir);cdecl;external gliblib name 'g_dir_rewind';
procedure g_dir_close(dir:PGDir);cdecl;external gliblib name 'g_dir_close';
{$ENDIF read_interface_rest}
