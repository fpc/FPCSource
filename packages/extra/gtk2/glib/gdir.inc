// {$include <glib/gerror.h>}
    type
      PGDir   = pointer;

    function  g_dir_open(path:Pgchar; flags:guint; error:PPGError):PGDir;cdecl;external gliblib name 'g_dir_open';

    function  g_dir_read_name (dir:PGDir): pgchar; cdecl;external gliblib name 'g_dir_read_name';

    procedure g_dir_rewind(dir:PGDir);cdecl;external gliblib name 'g_dir_rewind';

    procedure g_dir_close(dir:PGDir);cdecl;external gliblib name 'g_dir_close';


