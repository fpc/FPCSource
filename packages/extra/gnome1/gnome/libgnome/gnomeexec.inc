{$IfDef read_interface}

function gnome_execute_async(dir:Pchar; argc:longint; argv:PPchar):longint;cdecl;external libgnomedll name 'gnome_execute_async';
function gnome_execute_async_fds(dir:Pchar; argc:longint; argv:PPchar; close_fds:gboolean):longint;cdecl;external libgnomedll name 'gnome_execute_async_fds';
function gnome_execute_async_with_env(dir:Pchar; argc:longint; argv:PPchar; envc:longint; envv:PPchar):longint;cdecl;external libgnomedll name 'gnome_execute_async_with_env';
function gnome_execute_async_with_env_fds(dir:Pchar; argc:longint; argv:PPchar; envc:longint; envv:PPchar; close_fds:gboolean):longint;cdecl;external libgnomedll name 'gnome_execute_async_with_env_fds';
function gnome_execute_shell(dir:Pchar; commandline:Pchar):longint;cdecl;external libgnomedll name 'gnome_execute_shell';
function gnome_execute_shell_fds(dir:Pchar; commandline:Pchar; close_fds:gboolean):longint;cdecl;external libgnomedll name 'gnome_execute_shell_fds';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
