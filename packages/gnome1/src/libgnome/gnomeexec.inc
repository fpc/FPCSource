{$IfDef read_interface}

function gnome_execute_async(dir:PAnsiChar; argc:longint; argv:PPAnsiChar):longint;cdecl;external libgnomedll name 'gnome_execute_async';
function gnome_execute_async_fds(dir:PAnsiChar; argc:longint; argv:PPAnsiChar; close_fds:gboolean):longint;cdecl;external libgnomedll name 'gnome_execute_async_fds';
function gnome_execute_async_with_env(dir:PAnsiChar; argc:longint; argv:PPAnsiChar; envc:longint; envv:PPAnsiChar):longint;cdecl;external libgnomedll name 'gnome_execute_async_with_env';
function gnome_execute_async_with_env_fds(dir:PAnsiChar; argc:longint; argv:PPAnsiChar; envc:longint; envv:PPAnsiChar; close_fds:gboolean):longint;cdecl;external libgnomedll name 'gnome_execute_async_with_env_fds';
function gnome_execute_shell(dir:PAnsiChar; commandline:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_execute_shell';
function gnome_execute_shell_fds(dir:PAnsiChar; commandline:PAnsiChar; close_fds:gboolean):longint;cdecl;external libgnomedll name 'gnome_execute_shell_fds';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
