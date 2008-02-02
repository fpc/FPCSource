{$IfDef read_interface}

procedure gnome_remote_get_command(host:Pchar; argcp:Pgint; argvp:PPPchar);cdecl;external libgnomedll name 'gnome_remote_get_command';
procedure gnome_remote_set_command(host:Pchar; argc:gint; argv:PPchar);cdecl;external libgnomedll name 'gnome_remote_set_command';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
