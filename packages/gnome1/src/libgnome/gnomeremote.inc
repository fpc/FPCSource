{$IfDef read_interface}

procedure gnome_remote_get_command(host:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar);cdecl;external libgnomedll name 'gnome_remote_get_command';
procedure gnome_remote_set_command(host:PAnsiChar; argc:gint; argv:PPAnsiChar);cdecl;external libgnomedll name 'gnome_remote_set_command';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
