{$IfDef read_interface}

var
   gnome_sound_connection : longint;cvar;external;

procedure gnome_sound_init(hostname:PAnsiChar);cdecl;external libgnomedll name 'gnome_sound_init';
procedure gnome_sound_shutdown;cdecl;external libgnomedll name 'gnome_sound_shutdown';
function gnome_sound_sample_load(sample_name:PAnsiChar; filename:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_sound_sample_load';
procedure gnome_sound_play(filename:PAnsiChar);cdecl;external libgnomedll name 'gnome_sound_play';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
