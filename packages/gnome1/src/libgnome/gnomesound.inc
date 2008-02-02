{$IfDef read_interface}

var
   gnome_sound_connection : longint;cvar;external;

procedure gnome_sound_init(hostname:Pchar);cdecl;external libgnomedll name 'gnome_sound_init';
procedure gnome_sound_shutdown;cdecl;external libgnomedll name 'gnome_sound_shutdown';
function gnome_sound_sample_load(sample_name:Pchar; filename:Pchar):longint;cdecl;external libgnomedll name 'gnome_sound_sample_load';
procedure gnome_sound_play(filename:Pchar);cdecl;external libgnomedll name 'gnome_sound_play';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
