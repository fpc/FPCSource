{$IfDef read_interface}

function gnome_score_init(gamename:Pgchar):gint;cdecl;external libgnomedll name 'gnome_score_init';
function gnome_score_log(score:gfloat; level:Pgchar; higher_to_lower_score_order:gboolean):gint;cdecl;external libgnomedll name 'gnome_score_log';
function gnome_score_get_notable(gamename:Pgchar; level:Pgchar; names:PPPgchar; scores:PPgfloat; scoretimes:PPtime_t):gint;cdecl;external libgnomedll name 'gnome_score_get_notable';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
