{$IfDef read_interface}

function gnome_parse_geometry(geometry:Pgchar; xpos:Pgint; ypos:Pgint; width:Pgint; height:Pgint):gboolean;cdecl;external libgnomeuidll name 'gnome_parse_geometry';
function gnome_geometry_string(window:PGdkWindow):Pgchar;cdecl;external libgnomeuidll name 'gnome_geometry_string';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
