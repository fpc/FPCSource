{$IfDef read_interface}

function gnome_canvas_load_alpha(thefile:Pgchar):PGdkImlibImage;cdecl;external libgnomeuidll name 'gnome_canvas_load_alpha';
procedure gnome_canvas_destroy_image(image:PGdkImlibImage);cdecl;external libgnomeuidll name 'gnome_canvas_destroy_image';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
