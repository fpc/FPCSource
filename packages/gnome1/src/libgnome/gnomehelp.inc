{$IfDef read_interface}

type
   PGnomeHelpMenuEntry = ^TGnomeHelpMenuEntry;
   TGnomeHelpMenuEntry = record
        name : Pgchar;
        path : Pgchar;
     end;

function gnome_help_file_find_file(app:Pgchar; path:Pgchar):pgchar;cdecl;external libgnomedll name 'gnome_help_file_find_file';
function gnome_help_file_path(app:Pgchar; path:Pgchar):pgchar;cdecl;external libgnomedll name 'gnome_help_file_path';
procedure gnome_help_display(ignore:pointer; ref:PGnomeHelpMenuEntry);cdecl;external libgnomedll name 'gnome_help_display';
procedure gnome_help_pbox_display(ignore:pointer; page_num:gint; ref:PGnomeHelpMenuEntry);cdecl;external libgnomedll name 'gnome_help_pbox_display';
procedure gnome_help_pbox_goto(ignore:pointer; ignore2:longint; ref:PGnomeHelpMenuEntry);cdecl;external libgnomedll name 'gnome_help_pbox_goto';
procedure gnome_help_goto(ignore:pointer; afile:Pgchar);cdecl;external libgnomedll name 'gnome_help_goto';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
