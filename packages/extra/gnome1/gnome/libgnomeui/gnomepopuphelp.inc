{$IfDef read_interface}

Procedure gnome_widget_add_help(widget : PGTKWidget; help : PGChar);

procedure gnome_widget_add_help_with_uidata(widget:PGtkWidget; help:Pgchar; menuinfo:PGnomeUIInfo; user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_widget_add_help_with_uidata';

{$EndIf read_interface}

{$Ifdef read_implementation}

Procedure gnome_widget_add_help(widget : PGTKWidget; help : PGChar);
begin
  gnome_widget_add_help_with_uidata(widget,help,nil,nil);
end;

{$Endif read_implementation}
