{$IfDef read_interface}

function gnome_popup_menu_new(uiinfo:PGnomeUIInfo):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_popup_menu_new';
function gnome_popup_menu_new_with_accelgroup(uiinfo:PGnomeUIInfo; accelgroup:PGtkAccelGroup):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_popup_menu_new_with_accelgroup';
function gnome_popup_menu_get_accel_group(menu:PGtkMenu):PGtkAccelGroup;cdecl;external libgnomeuidll name 'gnome_popup_menu_get_accel_group';
procedure gnome_popup_menu_attach(popup:PGtkWidget; widget:PGtkWidget; user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_popup_menu_attach';
procedure gnome_popup_menu_do_popup(popup:PGtkWidget; pos_func:TGtkMenuPositionFunc; pos_data:gpointer; event:PGdkEventButton; user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_popup_menu_do_popup';
function gnome_popup_menu_do_popup_modal(popup:PGtkWidget; pos_func:TGtkMenuPositionFunc; pos_data:gpointer; event:PGdkEventButton; user_data:gpointer):longint;cdecl;external libgnomeuidll name 'gnome_popup_menu_do_popup_modal';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
