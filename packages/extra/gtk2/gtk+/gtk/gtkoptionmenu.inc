// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkOptionMenu = ^TGtkOptionMenu;
   TGtkOptionMenu = record
        button : TGtkButton;
        menu : PGtkWidget;
        menu_item : PGtkWidget;
        width : guint16;
        height : guint16;
     end;

{ Padding for future expansion  }
   PGtkOptionMenuClass = ^TGtkOptionMenuClass;
   TGtkOptionMenuClass = record
        parent_class : TGtkButtonClass;
        changed : procedure (option_menu:PGtkOptionMenu); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_OPTION_MENU : GType;
function GTK_OPTION_MENU(obj: pointer) : PGtkOptionMenu;
function GTK_OPTION_MENU_CLASS(klass: pointer) : PGtkOptionMenuClass;
function GTK_IS_OPTION_MENU(obj: pointer) : boolean;
function GTK_IS_OPTION_MENU_CLASS(klass: pointer) : boolean;
function GTK_OPTION_MENU_GET_CLASS(obj: pointer) : PGtkOptionMenuClass;


function gtk_option_menu_get_type:TGtkType; cdecl; external gtklib;
function gtk_option_menu_new:PGtkWidget; cdecl; external gtklib;
function gtk_option_menu_get_menu(option_menu:PGtkOptionMenu):PGtkWidget; cdecl; external gtklib;
procedure gtk_option_menu_set_menu(option_menu:PGtkOptionMenu; menu:PGtkWidget); cdecl; external gtklib;
procedure gtk_option_menu_remove_menu(option_menu:PGtkOptionMenu); cdecl; external gtklib;
function gtk_option_menu_get_history(option_menu:PGtkOptionMenu):gint; cdecl; external gtklib;
procedure gtk_option_menu_set_history(option_menu:PGtkOptionMenu; index:guint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_OPTION_MENU : GType;
begin
   GTK_TYPE_OPTION_MENU:=gtk_option_menu_get_type;
end;

function GTK_OPTION_MENU(obj: pointer) : PGtkOptionMenu;
begin
   GTK_OPTION_MENU:=PGtkOptionMenu(GTK_CHECK_CAST(obj,GTK_TYPE_OPTION_MENU));
end;

function GTK_OPTION_MENU_CLASS(klass: pointer) : PGtkOptionMenuClass;
begin
   GTK_OPTION_MENU_CLASS:=PGtkOptionMenuClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_OPTION_MENU));
end;

function GTK_IS_OPTION_MENU(obj: pointer) : boolean;
begin
   GTK_IS_OPTION_MENU:=GTK_CHECK_TYPE(obj,GTK_TYPE_OPTION_MENU);
end;

function GTK_IS_OPTION_MENU_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_OPTION_MENU_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_OPTION_MENU);
end;

function GTK_OPTION_MENU_GET_CLASS(obj: pointer) : PGtkOptionMenuClass;
begin
   GTK_OPTION_MENU_GET_CLASS:=PGtkOptionMenuClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_OPTION_MENU));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

