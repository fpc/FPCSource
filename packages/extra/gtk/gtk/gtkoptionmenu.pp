{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkOptionMenu = ^TGtkOptionMenu;
     TGtkOptionMenu = record
          button : TGtkButton;
          menu : PGtkWidget;
          menu_item : PGtkWidget;
          width : guint16;
          height : guint16;
       end;

     PGtkOptionMenuClass = ^TGtkOptionMenuClass;
     TGtkOptionMenuClass = record
          parent_class : TGtkButtonClass;
       end;

Type
  GTK_OPTION_MENU=PGtkOptionMenu;
  GTK_OPTION_MENU_CLASS=PGtkOptionMenuClass;

function  GTK_OPTION_MENU_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_option_menu_get_type';
function  GTK_IS_OPTION_MENU(obj:pointer):boolean;
function  GTK_IS_OPTION_MENU_CLASS(klass:pointer):boolean;

function  gtk_option_menu_get_type:TGtkType;cdecl;external gtkdll name 'gtk_option_menu_get_type';
function  gtk_option_menu_new:PGtkWidget;cdecl;external gtkdll name 'gtk_option_menu_new';
function  gtk_option_menu_get_menu(option_menu:PGtkOptionMenu):PGtkWidget;cdecl;external gtkdll name 'gtk_option_menu_get_menu';
procedure gtk_option_menu_set_menu(option_menu:PGtkOptionMenu; menu:PGtkWidget);cdecl;external gtkdll name 'gtk_option_menu_set_menu';
procedure gtk_option_menu_remove_menu(option_menu:PGtkOptionMenu);cdecl;external gtkdll name 'gtk_option_menu_remove_menu';
procedure gtk_option_menu_set_history(option_menu:PGtkOptionMenu; index:guint);cdecl;external gtkdll name 'gtk_option_menu_set_history';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_OPTION_MENU(obj:pointer):boolean;
begin
  GTK_IS_OPTION_MENU:=(obj<>nil) and GTK_IS_OPTION_MENU_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_OPTION_MENU_CLASS(klass:pointer):boolean;
begin
  GTK_IS_OPTION_MENU_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_OPTION_MENU_TYPE);
end;

{$endif read_implementation}


