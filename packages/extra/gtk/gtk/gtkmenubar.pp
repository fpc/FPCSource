{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkMenuBar = ^TGtkMenuBar;
     TGtkMenuBar = record
          menu_shell : TGtkMenuShell;
          shadow_type : TGtkShadowType;
       end;

     PGtkMenuBarClass = ^TGtkMenuBarClass;
     TGtkMenuBarClass = record
          parent_class : TGtkMenuShellClass;
       end;

Type
  GTK_MENU_BAR=PGtkMenuBar;
  GTK_MENU_BAR_CLASS=PGtkMenuBarClass;

function  GTK_MENU_BAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_menu_bar_get_type';
function  GTK_IS_MENU_BAR(obj:pointer):boolean;
function  GTK_IS_MENU_BAR_CLASS(klass:pointer):boolean;

function  gtk_menu_bar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_menu_bar_get_type';
function  gtk_menu_bar_new:PGtkWidget;cdecl;external gtkdll name 'gtk_menu_bar_new';
procedure gtk_menu_bar_append(menu_bar:PGtkMenuBar; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_bar_append';
procedure gtk_menu_bar_prepend(menu_bar:PGtkMenuBar; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_bar_prepend';
procedure gtk_menu_bar_insert(menu_bar:PGtkMenuBar; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_menu_bar_insert';
procedure gtk_menu_bar_set_shadow_type(menu_bar:PGtkMenuBar; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_menu_bar_set_shadow_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_MENU_BAR(obj:pointer):boolean;
begin
  GTK_IS_MENU_BAR:=(obj<>nil) and GTK_IS_MENU_BAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_MENU_BAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_MENU_BAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_MENU_BAR_TYPE);
end;

{$endif read_implementation}


