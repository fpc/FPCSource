// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkMenuBar = ^TGtkMenuBar;
   TGtkMenuBar = record
        menu_shell : TGtkMenuShell;
     end;

{ Padding for future expansion  }
   PGtkMenuBarClass = ^TGtkMenuBarClass;
   TGtkMenuBarClass = record
        parent_class : TGtkMenuShellClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_MENU_BAR : GType;
function GTK_MENU_BAR(obj: pointer) : PGtkMenuBar;
function GTK_MENU_BAR_CLASS(klass: pointer) : PGtkMenuBarClass;
function GTK_IS_MENU_BAR(obj: pointer) : boolean;
function GTK_IS_MENU_BAR_CLASS(klass: pointer) : boolean;
function GTK_MENU_BAR_GET_CLASS(obj: pointer) : PGtkMenuBarClass;


function gtk_menu_bar_get_type:TGtkType; cdecl; external gtklib;
function gtk_menu_bar_new:PGtkWidget; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_menu_bar_append(menu,child : PGtkWidget);
procedure gtk_menu_bar_prepend(menu,child : PGtkWidget);
procedure gtk_menu_bar_insert(menu,child : PGtkWidget; pos: gint);
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{ Private functions  }
procedure _gtk_menu_bar_cycle_focus(menubar:PGtkMenuBar; dir:TGtkDirectionType); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MENU_BAR : GType;
begin
   GTK_TYPE_MENU_BAR:=gtk_menu_bar_get_type;
end;

function GTK_MENU_BAR(obj: pointer) : PGtkMenuBar;
begin
   GTK_MENU_BAR:=PGtkMenuBar(GTK_CHECK_CAST(obj,GTK_TYPE_MENU_BAR));
end;

function GTK_MENU_BAR_CLASS(klass: pointer) : PGtkMenuBarClass;
begin
   GTK_MENU_BAR_CLASS:=PGtkMenuBarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_MENU_BAR));
end;

function GTK_IS_MENU_BAR(obj: pointer) : boolean;
begin
   GTK_IS_MENU_BAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_MENU_BAR);
end;

function GTK_IS_MENU_BAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_MENU_BAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_MENU_BAR);
end;

function GTK_MENU_BAR_GET_CLASS(obj: pointer) : PGtkMenuBarClass;
begin
   GTK_MENU_BAR_GET_CLASS:=PGtkMenuBarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_MENU_BAR));
end;

procedure gtk_menu_bar_append(menu,child : PGtkWidget);
begin
   gtk_menu_shell_append(PGtkMenuShell(menu),child);
end;

procedure gtk_menu_bar_prepend(menu,child : PGtkWidget);
begin
   gtk_menu_shell_prepend(PGtkMenuShell(menu),child);
end;

procedure gtk_menu_bar_insert(menu,child : PGtkWidget; pos: gint);
begin
   gtk_menu_shell_insert(PGtkMenuShell(menu),child,pos);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

