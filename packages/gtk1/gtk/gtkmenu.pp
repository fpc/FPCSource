{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkMenu = ^TGtkMenu;

     TGtkMenuPositionFunc = procedure (menu:PGtkMenu; x:Pgint; y:Pgint; user_data:gpointer); cdecl;
     TGtkMenuDetachFunc = procedure (attach_widget:PGtkWidget; menu:PGtkMenu); cdecl;

     TGtkMenu = record
          menu_shell : TGtkMenuShell;
          parent_menu_item : PGtkWidget;
          old_active_menu_item : PGtkWidget;
          accel_group : PGtkAccelGroup;
          position_func : TGtkMenuPositionFunc;
          position_func_data : gpointer;
          toplevel : PGtkWidget;
          tearoff_window : PGtkWidget;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkMenu_torn_off = $1;
     bp_TGtkMenu_torn_off = 0;
function  torn_off(var a : TGtkMenu) : guint;
procedure set_torn_off(var a : TGtkMenu; __torn_off : guint);

type
     PGtkMenuClass = ^TGtkMenuClass;
     TGtkMenuClass = record
          parent_class : TGtkMenuShellClass;
       end;

Type
  GTK_MENU = PGtkMenu;
  GTK_MENU_CLASS = PGtkMenuClass;

function  GTK_MENU_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_menu_get_type';
function  GTK_IS_MENU(obj:pointer):boolean;
function  GTK_IS_MENU_CLASS(klass:pointer):boolean;

function  gtk_menu_get_type:TGtkType;cdecl;external gtkdll name 'gtk_menu_get_type';
function  gtk_menu_new:PGtkWidget;cdecl;external gtkdll name 'gtk_menu_new';
procedure gtk_menu_append(menu:PGtkMenu; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_append';
procedure gtk_menu_prepend(menu:PGtkMenu; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_prepend';
procedure gtk_menu_insert(menu:PGtkMenu; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_menu_insert';
procedure gtk_menu_popup(menu:PGtkMenu; parent_menu_shell:PGtkWidget; parent_menu_item:PGtkWidget; func:TGtkMenuPositionFunc; data:gpointer;button:guint; activate_time:guint32);cdecl;external gtkdll name 'gtk_menu_popup';
procedure gtk_menu_reposition(menu:PGtkMenu);cdecl;external gtkdll name 'gtk_menu_reposition';
procedure gtk_menu_popdown(menu:PGtkMenu);cdecl;external gtkdll name 'gtk_menu_popdown';
function  gtk_menu_get_active(menu:PGtkMenu):PGtkWidget;cdecl;external gtkdll name 'gtk_menu_get_active';
procedure gtk_menu_set_active(menu:PGtkMenu; index:guint);cdecl;external gtkdll name 'gtk_menu_set_active';
procedure gtk_menu_set_accel_group(menu:PGtkMenu; accel_group:PGtkAccelGroup);cdecl;external gtkdll name 'gtk_menu_set_accel_group';
function  gtk_menu_get_accel_group(menu:PGtkMenu):PGtkAccelGroup;cdecl;external gtkdll name 'gtk_menu_get_accel_group';
function  gtk_menu_get_uline_accel_group(menu:PGtkMenu):PGtkAccelGroup;cdecl;external gtkdll name 'gtk_menu_get_uline_accel_group';
function  gtk_menu_ensure_uline_accel_group(menu:PGtkMenu):PGtkAccelGroup;cdecl;external gtkdll name 'gtk_menu_ensure_uline_accel_group';
procedure gtk_menu_attach_to_widget(menu:PGtkMenu; attach_widget:PGtkWidget; detacher:TGtkMenuDetachFunc);cdecl;external gtkdll name 'gtk_menu_attach_to_widget';
function  gtk_menu_get_attach_widget(menu:PGtkMenu):PGtkWidget;cdecl;external gtkdll name 'gtk_menu_get_attach_widget';
procedure gtk_menu_detach(menu:PGtkMenu);cdecl;external gtkdll name 'gtk_menu_detach';
procedure gtk_menu_set_tearoff_state(menu:PGtkMenu; torn_off:gboolean);cdecl;external gtkdll name 'gtk_menu_set_tearoff_state';
procedure gtk_menu_set_title(menu:PGtkMenu; title:Pgchar);cdecl;external gtkdll name 'gtk_menu_set_title';
procedure gtk_menu_reorder_child(menu:PGtkMenu; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_menu_reorder_child';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  torn_off(var a : TGtkMenu) : guint;
    begin
       torn_off:=(a.flag0 and bm_TGtkMenu_torn_off) shr bp_TGtkMenu_torn_off;
    end;

procedure set_torn_off(var a : TGtkMenu; __torn_off : guint);
    begin
       a.flag0:=a.flag0 or ((__torn_off shl bp_TGtkMenu_torn_off) and bm_TGtkMenu_torn_off);
    end;

function  GTK_IS_MENU(obj:pointer):boolean;
begin
  GTK_IS_MENU:=(obj<>nil) and GTK_IS_MENU_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_MENU_CLASS(klass:pointer):boolean;
begin
  GTK_IS_MENU_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_MENU_TYPE);
end;

{$endif read_implementation}


