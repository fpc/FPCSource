// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkMenu = ^TGtkMenu;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TGtkMenuPositionFunc = procedure (menu:PGtkMenu; x:Pgint; y:Pgint; push_in:Pgboolean; user_data:gpointer); cdecl;

   TGtkMenuDetachFunc = procedure (attach_widget:PGtkWidget; menu:PGtkMenu); cdecl;
{ Do _not_ touch these widgets directly. We hide the reference
     count from the toplevel to the menu, so it must be restored
     before operating on these widgets
    }
{ When a submenu of this menu is popped up, motion in this
     region is ignored
    }
{ The tearoff is active when it is torn off and the not-torn-off
     menu is not popped up.
    }
   TGtkMenu = record
        menu_shell : TGtkMenuShell;
        parent_menu_item : PGtkWidget;
        old_active_menu_item : PGtkWidget;
        accel_group : PGtkAccelGroup;
        accel_path : Pgchar;
        position_func : TGtkMenuPositionFunc;
        position_func_data : gpointer;
        toggle_size : guint;
        toplevel : PGtkWidget;
        tearoff_window : PGtkWidget;
        tearoff_hbox : PGtkWidget;
        tearoff_scrollbar : PGtkWidget;
        tearoff_adjustment : PGtkAdjustment;
        view_window : PGdkWindow;
        bin_window : PGdkWindow;
        scroll_offset : gint;
        saved_scroll_offset : gint;
        scroll_step : gint;
        timeout_id : guint;
        navigation_region : PGdkRegion;
        navigation_timeout : guint;
        flag0 : word;
     end;


{ Padding for future expansion  }
   PGtkMenuClass = ^TGtkMenuClass;
   TGtkMenuClass = record
        parent_class : TGtkMenuShellClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkMenu_needs_destruction_ref_count = $1;
   bp_TGtkMenu_needs_destruction_ref_count = 0;
   bm_TGtkMenu_torn_off = $2;
   bp_TGtkMenu_torn_off = 1;
   bm_TGtkMenu_tearoff_active = $4;
   bp_TGtkMenu_tearoff_active = 2;
   bm_TGtkMenu_scroll_fast = $8;
   bp_TGtkMenu_scroll_fast = 3;
   bm_TGtkMenu_upper_arrow_visible = $10;
   bp_TGtkMenu_upper_arrow_visible = 4;
   bm_TGtkMenu_lower_arrow_visible = $20;
   bp_TGtkMenu_lower_arrow_visible = 5;
   bm_TGtkMenu_upper_arrow_prelight = $40;
   bp_TGtkMenu_upper_arrow_prelight = 6;
   bm_TGtkMenu_lower_arrow_prelight = $80;
   bp_TGtkMenu_lower_arrow_prelight = 7;


function GTK_TYPE_MENU : GType;
function GTK_MENU(obj: pointer) : PGtkMenu;
function GTK_MENU_CLASS(klass: pointer) : PGtkMenuClass;
function GTK_IS_MENU(obj: pointer) : boolean;
function GTK_IS_MENU_CLASS(klass: pointer) : boolean;
function GTK_MENU_GET_CLASS(obj: pointer) : PGtkMenuClass;


function needs_destruction_ref_count(var a : TGtkMenu) : guint;
procedure set_needs_destruction_ref_count(var a : TGtkMenu; __needs_destruction_ref_count : guint);
function torn_off(var a : TGtkMenu) : guint; overload;
procedure set_torn_off(var a : TGtkMenu; __torn_off : guint); overload;
function tearoff_active(var a : TGtkMenu) : guint;
procedure set_tearoff_active(var a : TGtkMenu; __tearoff_active : guint);
function scroll_fast(var a : TGtkMenu) : guint;
procedure set_scroll_fast(var a : TGtkMenu; __scroll_fast : guint);
function upper_arrow_visible(var a : TGtkMenu) : guint;
procedure set_upper_arrow_visible(var a : TGtkMenu; __upper_arrow_visible : guint);
function lower_arrow_visible(var a : TGtkMenu) : guint;
procedure set_lower_arrow_visible(var a : TGtkMenu; __lower_arrow_visible : guint);
function upper_arrow_prelight(var a : TGtkMenu) : guint;
procedure set_upper_arrow_prelight(var a : TGtkMenu; __upper_arrow_prelight : guint);
function lower_arrow_prelight(var a : TGtkMenu) : guint;
procedure set_lower_arrow_prelight(var a : TGtkMenu; __lower_arrow_prelight : guint);

function gtk_menu_get_type:TGtkType; cdecl; external gtklib;
function gtk_menu_new:PGtkWidget; cdecl; external gtklib;
{ Display the menu onscreen  }
procedure gtk_menu_popup(menu:PGtkMenu; parent_menu_shell:PGtkWidget; parent_menu_item:PGtkWidget; func:TGtkMenuPositionFunc; data:gpointer;
            button:guint; activate_time:guint32); cdecl; external gtklib;
{ Position the menu according to its position function. Called
   from gtkmenuitem.c when a menu-item changes its allocation
  }
procedure gtk_menu_reposition(menu:PGtkMenu); cdecl; external gtklib;
procedure gtk_menu_popdown(menu:PGtkMenu); cdecl; external gtklib;
{ Keep track of the last menu item selected. (For the purposes
   of the option menu
  }
function gtk_menu_get_active(menu:PGtkMenu):PGtkWidget; cdecl; external gtklib;
procedure gtk_menu_set_active(menu:PGtkMenu; index:guint); cdecl; external gtklib;
{ set/get the accelerator group that holds global accelerators (should
   be added to the corresponding toplevel with gtk_window_add_accel_group().
  }
procedure gtk_menu_set_accel_group(menu:PGtkMenu; accel_group:PGtkAccelGroup); cdecl; external gtklib;
function gtk_menu_get_accel_group(menu:PGtkMenu):PGtkAccelGroup; cdecl; external gtklib;
procedure gtk_menu_set_accel_path(menu:PGtkMenu; accel_path:Pgchar); cdecl; external gtklib;
{ A reference count is kept for a widget when it is attached to
   a particular widget. This is typically a menu item; it may also
   be a widget with a popup menu - for instance, the Notebook widget.
  }
procedure gtk_menu_attach_to_widget(menu:PGtkMenu; attach_widget:PGtkWidget; detacher:TGtkMenuDetachFunc); cdecl; external gtklib;
procedure gtk_menu_detach(menu:PGtkMenu); cdecl; external gtklib;
{ This should be dumped in favor of data set when the menu is popped
   up - that is currently in the ItemFactory code, but should be
   in the Menu code.
  }
function gtk_menu_get_attach_widget(menu:PGtkMenu):PGtkWidget; cdecl; external gtklib;
procedure gtk_menu_set_tearoff_state(menu:PGtkMenu; torn_off:gboolean); cdecl; external gtklib;
function gtk_menu_get_tearoff_state(menu:PGtkMenu):gboolean; cdecl; external gtklib;
{ This sets the window manager title for the window that
   appears when a menu is torn off
  }
procedure gtk_menu_set_title(menu:PGtkMenu; title:Pgchar); cdecl; external gtklib;
function gtk_menu_get_title(menu:PGtkMenu):Pgchar; cdecl; external gtklib;
procedure gtk_menu_reorder_child(menu:PGtkMenu; child:PGtkWidget; position:gint); cdecl; external gtklib;
procedure gtk_menu_set_screen(menu:PGtkMenu; screen:PGdkScreen); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_menu_append(menu,child : PGtkWidget);
procedure gtk_menu_prepend(menu,child : PGtkWidget);
procedure gtk_menu_insert(menu,child: PGtkWidget; pos : gint);
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MENU : GType;
begin
   GTK_TYPE_MENU:=gtk_menu_get_type;
end;

function GTK_MENU(obj: pointer) : PGtkMenu;
begin
   GTK_MENU:=PGtkMenu(GTK_CHECK_CAST(obj,GTK_TYPE_MENU));
end;

function GTK_MENU_CLASS(klass: pointer) : PGtkMenuClass;
begin
   GTK_MENU_CLASS:=PGtkMenuClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_MENU));
end;

function GTK_IS_MENU(obj: pointer) : boolean;
begin
   GTK_IS_MENU:=GTK_CHECK_TYPE(obj,GTK_TYPE_MENU);
end;

function GTK_IS_MENU_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_MENU_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_MENU);
end;

function GTK_MENU_GET_CLASS(obj: pointer) : PGtkMenuClass;
begin
   GTK_MENU_GET_CLASS:=PGtkMenuClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_MENU));
end;

function needs_destruction_ref_count(var a : TGtkMenu) : guint;
begin
   needs_destruction_ref_count:=(a.flag0 and bm_TGtkMenu_needs_destruction_ref_count) shr bp_TGtkMenu_needs_destruction_ref_count;
end;

procedure set_needs_destruction_ref_count(var a : TGtkMenu; __needs_destruction_ref_count : guint);
begin
   a.flag0:=a.flag0 or ((__needs_destruction_ref_count shl bp_TGtkMenu_needs_destruction_ref_count) and bm_TGtkMenu_needs_destruction_ref_count);
end;

function torn_off(var a : TGtkMenu) : guint;
begin
   torn_off:=(a.flag0 and bm_TGtkMenu_torn_off) shr bp_TGtkMenu_torn_off;
end;

procedure set_torn_off(var a : TGtkMenu; __torn_off : guint);
begin
   a.flag0:=a.flag0 or ((__torn_off shl bp_TGtkMenu_torn_off) and bm_TGtkMenu_torn_off);
end;

function tearoff_active(var a : TGtkMenu) : guint;
begin
   tearoff_active:=(a.flag0 and bm_TGtkMenu_tearoff_active) shr bp_TGtkMenu_tearoff_active;
end;

procedure set_tearoff_active(var a : TGtkMenu; __tearoff_active : guint);
begin
   a.flag0:=a.flag0 or ((__tearoff_active shl bp_TGtkMenu_tearoff_active) and bm_TGtkMenu_tearoff_active);
end;

function scroll_fast(var a : TGtkMenu) : guint;
begin
   scroll_fast:=(a.flag0 and bm_TGtkMenu_scroll_fast) shr bp_TGtkMenu_scroll_fast;
end;

procedure set_scroll_fast(var a : TGtkMenu; __scroll_fast : guint);
begin
   a.flag0:=a.flag0 or ((__scroll_fast shl bp_TGtkMenu_scroll_fast) and bm_TGtkMenu_scroll_fast);
end;

function upper_arrow_visible(var a : TGtkMenu) : guint;
begin
   upper_arrow_visible:=(a.flag0 and bm_TGtkMenu_upper_arrow_visible) shr bp_TGtkMenu_upper_arrow_visible;
end;

procedure set_upper_arrow_visible(var a : TGtkMenu; __upper_arrow_visible : guint);
begin
   a.flag0:=a.flag0 or ((__upper_arrow_visible shl bp_TGtkMenu_upper_arrow_visible) and bm_TGtkMenu_upper_arrow_visible);
end;

function lower_arrow_visible(var a : TGtkMenu) : guint;
begin
   lower_arrow_visible:=(a.flag0 and bm_TGtkMenu_lower_arrow_visible) shr bp_TGtkMenu_lower_arrow_visible;
end;

procedure set_lower_arrow_visible(var a : TGtkMenu; __lower_arrow_visible : guint);
begin
   a.flag0:=a.flag0 or ((__lower_arrow_visible shl bp_TGtkMenu_lower_arrow_visible) and bm_TGtkMenu_lower_arrow_visible);
end;

function upper_arrow_prelight(var a : TGtkMenu) : guint;
begin
   upper_arrow_prelight:=(a.flag0 and bm_TGtkMenu_upper_arrow_prelight) shr bp_TGtkMenu_upper_arrow_prelight;
end;

procedure set_upper_arrow_prelight(var a : TGtkMenu; __upper_arrow_prelight : guint);
begin
   a.flag0:=a.flag0 or ((__upper_arrow_prelight shl bp_TGtkMenu_upper_arrow_prelight) and bm_TGtkMenu_upper_arrow_prelight);
end;

function lower_arrow_prelight(var a : TGtkMenu) : guint;
begin
   lower_arrow_prelight:=(a.flag0 and bm_TGtkMenu_lower_arrow_prelight) shr bp_TGtkMenu_lower_arrow_prelight;
end;

procedure set_lower_arrow_prelight(var a : TGtkMenu; __lower_arrow_prelight : guint);
begin
   a.flag0:=a.flag0 or ((__lower_arrow_prelight shl bp_TGtkMenu_lower_arrow_prelight) and bm_TGtkMenu_lower_arrow_prelight);
end;

procedure gtk_menu_append(menu,child : PGtkWidget);
begin
   gtk_menu_shell_append(PGtkMenuShell(menu),child);
end;

procedure gtk_menu_prepend(menu,child : PGtkWidget);
begin
   gtk_menu_shell_prepend(PGtkMenuShell(menu),child);
end;

procedure gtk_menu_insert(menu,child: PGtkWidget; pos : gint);
begin
   gtk_menu_shell_insert(PGtkMenuShell(menu),child,pos);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

