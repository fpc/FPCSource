// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkMenuItem = ^TGtkMenuItem;
   TGtkMenuItem = record
        item : TGtkItem;
        submenu : PGtkWidget;
        event_window : PGdkWindow;
        toggle_size : guint16;
        accelerator_width : guint16;
        accel_path : Pgchar;
        flag0 : word;
        timer : guint;
     end;


{ Padding for future expansion  }
   PGtkMenuItemClass = ^TGtkMenuItemClass;
   TGtkMenuItemClass = record
        parent_class : TGtkItemClass;
        flag0 : word;
        activate : procedure (menu_item:PGtkMenuItem); cdecl;
        activate_item : procedure (menu_item:PGtkMenuItem); cdecl;
        toggle_size_request : procedure (menu_item:PGtkMenuItem; requisition:Pgint); cdecl;
        toggle_size_allocate : procedure (menu_item:PGtkMenuItem; allocation:gint); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkMenuItem_show_submenu_indicator = $1;
   bp_TGtkMenuItem_show_submenu_indicator = 0;
   bm_TGtkMenuItem_submenu_placement = $2;
   bp_TGtkMenuItem_submenu_placement = 1;
   bm_TGtkMenuItem_submenu_direction = $4;
   bp_TGtkMenuItem_submenu_direction = 2;
   bm_TGtkMenuItem_right_justify = $8;
   bp_TGtkMenuItem_right_justify = 3;
   bm_TGtkMenuItem_timer_from_keypress = $10;
   bp_TGtkMenuItem_timer_from_keypress = 4;

   bm_TGtkMenuItemClass_hide_on_activate = $1;
   bp_TGtkMenuItemClass_hide_on_activate = 0;


function GTK_TYPE_MENU_ITEM : GType;
function GTK_MENU_ITEM(obj: pointer) : PGtkMenuItem;
function GTK_MENU_ITEM_CLASS(klass: pointer) : PGtkMenuItemClass;
function GTK_IS_MENU_ITEM(obj: pointer) : boolean;
function GTK_IS_MENU_ITEM_CLASS(klass: pointer) : boolean;
function GTK_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkMenuItemClass;


function show_submenu_indicator(var a : TGtkMenuItem) : guint;
procedure set_show_submenu_indicator(var a : TGtkMenuItem; __show_submenu_indicator : guint);
function submenu_placement(var a : TGtkMenuItem) : guint; overload;
procedure set_submenu_placement(var a : TGtkMenuItem; __submenu_placement : guint); overload;
function submenu_direction(var a : TGtkMenuItem) : guint;
procedure set_submenu_direction(var a : TGtkMenuItem; __submenu_direction : guint);
function right_justify(var a : TGtkMenuItem) : guint;
procedure set_right_justify(var a : TGtkMenuItem; __right_justify : guint);
function timer_from_keypress(var a : TGtkMenuItem) : guint;
procedure set_timer_from_keypress(var a : TGtkMenuItem; __timer_from_keypress : guint);
{ If the following flag is true, then we should always hide
     the menu when the MenuItem is activated. Otherwise, the
     it is up to the caller. For instance, when navigating
     a menu with the keyboard, <Space> doesn't hide, but
     <Return> does.
    }
function hide_on_activate(var a : TGtkMenuItemClass) : guint;
procedure set_hide_on_activate(var a : TGtkMenuItemClass; __hide_on_activate : guint);

function gtk_menu_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_menu_item_new:PGtkWidget; cdecl; external gtklib;
function gtk_menu_item_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_menu_item_new_with_mnemonic(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_menu_item_set_submenu(menu_item:PGtkMenuItem; submenu:PGtkWidget); cdecl; external gtklib;
function gtk_menu_item_get_submenu(menu_item:PGtkMenuItem):PGtkWidget; cdecl; external gtklib;
procedure gtk_menu_item_remove_submenu(menu_item:PGtkMenuItem); cdecl; external gtklib;
procedure gtk_menu_item_select(menu_item:PGtkMenuItem); cdecl; external gtklib;
procedure gtk_menu_item_deselect(menu_item:PGtkMenuItem); cdecl; external gtklib;
procedure gtk_menu_item_activate(menu_item:PGtkMenuItem); cdecl; external gtklib;
procedure gtk_menu_item_toggle_size_request(menu_item:PGtkMenuItem; requisition:Pgint); cdecl; external gtklib;
procedure gtk_menu_item_toggle_size_allocate(menu_item:PGtkMenuItem; allocation:gint); cdecl; external gtklib;
procedure gtk_menu_item_set_right_justified(menu_item:PGtkMenuItem; right_justified:gboolean); cdecl; external gtklib;
function gtk_menu_item_get_right_justified(menu_item:PGtkMenuItem):gboolean; cdecl; external gtklib;
procedure gtk_menu_item_set_accel_path(menu_item:PGtkMenuItem; accel_path:Pgchar); cdecl; external gtklib;
{ private  }
procedure _gtk_menu_item_refresh_accel_path(menu_item:PGtkMenuItem; prefix:Pgchar; accel_group:PGtkAccelGroup; group_changed:gboolean); cdecl; external gtklib;
function _gtk_menu_item_is_selectable(menu_item:PGtkWidget):gboolean; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_menu_item_right_justify(menu_item : PGtkMenuItem);
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MENU_ITEM : GType;
begin
   GTK_TYPE_MENU_ITEM:=gtk_menu_item_get_type;
end;

function GTK_MENU_ITEM(obj: pointer) : PGtkMenuItem;
begin
   GTK_MENU_ITEM:=PGtkMenuItem(GTK_CHECK_CAST(obj,GTK_TYPE_MENU_ITEM));
end;

function GTK_MENU_ITEM_CLASS(klass: pointer) : PGtkMenuItemClass;
begin
   GTK_MENU_ITEM_CLASS:=PGtkMenuItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_MENU_ITEM));
end;

function GTK_IS_MENU_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_MENU_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_MENU_ITEM);
end;

function GTK_IS_MENU_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_MENU_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_MENU_ITEM);
end;

function GTK_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkMenuItemClass;
begin
   GTK_MENU_ITEM_GET_CLASS:=PGtkMenuItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_MENU_ITEM));
end;

function show_submenu_indicator(var a : TGtkMenuItem) : guint;
begin
   show_submenu_indicator:=(a.flag0 and bm_TGtkMenuItem_show_submenu_indicator) shr bp_TGtkMenuItem_show_submenu_indicator;
end;

procedure set_show_submenu_indicator(var a : TGtkMenuItem; __show_submenu_indicator : guint);
begin
   a.flag0:=a.flag0 or ((__show_submenu_indicator shl bp_TGtkMenuItem_show_submenu_indicator) and bm_TGtkMenuItem_show_submenu_indicator);
end;

function submenu_placement(var a : TGtkMenuItem) : guint;
begin
   submenu_placement:=(a.flag0 and bm_TGtkMenuItem_submenu_placement) shr bp_TGtkMenuItem_submenu_placement;
end;

procedure set_submenu_placement(var a : TGtkMenuItem; __submenu_placement : guint);
begin
   a.flag0:=a.flag0 or ((__submenu_placement shl bp_TGtkMenuItem_submenu_placement) and bm_TGtkMenuItem_submenu_placement);
end;

function submenu_direction(var a : TGtkMenuItem) : guint;
begin
   submenu_direction:=(a.flag0 and bm_TGtkMenuItem_submenu_direction) shr bp_TGtkMenuItem_submenu_direction;
end;

procedure set_submenu_direction(var a : TGtkMenuItem; __submenu_direction : guint);
begin
   a.flag0:=a.flag0 or ((__submenu_direction shl bp_TGtkMenuItem_submenu_direction) and bm_TGtkMenuItem_submenu_direction);
end;

function right_justify(var a : TGtkMenuItem) : guint;
begin
   right_justify:=(a.flag0 and bm_TGtkMenuItem_right_justify) shr bp_TGtkMenuItem_right_justify;
end;

procedure set_right_justify(var a : TGtkMenuItem; __right_justify : guint);
begin
   a.flag0:=a.flag0 or ((__right_justify shl bp_TGtkMenuItem_right_justify) and bm_TGtkMenuItem_right_justify);
end;

function timer_from_keypress(var a : TGtkMenuItem) : guint;
begin
   timer_from_keypress:=(a.flag0 and bm_TGtkMenuItem_timer_from_keypress) shr bp_TGtkMenuItem_timer_from_keypress;
end;

procedure set_timer_from_keypress(var a : TGtkMenuItem; __timer_from_keypress : guint);
begin
   a.flag0:=a.flag0 or ((__timer_from_keypress shl bp_TGtkMenuItem_timer_from_keypress) and bm_TGtkMenuItem_timer_from_keypress);
end;

function hide_on_activate(var a : TGtkMenuItemClass) : guint;
begin
   hide_on_activate:=(a.flag0 and bm_TGtkMenuItemClass_hide_on_activate) shr bp_TGtkMenuItemClass_hide_on_activate;
end;

procedure set_hide_on_activate(var a : TGtkMenuItemClass; __hide_on_activate : guint);
begin
   a.flag0:=a.flag0 or ((__hide_on_activate shl bp_TGtkMenuItemClass_hide_on_activate) and bm_TGtkMenuItemClass_hide_on_activate);
end;

procedure gtk_menu_item_right_justify(menu_item : PGtkMenuItem);
begin
   gtk_menu_item_set_right_justified(menu_item,true);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

