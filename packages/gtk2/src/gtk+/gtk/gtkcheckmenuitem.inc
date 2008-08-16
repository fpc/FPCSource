// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCheckMenuItem = ^TGtkCheckMenuItem;
   TGtkCheckMenuItem = record
        menu_item : TGtkMenuItem;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkCheckMenuItemClass = ^TGtkCheckMenuItemClass;
   TGtkCheckMenuItemClass = record
        parent_class : TGtkMenuItemClass;
        toggled : procedure (check_menu_item:PGtkCheckMenuItem); cdecl;
        draw_indicator : procedure (check_menu_item:PGtkCheckMenuItem; area:PGdkRectangle); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkCheckMenuItem_active = $1;
   bp_TGtkCheckMenuItem_active = 0;
   bm_TGtkCheckMenuItem_always_show_toggle = $2;
   bp_TGtkCheckMenuItem_always_show_toggle = 1;
   bm_TGtkCheckMenuItem_inconsistent = $4;
   bp_TGtkCheckMenuItem_inconsistent = 2;
function GTK_TYPE_CHECK_MENU_ITEM : GType;
function GTK_CHECK_MENU_ITEM(obj: pointer) : PGtkCheckMenuItem;
function GTK_CHECK_MENU_ITEM_CLASS(klass: pointer) : PGtkCheckMenuItemClass;
function GTK_IS_CHECK_MENU_ITEM(obj: pointer) : boolean;
function GTK_IS_CHECK_MENU_ITEM_CLASS(klass: pointer) : boolean;
function GTK_CHECK_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkCheckMenuItemClass;

function active(var a : TGtkCheckMenuItem) : guint; overload;
procedure set_active(var a : TGtkCheckMenuItem; __active : guint); overload;
function always_show_toggle(var a : TGtkCheckMenuItem) : guint;
procedure set_always_show_toggle(var a : TGtkCheckMenuItem; __always_show_toggle : guint);
function inconsistent(var a : TGtkCheckMenuItem) : guint; overload;
procedure set_inconsistent(var a : TGtkCheckMenuItem; __inconsistent : guint); overload;


function gtk_check_menu_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_check_menu_item_new:PGtkWidget; cdecl; external gtklib;
function gtk_check_menu_item_new_with_label(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_check_menu_item_new_with_mnemonic(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_check_menu_item_set_active(check_menu_item:PGtkCheckMenuItem; is_active:gboolean); cdecl; external gtklib;
function gtk_check_menu_item_get_active(check_menu_item:PGtkCheckMenuItem):gboolean; cdecl; external gtklib;
procedure gtk_check_menu_item_toggled(check_menu_item:PGtkCheckMenuItem); cdecl; external gtklib;
procedure gtk_check_menu_item_set_inconsistent(check_menu_item:PGtkCheckMenuItem; setting:gboolean); cdecl; external gtklib;
function gtk_check_menu_item_get_inconsistent(check_menu_item:PGtkCheckMenuItem):gboolean; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_check_menu_item_set_show_toggle(menu_item:PGtkCheckMenuItem; always:gboolean); cdecl; external gtklib;

procedure gtk_check_menu_item_set_state(check_menu_item:PGtkCheckMenuItem; is_active:gboolean);
{$endif}

{$ifdef HasGTK2_4}
function gtk_check_menu_item_get_draw_as_radio(check_menu_item: PGtkCheckMenuItem):gboolean; cdecl; external gtklib;
procedure gtk_check_menu_item_set_draw_as_radio(check_menu_item: PGtkCheckMenuItem; draw_as_radio: gboolean); cdecl; external gtklib;
{$endif}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CHECK_MENU_ITEM : GType;
begin
   GTK_TYPE_CHECK_MENU_ITEM:=gtk_check_menu_item_get_type;
end;

function GTK_CHECK_MENU_ITEM(obj: pointer) : PGtkCheckMenuItem;
begin
   GTK_CHECK_MENU_ITEM:=PGtkCheckMenuItem(GTK_CHECK_CAST(obj,GTK_TYPE_CHECK_MENU_ITEM));
end;

function GTK_CHECK_MENU_ITEM_CLASS(klass: pointer) : PGtkCheckMenuItemClass;
begin
   GTK_CHECK_MENU_ITEM_CLASS:=PGtkCheckMenuItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CHECK_MENU_ITEM));
end;

function GTK_IS_CHECK_MENU_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_CHECK_MENU_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_CHECK_MENU_ITEM);
end;

function GTK_IS_CHECK_MENU_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CHECK_MENU_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CHECK_MENU_ITEM);
end;

function GTK_CHECK_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkCheckMenuItemClass;
begin
   GTK_CHECK_MENU_ITEM_GET_CLASS:=PGtkCheckMenuItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CHECK_MENU_ITEM));
end;

function active(var a : TGtkCheckMenuItem) : guint;
begin
   active:=(a.flag0 and bm_TGtkCheckMenuItem_active) shr bp_TGtkCheckMenuItem_active;
end;

procedure set_active(var a : TGtkCheckMenuItem; __active : guint);
begin
   a.flag0:=a.flag0 or ((__active shl bp_TGtkCheckMenuItem_active) and bm_TGtkCheckMenuItem_active);
end;

function always_show_toggle(var a : TGtkCheckMenuItem) : guint;
begin
   always_show_toggle:=(a.flag0 and bm_TGtkCheckMenuItem_always_show_toggle) shr bp_TGtkCheckMenuItem_always_show_toggle;
end;

procedure set_always_show_toggle(var a : TGtkCheckMenuItem; __always_show_toggle : guint);
begin
   a.flag0:=a.flag0 or ((__always_show_toggle shl bp_TGtkCheckMenuItem_always_show_toggle) and bm_TGtkCheckMenuItem_always_show_toggle);
end;

function inconsistent(var a : TGtkCheckMenuItem) : guint;
begin
   inconsistent:=(a.flag0 and bm_TGtkCheckMenuItem_inconsistent) shr bp_TGtkCheckMenuItem_inconsistent;
end;

procedure set_inconsistent(var a : TGtkCheckMenuItem; __inconsistent : guint);
begin
   a.flag0:=a.flag0 or ((__inconsistent shl bp_TGtkCheckMenuItem_inconsistent) and bm_TGtkCheckMenuItem_inconsistent);
end;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_check_menu_item_set_state(check_menu_item:PGtkCheckMenuItem; is_active:gboolean);
begin
  gtk_check_menu_item_set_active(check_menu_item,is_active);
end;
{$endif}

{$ENDIF read_implementation}
// included by gtk2.pas

