// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTearoffMenuItem = ^TGtkTearoffMenuItem;
   TGtkTearoffMenuItem = record
        menu_item : TGtkMenuItem;
        flag0 : word;
     end;


{ Padding for future expansion  }
   PGtkTearoffMenuItemClass = ^TGtkTearoffMenuItemClass;
   TGtkTearoffMenuItemClass = record
        parent_class : TGtkMenuItemClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTearoffMenuItem_torn_off = $1;
   bp_TGtkTearoffMenuItem_torn_off = 0;
function GTK_TYPE_TEAROFF_MENU_ITEM : GType;
function GTK_TEAROFF_MENU_ITEM(obj: pointer) : PGtkTearoffMenuItem;
function GTK_TEAROFF_MENU_ITEM_CLASS(klass: pointer) : PGtkTearoffMenuItemClass;
function GTK_IS_TEAROFF_MENU_ITEM(obj: pointer) : boolean;
function GTK_IS_TEAROFF_MENU_ITEM_CLASS(klass: pointer) : boolean;
function GTK_TEAROFF_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkTearoffMenuItemClass;


function torn_off(var a : TGtkTearoffMenuItem) : guint; overload;
procedure set_torn_off(var a : TGtkTearoffMenuItem; __torn_off : guint); overload;

function gtk_tearoff_menu_item_get_type:TGtkType; cdecl; external gtklib;
function gtk_tearoff_menu_item_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEAROFF_MENU_ITEM : GType;
begin
   GTK_TYPE_TEAROFF_MENU_ITEM:=gtk_tearoff_menu_item_get_type;
end;

function GTK_TEAROFF_MENU_ITEM(obj: pointer) : PGtkTearoffMenuItem;
begin
   GTK_TEAROFF_MENU_ITEM:=PGtkTearoffMenuItem(GTK_CHECK_CAST(obj,GTK_TYPE_TEAROFF_MENU_ITEM));
end;

function GTK_TEAROFF_MENU_ITEM_CLASS(klass: pointer) : PGtkTearoffMenuItemClass;
begin
   GTK_TEAROFF_MENU_ITEM_CLASS:=PGtkTearoffMenuItemClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TEAROFF_MENU_ITEM));
end;

function GTK_IS_TEAROFF_MENU_ITEM(obj: pointer) : boolean;
begin
   GTK_IS_TEAROFF_MENU_ITEM:=GTK_CHECK_TYPE(obj,GTK_TYPE_TEAROFF_MENU_ITEM);
end;

function GTK_IS_TEAROFF_MENU_ITEM_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEAROFF_MENU_ITEM_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEAROFF_MENU_ITEM);
end;

function GTK_TEAROFF_MENU_ITEM_GET_CLASS(obj: pointer) : PGtkTearoffMenuItemClass;
begin
   GTK_TEAROFF_MENU_ITEM_GET_CLASS:=PGtkTearoffMenuItemClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TEAROFF_MENU_ITEM));
end;

function torn_off(var a : TGtkTearoffMenuItem) : guint;
begin
   torn_off:=(a.flag0 and bm_TGtkTearoffMenuItem_torn_off) shr bp_TGtkTearoffMenuItem_torn_off;
end;

procedure set_torn_off(var a : TGtkTearoffMenuItem; __torn_off : guint);
begin
   a.flag0:=a.flag0 or ((__torn_off shl bp_TGtkTearoffMenuItem_torn_off) and bm_TGtkTearoffMenuItem_torn_off);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

