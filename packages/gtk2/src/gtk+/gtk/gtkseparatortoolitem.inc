// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGtkSeparatorToolItemPrivate = pointer;

  PGtkSeparatorToolItem = ^TGtkSeparatorToolItem;
  TGtkSeparatorToolItem = record
       parent : TGtkToolItem;
       priv : PGtkSeparatorToolItemPrivate;
    end;


  PGtkSeparatorToolItemClass = ^TGtkSeparatorToolItemClass;
  TGtkSeparatorToolItemClass = record
       parent_class : TGtkToolItemClass;
       _gtk_reserved1 : procedure ;cdecl;
       _gtk_reserved2 : procedure ;
       _gtk_reserved3 : procedure ;
       _gtk_reserved4 : procedure ;
    end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function GTK_TYPE_SEPARATOR_TOOL_ITEM : GType;
function GTK_SEPARATOR_TOOL_ITEM(obj : pointer) : PGtkSeparatorToolItem;
function GTK_SEPARATOR_TOOL_ITEM_CLASS(klass : pointer) : PGtkSeparatorToolItemClass;
function GTK_IS_SEPARATOR_TOOL_ITEM(obj : pointer) : boolean;
function GTK_IS_SEPARATOR_TOOL_ITEM_CLASS(klass : pointer) : boolean;
function GTK_SEPARATOR_TOOL_ITEM_GET_CLASS(obj : pointer) : PGtkSeparatorToolItemClass;

function gtk_separator_tool_item_get_type: GType; cdecl; external gtklib;
function gtk_separator_tool_item_new:PGtkToolItem; cdecl; external gtklib;
function gtk_separator_tool_item_get_draw(item:PGtkSeparatorToolItem):gboolean; cdecl; external gtklib;
procedure gtk_separator_tool_item_set_draw(item:PGtkSeparatorToolItem; draw:gboolean); cdecl; external gtklib;

{$endif}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

function GTK_TYPE_SEPARATOR_TOOL_ITEM : GType;
begin
  GTK_TYPE_SEPARATOR_TOOL_ITEM := gtk_separator_tool_item_get_type;
end;

function GTK_SEPARATOR_TOOL_ITEM(obj : pointer) : PGtkSeparatorToolItem;
begin
  GTK_SEPARATOR_TOOL_ITEM:=PGtkSeparatorToolItem(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_SEPARATOR_TOOL_ITEM));
end;

function GTK_SEPARATOR_TOOL_ITEM_CLASS(klass : pointer) : PGtkSeparatorToolItemClass;
begin
  GTK_SEPARATOR_TOOL_ITEM_CLASS:=PGtkSeparatorToolItemClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_SEPARATOR_TOOL_ITEM));
end;

function GTK_IS_SEPARATOR_TOOL_ITEM(obj : pointer) : boolean;
begin
  GTK_IS_SEPARATOR_TOOL_ITEM:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_SEPARATOR_TOOL_ITEM);
end;

function GTK_IS_SEPARATOR_TOOL_ITEM_CLASS(klass : pointer) : boolean;
begin
  GTK_IS_SEPARATOR_TOOL_ITEM_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_SEPARATOR_TOOL_ITEM);
end;

function GTK_SEPARATOR_TOOL_ITEM_GET_CLASS(obj : pointer) : PGtkSeparatorToolItemClass;
begin
  GTK_SEPARATOR_TOOL_ITEM_GET_CLASS:=PGtkSeparatorToolItemClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_SEPARATOR_TOOL_ITEM));
end;

{$ENDIF}