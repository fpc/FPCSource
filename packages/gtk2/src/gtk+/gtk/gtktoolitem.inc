{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkToolItemPrivate = pointer;

   PGtkToolItem = ^TGtkToolItem;
   TGtkToolItem = record
        parent : TGtkBin;
        priv : PGtkToolItemPrivate;
     end;

{ signals  }

{ Padding for future expansion  }
   PGtkToolItemClass = ^TGtkToolItemClass;
   TGtkToolItemClass = record
        parent_class : TGtkBinClass;
        create_menu_proxy : function (tool_item:PGtkToolItem):gboolean;cdecl;
        toolbar_reconfigured : procedure (tool_item:PGtkToolItem);
        set_tooltip : function (tool_item:PGtkToolItem; tooltips:PGtkTooltips; tip_text:Pgchar; tip_private:Pgchar):gboolean;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

{$ENDIF read_interface_types}

{$ifdef read_interface_rest}
function GTK_TYPE_TOOL_ITEM : GType;
function GTK_TOOL_ITEM(o : pointer) : PGtkToolItem;
function GTK_TOOL_ITEM_CLASS(klass : pointer) : PGtkToolItemClass;
function GTK_IS_TOOL_ITEM(o : pointer) : gboolean;
function GTK_IS_TOOL_ITEM_CLASS(klass : pointer) : gboolean;
function GTK_TOOL_ITEM_GET_CLASS(o : pointer) : PGtkToolItemClass;


function gtk_tool_item_get_type:GType;cdecl;external gtklib name 'gtk_tool_item_get_type';
function gtk_tool_item_new:PGtkToolItem;cdecl;external gtklib name 'gtk_tool_item_new';

procedure gtk_tool_item_set_homogeneous(tool_item:PGtkToolItem; homogeneous:gboolean);cdecl;external gtklib name 'gtk_tool_item_set_homogeneous';
function gtk_tool_item_get_homogeneous(tool_item:PGtkToolItem):gboolean;cdecl;external gtklib name 'gtk_tool_item_get_homogeneous';

procedure gtk_tool_item_set_expand(tool_item:PGtkToolItem; expand:gboolean);cdecl;external gtklib name 'gtk_tool_item_set_expand';
function gtk_tool_item_get_expand(tool_item:PGtkToolItem):gboolean;cdecl;external gtklib name 'gtk_tool_item_get_expand';

procedure gtk_tool_item_set_tooltip(tool_item:PGtkToolItem; tooltips:PGtkTooltips; tip_text:Pgchar; tip_private:Pgchar);cdecl;external gtklib name 'gtk_tool_item_set_tooltip';

procedure gtk_tool_item_set_use_drag_window(toolitem:PGtkToolItem; use_drag_window:gboolean);cdecl;external gtklib name 'gtk_tool_item_set_use_drag_window';
function gtk_tool_item_get_use_drag_window(toolitem:PGtkToolItem):gboolean;cdecl;external gtklib name 'gtk_tool_item_get_use_drag_window';

procedure gtk_tool_item_set_visible_horizontal(toolitem:PGtkToolItem; visible_horizontal:gboolean);cdecl;external gtklib name 'gtk_tool_item_set_visible_horizontal';
function gtk_tool_item_get_visible_horizontal(toolitem:PGtkToolItem):gboolean;cdecl;external gtklib name 'gtk_tool_item_get_visible_horizontal';

procedure gtk_tool_item_set_visible_vertical(toolitem:PGtkToolItem; visible_vertical:gboolean);cdecl;external gtklib name 'gtk_tool_item_set_visible_vertical';
function gtk_tool_item_get_visible_vertical(toolitem:PGtkToolItem):gboolean;cdecl;external gtklib name 'gtk_tool_item_get_visible_vertical';

function gtk_tool_item_get_is_important(tool_item:PGtkToolItem):gboolean;cdecl;external gtklib name 'gtk_tool_item_get_is_important';
procedure gtk_tool_item_set_is_important(tool_item:PGtkToolItem; is_important:gboolean);cdecl;external gtklib name 'gtk_tool_item_set_is_important';

function gtk_tool_item_get_icon_size(tool_item:PGtkToolItem):TGtkIconSize;cdecl;external gtklib name 'gtk_tool_item_get_icon_size';
function gtk_tool_item_get_orientation(tool_item:PGtkToolItem):TGtkOrientation;cdecl;external gtklib name 'gtk_tool_item_get_orientation';
function gtk_tool_item_get_toolbar_style(tool_item:PGtkToolItem):TGtkToolbarStyle;cdecl;external gtklib name 'gtk_tool_item_get_toolbar_style';
function gtk_tool_item_get_relief_style(tool_item:PGtkToolItem):TGtkReliefStyle;cdecl;external gtklib name 'gtk_tool_item_get_relief_style';
function gtk_tool_item_retrieve_proxy_menu_item(tool_item:PGtkToolItem):PGtkWidget;cdecl;external gtklib name 'gtk_tool_item_retrieve_proxy_menu_item';

function gtk_tool_item_get_proxy_menu_item(tool_item:PGtkToolItem; menu_item_id:Pgchar):PGtkWidget;cdecl;external gtklib name 'gtk_tool_item_get_proxy_menu_item';
procedure gtk_tool_item_set_proxy_menu_item(tool_item:PGtkToolItem; menu_item_id:Pgchar; menu_item:PGtkWidget);cdecl;external gtklib name 'gtk_tool_item_set_proxy_menu_item';

{ internal function  }
procedure _gtk_tool_item_toolbar_reconfigured(tool_item:PGtkToolItem);cdecl;external gtklib name '_gtk_tool_item_toolbar_reconfigured';

{$ifdef HasGTK2_6}
procedure gtk_tool_item_rebuild_menu(tool_item:PGtkToolItem);cdecl;external gtklib;
{$endif}

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_TOOL_ITEM : GType;
begin
   GTK_TYPE_TOOL_ITEM:=gtk_tool_item_get_type;
end;

function GTK_TOOL_ITEM(o : pointer) : PGtkToolItem;
begin
   GTK_TOOL_ITEM:=PGtkToolItem(G_TYPE_CHECK_INSTANCE_CAST(o,GTK_TYPE_TOOL_ITEM));
end;

function GTK_TOOL_ITEM_CLASS(klass : pointer) : PGtkToolItemClass;
begin
   GTK_TOOL_ITEM_CLASS:=PGtkToolItemClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TOOL_ITEM));
end;

function GTK_IS_TOOL_ITEM(o : pointer) : gboolean;
begin
   GTK_IS_TOOL_ITEM:=G_TYPE_CHECK_INSTANCE_TYPE(o,GTK_TYPE_TOOL_ITEM);
end;

function GTK_IS_TOOL_ITEM_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_TOOL_ITEM_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOOL_ITEM);
end;

function GTK_TOOL_ITEM_GET_CLASS(o : pointer) : PGtkToolItemClass;
begin
   GTK_TOOL_ITEM_GET_CLASS:=PGtkToolItemClass(G_TYPE_INSTANCE_GET_CLASS(o,GTK_TYPE_TOOL_ITEM));
end;


{$endif read_implementation}
