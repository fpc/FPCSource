
{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkToolButtonPrivate = pointer;

   PGtkToolButton = ^TGtkToolButton;
   TGtkToolButton = record
        parent : TGtkToolItem;
        priv : PGtkToolButtonPrivate;
     end;

{ signal  }
{ Padding for future expansion  }
   PGtkToolButtonClass = ^TGtkToolButtonClass;
   TGtkToolButtonClass = record
        parent_class : TGtkToolItemClass;
        button_type : GType;
        clicked : procedure (tool_item:PGtkToolButton);cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_TOOL_BUTTON : GType;
function GTK_TOOL_BUTTON(obj : pointer) : PGtkToolButton;
function GTK_TOOL_BUTTON_CLASS(klass : pointer) : PGtkToolButtonClass;
function GTK_IS_TOOL_BUTTON(obj : pointer) : gboolean;
function GTK_IS_TOOL_BUTTON_CLASS(klass : pointer) : gboolean;
function GTK_TOOL_BUTTON_GET_CLASS(obj : pointer) : PGtkToolButtonClass;


function gtk_tool_button_get_type:GType;cdecl;external gtklib name 'gtk_tool_button_get_type';

function gtk_tool_button_new(icon_widget:PGtkWidget; szLabel:Pgchar):PGtkToolItem;cdecl;external gtklib name 'gtk_tool_button_new';
function gtk_tool_button_new_from_stock(stock_id:Pgchar):PGtkToolItem;cdecl;external gtklib name 'gtk_tool_button_new_from_stock';

procedure gtk_tool_button_set_label(button:PGtkToolButton; szLabel:Pgchar);cdecl;external gtklib name 'gtk_tool_button_set_label';
function gtk_tool_button_get_label(button:PGtkToolButton):Pgchar;cdecl;external gtklib name 'gtk_tool_button_get_label';

procedure gtk_tool_button_set_use_underline(button:PGtkToolButton; use_underline:gboolean);cdecl;external gtklib name 'gtk_tool_button_set_use_underline';
function gtk_tool_button_get_use_underline(button:PGtkToolButton):gboolean;cdecl;external gtklib name 'gtk_tool_button_get_use_underline';


procedure gtk_tool_button_set_stock_id(button:PGtkToolButton; stock_id:Pgchar);cdecl;external gtklib name 'gtk_tool_button_set_stock_id';
function gtk_tool_button_get_stock_id(button:PGtkToolButton):Pgchar;cdecl;external gtklib name 'gtk_tool_button_get_stock_id';

procedure gtk_tool_button_set_icon_widget(button:PGtkToolButton; icon_widget:PGtkWidget);cdecl;external gtklib name 'gtk_tool_button_set_icon_widget';
function gtk_tool_button_get_icon_widget(button:PGtkToolButton):PGtkWidget;cdecl;external gtklib name 'gtk_tool_button_get_icon_widget';

procedure gtk_tool_button_set_label_widget(button:PGtkToolButton; label_widget:PGtkWidget);cdecl;external gtklib name 'gtk_tool_button_set_label_widget';
function gtk_tool_button_get_label_widget(button:PGtkToolButton):PGtkWidget;cdecl;external gtklib name 'gtk_tool_button_get_label_widget';

{ internal function  }
function _gtk_tool_button_get_button(button:PGtkToolButton):PGtkWidget;cdecl;external gtklib name '_gtk_tool_button_get_button';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_TOOL_BUTTON : GType;
begin
   GTK_TYPE_TOOL_BUTTON:=gtk_tool_button_get_type;
end;

function GTK_TOOL_BUTTON(obj : pointer) : PGtkToolButton;
begin
   GTK_TOOL_BUTTON:=PGtkToolButton(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TOOL_BUTTON));
end;

function GTK_TOOL_BUTTON_CLASS(klass : pointer) : PGtkToolButtonClass;
begin
   GTK_TOOL_BUTTON_CLASS:=PGtkToolButtonClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TOOL_BUTTON));
end;

function GTK_IS_TOOL_BUTTON(obj : pointer) : gboolean;
begin
   GTK_IS_TOOL_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TOOL_BUTTON);
end;

function GTK_IS_TOOL_BUTTON_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_TOOL_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOOL_BUTTON);
end;

function GTK_TOOL_BUTTON_GET_CLASS(obj : pointer) : PGtkToolButtonClass;
begin
   GTK_TOOL_BUTTON_GET_CLASS:=PGtkToolButtonClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TOOL_BUTTON));
end;

{$endif read_implementation}
