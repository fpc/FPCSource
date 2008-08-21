// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGtkMenuToolButtonPrivate = pointer;

  PGtkMenuToolButton = ^TGtkMenuToolButton;
  TGtkMenuToolButton = record
    parent : TGtkToolButton;
    priv : PGtkMenuToolButtonPrivate;
  end;


  PGtkMenuToolButtonClass = ^TGtkMenuToolButtonClass;
  TGtkMenuToolButtonClass = record
    parent_class : TGtkToolButtonClass;
    show_menu : procedure (button:PGtkMenuToolButton);cdecl;
    _gtk_reserved1 : procedure ; cdecl;
    _gtk_reserved2 : procedure ; cdecl;
    _gtk_reserved3 : procedure ; cdecl;
    _gtk_reserved4 : procedure ; cdecl;
  end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_MENU_TOOL_BUTTON : GType;
function GTK_MENU_TOOL_BUTTON(o : pointer): PGtkMenuToolButton;
function GTK_MENU_TOOL_BUTTON_CLASS(k : pointer) : PGtkMenuToolButtonClass;
function GTK_IS_MENU_TOOL_BUTTON(o : pointer) : boolean;
function GTK_IS_MENU_TOOL_BUTTON_CLASS(k : pointer) : boolean;
function GTK_MENU_TOOL_BUTTON_GET_CLASS(o : pointer) : PGtkMenuToolButtonClass;

function gtk_menu_tool_button_get_type:GType;cdecl;external gtklib;
function gtk_menu_tool_button_new(icon_widget:PGtkWidget; _label:Pgchar):PGtkToolItem;cdecl;external gtklib;
function gtk_menu_tool_button_new_from_stock(stock_id:Pgchar):PGtkToolItem;cdecl;external gtklib;
procedure gtk_menu_tool_button_set_menu(button:PGtkMenuToolButton; menu:PGtkWidget);cdecl;external gtklib;
function gtk_menu_tool_button_get_menu(button:PGtkMenuToolButton):PGtkWidget;cdecl;external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_menu_tool_button_set_arrow_tooltip(button:PGtkMenuToolButton; tooltips:PGtkTooltips; tip_text:Pgchar; tip_private:Pgchar);cdecl;external gtklib;
{$endif}

procedure gtk_menu_tool_button_set_arrow_tooltip_text(button:PGtkMenuToolButton; text:Pgchar);cdecl;external gtklib;
procedure gtk_menu_tool_button_set_arrow_tooltip_markup(button:PGtkMenuToolButton; markup:Pgchar);cdecl;external gtklib;

{$ENDIF read_interface_rest}
//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MENU_TOOL_BUTTON : GType;
begin
  GTK_TYPE_MENU_TOOL_BUTTON:=gtk_menu_tool_button_get_type;
end;

function GTK_MENU_TOOL_BUTTON(o : pointer) : PGtkMenuToolButton;
begin
  GTK_MENU_TOOL_BUTTON:=PGtkMenuToolButton(G_TYPE_CHECK_INSTANCE_CAST(o,GTK_TYPE_MENU_TOOL_BUTTON));
end;

function GTK_MENU_TOOL_BUTTON_CLASS(k : pointer) : PGtkMenuToolButtonClass;
begin
  GTK_MENU_TOOL_BUTTON_CLASS:=PGtkMenuToolButtonClass(G_TYPE_CHECK_CLASS_CAST(k,GTK_TYPE_MENU_TOOL_BUTTON));
end;

function GTK_IS_MENU_TOOL_BUTTON(o : pointer) : boolean;
begin
  GTK_IS_MENU_TOOL_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(o,GTK_TYPE_MENU_TOOL_BUTTON);
end;

function GTK_IS_MENU_TOOL_BUTTON_CLASS(k : pointer) : boolean;
begin
  GTK_IS_MENU_TOOL_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(k,GTK_TYPE_MENU_TOOL_BUTTON);
end;

function GTK_MENU_TOOL_BUTTON_GET_CLASS(o : pointer) : PGtkMenuToolButtonClass;
begin
  GTK_MENU_TOOL_BUTTON_GET_CLASS:=PGtkMenuToolButtonClass(G_TYPE_INSTANCE_GET_CLASS(o,GTK_TYPE_MENU_TOOL_BUTTON));
end;
{$ENDIF read_implementation}
// included by gtk2.pas
