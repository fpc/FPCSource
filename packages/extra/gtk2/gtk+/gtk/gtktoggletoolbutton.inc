
{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkToggleToolButtonPrivate = pointer;

   PGtkToggleToolButton = ^TGtkToggleToolButton;
   TGtkToggleToolButton = record
        parent : TGtkToolButton;
        priv : PGtkToggleToolButtonPrivate;
     end;

{ signal  }
{ Padding for future expansion  }
   PGtkToggleToolButtonClass = ^TGtkToggleToolButtonClass;
   TGtkToggleToolButtonClass = record
        parent_class : TGtkToolButtonClass;
        toggled : procedure (button:PGtkToggleToolButton);cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
        _gtk_reserved4 : procedure ;
     end;
{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_TOGGLE_TOOL_BUTTON : GType;
function GTK_TOGGLE_TOOL_BUTTON(obj : pointer) : PGtkToggleToolButton;
function GTK_TOGGLE_TOOL_BUTTON_CLASS(klass : pointer) : PGtkToggleToolButtonClass;
function GTK_IS_TOGGLE_TOOL_BUTTON(obj : pointer) : gboolean;
function GTK_IS_TOGGLE_TOOL_BUTTON_CLASS(klass : pointer) : gboolean;
function GTK_TOGGLE_TOOL_BUTTON_GET_CLASS(obj : pointer) : PGtkToggleToolButtonClass;


function gtk_toggle_tool_button_get_type:GType;cdecl;external gtklib name 'gtk_toggle_tool_button_get_type';
function gtk_toggle_tool_button_new:PGtkToolItem;cdecl;external gtklib name 'gtk_toggle_tool_button_new';
(* Const before type ignored *)
function gtk_toggle_tool_button_new_from_stock(stock_id:Pgchar):PGtkToolItem;cdecl;external gtklib name 'gtk_toggle_tool_button_new_from_stock';
procedure gtk_toggle_tool_button_set_active(button:PGtkToggleToolButton; is_active:gboolean);cdecl;external gtklib name 'gtk_toggle_tool_button_set_active';
function gtk_toggle_tool_button_get_active(button:PGtkToggleToolButton):gboolean;cdecl;external gtklib name 'gtk_toggle_tool_button_get_active';
{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_TOGGLE_TOOL_BUTTON : GType;
begin
   GTK_TYPE_TOGGLE_TOOL_BUTTON:=gtk_toggle_tool_button_get_type;
end;

function GTK_TOGGLE_TOOL_BUTTON(obj : pointer) : PGtkToggleToolButton;
begin
   GTK_TOGGLE_TOOL_BUTTON:=PGtkToggleToolButton(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TOGGLE_TOOL_BUTTON));
end;

function GTK_TOGGLE_TOOL_BUTTON_CLASS(klass : pointer) : PGtkToggleToolButtonClass;
begin
   GTK_TOGGLE_TOOL_BUTTON_CLASS:=PGtkToggleToolButtonClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TOGGLE_TOOL_BUTTON));
end;

function GTK_IS_TOGGLE_TOOL_BUTTON(obj : pointer) : gboolean;
begin
   GTK_IS_TOGGLE_TOOL_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TOGGLE_TOOL_BUTTON);
end;

function GTK_IS_TOGGLE_TOOL_BUTTON_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_TOGGLE_TOOL_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOGGLE_TOOL_BUTTON);
end;

function GTK_TOGGLE_TOOL_BUTTON_GET_CLASS(obj : pointer) : PGtkToggleToolButtonClass;
begin
   GTK_TOGGLE_TOOL_BUTTON_GET_CLASS:=PGtkToggleToolButtonClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TOGGLE_TOOL_BUTTON));
end;

{$endif read_implementation}
