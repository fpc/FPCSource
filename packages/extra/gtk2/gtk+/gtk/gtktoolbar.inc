// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkToolbarChildType = ^TGtkToolbarChildType;
   TGtkToolbarChildType = (
     GTK_TOOLBAR_CHILD_SPACE,
     GTK_TOOLBAR_CHILD_BUTTON,
     GTK_TOOLBAR_CHILD_TOGGLEBUTTON,
     GTK_TOOLBAR_CHILD_RADIOBUTTON,
     GTK_TOOLBAR_CHILD_WIDGET
   );

   PGtkToolbarSpaceStyle = ^TGtkToolbarSpaceStyle;
   TGtkToolbarSpaceStyle = (
     GTK_TOOLBAR_SPACE_EMPTY,
     GTK_TOOLBAR_SPACE_LINE
   );

   PGtkToolbarChild = ^TGtkToolbarChild;
   TGtkToolbarChild = record
        _type : TGtkToolbarChildType;
        widget : PGtkWidget;
        icon : PGtkWidget;
        _label : PGtkWidget;
     end;

   PGtkToolbar = ^TGtkToolbar;
   TGtkToolbar = record
        container : TGtkContainer;
        num_children : gint;
        children : PGList;
        orientation : TGtkOrientation;
        style : TGtkToolbarStyle;
        icon_size : TGtkIconSize;
        tooltips : PGtkTooltips;
        button_maxw : gint;
        button_maxh : gint;
        style_set_connection : guint;
        icon_size_connection : guint;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkToolbarClass = ^TGtkToolbarClass;
   TGtkToolbarClass = record
        parent_class : TGtkContainerClass;
        orientation_changed : procedure (toolbar:PGtkToolbar; orientation:TGtkOrientation); cdecl;
        style_changed : procedure (toolbar:PGtkToolbar; style:TGtkToolbarStyle); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkToolbar_style_set = $1;
   bp_TGtkToolbar_style_set = 0;
   bm_TGtkToolbar_icon_size_set = $2;
   bp_TGtkToolbar_icon_size_set = 1;


function GTK_TYPE_TOOLBAR : GType;
function GTK_TOOLBAR(obj: pointer) : PGtkToolbar;
function GTK_TOOLBAR_CLASS(klass: pointer) : PGtkToolbarClass;
function GTK_IS_TOOLBAR(obj: pointer) : boolean;
function GTK_IS_TOOLBAR_CLASS(klass: pointer) : boolean;
function GTK_TOOLBAR_GET_CLASS(obj: pointer) : PGtkToolbarClass;

function style_set(var a : TGtkToolbar) : guint;
procedure set_style_set(var a : TGtkToolbar; __style_set : guint);
function icon_size_set(var a : TGtkToolbar) : guint;
procedure set_icon_size_set(var a : TGtkToolbar; __icon_size_set : guint);


function gtk_toolbar_get_type:TGtkType; cdecl; external gtklib;
function gtk_toolbar_new:PGtkWidget; cdecl; external gtklib;
{ Simple button items  }
function gtk_toolbar_append_item(toolbar:PGtkToolbar; text:Pchar; tooltip_text:Pchar; tooltip_private_text:Pchar; icon:PGtkWidget;
           callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget; cdecl; external gtklib;
function gtk_toolbar_prepend_item(toolbar:PGtkToolbar; text:Pchar; tooltip_text:Pchar; tooltip_private_text:Pchar; icon:PGtkWidget;
           callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget; cdecl; external gtklib;
function gtk_toolbar_insert_item(toolbar:PGtkToolbar; text:Pchar; tooltip_text:Pchar; tooltip_private_text:Pchar; icon:PGtkWidget;
           callback:TGtkSignalFunc; user_data:gpointer; position:gint):PGtkWidget; cdecl; external gtklib;
{ Stock Items  }
function gtk_toolbar_insert_stock(toolbar:PGtkToolbar; stock_id:Pgchar; tooltip_text:Pchar; tooltip_private_text:Pchar; callback:TGtkSignalFunc;
           user_data:gpointer; position:gint):PGtkWidget; cdecl; external gtklib;
{ Space Items  }
procedure gtk_toolbar_append_space(toolbar:PGtkToolbar); cdecl; external gtklib;
procedure gtk_toolbar_prepend_space(toolbar:PGtkToolbar); cdecl; external gtklib;
procedure gtk_toolbar_insert_space(toolbar:PGtkToolbar; position:gint); cdecl; external gtklib;
procedure gtk_toolbar_remove_space(toolbar:PGtkToolbar; position:gint); cdecl; external gtklib;
{ Any element type  }
function gtk_toolbar_append_element(toolbar:PGtkToolbar; _type:TGtkToolbarChildType; widget:PGtkWidget; text:Pchar; tooltip_text:Pchar;
           tooltip_private_text:Pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget; cdecl; external gtklib;
function gtk_toolbar_prepend_element(toolbar:PGtkToolbar; _type:TGtkToolbarChildType; widget:PGtkWidget; text:Pchar; tooltip_text:Pchar;
           tooltip_private_text:Pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget; cdecl; external gtklib;
function gtk_toolbar_insert_element(toolbar:PGtkToolbar; _type:TGtkToolbarChildType; widget:PGtkWidget; text:Pchar; tooltip_text:Pchar;
           tooltip_private_text:Pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer; position:gint):PGtkWidget; cdecl; external gtklib;
{ Generic Widgets  }
procedure gtk_toolbar_append_widget(toolbar:PGtkToolbar; widget:PGtkWidget; tooltip_text:Pchar; tooltip_private_text:Pchar); cdecl; external gtklib;
procedure gtk_toolbar_prepend_widget(toolbar:PGtkToolbar; widget:PGtkWidget; tooltip_text:Pchar; tooltip_private_text:Pchar); cdecl; external gtklib;
procedure gtk_toolbar_insert_widget(toolbar:PGtkToolbar; widget:PGtkWidget; tooltip_text:Pchar; tooltip_private_text:Pchar; position:gint); cdecl; external gtklib;
{ Style functions  }
procedure gtk_toolbar_set_orientation(toolbar:PGtkToolbar; orientation:TGtkOrientation); cdecl; external gtklib;
procedure gtk_toolbar_set_style(toolbar:PGtkToolbar; style:TGtkToolbarStyle); cdecl; external gtklib;
procedure gtk_toolbar_set_icon_size(toolbar:PGtkToolbar; icon_size:TGtkIconSize); cdecl; external gtklib;
procedure gtk_toolbar_set_tooltips(toolbar:PGtkToolbar; enable:gboolean); cdecl; external gtklib;
procedure gtk_toolbar_unset_style(toolbar:PGtkToolbar); cdecl; external gtklib;
procedure gtk_toolbar_unset_icon_size(toolbar:PGtkToolbar); cdecl; external gtklib;
function gtk_toolbar_get_orientation(toolbar:PGtkToolbar):TGtkOrientation; cdecl; external gtklib;
function gtk_toolbar_get_style(toolbar:PGtkToolbar):TGtkToolbarStyle; cdecl; external gtklib;
function gtk_toolbar_get_icon_size(toolbar:PGtkToolbar):TGtkIconSize; cdecl; external gtklib;
function gtk_toolbar_get_tooltips(toolbar:PGtkToolbar):gboolean; cdecl; external gtklib;

{$IFDEF HasGTK2_4}
function gtk_separator_tool_item_get_type:TGtkType; cdecl; external gtklib;
function GTK_TYPE_SEPARATOR_TOOL_ITEM : GType;
{$ENDIF HasGTK2_4}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TOOLBAR : GType;
begin
   GTK_TYPE_TOOLBAR:=gtk_toolbar_get_type;
end;

function GTK_TOOLBAR(obj: pointer) : PGtkToolbar;
begin
   GTK_TOOLBAR:=PGtkToolbar(GTK_CHECK_CAST(obj,GTK_TYPE_TOOLBAR));
end;

function GTK_TOOLBAR_CLASS(klass: pointer) : PGtkToolbarClass;
begin
   GTK_TOOLBAR_CLASS:=PGtkToolbarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TOOLBAR));
end;

function GTK_IS_TOOLBAR(obj: pointer) : boolean;
begin
   GTK_IS_TOOLBAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_TOOLBAR);
end;

function GTK_IS_TOOLBAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TOOLBAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TOOLBAR);
end;

function GTK_TOOLBAR_GET_CLASS(obj: pointer) : PGtkToolbarClass;
begin
   GTK_TOOLBAR_GET_CLASS:=PGtkToolbarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TOOLBAR));
end;

function style_set(var a : TGtkToolbar) : guint;
begin
   style_set:=(a.flag0 and bm_TGtkToolbar_style_set) shr bp_TGtkToolbar_style_set;
end;

procedure set_style_set(var a : TGtkToolbar; __style_set : guint);
begin
   a.flag0:=a.flag0 or ((__style_set shl bp_TGtkToolbar_style_set) and bm_TGtkToolbar_style_set);
end;

function icon_size_set(var a : TGtkToolbar) : guint;
begin
   icon_size_set:=(a.flag0 and bm_TGtkToolbar_icon_size_set) shr bp_TGtkToolbar_icon_size_set;
end;

procedure set_icon_size_set(var a : TGtkToolbar; __icon_size_set : guint);
begin
   a.flag0:=a.flag0 or ((__icon_size_set shl bp_TGtkToolbar_icon_size_set) and bm_TGtkToolbar_icon_size_set);
end;

{$IFDEF HasGTK2_4}
function GTK_TYPE_SEPARATOR_TOOL_ITEM : GType;
begin
  GTK_TYPE_SEPARATOR_TOOL_ITEM:=gtk_separator_tool_item_get_type;
end;
{$ENDIF HasGTK2_4}

{$ENDIF read_implementation}
// included by gtk2.pas

