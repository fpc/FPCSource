// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< public > }
{< private > }
   PGtkLayout = ^TGtkLayout;
   TGtkLayout = record
        container : TGtkContainer;
        children : PGList;
        width : guint;
        height : guint;
        hadjustment : PGtkAdjustment;
        vadjustment : PGtkAdjustment;
        bin_window : PGdkWindow;
        visibility : TGdkVisibilityState;
        scroll_x : gint;
        scroll_y : gint;
        freeze_count : guint;
     end;

{ Padding for future expansion  }
   PGtkLayoutClass = ^TGtkLayoutClass;
   TGtkLayoutClass = record
        parent_class : TGtkContainerClass;
        set_scroll_adjustments : procedure (layout:PGtkLayout; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_LAYOUT : GType;
function GTK_LAYOUT(obj: pointer) : PGtkLayout;
function GTK_LAYOUT_CLASS(klass: pointer) : PGtkLayoutClass;
function GTK_IS_LAYOUT(obj: pointer) : boolean;
function GTK_IS_LAYOUT_CLASS(klass: pointer) : boolean;
function GTK_LAYOUT_GET_CLASS(obj: pointer) : PGtkLayoutClass;


function gtk_layout_get_type:TGtkType; cdecl; external gtklib;
function gtk_layout_new(hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
procedure gtk_layout_put(layout:PGtkLayout; child_widget:PGtkWidget; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_layout_move(layout:PGtkLayout; child_widget:PGtkWidget; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_layout_set_size(layout:PGtkLayout; width:guint; height:guint); cdecl; external gtklib;
procedure gtk_layout_get_size(layout:PGtkLayout; width:Pguint; height:Pguint); cdecl; external gtklib;
function gtk_layout_get_hadjustment(layout:PGtkLayout):PGtkAdjustment; cdecl; external gtklib;
function gtk_layout_get_vadjustment(layout:PGtkLayout):PGtkAdjustment; cdecl; external gtklib;
procedure gtk_layout_set_hadjustment(layout:PGtkLayout; adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_layout_set_vadjustment(layout:PGtkLayout; adjustment:PGtkAdjustment); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
{ These disable and enable moving and repainting the scrolling window
   of the GtkLayout, respectively.  If you want to update the layout's
   offsets but do not want it to repaint itself, you should use these
   functions.

   - I don't understand these are supposed to work, so I suspect
   - they don't now.                    OWT 1/20/98
  }

procedure gtk_layout_freeze(layout:PGtkLayout); cdecl; external gtklib;
procedure gtk_layout_thaw(layout:PGtkLayout); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_LAYOUT : GType;
begin
   GTK_TYPE_LAYOUT:=gtk_layout_get_type;
end;

function GTK_LAYOUT(obj: pointer) : PGtkLayout;
begin
   GTK_LAYOUT:=PGtkLayout(GTK_CHECK_CAST(obj,GTK_TYPE_LAYOUT));
end;

function GTK_LAYOUT_CLASS(klass: pointer) : PGtkLayoutClass;
begin
   GTK_LAYOUT_CLASS:=PGtkLayoutClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_LAYOUT));
end;

function GTK_IS_LAYOUT(obj: pointer) : boolean;
begin
   GTK_IS_LAYOUT:=GTK_CHECK_TYPE(obj,GTK_TYPE_LAYOUT);
end;

function GTK_IS_LAYOUT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_LAYOUT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_LAYOUT);
end;

function GTK_LAYOUT_GET_CLASS(obj: pointer) : PGtkLayoutClass;
begin
   GTK_LAYOUT_GET_CLASS:=PGtkLayoutClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_LAYOUT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

