// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkButtonBox = ^TGtkButtonBox;
   TGtkButtonBox = record
        box : TGtkBox;
        child_min_width : gint;
        child_min_height : gint;
        child_ipad_x : gint;
        child_ipad_y : gint;
        layout_style : TGtkButtonBoxStyle;
     end;

   PGtkButtonBoxClass = ^TGtkButtonBoxClass;
   TGtkButtonBoxClass = record
        parent_class : TGtkBoxClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_BUTTONBOX_DEFAULT = -(1);

function GTK_TYPE_BUTTON_BOX : GType;
function GTK_BUTTON_BOX(obj: pointer) : PGtkButtonBox;
function GTK_BUTTON_BOX_CLASS(klass: pointer) : PGtkButtonBoxClass;
function GTK_IS_BUTTON_BOX(obj: pointer) : boolean;
function GTK_IS_BUTTON_BOX_CLASS(klass: pointer) : boolean;
function GTK_BUTTON_BOX_GET_CLASS(obj: pointer) : PGtkButtonBoxClass;


function gtk_button_box_get_type:TGtkType; cdecl; external gtklib;
function gtk_button_box_get_layout(widget:PGtkButtonBox):TGtkButtonBoxStyle; cdecl; external gtklib;
procedure gtk_button_box_set_layout(widget:PGtkButtonBox; layout_style:TGtkButtonBoxStyle); cdecl; external gtklib;
procedure gtk_button_box_set_child_secondary(widget:PGtkButtonBox; child:PGtkWidget; is_secondary:gboolean); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_button_box_set_spacing(b: pointer; s : gint);
function gtk_button_box_get_spacing(b : pointer) : gint;

procedure gtk_button_box_set_child_size(widget:PGtkButtonBox; min_width:gint; min_height:gint); cdecl; external gtklib;
procedure gtk_button_box_set_child_ipadding(widget:PGtkButtonBox; ipad_x:gint; ipad_y:gint); cdecl; external gtklib;
procedure gtk_button_box_get_child_size(widget:PGtkButtonBox; min_width:Pgint; min_height:Pgint); cdecl; external gtklib;
procedure gtk_button_box_get_child_ipadding(widget:PGtkButtonBox; ipad_x:Pgint; ipad_y:Pgint); cdecl; external gtklib;
{$endif}
{ Internal method - do not use.  }

procedure _gtk_button_box_child_requisition(widget:PGtkWidget; nvis_children:Plongint; nvis_secondaries:Plongint; width:Plongint; height:Plongint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_BUTTON_BOX : GType;
begin
   GTK_TYPE_BUTTON_BOX:=gtk_button_box_get_type;
end;

function GTK_BUTTON_BOX(obj: pointer) : PGtkButtonBox;
begin
   GTK_BUTTON_BOX:=PGtkButtonBox(GTK_CHECK_CAST(obj,GTK_TYPE_BUTTON_BOX));
end;

function GTK_BUTTON_BOX_CLASS(klass: pointer) : PGtkButtonBoxClass;
begin
   GTK_BUTTON_BOX_CLASS:=PGtkButtonBoxClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_BUTTON_BOX));
end;

function GTK_IS_BUTTON_BOX(obj: pointer) : boolean;
begin
   GTK_IS_BUTTON_BOX:=GTK_CHECK_TYPE(obj,GTK_TYPE_BUTTON_BOX);
end;

function GTK_IS_BUTTON_BOX_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_BUTTON_BOX_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_BUTTON_BOX);
end;

function GTK_BUTTON_BOX_GET_CLASS(obj: pointer) : PGtkButtonBoxClass;
begin
   GTK_BUTTON_BOX_GET_CLASS:=PGtkButtonBoxClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_BUTTON_BOX));
end;

procedure gtk_button_box_set_spacing(b: pointer; s : gint);
begin
   gtk_box_set_spacing(GTK_BOX(b),s);
end;

function gtk_button_box_get_spacing(b : pointer) : gint;
begin
   gtk_button_box_get_spacing:=gtk_box_get_spacing(GTK_BOX(b));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

