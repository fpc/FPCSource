// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkVButtonBox = ^TGtkVButtonBox;
   TGtkVButtonBox = record
        button_box : TGtkButtonBox;
     end;

   PGtkVButtonBoxClass = ^TGtkVButtonBoxClass;
   TGtkVButtonBoxClass = record
        parent_class : TGtkButtonBoxClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VBUTTON_BOX : GType;
function GTK_VBUTTON_BOX(obj: pointer) : PGtkVButtonBox;
function GTK_VBUTTON_BOX_CLASS(klass: pointer) : PGtkVButtonBoxClass;
function GTK_IS_VBUTTON_BOX(obj: pointer) : boolean;
function GTK_IS_VBUTTON_BOX_CLASS(klass: pointer) : boolean;
function GTK_VBUTTON_BOX_GET_CLASS(obj: pointer) : PGtkVButtonBoxClass;



function gtk_vbutton_box_get_type:TGtkType; cdecl; external gtklib;
function gtk_vbutton_box_new:PGtkWidget; cdecl; external gtklib;
{ buttons can be added by gtk_container_add()  }

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_vbutton_box_get_spacing_default:gint; cdecl; external gtklib;
procedure gtk_vbutton_box_set_spacing_default(spacing:gint); cdecl; external gtklib;
function gtk_vbutton_box_get_layout_default:TGtkButtonBoxStyle; cdecl; external gtklib;
procedure gtk_vbutton_box_set_layout_default(layout:TGtkButtonBoxStyle); cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VBUTTON_BOX : GType;
begin
   GTK_TYPE_VBUTTON_BOX:=gtk_vbutton_box_get_type;
end;

function GTK_VBUTTON_BOX(obj: pointer) : PGtkVButtonBox;
begin
   GTK_VBUTTON_BOX:=PGtkVButtonBox(GTK_CHECK_CAST(obj,GTK_TYPE_VBUTTON_BOX));
end;

function GTK_VBUTTON_BOX_CLASS(klass: pointer) : PGtkVButtonBoxClass;
begin
   GTK_VBUTTON_BOX_CLASS:=PGtkVButtonBoxClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VBUTTON_BOX));
end;

function GTK_IS_VBUTTON_BOX(obj: pointer) : boolean;
begin
   GTK_IS_VBUTTON_BOX:=GTK_CHECK_TYPE(obj,GTK_TYPE_VBUTTON_BOX);
end;

function GTK_IS_VBUTTON_BOX_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VBUTTON_BOX_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VBUTTON_BOX);
end;

function GTK_VBUTTON_BOX_GET_CLASS(obj: pointer) : PGtkVButtonBoxClass;
begin
   GTK_VBUTTON_BOX_GET_CLASS:=PGtkVButtonBoxClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VBUTTON_BOX));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

