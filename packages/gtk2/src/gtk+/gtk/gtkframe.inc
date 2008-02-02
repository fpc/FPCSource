// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkFrame = ^TGtkFrame;
   TGtkFrame = record
        bin : TGtkBin;
        label_widget : PGtkWidget;
        shadow_type : gint16;
        label_xalign : gfloat;
        label_yalign : gfloat;
        child_allocation : TGtkAllocation;
     end;

   PGtkFrameClass = ^TGtkFrameClass;
   TGtkFrameClass = record
        parent_class : TGtkBinClass;
        compute_child_allocation : procedure (frame:PGtkFrame; allocation:PGtkAllocation); cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_FRAME : GType;
function GTK_FRAME(obj: pointer) : PGtkFrame;
function GTK_FRAME_CLASS(klass: pointer) : PGtkFrameClass;
function GTK_IS_FRAME(obj: pointer) : boolean;
function GTK_IS_FRAME_CLASS(klass: pointer) : boolean;
function GTK_FRAME_GET_CLASS(obj: pointer) : PGtkFrameClass;



function gtk_frame_get_type:TGtkType; cdecl; external gtklib;
function gtk_frame_new(_label:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_frame_set_label(frame:PGtkFrame; _label:Pgchar); cdecl; external gtklib;
function gtk_frame_get_label(frame:PGtkFrame):Pgchar; cdecl; external gtklib;
procedure gtk_frame_set_label_widget(frame:PGtkFrame; label_widget:PGtkWidget); cdecl; external gtklib;
function gtk_frame_get_label_widget(frame:PGtkFrame):PGtkWidget; cdecl; external gtklib;
procedure gtk_frame_set_label_align(frame:PGtkFrame; xalign:gfloat; yalign:gfloat); cdecl; external gtklib;
procedure gtk_frame_get_label_align(frame:PGtkFrame; xalign:Pgfloat; yalign:Pgfloat); cdecl; external gtklib;
procedure gtk_frame_set_shadow_type(frame:PGtkFrame; _type:TGtkShadowType); cdecl; external gtklib;
function gtk_frame_get_shadow_type(frame:PGtkFrame):TGtkShadowType; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_FRAME : GType;
begin
   GTK_TYPE_FRAME:=gtk_frame_get_type;
end;

function GTK_FRAME(obj: pointer) : PGtkFrame;
begin
   GTK_FRAME:=PGtkFrame(GTK_CHECK_CAST(obj,GTK_TYPE_FRAME));
end;

function GTK_FRAME_CLASS(klass: pointer) : PGtkFrameClass;
begin
   GTK_FRAME_CLASS:=PGtkFrameClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_FRAME));
end;

function GTK_IS_FRAME(obj: pointer) : boolean;
begin
   GTK_IS_FRAME:=GTK_CHECK_TYPE(obj,GTK_TYPE_FRAME);
end;

function GTK_IS_FRAME_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_FRAME_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_FRAME);
end;

function GTK_FRAME_GET_CLASS(obj: pointer) : PGtkFrameClass;
begin
   GTK_FRAME_GET_CLASS:=PGtkFrameClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_FRAME));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

