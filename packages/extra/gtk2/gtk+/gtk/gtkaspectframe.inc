// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkAspectFrame = ^TGtkAspectFrame;
   TGtkAspectFrame = record
        frame : TGtkFrame;
        xalign : gfloat;
        yalign : gfloat;
        ratio : gfloat;
        obey_child : gboolean;
        center_allocation : TGtkAllocation;
     end;

   PGtkAspectFrameClass = ^TGtkAspectFrameClass;
   TGtkAspectFrameClass = record
        parent_class : TGtkFrameClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ASPECT_FRAME : GType;
function GTK_ASPECT_FRAME(obj: pointer) : PGtkAspectFrame;
function GTK_ASPECT_FRAME_CLASS(klass: pointer) : PGtkAspectFrameClass;
function GTK_IS_ASPECT_FRAME(obj: pointer) : boolean;
function GTK_IS_ASPECT_FRAME_CLASS(klass: pointer) : boolean;
function GTK_ASPECT_FRAME_GET_CLASS(obj: pointer) : PGtkAspectFrameClass;


function gtk_aspect_frame_get_type:TGtkType; cdecl; external gtklib;
function gtk_aspect_frame_new(_label:Pgchar; xalign:gfloat; yalign:gfloat; ratio:gfloat; obey_child:gboolean):PGtkWidget; cdecl; external gtklib;
procedure gtk_aspect_frame_set(aspect_frame:PGtkAspectFrame; xalign:gfloat; yalign:gfloat; ratio:gfloat; obey_child:gboolean); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ASPECT_FRAME : GType;
begin
   GTK_TYPE_ASPECT_FRAME:=gtk_aspect_frame_get_type;
end;

function GTK_ASPECT_FRAME(obj: pointer) : PGtkAspectFrame;
begin
   GTK_ASPECT_FRAME:=PGtkAspectFrame(GTK_CHECK_CAST(obj,GTK_TYPE_ASPECT_FRAME));
end;

function GTK_ASPECT_FRAME_CLASS(klass: pointer) : PGtkAspectFrameClass;
begin
   GTK_ASPECT_FRAME_CLASS:=PGtkAspectFrameClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ASPECT_FRAME));
end;

function GTK_IS_ASPECT_FRAME(obj: pointer) : boolean;
begin
   GTK_IS_ASPECT_FRAME:=GTK_CHECK_TYPE(obj,GTK_TYPE_ASPECT_FRAME);
end;

function GTK_IS_ASPECT_FRAME_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ASPECT_FRAME_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ASPECT_FRAME);
end;

function GTK_ASPECT_FRAME_GET_CLASS(obj: pointer) : PGtkAspectFrameClass;
begin
   GTK_ASPECT_FRAME_GET_CLASS:=PGtkAspectFrameClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ASPECT_FRAME));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

