{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkAspectFrame = ^TGtkAspectFrame;
       TGtkAspectFrame = record
            frame : TGtkFrame;
            xalign : gfloat;
            yalign : gfloat;
            ratio : gfloat;
            obey_child : gint;
            center_allocation : TGtkAllocation;
         end;

       PGtkAspectFrameClass = ^TGtkAspectFrameClass;
       TGtkAspectFrameClass = record
            parent_class : TGtkBinClass;
         end;

Type
  GTK_ASPECT_FRAME=PGtkAspectFrame;
  GTK_ASPECT_FRAME_CLASS=PGtkAspectFrameClass;

function  GTK_ASPECT_FRAME_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_aspect_frame_get_type';
function  GTK_IS_ASPECT_FRAME(obj:pointer):boolean;
function  GTK_IS_ASPECT_FRAME_CLASS(klass:pointer):boolean;

function  gtk_aspect_frame_get_type:TGtkType;cdecl;external gtkdll name 'gtk_aspect_frame_get_type';
function  gtk_aspect_frame_new (thelabel:Pgchar; xalign:gfloat; yalign:gfloat; ratio:gfloat; obey_child:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_aspect_frame_new';
procedure gtk_aspect_frame_set(aspect_frame:PGtkAspectFrame; xalign:gfloat; yalign:gfloat; ratio:gfloat; obey_child:gint);cdecl;external gtkdll name 'gtk_aspect_frame_set';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ASPECT_FRAME(obj:pointer):boolean;
begin
  GTK_IS_ASPECT_FRAME:=(obj<>nil) and GTK_IS_ASPECT_FRAME_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ASPECT_FRAME_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ASPECT_FRAME_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ASPECT_FRAME_TYPE);
end;

{$endif read_implementation}


