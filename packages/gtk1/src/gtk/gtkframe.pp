{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkFrame = ^TGtkFrame;
       TGtkFrame = record
            bin : TGtkBin;
            thelabel : Pgchar;
            shadow_type : gint16;
            label_width : gint16;
            label_height : gint16;
            label_xalign : gfloat;
            label_yalign : gfloat;
         end;

       PGtkFrameClass = ^TGtkFrameClass;
       TGtkFrameClass = record
            parent_class : TGtkBinClass;
         end;

Type
  GTK_FRAME=PGtkFrame;
  GTK_FRAME_CLASS=PGtkFrameClass;

function  GTK_FRAME_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_frame_get_type';
function  GTK_IS_FRAME(obj:pointer):boolean;
function  GTK_IS_FRAME_CLASS(klass:pointer):boolean;

function  gtk_frame_get_type:TGtkType;cdecl;external gtkdll name 'gtk_frame_get_type';
function  gtk_frame_new (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_frame_new';
procedure gtk_frame_set_label(frame:PGtkFrame; thelabel:Pgchar);cdecl;external gtkdll name 'gtk_frame_set_label';
procedure gtk_frame_set_label_align(frame:PGtkFrame; xalign:gfloat; yalign:gfloat);cdecl;external gtkdll name 'gtk_frame_set_label_align';
procedure gtk_frame_set_shadow_type(frame:PGtkFrame; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_frame_set_shadow_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_FRAME(obj:pointer):boolean;
begin
  GTK_IS_FRAME:=(obj<>nil) and GTK_IS_FRAME_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_FRAME_CLASS(klass:pointer):boolean;
begin
  GTK_IS_FRAME_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_FRAME_TYPE);
end;

{$endif read_implementation}


