{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkAlignment = ^TGtkAlignment;
       TGtkAlignment = record
            bin : TGtkBin;
            xalign : gfloat;
            yalign : gfloat;
            xscale : gfloat;
            yscale : gfloat;
         end;

       PGtkAlignmentClass = ^TGtkAlignmentClass;
       TGtkAlignmentClass = record
            parent_class : TGtkBinClass;
         end;

Type
  GTK_ALIGNMENT=PGtkAlignment;
  GTK_ALIGNMENT_CLASS=PGtkAlignmentClass;

function  GTK_ALIGNMENT_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_alignment_get_type';
function  GTK_IS_ALIGNMENT(obj:pointer):boolean;
function  GTK_IS_ALIGNMENT_CLASS(klass:pointer):boolean;

function  gtk_alignment_get_type:TGtkType;cdecl;external gtkdll name 'gtk_alignment_get_type';
function  gtk_alignment_new (xalign:gfloat; yalign:gfloat; xscale:gfloat; yscale:gfloat):PGtkWidget;cdecl;external gtkdll name 'gtk_alignment_new';
procedure gtk_alignment_set(alignment:PGtkAlignment; xalign:gfloat; yalign:gfloat; xscale:gfloat; yscale:gfloat);cdecl;external gtkdll name 'gtk_alignment_set';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ALIGNMENT(obj:pointer):boolean;
begin
  GTK_IS_ALIGNMENT:=(obj<>nil) and GTK_IS_ALIGNMENT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ALIGNMENT_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ALIGNMENT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ALIGNMENT_TYPE);
end;

{$endif read_implementation}

