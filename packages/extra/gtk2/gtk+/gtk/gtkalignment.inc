// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
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

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ALIGNMENT : GType;
function GTK_ALIGNMENT(obj: pointer) : PGtkAlignment;
function GTK_ALIGNMENT_CLASS(klass: pointer) : PGtkAlignmentClass;
function GTK_IS_ALIGNMENT(obj: pointer) : boolean;
function GTK_IS_ALIGNMENT_CLASS(klass: pointer) : boolean;
function GTK_ALIGNMENT_GET_CLASS(obj: pointer) : PGtkAlignmentClass;


function gtk_alignment_get_type:TGtkType; cdecl; external gtklib;
function gtk_alignment_new(xalign:gfloat; yalign:gfloat; xscale:gfloat; yscale:gfloat):PGtkWidget; cdecl; external gtklib;
procedure gtk_alignment_set(alignment:PGtkAlignment; xalign:gfloat; yalign:gfloat; xscale:gfloat; yscale:gfloat); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ALIGNMENT : GType;
begin
   GTK_TYPE_ALIGNMENT:=gtk_alignment_get_type;
end;

function GTK_ALIGNMENT(obj: pointer) : PGtkAlignment;
begin
   GTK_ALIGNMENT:=PGtkAlignment(GTK_CHECK_CAST(obj,GTK_TYPE_ALIGNMENT));
end;

function GTK_ALIGNMENT_CLASS(klass: pointer) : PGtkAlignmentClass;
begin
   GTK_ALIGNMENT_CLASS:=PGtkAlignmentClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ALIGNMENT));
end;

function GTK_IS_ALIGNMENT(obj: pointer) : boolean;
begin
   GTK_IS_ALIGNMENT:=GTK_CHECK_TYPE(obj,GTK_TYPE_ALIGNMENT);
end;

function GTK_IS_ALIGNMENT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ALIGNMENT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ALIGNMENT);
end;

function GTK_ALIGNMENT_GET_CLASS(obj: pointer) : PGtkAlignmentClass;
begin
   GTK_ALIGNMENT_GET_CLASS:=PGtkAlignmentClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ALIGNMENT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

