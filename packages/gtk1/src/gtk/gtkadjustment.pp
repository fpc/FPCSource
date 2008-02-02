{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

   type
     PGtkAdjustment = ^TGtkAdjustment;
     TGtkAdjustment = record
          data : TGtkData;
          lower : gfloat;
          upper : gfloat;
          value : gfloat;
          step_increment : gfloat;
          page_increment : gfloat;
          page_size : gfloat;
       end;

     AdjustProc = procedure (adjustment:PGtkAdjustment);cdecl;

     PGtkAdjustmentClass = ^TGtkAdjustmentClass;
     TGtkAdjustmentClass = record
          parent_class : TGtkDataClass;
          changed : AdjustProc;
          value_changed : AdjustProc;
       end;

type
  GTK_ADJUSTMENT=PGtkAdjustment;
  GTK_ADJUSTMENT_CLASS=PGtkAdjustmentClass;

function  GTK_ADJUSTMENT_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_adjustment_get_type';
function  GTK_IS_ADJUSTMENT(obj:pointer):boolean;
function  GTK_IS_ADJUSTMENT_CLASS(klass:pointer):boolean;

function  gtk_adjustment_get_type:TGtkType;cdecl;external gtkdll name 'gtk_adjustment_get_type';
function  gtk_adjustment_new(value:gfloat; lower:gfloat; upper:gfloat; step_increment:gfloat; page_increment:gfloat;page_size:gfloat):PGtkObject;cdecl;external gtkdll name 'gtk_adjustment_new';
procedure gtk_adjustment_changed(adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_adjustment_changed';
procedure gtk_adjustment_value_changed(adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_adjustment_value_changed';
procedure gtk_adjustment_clamp_page(adjustment:PGtkAdjustment; lower:gfloat; upper:gfloat);cdecl;external gtkdll name 'gtk_adjustment_clamp_page';
procedure gtk_adjustment_set_value(adjustment:PGtkAdjustment; value:gfloat);cdecl;external gtkdll name 'gtk_adjustment_set_value';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ADJUSTMENT(obj:pointer):boolean;
begin
  GTK_IS_ADJUSTMENT:=(obj<>nil) and GTK_IS_ADJUSTMENT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ADJUSTMENT_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ADJUSTMENT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ADJUSTMENT_TYPE);
end;

{$endif read_implementation}

