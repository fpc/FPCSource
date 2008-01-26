{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkGammaCurve = ^TGtkGammaCurve;
     TGtkGammaCurve = record
          vbox : TGtkVBox;
          table : PGtkWidget;
          curve : PGtkWidget;
          button : array[0..4] of PGtkWidget;
          gamma : gfloat;
          gamma_dialog : PGtkWidget;
          gamma_text : PGtkWidget;
       end;

     PGtkGammaCurveClass = ^TGtkGammaCurveClass;
     TGtkGammaCurveClass = record
          parent_class : TGtkVBoxClass;
       end;

type
  GTK_GAMMA_CURVE=PGtkGammaCurve;
  GTK_GAMMA_CURVE_CLASS=PGtkGammaCurveClass;

function  GTK_GAMMA_CURVE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_gamma_curve_get_type';
function  GTK_IS_GAMMA_CURVE(obj:pointer):boolean;
function  GTK_IS_GAMMA_CURVE_CLASS(klass:pointer):boolean;

function  gtk_gamma_curve_get_type:TGtkType;cdecl;external gtkdll name 'gtk_gamma_curve_get_type';
function  gtk_gamma_curve_new :PGtkWidget;cdecl;external gtkdll name 'gtk_gamma_curve_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_GAMMA_CURVE(obj:pointer):boolean;
begin
  GTK_IS_GAMMA_CURVE:=(obj<>nil) and GTK_IS_GAMMA_CURVE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_GAMMA_CURVE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_GAMMA_CURVE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_GAMMA_CURVE_TYPE);
end;

{$endif read_implementation}


