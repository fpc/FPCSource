// included by gtk2.pas

{
   NOTE this widget is considered too specialized/little-used for
   GTK+, and will in the future be moved to some other package.  If
   your application needs this widget, feel free to use it, as the
   widget does work and is useful in some applications; it's just not
   of general interest. However, we are not accepting new features for
   the widget, and it will eventually move out of the GTK+
   distribution.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ spline, linear, free, gamma, reset  }
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

{ Padding for future expansion  }
   PGtkGammaCurveClass = ^TGtkGammaCurveClass;
   TGtkGammaCurveClass = record
        parent_class : TGtkVBoxClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_GAMMA_CURVE : GType;
function GTK_GAMMA_CURVE(obj: pointer) : PGtkGammaCurve;
function GTK_GAMMA_CURVE_CLASS(klass: pointer) : PGtkGammaCurveClass;
function GTK_IS_GAMMA_CURVE(obj: pointer) : boolean;
function GTK_IS_GAMMA_CURVE_CLASS(klass: pointer) : boolean;
function GTK_GAMMA_CURVE_GET_CLASS(obj: pointer) : PGtkGammaCurveClass;


function gtk_gamma_curve_get_type:TGtkType; cdecl; external gtklib;
function gtk_gamma_curve_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_GAMMA_CURVE : GType;
begin
   GTK_TYPE_GAMMA_CURVE:=gtk_gamma_curve_get_type;
end;

function GTK_GAMMA_CURVE(obj: pointer) : PGtkGammaCurve;
begin
   GTK_GAMMA_CURVE:=PGtkGammaCurve(GTK_CHECK_CAST(obj,GTK_TYPE_GAMMA_CURVE));
end;

function GTK_GAMMA_CURVE_CLASS(klass: pointer) : PGtkGammaCurveClass;
begin
   GTK_GAMMA_CURVE_CLASS:=PGtkGammaCurveClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_GAMMA_CURVE));
end;

function GTK_IS_GAMMA_CURVE(obj: pointer) : boolean;
begin
   GTK_IS_GAMMA_CURVE:=GTK_CHECK_TYPE(obj,GTK_TYPE_GAMMA_CURVE);
end;

function GTK_IS_GAMMA_CURVE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_GAMMA_CURVE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_GAMMA_CURVE);
end;

function GTK_GAMMA_CURVE_GET_CLASS(obj: pointer) : PGtkGammaCurveClass;
begin
   GTK_GAMMA_CURVE_GET_CLASS:=PGtkGammaCurveClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_GAMMA_CURVE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

