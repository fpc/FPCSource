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
{ (cached) graph height in pixels  }
{ point currently grabbed  }
{ (cached) curve points:  }
{ control points:  }
{ number of control points  }
{ array of control points  }
   Tctlpoint = array[0..1] of gfloat;
   Pctlpoint = ^Tctlpoint;

   PGtkCurve = ^TGtkCurve;
   TGtkCurve = record
        graph : TGtkDrawingArea;
        cursor_type : gint;
        min_x : gfloat;
        max_x : gfloat;
        min_y : gfloat;
        max_y : gfloat;
        pixmap : PGdkPixmap;
        curve_type : TGtkCurveType;
        height : gint;
        grab_point : gint;
        last : gint;
        num_points : gint;
        point : PGdkPoint;
        num_ctlpoints : gint;
        ctlpoint : Pctlpoint;
     end;

{ Padding for future expansion  }
   PGtkCurveClass = ^TGtkCurveClass;
   TGtkCurveClass = record
        parent_class : TGtkDrawingAreaClass;
        curve_type_changed : procedure (curve:PGtkCurve); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_CURVE : GType;
function GTK_CURVE(obj: pointer) : PGtkCurve;
function GTK_CURVE_CLASS(klass: pointer) : PGtkCurveClass;
function GTK_IS_CURVE(obj: pointer) : boolean;
function GTK_IS_CURVE_CLASS(klass: pointer) : boolean;
function GTK_CURVE_GET_CLASS(obj: pointer) : PGtkCurveClass;


function gtk_curve_get_type:TGtkType; cdecl; external gtklib;
function gtk_curve_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_curve_reset(curve:PGtkCurve); cdecl; external gtklib;
procedure gtk_curve_set_gamma(curve:PGtkCurve; gamma:gfloat); cdecl; external gtklib;
procedure gtk_curve_set_range(curve:PGtkCurve; min_x:gfloat; max_x:gfloat; min_y:gfloat; max_y:gfloat); cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_curve_get_vector(curve:PGtkCurve; veclen:longint; vector:array of gfloat); cdecl; external gtklib;
procedure gtk_curve_set_vector(curve:PGtkCurve; veclen:longint; vector:array of gfloat); cdecl; external gtklib;
{$ELSE}
procedure gtk_curve_get_vector(curve:PGtkCurve; veclen:longint); varargs; cdecl; external gtklib;
procedure gtk_curve_set_vector(curve:PGtkCurve; veclen:longint); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_curve_set_curve_type(curve:PGtkCurve; _type:TGtkCurveType); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CURVE : GType;
begin
   GTK_TYPE_CURVE:=gtk_curve_get_type;
end;

function GTK_CURVE(obj: pointer) : PGtkCurve;
begin
   GTK_CURVE:=PGtkCurve(GTK_CHECK_CAST(obj,GTK_TYPE_CURVE));
end;

function GTK_CURVE_CLASS(klass: pointer) : PGtkCurveClass;
begin
   GTK_CURVE_CLASS:=PGtkCurveClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CURVE));
end;

function GTK_IS_CURVE(obj: pointer) : boolean;
begin
   GTK_IS_CURVE:=GTK_CHECK_TYPE(obj,GTK_TYPE_CURVE);
end;

function GTK_IS_CURVE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CURVE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CURVE);
end;

function GTK_CURVE_GET_CLASS(obj: pointer) : PGtkCurveClass;
begin
   GTK_CURVE_GET_CLASS:=PGtkCurveClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CURVE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

