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
   PGtkRulerMetric = ^TGtkRulerMetric;

{ All distances below are in 1/72nd's of an inch. (According to
   Adobe that's a point, but points are really 1/72.27 in.)
  }
{ The upper limit of the ruler (in points)  }
{ The lower limit of the ruler  }
{ The position of the mark on the ruler  }
{ The maximum size of the ruler  }
   PGtkRuler = ^TGtkRuler;
   TGtkRuler = record
        widget : TGtkWidget;
        backing_store : PGdkPixmap;
        non_gr_exp_gc : PGdkGC;
        metric : PGtkRulerMetric;
        xsrc : gint;
        ysrc : gint;
        slider_size : gint;
        lower : gdouble;
        upper : gdouble;
        position : gdouble;
        max_size : gdouble;
     end;

{ Padding for future expansion  }
   PGtkRulerClass = ^TGtkRulerClass;
   TGtkRulerClass = record
        parent_class : TGtkWidgetClass;
        draw_ticks : procedure (ruler:PGtkRuler); cdecl;
        draw_pos : procedure (ruler:PGtkRuler); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{ This should be points_per_unit. This is the size of the unit
     in 1/72nd's of an inch and has nothing to do with screen pixels  }
{ five possible modes of subdivision  }
   TGtkRulerMetric = record
        metric_name : Pgchar;
        abbrev : Pgchar;
        pixels_per_unit : gdouble;
        ruler_scale : array[0..9] of gdouble;
        subdivide : array[0..4] of gint;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_RULER : GType;
function GTK_RULER(obj: pointer) : PGtkRuler;
function GTK_RULER_CLASS(klass: pointer) : PGtkRulerClass;
function GTK_IS_RULER(obj: pointer) : boolean;
function GTK_IS_RULER_CLASS(klass: pointer) : boolean;
function GTK_RULER_GET_CLASS(obj: pointer) : PGtkRulerClass;


function gtk_ruler_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_ruler_set_metric(ruler:PGtkRuler; metric:TGtkMetricType); cdecl; external gtklib;
procedure gtk_ruler_set_range(ruler:PGtkRuler; lower:gdouble; upper:gdouble; position:gdouble; max_size:gdouble); cdecl; external gtklib;
procedure gtk_ruler_draw_ticks(ruler:PGtkRuler); cdecl; external gtklib;
procedure gtk_ruler_draw_pos(ruler:PGtkRuler); cdecl; external gtklib;
function gtk_ruler_get_metric(ruler:PGtkRuler):TGtkMetricType; cdecl; external gtklib;
procedure gtk_ruler_get_range(ruler:PGtkRuler; lower:Pgdouble; upper:Pgdouble; position:Pgdouble; max_size:Pgdouble); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_RULER : GType;
begin
   GTK_TYPE_RULER:=gtk_ruler_get_type;
end;

function GTK_RULER(obj: pointer) : PGtkRuler;
begin
   GTK_RULER:=PGtkRuler(GTK_CHECK_CAST(obj,GTK_TYPE_RULER));
end;

function GTK_RULER_CLASS(klass: pointer) : PGtkRulerClass;
begin
   GTK_RULER_CLASS:=PGtkRulerClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_RULER));
end;

function GTK_IS_RULER(obj: pointer) : boolean;
begin
   GTK_IS_RULER:=GTK_CHECK_TYPE(obj,GTK_TYPE_RULER);
end;

function GTK_IS_RULER_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_RULER_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_RULER);
end;

function GTK_RULER_GET_CLASS(obj: pointer) : PGtkRulerClass;
begin
   GTK_RULER_GET_CLASS:=PGtkRulerClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_RULER));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

