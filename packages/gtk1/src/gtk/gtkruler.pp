{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkRulerMetric = ^TGtkRulerMetric;

     PGtkRuler = ^TGtkRuler;
     TGtkRuler = record
          widget : TGtkWidget;
          backing_store : PGdkPixmap;
          non_gr_exp_gc : PGdkGC;
          metric : PGtkRulerMetric;
          xsrc : gint;
          ysrc : gint;
          slider_size : gint;
          lower : gfloat;
          upper : gfloat;
          position : gfloat;
          max_size : gfloat;
       end;

     PGtkRulerClass = ^TGtkRulerClass;
     TGtkRulerClass = record
          parent_class : TGtkWidgetClass;
          draw_ticks : procedure (ruler:PGtkRuler); cdecl;
          draw_pos : procedure (ruler:PGtkRuler); cdecl;
       end;

     TGtkRulerMetric = record
          metric_name : Pgchar;
          abbrev : Pgchar;
          pixels_per_unit : gfloat;
          ruler_scale : array[0..9] of gfloat;
          subdivide : array[0..4] of gint;
       end;

Type
  GTK_RULER=PGtkRuler;
  GTK_RULER_CLASS=PGtkRulerClass;

function  GTK_RULER_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_ruler_get_type';
function  GTK_IS_RULER(obj:pointer):boolean;
function  GTK_IS_RULER_CLASS(klass:pointer):boolean;

function  gtk_ruler_get_type:TGtkType;cdecl;external gtkdll name 'gtk_ruler_get_type';
procedure gtk_ruler_set_metric(ruler:PGtkRuler; metric:TGtkMetricType);cdecl;external gtkdll name 'gtk_ruler_set_metric';
procedure gtk_ruler_set_range(ruler:PGtkRuler; lower:gfloat; upper:gfloat; position:gfloat; max_size:gfloat);cdecl;external gtkdll name 'gtk_ruler_set_range';
procedure gtk_ruler_draw_ticks(ruler:PGtkRuler);cdecl;external gtkdll name 'gtk_ruler_draw_ticks';
procedure gtk_ruler_draw_pos(ruler:PGtkRuler);cdecl;external gtkdll name 'gtk_ruler_draw_pos';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_RULER(obj:pointer):boolean;
begin
  GTK_IS_RULER:=(obj<>nil) and GTK_IS_RULER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_RULER_CLASS(klass:pointer):boolean;
begin
  GTK_IS_RULER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_RULER_TYPE);
end;

{$endif read_implementation}


