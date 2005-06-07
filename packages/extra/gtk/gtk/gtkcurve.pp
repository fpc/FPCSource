{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     Tgfloatarray = Array[0..1] of gfloat;
     PgfloatArray = ^TgfloatArray;

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
          ctlpoint : PgfloatArray;
       end;

     PGtkCurveClass = ^TGtkCurveClass;
     TGtkCurveClass = record
          parent_class : TGtkDrawingAreaClass;
          curve_type_changed : procedure (curve:PGtkCurve); cdecl;
       end;

Type
  GTK_CURVE=PGtkCurve;
  GTK_CURVE_CLASS=PGtkCurveClass;

function  GTK_CURVE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_curve_get_type';
function  GTK_IS_CURVE(obj:pointer):boolean;
function  GTK_IS_CURVE_CLASS(klass:pointer):boolean;

function  gtk_curve_get_type:TGtkType;cdecl;external gtkdll name 'gtk_curve_get_type';
function  gtk_curve_new : PGtkWidget;cdecl;external gtkdll name 'gtk_curve_new';
procedure gtk_curve_reset(curve:PGtkCurve);cdecl;external gtkdll name 'gtk_curve_reset';
procedure gtk_curve_set_gamma(curve:PGtkCurve; gamma:gfloat);cdecl;external gtkdll name 'gtk_curve_set_gamma';
procedure gtk_curve_set_range(curve:PGtkCurve; min_x:gfloat; max_x:gfloat; min_y:gfloat; max_y:gfloat);cdecl;external gtkdll name 'gtk_curve_set_range';
procedure gtk_curve_get_vector(curve:PGtkCurve; veclen:longint; vector:Pgfloat);cdecl;external gtkdll name 'gtk_curve_get_vector';
procedure gtk_curve_set_vector(curve:PGtkCurve; veclen:longint; vector:Pgfloat);cdecl;external gtkdll name 'gtk_curve_set_vector';
procedure gtk_curve_set_curve_type(curve:PGtkCurve; thetype:TGtkCurveType);cdecl;external gtkdll name 'gtk_curve_set_curve_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_CURVE(obj:pointer):boolean;
begin
  GTK_IS_CURVE:=(obj<>nil) and GTK_IS_CURVE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CURVE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CURVE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CURVE_TYPE);
end;

{$endif read_implementation}


