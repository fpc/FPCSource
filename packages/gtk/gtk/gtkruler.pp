{
   $Id$
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


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:13:01  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:18  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:16  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:48  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:26  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:06  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

