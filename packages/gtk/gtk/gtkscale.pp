{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkScale = ^TGtkScale;
     TGtkScale = record
          range : TGtkRange;
          flag0 : longint;
       end;

  const
     bm_TGtkScale_draw_value = $1;
     bp_TGtkScale_draw_value = 0;
     bm_TGtkScale_value_pos = $6;
     bp_TGtkScale_value_pos = 1;
function  draw_value(var a : TGtkScale) : guint;
procedure set_draw_value(var a : TGtkScale; __draw_value : guint);
function  value_pos(var a : TGtkScale) : guint;
procedure set_value_pos(var a : TGtkScale; __value_pos : guint);

  type
     PGtkScaleClass = ^TGtkScaleClass;
     TGtkScaleClass = record
          parent_class : TGtkRangeClass;
          slider_length : gint;
          value_spacing : gint;
          draw_value : procedure (scale:PGtkScale); cdecl;
       end;

Type
  GTK_SCALE=PGtkScale;
  GTK_SCALE_CLASS=PGtkScaleClass;

function  GTK_SCALE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_scale_get_type';
function  GTK_IS_SCALE(obj:pointer):boolean;
function  GTK_IS_SCALE_CLASS(klass:pointer):boolean;

function  gtk_scale_get_type:TGtkType;cdecl;external gtkdll name 'gtk_scale_get_type';
procedure gtk_scale_set_digits(scale:PGtkScale; digits:gint);cdecl;external gtkdll name 'gtk_scale_set_digits';
procedure gtk_scale_set_draw_value(scale:PGtkScale; draw_value:gboolean);cdecl;external gtkdll name 'gtk_scale_set_draw_value';
procedure gtk_scale_set_value_pos(scale:PGtkScale; pos:TGtkPositionType);cdecl;external gtkdll name 'gtk_scale_set_value_pos';
{$ifndef gtkwin}
function  gtk_scale_value_width(scale:PGtkScale):gint;cdecl;external gtkdll name 'gtk_scale_value_width';
procedure gtk_scale_draw_value(scale:PGtkScale);cdecl;external gtkdll name 'gtk_scale_draw_value';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  draw_value(var a : TGtkScale) : guint;
    begin
       draw_value:=(a.flag0 and bm_TGtkScale_draw_value) shr bp_TGtkScale_draw_value;
    end;

procedure set_draw_value(var a : TGtkScale; __draw_value : guint);
    begin
       a.flag0:=a.flag0 or ((__draw_value shl bp_TGtkScale_draw_value) and bm_TGtkScale_draw_value);
    end;

function  value_pos(var a : TGtkScale) : guint;
    begin
       value_pos:=(a.flag0 and bm_TGtkScale_value_pos) shr bp_TGtkScale_value_pos;
    end;

procedure set_value_pos(var a : TGtkScale; __value_pos : guint);
    begin
       a.flag0:=a.flag0 or ((__value_pos shl bp_TGtkScale_value_pos) and bm_TGtkScale_value_pos);
    end;

function  GTK_IS_SCALE(obj:pointer):boolean;
begin
  GTK_IS_SCALE:=(obj<>nil) and GTK_IS_SCALE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_SCALE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_SCALE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_SCALE_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.10  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/10/05 09:28:27  peter
    * patches from Frank Loemker

  Revision 1.8  1999/07/23 16:13:02  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:19  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:17  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:49  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:27  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:07  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

