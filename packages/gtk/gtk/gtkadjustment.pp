{
   $Id$
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

{
  $Log$
  Revision 1.1  2000-07-13 06:34:02  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.11  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.10  1999/07/23 16:11:51  peter
    * use packrecords C

  Revision 1.9  1999/05/11 00:38:03  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:18:50  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:02:54  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 15:09:50  peter
    * more fixes

  Revision 1.5  1999/05/07 10:40:23  peter
    * first things for 1.2

  Revision 1.4  1998/10/21 22:25:15  peter
    * fixed some wrong cdecls

  Revision 1.3  1998/10/21 20:22:05  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

