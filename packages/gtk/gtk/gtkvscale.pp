{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkVScale = ^TGtkVScale;
     TGtkVScale = record
          scale : TGtkScale;
       end;

     PGtkVScaleClass = ^TGtkVScaleClass;
     TGtkVScaleClass = record
          parent_class : TGtkScaleClass;
       end;

Type
  GTK_VSCALE=PGtkVScale;
  GTK_VSCALE_CLASS=PGtkVScaleClass;

function  GTK_VSCALE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vscale_get_type';
function  GTK_IS_VSCALE(obj:pointer):boolean;
function  GTK_IS_VSCALE_CLASS(klass:pointer):boolean;

function  gtk_vscale_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vscale_get_type';
function  gtk_vscale_new(adjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_vscale_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VSCALE(obj:pointer):boolean;
begin
  GTK_IS_VSCALE:=(obj<>nil) and GTK_IS_VSCALE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VSCALE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VSCALE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VSCALE_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:08  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:51  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:13:26  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:47  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:47  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:10:23  peter
    * more fixes

  Revision 1.4  1998/11/09 10:10:48  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:29  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

