{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkPixmap = ^TGtkPixmap;
     TGtkPixmap = record
          misc : TGtkMisc;
          pixmap : PGdkPixmap;
          mask : PGdkBitmap;
          pixmap_insensitive : PGdkPixmap;
          flag0 : word;
       end;
  const
     bm_TGtkPixmap_build_insensitive = $1;
     bp_TGtkPixmap_build_insensitive = 0;
function  build_insensitive(var a : TGtkPixmap) : guint;
procedure set_build_insensitive(var a : TGtkPixmap; __build_insensitive : guint);

  type
     PGtkPixmapClass = ^TGtkPixmapClass;
     TGtkPixmapClass = record
          parent_class : TGtkMiscClass;
       end;

Type
  GTK_PIXMAP=PGtkPixmap;
  GTK_PIXMAP_CLASS=PGtkPixmapClass;

function  GTK_PIXMAP_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_pixmap_get_type';
function  GTK_IS_PIXMAP(obj:pointer):boolean;
function  GTK_IS_PIXMAP_CLASS(klass:pointer):boolean;

function  gtk_pixmap_get_type:TGtkType;cdecl;external gtkdll name 'gtk_pixmap_get_type';
function  gtk_pixmap_new(pixmap:PGdkPixmap; mask:PGdkBitmap):PGtkWidget;cdecl;external gtkdll name 'gtk_pixmap_new';
procedure gtk_pixmap_set(pixmap:PGtkPixmap; val:PGdkPixmap; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_pixmap_set';
procedure gtk_pixmap_get(pixmap:PGtkPixmap; val:PPGdkPixmap; mask:PPGdkBitmap);cdecl;external gtkdll name 'gtk_pixmap_get';
procedure gtk_pixmap_set_build_insensitive(pixmap:PGtkPixmap; build:guint);cdecl;external gtkdll name 'gtk_pixmap_set_build_insensitive';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  build_insensitive(var a : TGtkPixmap) : guint;
    begin
       build_insensitive:=(a.flag0 and bm_TGtkPixmap_build_insensitive) shr bp_TGtkPixmap_build_insensitive;
    end;

procedure set_build_insensitive(var a : TGtkPixmap; __build_insensitive : guint);
    begin
       a.flag0:=a.flag0 or ((__build_insensitive shl bp_TGtkPixmap_build_insensitive) and bm_TGtkPixmap_build_insensitive);
    end;

function  GTK_IS_PIXMAP(obj:pointer):boolean;
begin
  GTK_IS_PIXMAP:=(obj<>nil) and GTK_IS_PIXMAP_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_PIXMAP_CLASS(klass:pointer):boolean;
begin
  GTK_IS_PIXMAP_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_PIXMAP_TYPE);
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

  Revision 1.8  1999/07/23 16:12:52  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:09  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:02  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:37  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:20  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:59  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

