{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkImage = ^TGtkImage;
     TGtkImage = record
          misc : TGtkMisc;
          image : PGdkImage;
          mask : PGdkBitmap;
       end;

     PGtkImageClass = ^TGtkImageClass;
     TGtkImageClass = record
          parent_class : TGtkMiscClass;
       end;

Type
  GTK_IMAGE=PGtkImage;
  GTK_IMAGE_CLASS=PGtkImageClass;

function  GTK_IMAGE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_image_get_type';
function  GTK_IS_IMAGE(obj:pointer):boolean;
function  GTK_IS_IMAGE_CLASS(klass:pointer):boolean;

function  gtk_image_get_type:TGtkType;cdecl;external gtkdll name 'gtk_image_get_type';
function  gtk_image_new(val:PGdkImage; mask:PGdkBitmap):PGtkWidget;cdecl;external gtkdll name 'gtk_image_new';
procedure gtk_image_set(image:PGtkImage; val:PGdkImage; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_image_set';
procedure gtk_image_get(image:PGtkImage; val:PPGdkImage; mask:PPGdkBitmap);cdecl;external gtkdll name 'gtk_image_get';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_IMAGE(obj:pointer):boolean;
begin
  GTK_IS_IMAGE:=(obj<>nil) and GTK_IS_IMAGE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_IMAGE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_IMAGE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_IMAGE_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:31  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:46  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:37  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 17:40:25  peter
    * more updates

  Revision 1.4  1998/11/09 10:10:03  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:41  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

