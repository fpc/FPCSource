{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHRuler = ^TGtkHRuler;
     TGtkHRuler = record
          ruler : TGtkRuler;
       end;

     PGtkHRulerClass = ^TGtkHRulerClass;
     TGtkHRulerClass = record
          parent_class : TGtkRulerClass;
       end;

Type
  GTK_HRULER=PGtkHRuler;
  GTK_HRULER_CLASS=PGtkHRulerClass;

function  GTK_HRULER_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hruler_get_type';
function  GTK_IS_HRULER(obj:pointer):boolean;
function  GTK_IS_HRULER_CLASS(klass:pointer):boolean;

function  gtk_hruler_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hruler_get_type';
function  gtk_hruler_new:PGtkWidget;cdecl;external gtkdll name 'gtk_hruler_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HRULER(obj:pointer):boolean;
begin
  GTK_IS_HRULER:=(obj<>nil) and GTK_IS_HRULER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HRULER_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HRULER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HRULER_TYPE);
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

  Revision 1.8  1999/07/23 16:12:27  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:42  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:33  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:10:05  peter
    * more fixes

  Revision 1.4  1998/11/09 10:09:59  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:37  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

