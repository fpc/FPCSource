{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHSeparator = ^TGtkHSeparator;
     TGtkHSeparator = record
          separator : TGtkSeparator;
       end;

     PGtkHSeparatorClass = ^TGtkHSeparatorClass;
     TGtkHSeparatorClass = record
          parent_class : TGtkSeparatorClass;
       end;

Type
  GTK_HSEPARATOR=PGtkHSeparator;
  GTK_HSEPARATOR_CLASS=PGtkHSeparatorClass;

function  GTK_HSEPARATOR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hseparator_get_type';
function  GTK_IS_HSEPARATOR(obj:pointer):boolean;
function  GTK_IS_HSEPARATOR_CLASS(klass:pointer):boolean;

function  gtk_hseparator_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hseparator_get_type';
function  gtk_hseparator_new:PGtkWidget;cdecl;external gtkdll name 'gtk_hseparator_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HSEPARATOR(obj:pointer):boolean;
begin
  GTK_IS_HSEPARATOR:=(obj<>nil) and GTK_IS_HSEPARATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HSEPARATOR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HSEPARATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HSEPARATOR_TYPE);
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

  Revision 1.8  1999/07/23 16:12:30  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:45  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:36  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:10:08  peter
    * more fixes

  Revision 1.4  1998/11/09 10:10:02  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:40  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

