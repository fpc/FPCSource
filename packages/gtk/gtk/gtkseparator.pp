{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkSeparator = ^TGtkSeparator;
     TGtkSeparator = record
          widget : TGtkWidget;
       end;

     PGtkSeparatorClass = ^TGtkSeparatorClass;
     TGtkSeparatorClass = record
          parent_class : TGtkWidgetClass;
       end;

Type
  GTK_SEPARATOR=PGtkSeparator;
  GTK_SEPARATOR_CLASS=PGtkSeparatorClass;

function  GTK_SEPARATOR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_separator_get_type';
function  GTK_IS_SEPARATOR(obj:pointer):boolean;
function  GTK_IS_SEPARATOR_CLASS(klass:pointer):boolean;

function  gtk_separator_get_type:TGtkType;cdecl;external gtkdll name 'gtk_separator_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_SEPARATOR(obj:pointer):boolean;
begin
  GTK_IS_SEPARATOR:=(obj<>nil) and GTK_IS_SEPARATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_SEPARATOR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_SEPARATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_SEPARATOR_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:06  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:13:06  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:23  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:22  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:53  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:30  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:11  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

