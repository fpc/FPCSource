{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkVSeparator = ^TGtkVSeparator;
     TGtkVSeparator = record
          separator : TGtkSeparator;
       end;

     PGtkVSeparatorClass = ^TGtkVSeparatorClass;
     TGtkVSeparatorClass = record
          parent_class : TGtkSeparatorClass;
       end;

Type
  GTK_VSEPARATOR=PGtkVSeparator;
  GTK_VSEPARATOR_CLASS=PGtkVSeparatorClass;

function  GTK_VSEPARATOR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vseparator_get_type';
function  GTK_IS_VSEPARATOR(obj:pointer):boolean;
function  GTK_IS_VSEPARATOR_CLASS(klass:pointer):boolean;

function  gtk_vseparator_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vseparator_get_type';
function  gtk_vseparator_new:PGtkWidget;cdecl;external gtkdll name 'gtk_vseparator_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VSEPARATOR(obj:pointer):boolean;
begin
  GTK_IS_VSEPARATOR:=(obj<>nil) and GTK_IS_VSEPARATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VSEPARATOR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VSEPARATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VSEPARATOR_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:08  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.10  1999/10/06 17:42:51  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/07/23 16:13:28  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:39:49  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:20:49  peter
    * cdecl fixes

  Revision 1.6  1999/05/10 09:04:13  peter
    * gtk 1.2 port working

  Revision 1.5  1999/05/07 15:10:25  peter
    * more fixes

  Revision 1.4  1998/11/09 10:10:50  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:32  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

