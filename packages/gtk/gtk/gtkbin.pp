{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkBin = ^TGtkBin;
       TGtkBin = record
            container : TGtkContainer;
            child : PGtkWidget;
         end;

       PGtkBinClass = ^TGtkBinClass;
       TGtkBinClass = record
            parent_class : TGtkContainerClass;
         end;

Type
  GTK_BIN=PGtkBin;
  GTK_BIN_CLASS=PGtkBinClass;

function  GTK_BIN_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_bin_get_type';
function  GTK_IS_BIN(obj:pointer):boolean;
function  GTK_IS_BIN_CLASS(klass:pointer):boolean;

function  gtk_bin_get_type:TGtkType;cdecl;external gtkdll name 'gtk_bin_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_BIN(obj:pointer):boolean;
begin
  GTK_IS_BIN:=(obj<>nil) and GTK_IS_BIN_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BIN_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BIN_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BIN_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.11  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.10  1999/07/23 16:11:58  peter
    * use packrecords C

  Revision 1.9  1999/05/11 00:38:10  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:18:57  peter
    * cdecl fixes

  Revision 1.7  1999/05/07 15:09:54  peter
    * more fixes

  Revision 1.6  1999/05/07 10:40:29  peter
    * first things for 1.2

  Revision 1.5  1998/11/09 10:09:35  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/22 11:37:40  peter
    * fixes for win32

  Revision 1.3  1998/10/21 20:22:10  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


