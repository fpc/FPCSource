{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkData = ^TGtkData;
       TGtkData = record
            theobject : TGtkObject;
         end;

       DisconnectProc  = procedure (data : PGtkdata);cdecl;

       PGtkDataClass = ^TGtkDataClass;
       TGtkDataClass = record
            parent_class : TGtkObjectClass;
            disconnect : disconnectproc;
         end;

Type
  GTK_DATA=PGtkData;
  GTK_DATA_CLASS=PGtkDataClass;

function  GTK_DATA_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_data_get_type';
function  GTK_IS_DATA(obj:pointer):boolean;
function  GTK_IS_DATA_CLASS(klass:pointer):boolean;

function  gtk_data_get_type:TGtkType;cdecl;external gtkdll name 'gtk_data_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_DATA(obj:pointer):boolean;
begin
  GTK_IS_DATA:=(obj<>nil) and GTK_IS_DATA_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_DATA_CLASS(klass:pointer):boolean;
begin
  GTK_IS_DATA_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_DATA_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:12  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:24  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:11  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:09:59  peter
    * more fixes

  Revision 1.4  1998/11/09 10:09:45  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:20  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

