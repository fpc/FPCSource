{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkScrollbar = ^TGtkScrollbar;
       TGtkScrollbar = record
            range : TGtkRange;
         end;

       PGtkScrollbarClass = ^TGtkScrollbarClass;
       TGtkScrollbarClass = record
            parent_class : TGtkRangeClass;
         end;

Type
  GTK_SCROLLBAR=PGtkScrollbar;
  GTK_SCROLLBAR_CLASS=PGtkScrollbarClass;

function  GTK_SCROLLBAR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_scrollbar_get_type';
function  GTK_IS_SCROLLBAR(obj:pointer):boolean;
function  GTK_IS_SCROLLBAR_CLASS(klass:pointer):boolean;

function  gtk_scrollbar_get_type:TGtkType;cdecl;external gtkdll name 'gtk_scrollbar_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_SCROLLBAR(obj:pointer):boolean;
begin
  GTK_IS_SCROLLBAR:=(obj<>nil) and GTK_IS_SCROLLBAR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_SCROLLBAR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_SCROLLBAR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_SCROLLBAR_TYPE);
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

  Revision 1.8  1999/07/23 16:13:03  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:20  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:19  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:50  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:28  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:08  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

