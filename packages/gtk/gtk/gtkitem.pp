{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkItem = ^TGtkItem;
       TGtkItem = record
            bin : TGtkBin;
         end;

       PGtkItemClass = ^TGtkItemClass;
       TGtkItemClass = record
            parent_class : TGtkBinClass;
            select : procedure (item:PGtkItem); cdecl;
            deselect : procedure (item:PGtkItem); cdecl;
            toggle : procedure (item:PGtkItem); cdecl;
         end;

Type
  GTK_ITEM=PGtkItem;
  GTK_ITEM_CLASS=PGtkItemClass;

function  GTK_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_item_get_type';
function  GTK_IS_ITEM(obj:pointer):boolean;
function  GTK_IS_ITEM_CLASS(klass:pointer):boolean;

function  gtk_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_item_get_type';
procedure gtk_item_select(item:PGtkItem);cdecl;external gtkdll name 'gtk_item_select';
procedure gtk_item_deselect(item:PGtkItem);cdecl;external gtkdll name 'gtk_item_deselect';
procedure gtk_item_toggle(item:PGtkItem);cdecl;external gtkdll name 'gtk_item_toggle';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ITEM(obj:pointer):boolean;
begin
  GTK_IS_ITEM:=(obj<>nil) and GTK_IS_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ITEM_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:34  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:49  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:40  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 17:40:26  peter
    * more updates

  Revision 1.4  1998/11/09 10:10:05  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:43  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

