{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkTearoffMenuItem = ^TGtkTearoffMenuItem;
     TGtkTearoffMenuItem = record
          menu_item : TGtkMenuItem;
          flag0 : longint;
       end;

  const
     bm_TGtkTearoffMenuItem_torn_off = $1;
     bp_TGtkTearoffMenuItem_torn_off = 0;
function  torn_off(var a : TGtkTearoffMenuItem) : guint;
procedure set_torn_off(var a : TGtkTearoffMenuItem; __torn_off : guint);

  type
     PGtkTearoffMenuItemClass = ^TGtkTearoffMenuItemClass;
     TGtkTearoffMenuItemClass = record
          parent_class : TGtkMenuItemClass;
       end;

type
  GTK_TEAROFF_MENU_ITEM=PGtkTearoffMenuItem;
  GTK_TEAROFF_MENU_ITEM_CLASS=PGtkTearoffMenuItemClass;

function  GTK_TEAROFF_MENU_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_tearoff_menu_item_get_type';
function  GTK_IS_TEAROFF_MENU_ITEM(obj:pointer):boolean;
function  GTK_IS_TEAROFF_MENU_ITEM_CLASS(klass:pointer):boolean;

function  gtk_tearoff_menu_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_tearoff_menu_item_get_type';
function  gtk_tearoff_menu_item_new:PGtkWidget;cdecl;external gtkdll name 'gtk_tearoff_menu_item_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}


function  torn_off(var a : TGtkTearoffMenuItem) : guint;
    begin
       torn_off:=(a.flag0 and bm_TGtkTearoffMenuItem_torn_off) shr bp_TGtkTearoffMenuItem_torn_off;
    end;

procedure set_torn_off(var a : TGtkTearoffMenuItem; __torn_off : guint);
    begin
       a.flag0:=a.flag0 or ((__torn_off shl bp_TGtkTearoffMenuItem_torn_off) and bm_TGtkTearoffMenuItem_torn_off);
    end;

function  GTK_IS_TEAROFF_MENU_ITEM(obj:pointer):boolean;
begin
  GTK_IS_TEAROFF_MENU_ITEM:=(obj<>nil) and GTK_IS_TEAROFF_MENU_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_TEAROFF_MENU_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_TEAROFF_MENU_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TEAROFF_MENU_ITEM_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:06  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.6  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.5  1999/10/05 09:28:27  peter
    * patches from Frank Loemker

  Revision 1.4  1999/07/23 16:13:13  peter
    * use packrecords C

  Revision 1.3  1999/05/11 00:39:31  peter
    * win32 fixes

  Revision 1.2  1999/05/10 15:20:30  peter
    * cdecl fixes

  Revision 1.1  1999/05/10 09:14:00  peter
    + new gtk 1.2 files

}

