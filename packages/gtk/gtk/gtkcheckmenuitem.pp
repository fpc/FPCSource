{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkCheckMenuItem = ^TGtkCheckMenuItem;
       TGtkCheckMenuItem = record
            menu_item : TGtkMenuItem;
            flag0 : longint;
         end;

    const
       bm_checkmenuitem_active = 1;
       bp_checkmenuitem_active = 0;
       bm_checkmenuitem_always_show_toggle = 2;
       bp_checkmenuitem_always_show_toggle = 1;
function  active(var a : TGtkCheckMenuItem) : guint;
procedure set_active(var a : TGtkCheckMenuItem; __active : guint);
function  always_show_toggle(var a : TGtkCheckMenuItem) : guint;
procedure set_always_show_toggle(var a : TGtkCheckMenuItem; __always_show_toggle : guint);

    type
       PGtkCheckMenuItemClass = ^TGtkCheckMenuItemClass;
       TGtkCheckMenuItemClass = record
            parent_class : TGtkMenuItemClass;
            toggled : procedure (check_menu_item:PGtkCheckMenuItem); cdecl;
            draw_indicator : procedure (check_menu_item:PGtkCheckMenuItem; area:PGdkRectangle); cdecl;
         end;

Type
  GTK_CHECK_MENU_ITEM=PGtkCheckMenuItem;
  GTK_CHECK_MENU_ITEM_CLASS=PGtkCheckMenuItemClass;

function  GTK_CHECK_MENU_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_check_menu_item_get_type';
function  GTK_IS_CHECK_MENU_ITEM(obj:pointer):boolean;
function  GTK_IS_CHECK_MENU_ITEM_CLASS(klass:pointer):boolean;

function  gtk_check_menu_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_check_menu_item_get_type';
function  gtk_check_menu_item_new : PGtkWidget;cdecl;external gtkdll name 'gtk_check_menu_item_new'; 
function  gtk_check_menu_item_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_check_menu_item_new_with_label'; 
procedure gtk_check_menu_item_set_active(check_menu_item:PGtkCheckMenuItem; is_active:gboolean);cdecl;external gtkdll name 'gtk_check_menu_item_set_active';
procedure gtk_check_menu_item_set_show_toggle(menu_item:PGtkCheckMenuItem; always:gboolean);cdecl;external gtkdll name 'gtk_check_menu_item_set_show_toggle';
procedure gtk_check_menu_item_toggled(check_menu_item:PGtkCheckMenuItem);cdecl;external gtkdll name 'gtk_check_menu_item_toggled';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  active(var a : TGtkCheckMenuItem) : guint;
      begin
         active:=(a.flag0 and bm_checkmenuitem_active) shr bp_checkmenuitem_active;
      end;

procedure set_active(var a : TGtkCheckMenuItem; __active : guint);
      begin
         a.flag0:=a.flag0 or ((__active shl bp_checkmenuitem_active) and bm_checkmenuitem_active);
      end;

function  always_show_toggle(var a : TGtkCheckMenuItem) : guint;
      begin
         always_show_toggle:=(a.flag0 and bm_checkmenuitem_always_show_toggle) shr bp_checkmenuitem_always_show_toggle;
      end;

procedure set_always_show_toggle(var a : TGtkCheckMenuItem; __always_show_toggle : guint);
      begin
         a.flag0:=a.flag0 or ((__always_show_toggle shl bp_checkmenuitem_always_show_toggle) and bm_checkmenuitem_always_show_toggle);
      end;

function  GTK_IS_CHECK_MENU_ITEM(obj:pointer):boolean;
begin
  GTK_IS_CHECK_MENU_ITEM:=(obj<>nil) and GTK_IS_CHECK_MENU_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CHECK_MENU_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CHECK_MENU_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CHECK_MENU_ITEM_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.2  2000-01-24 18:35:38  sg
  * added gtk_check_menu_item_new and ~_new_with_label

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.12  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.11  1999/10/05 09:28:27  peter
    * patches from Frank Loemker

  Revision 1.10  1999/07/23 16:12:04  peter
    * use packrecords C

  Revision 1.9  1999/05/11 00:38:16  peter
    * win32 fixes

  Revision 1.8  1999/05/10 15:19:03  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:03:01  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 15:09:56  peter
    * more fixes

  Revision 1.5  1999/05/07 10:40:33  peter
    * first things for 1.2

  Revision 1.4  1998/11/09 10:09:39  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:14  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

