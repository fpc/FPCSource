{
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


