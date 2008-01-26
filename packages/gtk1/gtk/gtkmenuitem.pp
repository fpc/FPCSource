{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkMenuItem = ^TGtkMenuItem;
       TGtkMenuItem = record
            item : TGtkItem;
            submenu : PGtkWidget;
            accelerator_signal : guint;
            toggle_size : guint16;
            accelerator_width : guint16;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
            timer : guint;
         end;

    const
       bm_show_toggle_indicator = 1;
       bp_show_toggle_indicator = 0;
       bm_show_submenu_indicator = 2;
       bp_show_submenu_indicator = 1;
       bm_submenu_placement = 4;
       bp_submenu_placement = 2;
       bm_submenu_direction = 8;
       bp_submenu_direction = 3;
       bm_right_justify = 16;
       bp_right_justify = 4;
function  show_toggle_indicator(var a : TGtkMenuItem) : guint;
procedure set_show_toggle_indicator(var a : TGtkMenuItem; __show_toggle_indicator : guint);
function  show_submenu_indicator(var a : TGtkMenuItem) : guint;
procedure set_show_submenu_indicator(var a : TGtkMenuItem; __show_submenu_indicator : guint);
function  submenu_placement(var a : TGtkMenuItem) : guint;
procedure set_submenu_placement(var a : TGtkMenuItem; __submenu_placement : guint);
function  submenu_direction(var a : TGtkMenuItem) : guint;
procedure set_submenu_direction(var a : TGtkMenuItem; __submenu_direction : guint);
function  right_justify(var a : TGtkMenuItem) : guint;
procedure set_right_justify(var a : TGtkMenuItem; __right_justify : guint);

    type
       PGtkMenuItemClass = ^TGtkMenuItemClass;
       TGtkMenuItemClass = record
          parent_class : TGtkItemClass;
          toggle_size : guint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          activate : procedure (menu_item:PGtkMenuItem);cdecl;
          activate_item : procedure (menu_item:PGtkMenuItem);cdecl;
        end;

Type
  GTK_MENU_ITEM=PGtkMenuItem;
  GTK_MENU_ITEM_CLASS=PGtkMenuItemClass;

function  GTK_MENU_ITEM_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_menu_item_get_type';
function  GTK_IS_MENU_ITEM(obj:pointer):boolean;
function  GTK_IS_MENU_ITEM_CLASS(klass:pointer):boolean;

function  gtk_menu_item_get_type:TGtkType;cdecl;external gtkdll name 'gtk_menu_item_get_type';
function  gtk_menu_item_new : PGtkWidget;cdecl;external gtkdll name 'gtk_menu_item_new';
function  gtk_menu_item_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_menu_item_new_with_label';
procedure gtk_menu_item_set_submenu(menu_item:PGtkMenuItem; submenu:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_item_set_submenu';
procedure gtk_menu_item_remove_submenu(menu_item:PGtkMenuItem);cdecl;external gtkdll name 'gtk_menu_item_remove_submenu';
procedure gtk_menu_item_set_placement(menu_item:PGtkMenuItem; placement:TGtkSubmenuPlacement);cdecl;external gtkdll name 'gtk_menu_item_set_placement';
procedure gtk_menu_item_configure(menu_item:PGtkMenuItem; show_toggle_indicator:gint; show_submenu_indicator:gint);cdecl;external gtkdll name 'gtk_menu_item_configure';
procedure gtk_menu_item_select(menu_item:PGtkMenuItem);cdecl;external gtkdll name 'gtk_menu_item_select';
procedure gtk_menu_item_deselect(menu_item:PGtkMenuItem);cdecl;external gtkdll name 'gtk_menu_item_deselect';
procedure gtk_menu_item_activate(menu_item:PGtkMenuItem);cdecl;external gtkdll name 'gtk_menu_item_activate';
procedure gtk_menu_item_right_justify(menu_item:PGtkMenuItem);cdecl;external gtkdll name 'gtk_menu_item_right_justify';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  show_toggle_indicator(var a : TGtkMenuItem) : guint;
      begin
         show_toggle_indicator:=(a.flag0 and bm_show_toggle_indicator) shr bp_show_toggle_indicator;
      end;

procedure set_show_toggle_indicator(var a : TGtkMenuItem; __show_toggle_indicator : guint);
      begin
         a.flag0:=a.flag0 or ((__show_toggle_indicator shl bp_show_toggle_indicator) and bm_show_toggle_indicator);
      end;

function  show_submenu_indicator(var a : TGtkMenuItem) : guint;
      begin
         show_submenu_indicator:=(a.flag0 and bm_show_submenu_indicator) shr bp_show_submenu_indicator;
      end;

procedure set_show_submenu_indicator(var a : TGtkMenuItem; __show_submenu_indicator : guint);
      begin
         a.flag0:=a.flag0 or ((__show_submenu_indicator shl bp_show_submenu_indicator) and bm_show_submenu_indicator);
      end;

function  submenu_placement(var a : TGtkMenuItem) : guint;
      begin
         submenu_placement:=(a.flag0 and bm_submenu_placement) shr bp_submenu_placement;
      end;

procedure set_submenu_placement(var a : TGtkMenuItem; __submenu_placement : guint);
      begin
         a.flag0:=a.flag0 or ((__submenu_placement shl bp_submenu_placement) and bm_submenu_placement);
      end;

function  submenu_direction(var a : TGtkMenuItem) : guint;
      begin
         submenu_direction:=(a.flag0 and bm_submenu_direction) shr bp_submenu_direction;
      end;

procedure set_submenu_direction(var a : TGtkMenuItem; __submenu_direction : guint);
      begin
         a.flag0:=a.flag0 or ((__submenu_direction shl bp_submenu_direction) and bm_submenu_direction);
      end;

function  right_justify(var a : TGtkMenuItem) : guint;
      begin
         right_justify:=(a.flag0 and bm_right_justify) shr bp_right_justify;
      end;

procedure set_right_justify(var a : TGtkMenuItem; __right_justify : guint);
      begin
         a.flag0:=a.flag0 or ((__right_justify shl bp_right_justify) and bm_right_justify);
      end;

function  GTK_IS_MENU_ITEM(obj:pointer):boolean;
begin
  GTK_IS_MENU_ITEM:=(obj<>nil) and GTK_IS_MENU_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_MENU_ITEM_CLASS(klass:pointer):boolean;
begin
  GTK_IS_MENU_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_MENU_ITEM_TYPE);
end;

{$endif read_implementation}


