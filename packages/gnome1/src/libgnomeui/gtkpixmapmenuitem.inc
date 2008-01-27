{$IfDef read_interface}

type
   PGtkPixmapMenuItem = ^TGtkPixmapMenuItem;
   TGtkPixmapMenuItem = record
        menu_item : TGtkMenuItem;
        pixmap : PGtkWidget;
     end;
   GTK_PIXMAP_MENU_ITEM = PGtkPixmapMenuItem;

   PGtkPixmapMenuItemClass = ^TGtkPixmapMenuItemClass;
   TGtkPixmapMenuItemClass = record
        parent_class : TGtkMenuItemClass;
     end;
   GTK_PIXMAP_MENU_ITEM_CLASS = PGtkPixmapMenuItemClass;

function GNOME_TYPE_PIXMAP_MENU_ITEM : TGTKType;
function GNOME_IS_PIXMAP_MENU_ITEM(obj : Pointer) : gboolean;
function GNOME_IS_PIXMAP_MENU_ITEM_CLASS(klass : Pointer) : gboolean;

function gtk_pixmap_menu_item_get_type:TGtkType;cdecl;external libgnomeuidll name 'gtk_pixmap_menu_item_get_type';
function gtk_pixmap_menu_item_new:PGtkWidget;cdecl;external libgnomeuidll name 'gtk_pixmap_menu_item_new';
procedure gtk_pixmap_menu_item_set_pixmap(menu_item:PGtkPixmapMenuItem; pixmap:PGtkWidget);cdecl;external libgnomeuidll name 'gtk_pixmap_menu_item_set_pixmap';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_PIXMAP_MENU_ITEM : TGTKType;
begin
  GNOME_TYPE_PIXMAP_MENU_ITEM:=gnome_dialog_get_type;
end;

function GNOME_IS_PIXMAP_MENU_ITEM(obj : Pointer) : gboolean;
begin
   GNOME_IS_PIXMAP_MENU_ITEM:=(obj<>nil) and GNOME_IS_PIXMAP_MENU_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_PIXMAP_MENU_ITEM_CLASS(klass : Pointer) : gboolean;
begin
   GNOME_IS_PIXMAP_MENU_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_PIXMAP_MENU_ITEM);
end;

{$Endif read_implementation}
