// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TGtkPrintFunc = procedure (func_data:gpointer; str:Pgchar); cdecl;

   PGtkTranslateFunc = ^TGtkTranslateFunc;
   TGtkTranslateFunc = gchar;

   TGtkItemFactoryCallback = procedure ; cdecl;

   TGtkItemFactoryCallback1 = procedure (callback_data:gpointer; callback_action:guint; widget:PGtkWidget); cdecl;

   PGtkItemFactory = ^TGtkItemFactory;
   TGtkItemFactory = record
        anObject : TGtkObject;
        path : Pgchar;
        accel_group : PGtkAccelGroup;
        widget : PGtkWidget;
        items : PGSList;
        translate_func : TGtkTranslateFunc;
        translate_data : gpointer;
        translate_notify : TGtkDestroyNotify;
     end;

{ Padding for future expansion  }
   PGtkItemFactoryClass = ^TGtkItemFactoryClass;
   TGtkItemFactoryClass = record
        object_class : TGtkObjectClass;
        item_ht : PGHashTable;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{ possible values:
     NULL               -> "<Item>"
     ""                 -> "<Item>"
     "<Title>"          -> create a title item
     "<Item>"           -> create a simple item
     "<ImageItem>"      -> create an item holding an image
     "<StockItem>"      -> create an item holding a stock image
     "<CheckItem>"      -> create a check item
     "<ToggleItem>"     -> create a toggle item
     "<RadioItem>"      -> create a radio item
     <path>             -> path of a radio item to link against
     "<Separator>"      -> create a separator
     "<Tearoff>"        -> create a tearoff separator
     "<Branch>"         -> create an item to hold sub items
     "<LastBranch>"     -> create a right justified item to hold sub items
    }
{ Extra data for some item types:
      ImageItem  -> pointer to inlined pixbuf stream
      StockItem  -> name of stock item
    }
   PGtkItemFactoryEntry = ^TGtkItemFactoryEntry;
   TGtkItemFactoryEntry = record
        path : Pgchar;
        accelerator : Pgchar;
        callback : TGtkItemFactoryCallback;
        callback_action : guint;
        item_type : Pgchar;
        extra_data : gconstpointer;
     end;

   PGtkItemFactoryItem = ^TGtkItemFactoryItem;
   TGtkItemFactoryItem = record
        path : Pgchar;
        widgets : PGSList;
     end;

{$ifndef GTK_DISABLE_DEPRECATED}
{ Compatibility functions for deprecated GtkMenuFactory code }
   TGtkMenuCallback = procedure (widget:PGtkWidget; user_data:gpointer); cdecl;

   PGtkMenuEntry = ^TGtkMenuEntry;
   TGtkMenuEntry = record
        path : Pgchar;
        accelerator : Pgchar;
        callback : TGtkMenuCallback;
        callback_data : gpointer;
        widget : PGtkWidget;
     end;

   TGtkItemFactoryCallback2 = procedure (widget:PGtkWidget; callback_data:gpointer; callback_action:guint); cdecl;
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ITEM_FACTORY : GType;
function GTK_ITEM_FACTORY(anObject: pointer) : PGtkItemFactory;
function GTK_ITEM_FACTORY_CLASS(klass: pointer) : PGtkItemFactoryClass;
function GTK_IS_ITEM_FACTORY(anObject: pointer) : boolean;
function GTK_IS_ITEM_FACTORY_CLASS(klass: pointer) : boolean;
function GTK_ITEM_FACTORY_GET_CLASS(obj: pointer) : PGtkItemFactoryClass;


function gtk_item_factory_get_type:TGtkType; cdecl; external gtklib;
{ `container_type' must be of GTK_TYPE_MENU_BAR, GTK_TYPE_MENU,
   or GTK_TYPE_OPTION_MENU.
  }
function gtk_item_factory_new(container_type:TGtkType; path:Pgchar; accel_group:PGtkAccelGroup):PGtkItemFactory; cdecl; external gtklib;
procedure gtk_item_factory_construct(ifactory:PGtkItemFactory; container_type:TGtkType; path:Pgchar; accel_group:PGtkAccelGroup); cdecl; external gtklib;
{ These functions operate on GtkItemFactoryClass basis.
  }
procedure gtk_item_factory_add_foreign(accel_widget:PGtkWidget; full_path:Pgchar; accel_group:PGtkAccelGroup; keyval:guint; modifiers:TGdkModifierType); cdecl; external gtklib;
function gtk_item_factory_from_widget(widget:PGtkWidget):PGtkItemFactory; cdecl; external gtklib;
function gtk_item_factory_path_from_widget(widget:PGtkWidget):Pgchar; cdecl; external gtklib;
function gtk_item_factory_get_item(ifactory:PGtkItemFactory; path:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_item_factory_get_widget(ifactory:PGtkItemFactory; path:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_item_factory_get_widget_by_action(ifactory:PGtkItemFactory; action:guint):PGtkWidget; cdecl; external gtklib;
function gtk_item_factory_get_item_by_action(ifactory:PGtkItemFactory; action:guint):PGtkWidget; cdecl; external gtklib;
procedure gtk_item_factory_create_item(ifactory:PGtkItemFactory; entry:PGtkItemFactoryEntry; callback_data:gpointer; callback_type:guint); cdecl; external gtklib;
procedure gtk_item_factory_create_items(ifactory:PGtkItemFactory; n_entries:guint; entries:PGtkItemFactoryEntry; callback_data:gpointer); cdecl; external gtklib;
procedure gtk_item_factory_delete_item(ifactory:PGtkItemFactory; path:Pgchar); cdecl; external gtklib;
procedure gtk_item_factory_delete_entry(ifactory:PGtkItemFactory; entry:PGtkItemFactoryEntry); cdecl; external gtklib;
procedure gtk_item_factory_delete_entries(ifactory:PGtkItemFactory; n_entries:guint; entries:PGtkItemFactoryEntry); cdecl; external gtklib;
procedure gtk_item_factory_popup(ifactory:PGtkItemFactory; x:guint; y:guint; mouse_button:guint; time:guint32); cdecl; external gtklib;
procedure gtk_item_factory_popup_with_data(ifactory:PGtkItemFactory; popup_data:gpointer; destroy:TGtkDestroyNotify; x:guint; y:guint;
            mouse_button:guint; time:guint32); cdecl; external gtklib;
function gtk_item_factory_popup_data(ifactory:PGtkItemFactory):gpointer; cdecl; external gtklib;
function gtk_item_factory_popup_data_from_widget(widget:PGtkWidget):gpointer; cdecl; external gtklib;
procedure gtk_item_factory_set_translate_func(ifactory:PGtkItemFactory; func:TGtkTranslateFunc; data:gpointer; notify:TGtkDestroyNotify); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_item_factory_from_path(path:Pgchar):PGtkItemFactory; cdecl; external gtklib;
procedure gtk_item_factory_create_menu_entries(n_entries:guint; entries:PGtkMenuEntry); cdecl; external gtklib;
procedure gtk_item_factories_path_delete(ifactory_path:Pgchar; path:Pgchar); cdecl; external gtklib;

procedure gtk_item_factory_create_items_ac(ifactory:PGtkItemFactory; n_entries:guint; entries:PGtkItemFactoryEntry; callback_data:gpointer; callback_type:guint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ITEM_FACTORY : GType;
begin
   GTK_TYPE_ITEM_FACTORY:=gtk_item_factory_get_type;
end;

function GTK_ITEM_FACTORY(anObject: pointer) : PGtkItemFactory;
begin
   GTK_ITEM_FACTORY:=PGtkItemFactory(GTK_CHECK_CAST(anObject,GTK_TYPE_ITEM_FACTORY));
end;

function GTK_ITEM_FACTORY_CLASS(klass: pointer) : PGtkItemFactoryClass;
begin
   GTK_ITEM_FACTORY_CLASS:=PGtkItemFactoryClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ITEM_FACTORY));
end;

function GTK_IS_ITEM_FACTORY(anObject: pointer) : boolean;
begin
   GTK_IS_ITEM_FACTORY:=GTK_CHECK_TYPE(anObject,GTK_TYPE_ITEM_FACTORY);
end;

function GTK_IS_ITEM_FACTORY_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ITEM_FACTORY_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ITEM_FACTORY);
end;

function GTK_ITEM_FACTORY_GET_CLASS(obj: pointer) : PGtkItemFactoryClass;
begin
   GTK_ITEM_FACTORY_GET_CLASS:=PGtkItemFactoryClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ITEM_FACTORY));
end;
{$ENDIF read_implementation}

// included by gtk2.pas

