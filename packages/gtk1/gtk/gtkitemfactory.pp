{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkPrintFunc = procedure (func_data:gpointer; str:Pgchar);cdecl;

     TGtkTranslateFunc = function (path:Pgchar; func_data:gpointer):Pgchar;cdecl;

     TGtkItemFactoryCallback = procedure ;cdecl;

     TGtkItemFactoryCallback1 = procedure (callback_data:gpointer; callback_action:guint; widget:PGtkWidget);cdecl;

     TGtkItemFactoryCallback2 = procedure (widget:PGtkWidget; callback_data:gpointer; callback_action:guint);cdecl;

     PGtkItemFactory = ^TGtkItemFactory;
     TGtkItemFactory = record
          theobject : TGtkObject;
          path : Pgchar;
          accel_group : PGtkAccelGroup;
          widget : PGtkWidget;
          items : PGSList;
          translate_func : TGtkTranslateFunc;
          translate_data : gpointer;
          translate_notify : TGtkDestroyNotify;
       end;

     PGtkItemFactoryClass = ^TGtkItemFactoryClass;
     TGtkItemFactoryClass = record
          object_class : TGtkObjectClass;
          cpair_comment_single : Pgchar;
          item_ht : PGHashTable;
          dummy : gpointer;
       end;

     PGtkItemFactoryEntry = ^TGtkItemFactoryEntry;
     TGtkItemFactoryEntry = record
          path : Pgchar;
          accelerator : Pgchar;
          callback : TGtkItemFactoryCallback;
          callback_action : guint;
          item_type : Pgchar;
       end;

     PGtkItemFactoryItem = ^TGtkItemFactoryItem;
     TGtkItemFactoryItem = record
          path : Pgchar;
          accelerator_key : guint;
          accelerator_mods : guint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          dummy : Pgchar;
          widgets : PGSList;
       end;

  const
     bm_TGtkItemFactoryItem_modified = $1;
     bp_TGtkItemFactoryItem_modified = 0;
     bm_TGtkItemFactoryItem_in_propagation = $2;
     bp_TGtkItemFactoryItem_in_propagation = 1;
function  modified(var a : TGtkItemFactoryItem) : guint;
procedure set_modified(var a : TGtkItemFactoryItem; __modified : guint);
function  in_propagation(var a : TGtkItemFactoryItem) : guint;
procedure set_in_propagation(var a : TGtkItemFactoryItem; __in_propagation : guint);

type
  GTK_ITEM_FACTORY=TGtkItemFactory;
  GTK_ITEM_FACTORY_CLASS=TGtkItemFactoryClass;

function  GTK_ITEM_FACTORY_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_item_factory_get_type';
function  GTK_IS_ITEM_FACTORY(obj:pointer):boolean;
function  GTK_IS_ITEM_FACTORY_CLASS(klass:pointer):boolean;

function  gtk_item_factory_get_type:TGtkType;cdecl;external gtkdll name 'gtk_item_factory_get_type';
function  gtk_item_factory_new(container_type:TGtkType; path:Pgchar; accel_group:PGtkAccelGroup):PGtkItemFactory;cdecl;external gtkdll name 'gtk_item_factory_new';
procedure gtk_item_factory_construct(ifactory:PGtkItemFactory; container_type:TGtkType; path:Pgchar; accel_group:PGtkAccelGroup);cdecl;external gtkdll name 'gtk_item_factory_construct';
procedure gtk_item_factory_parse_rc(file_name:Pgchar);cdecl;external gtkdll name 'gtk_item_factory_parse_rc';
procedure gtk_item_factory_parse_rc_string(rc_string:Pgchar);cdecl;external gtkdll name 'gtk_item_factory_parse_rc_string';
procedure gtk_item_factory_parse_rc_scanner(scanner:PGScanner);cdecl;external gtkdll name 'gtk_item_factory_parse_rc_scanner';
procedure gtk_item_factory_add_foreign(accel_widget:PGtkWidget; full_path:Pgchar; accel_group:PGtkAccelGroup; keyval:guint; modifiers:TGdkModifierType);cdecl;external gtkdll name 'gtk_item_factory_add_foreign';
function  gtk_item_factory_from_widget(widget:PGtkWidget):PGtkItemFactory;cdecl;external gtkdll name 'gtk_item_factory_from_widget';
function  gtk_item_factory_path_from_widget(widget:PGtkWidget):Pgchar;cdecl;external gtkdll name 'gtk_item_factory_path_from_widget';
function  gtk_item_factory_get_item(ifactory:PGtkItemFactory; path:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_item_factory_get_item';
function  gtk_item_factory_get_widget(ifactory:PGtkItemFactory; path:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_item_factory_get_widget';
function  gtk_item_factory_get_widget_by_action(ifactory:PGtkItemFactory; action:guint):PGtkWidget;cdecl;external gtkdll name 'gtk_item_factory_get_widget_by_action';
function  gtk_item_factory_get_item_by_action(ifactory:PGtkItemFactory; action:guint):PGtkWidget;cdecl;external gtkdll name 'gtk_item_factory_get_item_by_action';
procedure gtk_item_factory_dump_items(path_pspec:PGtkPatternSpec; modified_only:gboolean; print_func:TGtkPrintFunc; func_data:gpointer);cdecl;external gtkdll name 'gtk_item_factory_dump_items';
procedure gtk_item_factory_dump_rc(file_name:Pgchar; path_pspec:PGtkPatternSpec; modified_only:gboolean);cdecl;external gtkdll name 'gtk_item_factory_dump_rc';
procedure gtk_item_factory_print_func(FILE_pointer:gpointer; thestring:Pgchar);cdecl;external gtkdll name 'gtk_item_factory_print_func';
procedure gtk_item_factory_create_item(ifactory:PGtkItemFactory; entry:PGtkItemFactoryEntry; callback_data:gpointer; callback_type:guint);cdecl;external gtkdll name 'gtk_item_factory_create_item';
procedure gtk_item_factory_create_items(ifactory:PGtkItemFactory; n_entries:guint; entries:PGtkItemFactoryEntry; callback_data:gpointer);cdecl;external gtkdll name 'gtk_item_factory_create_items';
procedure gtk_item_factory_delete_item(ifactory:PGtkItemFactory; path:Pgchar);cdecl;external gtkdll name 'gtk_item_factory_delete_item';
procedure gtk_item_factory_delete_entry(ifactory:PGtkItemFactory; entry:PGtkItemFactoryEntry);cdecl;external gtkdll name 'gtk_item_factory_delete_entry';
procedure gtk_item_factory_delete_entries(ifactory:PGtkItemFactory; n_entries:guint; entries:PGtkItemFactoryEntry);cdecl;external gtkdll name 'gtk_item_factory_delete_entries';
procedure gtk_item_factory_popup(ifactory:PGtkItemFactory; x:guint; y:guint; mouse_button:guint; time:guint32);cdecl;external gtkdll name 'gtk_item_factory_popup';
procedure gtk_item_factory_popup_with_data(ifactory:PGtkItemFactory; popup_data:gpointer; destroy:TGtkDestroyNotify; x:guint; y:guint; mouse_button:guint; time:guint32);cdecl;external gtkdll name 'gtk_item_factory_popup_with_data';
function  gtk_item_factory_popup_data(ifactory:PGtkItemFactory):gpointer;cdecl;external gtkdll name 'gtk_item_factory_popup_data';
function  gtk_item_factory_popup_data_from_widget(widget:PGtkWidget):gpointer;cdecl;external gtkdll name 'gtk_item_factory_popup_data_from_widget';
procedure gtk_item_factory_set_translate_func(ifactory:PGtkItemFactory; func:TGtkTranslateFunc; data:gpointer; notify:TGtkDestroyNotify);cdecl;external gtkdll name 'gtk_item_factory_set_translate_func';
function  gtk_item_factory_from_path(path:Pgchar):PGtkItemFactory;cdecl;external gtkdll name 'gtk_item_factory_from_path';
procedure gtk_item_factory_create_menu_entries(n_entries:guint; entries:PGtkMenuEntry);cdecl;external gtkdll name 'gtk_item_factory_create_menu_entries';
procedure gtk_item_factories_path_delete(ifactory_path:Pgchar; path:Pgchar);cdecl;external gtkdll name 'gtk_item_factories_path_delete';
procedure gtk_item_factory_create_items_ac(ifactory:PGtkItemFactory; n_entries:guint; entries:PGtkItemFactoryEntry; callback_data:gpointer; callback_type:guint);cdecl;external gtkdll name 'gtk_item_factory_create_items_ac';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  modified(var a : TGtkItemFactoryItem) : guint;
    begin
       modified:=(a.flag0 and bm_TGtkItemFactoryItem_modified) shr bp_TGtkItemFactoryItem_modified;
    end;

procedure set_modified(var a : TGtkItemFactoryItem; __modified : guint);
    begin
       a.flag0:=a.flag0 or ((__modified shl bp_TGtkItemFactoryItem_modified) and bm_TGtkItemFactoryItem_modified);
    end;

function  in_propagation(var a : TGtkItemFactoryItem) : guint;
    begin
       in_propagation:=(a.flag0 and bm_TGtkItemFactoryItem_in_propagation) shr bp_TGtkItemFactoryItem_in_propagation;
    end;

procedure set_in_propagation(var a : TGtkItemFactoryItem; __in_propagation : guint);
    begin
       a.flag0:=a.flag0 or ((__in_propagation shl bp_TGtkItemFactoryItem_in_propagation) and bm_TGtkItemFactoryItem_in_propagation);
    end;

function  GTK_IS_ITEM_FACTORY(obj:pointer):boolean;
begin
  GTK_IS_ITEM_FACTORY:=(obj<>nil) and GTK_IS_ITEM_FACTORY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ITEM_FACTORY_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ITEM_FACTORY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ITEM_FACTORY_TYPE);
end;

{$endif read_implementation}


