{$IfDef read_interface}
type
   TGnomeUIInfoType = (GNOME_APP_UI_ENDOFINFO,GNOME_APP_UI_ITEM,
     GNOME_APP_UI_TOGGLEITEM,GNOME_APP_UI_RADIOITEMS,
     GNOME_APP_UI_SUBTREE,GNOME_APP_UI_SEPARATOR,
     GNOME_APP_UI_HELP,GNOME_APP_UI_BUILDER_DATA,
     GNOME_APP_UI_ITEM_CONFIGURABLE,GNOME_APP_UI_SUBTREE_STOCK
     );

   TGnomeUIInfoConfigurableTypes = (GNOME_APP_CONFIGURABLE_ITEM_NEW,GNOME_APP_CONFIGURABLE_ITEM_OPEN,
     GNOME_APP_CONFIGURABLE_ITEM_SAVE,GNOME_APP_CONFIGURABLE_ITEM_SAVE_AS,
     GNOME_APP_CONFIGURABLE_ITEM_REVERT,GNOME_APP_CONFIGURABLE_ITEM_PRINT,
     GNOME_APP_CONFIGURABLE_ITEM_PRINT_SETUP,
     GNOME_APP_CONFIGURABLE_ITEM_CLOSE,GNOME_APP_CONFIGURABLE_ITEM_EXIT,
     GNOME_APP_CONFIGURABLE_ITEM_CUT,GNOME_APP_CONFIGURABLE_ITEM_COPY,
     GNOME_APP_CONFIGURABLE_ITEM_PASTE,GNOME_APP_CONFIGURABLE_ITEM_CLEAR,
     GNOME_APP_CONFIGURABLE_ITEM_UNDO,GNOME_APP_CONFIGURABLE_ITEM_REDO,
     GNOME_APP_CONFIGURABLE_ITEM_FIND,GNOME_APP_CONFIGURABLE_ITEM_FIND_AGAIN,
     GNOME_APP_CONFIGURABLE_ITEM_REPLACE,GNOME_APP_CONFIGURABLE_ITEM_PROPERTIES,
     GNOME_APP_CONFIGURABLE_ITEM_PREFERENCES,
     GNOME_APP_CONFIGURABLE_ITEM_ABOUT,GNOME_APP_CONFIGURABLE_ITEM_SELECT_ALL,
     GNOME_APP_CONFIGURABLE_ITEM_NEW_WINDOW,
     GNOME_APP_CONFIGURABLE_ITEM_CLOSE_WINDOW,
     GNOME_APP_CONFIGURABLE_ITEM_NEW_GAME,
     GNOME_APP_CONFIGURABLE_ITEM_PAUSE_GAME,
     GNOME_APP_CONFIGURABLE_ITEM_RESTART_GAME,
     GNOME_APP_CONFIGURABLE_ITEM_UNDO_MOVE,
     GNOME_APP_CONFIGURABLE_ITEM_REDO_MOVE,
     GNOME_APP_CONFIGURABLE_ITEM_HINT,GNOME_APP_CONFIGURABLE_ITEM_SCORES,
     GNOME_APP_CONFIGURABLE_ITEM_END_GAME
     );

   TGnomeUIPixmapType = (GNOME_APP_PIXMAP_NONE,GNOME_APP_PIXMAP_STOCK,
     GNOME_APP_PIXMAP_DATA,GNOME_APP_PIXMAP_FILENAME
     );

   PGnomeUIInfo = ^TGnomeUIInfo;
   TGnomeUIInfo = record
        thetype : TGnomeUIInfoType;
        thelabel : Pgchar;
        hint : Pgchar;
        moreinfo : gpointer;//aka Callback
        user_data : gpointer;
        unused_data : gpointer;
        pixmap_type : TGnomeUIPixmapType;
        pixmap_info : gconstpointer;
        accelerator_key : guint;
        ac_mods : TGdkModifierType;
        widget : PGtkWidget;
     end;

const
   GNOMEUIINFO_KEY_UIDATA :Pchar = 'uidata';
   GNOMEUIINFO_KEY_UIBDATA :Pchar = 'uibdata';

function GNOMEUIINFO_END : TGnomeUIInfo;
function GNOMEUIINFO_SEPARATOR : TGnomeUIInfo;
function GNOMEUIINFO_ITEM(thelabel : Pgchar;tooltip:Pgchar; callback : TGTKCallback; xpm_data : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_ITEM_STOCK(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;stock_id : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_ITEM_NONE(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback) : TGnomeUIInfo;
function GNOMEUIINFO_ITEM_DATA(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;user_data : gpointer;xpm_data : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_TOGGLEITEM(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;xpm_data : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_TOGGLEITEM_DATA(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;user_data : gpointer;xpm_data : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_HELP(app_name : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_SUBTREE(thelabel : Pgchar;tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_SUBTREE_HINT(thelabel : Pgchar;hint : Pgchar;tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_SUBTREE_STOCK(thelabel : Pgchar;tree : gpointer;stock_id : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_RADIOLIST(list : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_RADIOITEM(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;xpm_data : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_RADIOITEM_DATA(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;user_data : gpointer;xpm_data : Pgchar) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_NEW_ITEM(thelabel : Pgchar;tip : Pgchar;cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_NEW_SUBTREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_OPEN_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_SAVE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_SAVE_AS_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_REVERT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_PRINT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_PRINT_SETUP_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_CLOSE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_EXIT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_CUT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_COPY_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_PASTE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_SELECT_ALL_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_CLEAR_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_UNDO_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_REDO_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_FIND_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_FIND_AGAIN_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_REPLACE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_PROPERTIES_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_PREFERENCES_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_NEW_WINDOW_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_CLOSE_WINDOW_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_ABOUT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_NEW_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_PAUSE_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_RESTART_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_UNDO_MOVE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_REDO_MOVE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_HINT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_SCORES_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_END_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;

function gnome_app_helper_gettext(_string:Pgchar):Pgchar;cdecl;external libgnomeuidll name 'gnome_app_helper_gettext';

function GNOMEUIINFO_MENU_FILE_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_EDIT_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_VIEW_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_SETTINGS_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_FILES_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_WINDOWS_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_HELP_TREE(tree : gpointer) : TGnomeUIInfo;
function GNOMEUIINFO_MENU_GAME_TREE(tree : gpointer) : TGnomeUIInfo;


const
   GNOME_MENU_FILE_STRING :Pchar = '_File';
   GNOME_MENU_FILE_PATH :Pchar = '_File/';
   GNOME_MENU_EDIT_STRING :Pchar = '_Edit';
   GNOME_MENU_EDIT_PATH :Pchar = '_Edit/';
   GNOME_MENU_VIEW_STRING :Pchar = '_View';
   GNOME_MENU_VIEW_PATH :Pchar = '_View/';
   GNOME_MENU_SETTINGS_STRING :Pchar = '_Settings';
   GNOME_MENU_SETTINGS_PATH :Pchar = '_Settings/';
   GNOME_MENU_NEW_STRING :Pchar = '_New';
   GNOME_MENU_NEW_PATH :Pchar = '_New/';
   GNOME_MENU_FILES_STRING :Pchar = 'Fi_les';
   GNOME_MENU_FILES_PATH :Pchar = 'Fi_les/';
   GNOME_MENU_WINDOWS_STRING :Pchar = '_Windows';
   GNOME_MENU_WINDOWS_PATH :Pchar = '_Windows/';
type
   PGnomeUIBuilderData = ^TGnomeUIBuilderData;
   TGnomeUISignalConnectFunc = procedure (uiinfo:PGnomeUIInfo; signal_name:Pgchar; uibdata:PGnomeUIBuilderData);cdecl;
   TGnomeUIBuilderData = record
        connect_func : TGnomeUISignalConnectFunc;
        data : gpointer;
        is_interp : gboolean;
        relay_func : TGtkCallbackMarshal;
        destroy_func : TGtkDestroyNotify;
     end;

procedure gnome_accelerators_sync;cdecl;external libgnomeuidll name 'gnome_accelerators_sync';
procedure gnome_app_fill_menu(menu_shell:PGtkMenuShell; uiinfo:PGnomeUIInfo; accel_group:PGtkAccelGroup; uline_accels:gboolean; pos:gint);cdecl;external libgnomeuidll name 'gnome_app_fill_menu';
procedure gnome_app_fill_menu_with_data(menu_shell:PGtkMenuShell; uiinfo:PGnomeUIInfo; accel_group:PGtkAccelGroup; uline_accels:gboolean; pos:gint;
            user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_app_fill_menu_with_data';
procedure gnome_app_fill_menu_custom(menu_shell:PGtkMenuShell; uiinfo:PGnomeUIInfo; uibdata:PGnomeUIBuilderData; accel_group:PGtkAccelGroup; uline_accels:gboolean;
            pos:gint);cdecl;external libgnomeuidll name 'gnome_app_fill_menu_custom';
procedure gnome_app_ui_configure_configurable(uiinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_ui_configure_configurable';
procedure gnome_app_create_menus(app:PGnomeApp; uiinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_create_menus';
procedure gnome_app_create_menus_interp(app:PGnomeApp; uiinfo:PGnomeUIInfo; relay_func:TGtkCallbackMarshal; data:gpointer; destroy_func:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_app_create_menus_interp';
procedure gnome_app_create_menus_with_data(app:PGnomeApp; uiinfo:PGnomeUIInfo; user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_app_create_menus_with_data';
procedure gnome_app_create_menus_custom(app:PGnomeApp; uiinfo:PGnomeUIInfo; uibdata:PGnomeUIBuilderData);cdecl;external libgnomeuidll name 'gnome_app_create_menus_custom';
procedure gnome_app_fill_toolbar(toolbar:PGtkToolbar; uiinfo:PGnomeUIInfo; accel_group:PGtkAccelGroup);cdecl;external libgnomeuidll name 'gnome_app_fill_toolbar';
procedure gnome_app_fill_toolbar_with_data(toolbar:PGtkToolbar; uiinfo:PGnomeUIInfo; accel_group:PGtkAccelGroup; user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_app_fill_toolbar_with_data';
procedure gnome_app_fill_toolbar_custom(toolbar:PGtkToolbar; uiinfo:PGnomeUIInfo; uibdata:PGnomeUIBuilderData; accel_group:PGtkAccelGroup);cdecl;external libgnomeuidll name 'gnome_app_fill_toolbar_custom';
procedure gnome_app_create_toolbar(app:PGnomeApp; uiinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_create_toolbar';
procedure gnome_app_create_toolbar_interp(app:PGnomeApp; uiinfo:PGnomeUIInfo; relay_func:TGtkCallbackMarshal; data:gpointer; destroy_func:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_app_create_toolbar_interp';
procedure gnome_app_create_toolbar_with_data(app:PGnomeApp; uiinfo:PGnomeUIInfo; user_data:gpointer);cdecl;external libgnomeuidll name 'gnome_app_create_toolbar_with_data';
procedure gnome_app_create_toolbar_custom(app:PGnomeApp; uiinfo:PGnomeUIInfo; uibdata:PGnomeUIBuilderData);cdecl;external libgnomeuidll name 'gnome_app_create_toolbar_custom';
function gnome_app_find_menu_pos(parent:PGtkWidget; path:Pgchar; pos:Pgint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_find_menu_pos';
procedure gnome_app_remove_menus(app:PGnomeApp; path:Pgchar; items:gint);cdecl;external libgnomeuidll name 'gnome_app_remove_menus';
procedure gnome_app_remove_menu_range(app:PGnomeApp; path:Pgchar; start:gint; items:gint);cdecl;external libgnomeuidll name 'gnome_app_remove_menu_range';
procedure gnome_app_insert_menus_custom(app:PGnomeApp; path:Pgchar; menuinfo:PGnomeUIInfo; uibdata:PGnomeUIBuilderData);cdecl;external libgnomeuidll name 'gnome_app_insert_menus_custom';
procedure gnome_app_insert_menus(app:PGnomeApp; path:Pgchar; menuinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_insert_menus';
procedure gnome_app_insert_menus_with_data(app:PGnomeApp; path:Pgchar; menuinfo:PGnomeUIInfo; data:gpointer);cdecl;external libgnomeuidll name 'gnome_app_insert_menus_with_data';
procedure gnome_app_insert_menus_interp(app:PGnomeApp; path:Pgchar; menuinfo:PGnomeUIInfo; relay_func:TGtkCallbackMarshal; data:gpointer;
            destroy_func:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_app_insert_menus_interp';
procedure gnome_app_install_appbar_menu_hints(appbar:PGnomeAppBar; uiinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_install_appbar_menu_hints';
procedure gnome_app_install_statusbar_menu_hints(bar:PGtkStatusbar; uiinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_install_statusbar_menu_hints';
procedure gnome_app_install_menu_hints(app:PGnomeApp; uinfo:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_app_install_menu_hints';

{$EndIf read_interface}

{$Ifdef read_implementation}


function GETGNOMEUIINFO(thetype : TGnomeUIInfoType; thelabel : Pgchar; hint : Pgchar; moreinfo : gpointer;
  user_data : gpointer;unused_data : gpointer; pixmap_type : TGnomeUIPixmapType; pixmap_info : Pgchar;
  accelerator_key : guint; ac_mods : TGdkModifierType;widget : PGtkWidget) : TGnomeUIInfo;
begin
  Result.thetype := thetype;
  Result.thelabel := thelabel;
  Result.hint := hint;
  Result.moreinfo := moreinfo;
  Result.user_data := user_data;
  Result.unused_data := unused_data;
  Result.pixmap_type := pixmap_type;
  Result.pixmap_info := pixmap_info;
  Result.accelerator_key := accelerator_key;
  Result.ac_mods := ac_mods;
  Result.widget := widget;
end;

function GNOMEUIINFO_END: TGnomeUIInfo;
begin
  GNOMEUIINFO_END:=GETGNOMEUIINFO(GNOME_APP_UI_ENDOFINFO,nil,nil,nil,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_SEPARATOR: TGnomeUIInfo;
begin
   GNOMEUIINFO_SEPARATOR:=GETGNOMEUIINFO(GNOME_APP_UI_SEPARATOR,nil,nil,nil,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_ITEM(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;xpm_data : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM,thelabel,tooltip,gpointer(callback),nil,nil,GNOME_APP_PIXMAP_DATA,xpm_data,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_ITEM_STOCK(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;stock_id : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_ITEM_STOCK:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM,thelabel,tooltip,gpointer(callback),nil,nil,GNOME_APP_PIXMAP_STOCK,stock_id,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_ITEM_NONE(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback) : TGnomeUIInfo;
begin
   GNOMEUIINFO_ITEM_NONE:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM,thelabel,tooltip,gpointer(callback),nil,nil,GNOME_APP_PIXMAP_NONE,nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_ITEM_DATA(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;user_data : gpointer;xpm_data : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_ITEM_DATA:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM,thelabel,tooltip,gpointer(callback),user_data,nil,GNOME_APP_PIXMAP_DATA,xpm_data,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_TOGGLEITEM(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;xpm_data : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_TOGGLEITEM:=GETGNOMEUIINFO(GNOME_APP_UI_TOGGLEITEM,thelabel,tooltip,gpointer(callback),nil,nil,GNOME_APP_PIXMAP_DATA,xpm_data,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_TOGGLEITEM_DATA(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;user_data : gpointer;xpm_data : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_TOGGLEITEM_DATA:=GETGNOMEUIINFO(GNOME_APP_UI_TOGGLEITEM,thelabel,tooltip,gpointer(callback),user_data,nil,GNOME_APP_PIXMAP_DATA,xpm_data,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_HELP(app_name : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_HELP:=GETGNOMEUIINFO(GNOME_APP_UI_HELP,nil,nil,app_name,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_SUBTREE(thelabel : Pgchar;tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_SUBTREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE,thelabel,nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_SUBTREE_HINT(thelabel : Pgchar;hint : Pgchar;tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_SUBTREE_HINT:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE,thelabel,hint,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_SUBTREE_STOCK(thelabel : Pgchar;tree : gpointer;stock_id : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_SUBTREE_STOCK:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE,thelabel,nil,tree,nil,nil,GNOME_APP_PIXMAP_STOCK,stock_id,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_RADIOLIST(list : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_RADIOLIST:=GETGNOMEUIINFO(GNOME_APP_UI_RADIOITEMS,nil,nil,list,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_RADIOITEM(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;xpm_data : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_RADIOITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM,thelabel,tooltip,gpointer(callback),nil,nil,GNOME_APP_PIXMAP_DATA,xpm_data,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_RADIOITEM_DATA(thelabel : Pgchar;tooltip:Pgchar;callback : TGTKCallback;user_data : gpointer;xpm_data : Pgchar) : TGnomeUIInfo;
begin
   GNOMEUIINFO_RADIOITEM_DATA:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM,thelabel,tooltip,gpointer(callback),user_data,nil,GNOME_APP_PIXMAP_DATA,xpm_data,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_NEW_ITEM(thelabel : Pgchar;tip : Pgchar;cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_NEW_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,thelabel,tip,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_NEW),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_NEW_SUBTREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_NEW_SUBTREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_New',nil,tree,nil,nil,GNOME_APP_PIXMAP_STOCK,GNOME_STOCK_MENU_NEW,GNOME_KEY_NAME_NEW,GNOME_KEY_MOD_NEW,nil);
end;

function GNOMEUIINFO_MENU_OPEN_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_OPEN_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_OPEN),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_SAVE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_SAVE_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_SAVE),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_SAVE_AS_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_SAVE_AS_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_SAVE_AS),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_REVERT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_REVERT_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_REVERT),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_PRINT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_PRINT_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_PRINT),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_PRINT_SETUP_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_PRINT_SETUP_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_PRINT_SETUP),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_CLOSE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_CLOSE_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_CLOSE),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_EXIT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_EXIT_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_EXIT),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_CUT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_CUT_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_CUT),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_COPY_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_COPY_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_COPY),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_PASTE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_PASTE_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_PASTE),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_SELECT_ALL_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_SELECT_ALL_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_SELECT_ALL),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_CLEAR_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_CLEAR_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_CLEAR),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_UNDO_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_UNDO_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_UNDO),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_REDO_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_REDO_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_REDO),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_FIND_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_FIND_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_FIND),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_FIND_AGAIN_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_FIND_AGAIN_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_FIND_AGAIN),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_REPLACE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_REPLACE_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_REPLACE),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_PROPERTIES_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_PROPERTIES_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_PROPERTIES),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_PREFERENCES_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_PREFERENCES_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_PREFERENCES),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_NEW_WINDOW_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_NEW_WINDOW_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_NEW_WINDOW),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_CLOSE_WINDOW_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_CLOSE_WINDOW_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_CLOSE_WINDOW),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_ABOUT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_ABOUT_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_ABOUT),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_NEW_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_NEW_GAME_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_NEW_GAME),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_PAUSE_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_PAUSE_GAME_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_PAUSE_GAME),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_RESTART_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_RESTART_GAME_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_RESTART_GAME),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_UNDO_MOVE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_UNDO_MOVE_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_UNDO_MOVE),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_REDO_MOVE_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_REDO_MOVE_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_REDO_MOVE),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_HINT_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_HINT_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_HINT),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_SCORES_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_SCORES_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_SCORES),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_END_GAME_ITEM(cb : TGTKCallback;data:gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_END_GAME_ITEM:=GETGNOMEUIINFO(GNOME_APP_UI_ITEM_CONFIGURABLE,nil,nil,gpointer(cb),gpointer(data),nil,GNOME_APP_PIXMAP_NONE,nil,longint(GNOME_APP_CONFIGURABLE_ITEM_END_GAME),TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_FILE_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_FILE_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_File',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_EDIT_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_EDIT_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_Edit',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_VIEW_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_VIEW_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_View',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_SETTINGS_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_SETTINGS_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_Settings',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_FILES_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_FILES_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'Fi_les',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_WINDOWS_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_WINDOWS_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_Windows',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_HELP_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_HELP_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_Help',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

function GNOMEUIINFO_MENU_GAME_TREE(tree : gpointer) : TGnomeUIInfo;
begin
   GNOMEUIINFO_MENU_GAME_TREE:=GETGNOMEUIINFO(GNOME_APP_UI_SUBTREE_STOCK,'_Game',nil,tree,nil,nil,TGnomeUIPixmapType(0),nil,0,TGdkModifierType(0),nil);
end;

{$Endif read_implementation}
