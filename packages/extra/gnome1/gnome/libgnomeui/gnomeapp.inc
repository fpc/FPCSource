{$IfDef read_interface}
const
   GNOME_APP_MENUBAR_NAME = 'Menubar';
   GNOME_APP_TOOLBAR_NAME = 'Toolbar';

type
   PGnomeApp = ^TGnomeApp;
   TGnomeApp = record
        parent_object : TGtkWindow;
        name : Pgchar;
        prefix : Pgchar;
        dock : PGtkWidget;
        statusbar : PGtkWidget;
        vbox : PGtkWidget;
        menubar : PGtkWidget;
        contents : PGtkWidget;
        layout : PGnomeDockLayout;
        accel_group : PGtkAccelGroup;
        flag0 : word;
     end;
   GNOME_APP = PGnomeApp;

const
   bm__GnomeApp_enable_layout_config = $1;
   bp__GnomeApp_enable_layout_config = 0;

function enable_layout_config(var a : TGnomeApp) : gboolean;
procedure set_enable_layout_config(var a : TGnomeApp; __enable_layout_config : gboolean);

type
   PGnomeAppClass = ^TGnomeAppClass;
   TGnomeAppClass = record
        parent_class : TGtkWindowClass;
     end;
   GNOME_APP_CLASS = PGnomeAppClass;

function GNOME_TYPE_APP : TGTKType;
function GNOME_IS_APP(obj : Pointer) : Boolean;
function GNOME_IS_APP_CLASS(klass : Pointer) : Boolean;

function gnome_app_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_app_get_type';
function gnome_app_new(appname:Pgchar; title:Pgchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_app_new';
procedure gnome_app_construct(app:PGnomeApp; appname:Pgchar; title:Pgchar);cdecl;external libgnomeuidll name 'gnome_app_construct';
procedure gnome_app_set_menus(app:PGnomeApp; menubar:PGtkMenuBar);cdecl;external libgnomeuidll name 'gnome_app_set_menus';
procedure gnome_app_set_toolbar(app:PGnomeApp; toolbar:PGtkToolbar);cdecl;external libgnomeuidll name 'gnome_app_set_toolbar';
procedure gnome_app_set_statusbar(app:PGnomeApp; statusbar:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_app_set_statusbar';
procedure gnome_app_set_statusbar_custom(app:PGnomeApp; container:PGtkWidget; statusbar:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_app_set_statusbar_custom';
procedure gnome_app_set_contents(app:PGnomeApp; contents:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_app_set_contents';
procedure gnome_app_add_toolbar(app:PGnomeApp; toolbar:PGtkToolbar; name:Pgchar; behavior:TGnomeDockItemBehavior; placement:TGnomeDockPlacement;
            band_num:gint; band_position:gint; offset:gint);cdecl;external libgnomeuidll name 'gnome_app_add_toolbar';
procedure gnome_app_add_docked(app:PGnomeApp; widget:PGtkWidget; name:Pgchar; behavior:TGnomeDockItemBehavior; placement:TGnomeDockPlacement;
            band_num:gint; band_position:gint; offset:gint);cdecl;external libgnomeuidll name 'gnome_app_add_docked';
procedure gnome_app_add_dock_item(app:PGnomeApp; item:PGnomeDockItem; placement:TGnomeDockPlacement; band_num:gint; band_position:gint;
            offset:gint);cdecl;external libgnomeuidll name 'gnome_app_add_dock_item';
procedure gnome_app_enable_layout_config(app:PGnomeApp; enable:gboolean);cdecl;external libgnomeuidll name 'gnome_app_enable_layout_config';
function gnome_app_get_dock(app:PGnomeApp):PGnomeDock;cdecl;external libgnomeuidll name 'gnome_app_get_dock';
function gnome_app_get_dock_item_by_name(app:PGnomeApp; name:Pgchar):PGnomeDockItem;cdecl;external libgnomeuidll name 'gnome_app_get_dock_item_by_name';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_APP : TGTKType;
begin
  GNOME_TYPE_APP:=gnome_app_get_type;
end;

function GNOME_IS_APP(obj : pointer) : Boolean;
begin
   GNOME_IS_APP:=(obj<>nil) and GNOME_IS_APP_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_APP_CLASS(klass : pointer) : boolean;
begin
   GNOME_IS_APP_CLASS:= (klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_APP);
end;

function enable_layout_config(var a : TGnomeApp) : gboolean;
begin
   enable_layout_config:=gboolean((a.flag0 and bm__GnomeApp_enable_layout_config) shr bp__GnomeApp_enable_layout_config);
end;

procedure set_enable_layout_config(var a : TGnomeApp; __enable_layout_config : gboolean);
begin
   a.flag0:=a.flag0 or ((gint(__enable_layout_config) shl bp__GnomeApp_enable_layout_config) and bm__GnomeApp_enable_layout_config);
end;

{$Endif read_implementation}
