{$IfDef read_interface}

type
   TGnomeMDIMode = (GNOME_MDI_NOTEBOOK,GNOME_MDI_TOPLEVEL,
     GNOME_MDI_MODAL,GNOME_MDI_DEFAULT_MODE := 42);

   PGnomeMDI = ^TGnomeMDI;
   TGnomeMDI = record
        theobject : TGtkObject;
        mode : TGnomeMDIMode;
        tab_pos : TGtkPositionType;
        signal_id : guint;
        flag0 : word;
        appname : Pgchar;
        title : Pgchar;
        menu_template : PGnomeUIInfo;
        toolbar_template : PGnomeUIInfo;
        active_child : PGnomeMDIChild;
        active_view : PGtkWidget;
        active_window : PGnomeApp;
        windows : PGList;
        children : PGList;
        registered : PGSList;
        child_menu_path : Pgchar;
        child_list_path : Pgchar;
     end;
   GNOME_MDI = PGnomeMDI;

const
   bm__GnomeMDI_in_drag = $1;
   bp__GnomeMDI_in_drag = 0;
function in_drag(var a : TGnomeMDI) : gint;
procedure set_in_drag(var a : TGnomeMDI; __in_drag : gint);

type
   PGnomeMDIClass = ^TGnomeMDIClass;
   TGnomeMDIClass = record
        parent_class : TGtkObjectClass;
        add_child : function (_para1:PGnomeMDI; _para2:PGnomeMDIChild):gint;cdecl;
        remove_child : function (_para1:PGnomeMDI; _para2:PGnomeMDIChild):gint;cdecl;
        add_view : function (_para1:PGnomeMDI; _para2:PGtkWidget):gint;cdecl;
        remove_view : function (_para1:PGnomeMDI; _para2:PGtkWidget):gint;cdecl;
        child_changed : procedure (_para1:PGnomeMDI; _para2:PGnomeMDIChild);cdecl;
        view_changed : procedure (_para1:PGnomeMDI; _para2:PGtkWidget);cdecl;
        app_created : procedure (_para1:PGnomeMDI; _para2:PGnomeApp);cdecl;
     end;
   GNOME_MDI_CLASS = PGnomeMDIClass;

function GNOME_TYPE_MDI : TGTKType;
function GNOME_IS_MDI(obj : Pointer) : Boolean;
function GNOME_IS_MDI_CLASS(klass : Pointer) : Boolean;

function gnome_mdi_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_mdi_get_type';
function gnome_mdi_new(appname:Pgchar; title:Pgchar):PGtkObject;cdecl;external libgnomeuidll name 'gnome_mdi_new';
procedure gnome_mdi_set_mode(mdi:PGnomeMDI; mode:TGnomeMDIMode);cdecl;external libgnomeuidll name 'gnome_mdi_set_mode';
procedure gnome_mdi_set_menubar_template(mdi:PGnomeMDI; menu_tmpl:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_mdi_set_menubar_template';
procedure gnome_mdi_set_toolbar_template(mdi:PGnomeMDI; tbar_tmpl:PGnomeUIInfo);cdecl;external libgnomeuidll name 'gnome_mdi_set_toolbar_template';
procedure gnome_mdi_set_child_menu_path(mdi:PGnomeMDI; path:Pgchar);cdecl;external libgnomeuidll name 'gnome_mdi_set_child_menu_path';
procedure gnome_mdi_set_child_list_path(mdi:PGnomeMDI; path:Pgchar);cdecl;external libgnomeuidll name 'gnome_mdi_set_child_list_path';
function gnome_mdi_add_view(mdi:PGnomeMDI; child:PGnomeMDIChild):gint;cdecl;external libgnomeuidll name 'gnome_mdi_add_view';
function gnome_mdi_add_toplevel_view(mdi:PGnomeMDI; child:PGnomeMDIChild):gint;cdecl;external libgnomeuidll name 'gnome_mdi_add_toplevel_view';
function gnome_mdi_remove_view(mdi:PGnomeMDI; view:PGtkWidget; force:gint):gint;cdecl;external libgnomeuidll name 'gnome_mdi_remove_view';
function gnome_mdi_get_active_view(mdi:PGnomeMDI):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_mdi_get_active_view';
procedure gnome_mdi_set_active_view(mdi:PGnomeMDI; view:PGtkWidget);cdecl;external libgnomeuidll name 'gnome_mdi_set_active_view';
function gnome_mdi_add_child(mdi:PGnomeMDI; child:PGnomeMDIChild):gint;cdecl;external libgnomeuidll name 'gnome_mdi_add_child';
function gnome_mdi_remove_child(mdi:PGnomeMDI; child:PGnomeMDIChild; force:gint):gint;cdecl;external libgnomeuidll name 'gnome_mdi_remove_child';
function gnome_mdi_remove_all(mdi:PGnomeMDI; force:gint):gint;cdecl;external libgnomeuidll name 'gnome_mdi_remove_all';
procedure gnome_mdi_open_toplevel(mdi:PGnomeMDI);cdecl;external libgnomeuidll name 'gnome_mdi_open_toplevel';
procedure gnome_mdi_update_child(mdi:PGnomeMDI; child:PGnomeMDIChild);cdecl;external libgnomeuidll name 'gnome_mdi_update_child';
function gnome_mdi_get_active_child(mdi:PGnomeMDI):PGnomeMDIChild;cdecl;external libgnomeuidll name 'gnome_mdi_get_active_child';
function gnome_mdi_find_child(mdi:PGnomeMDI; name:Pgchar):PGnomeMDIChild;cdecl;external libgnomeuidll name 'gnome_mdi_find_child';
function gnome_mdi_get_active_window(mdi:PGnomeMDI):PGnomeApp;cdecl;external libgnomeuidll name 'gnome_mdi_get_active_window';
procedure gnome_mdi_register(mdi:PGnomeMDI; theobject:PGtkObject);cdecl;external libgnomeuidll name 'gnome_mdi_register';
procedure gnome_mdi_unregister(mdi:PGnomeMDI; theobject:PGtkObject);cdecl;external libgnomeuidll name 'gnome_mdi_unregister';
function gnome_mdi_get_app_from_view(view:PGtkWidget):PGnomeApp;cdecl;external libgnomeuidll name 'gnome_mdi_get_app_from_view';
function gnome_mdi_get_child_from_view(view:PGtkWidget):PGnomeMDIChild;cdecl;external libgnomeuidll name 'gnome_mdi_get_child_from_view';
function gnome_mdi_get_view_from_window(mdi:PGnomeMDI; app:PGnomeApp):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_mdi_get_view_from_window';
function gnome_mdi_get_menubar_info(app:PGnomeApp):PGnomeUIInfo;cdecl;external libgnomeuidll name 'gnome_mdi_get_menubar_info';
function gnome_mdi_get_toolbar_info(app:PGnomeApp):PGnomeUIInfo;cdecl;external libgnomeuidll name 'gnome_mdi_get_toolbar_info';
function gnome_mdi_get_child_menu_info(app:PGnomeApp):PGnomeUIInfo;cdecl;external libgnomeuidll name 'gnome_mdi_get_child_menu_info';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_MDI : TGTKType;
begin
  GNOME_TYPE_MDI:=gnome_mdi_get_type;
end;

function GNOME_IS_MDI(obj : Pointer) : Boolean;
begin
   GNOME_IS_MDI:=(obj<>nil) and GNOME_IS_MDI_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_MDI_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_MDI_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_MDI);
end;

function in_drag(var a : TGnomeMDI) : gint;
begin
   in_drag:=(a.flag0 and bm__GnomeMDI_in_drag) shr bp__GnomeMDI_in_drag;
end;

procedure set_in_drag(var a : TGnomeMDI; __in_drag : gint);
begin
   a.flag0:=a.flag0 or ((__in_drag shl bp__GnomeMDI_in_drag) and bm__GnomeMDI_in_drag);
end;

{$Endif read_implementation}
