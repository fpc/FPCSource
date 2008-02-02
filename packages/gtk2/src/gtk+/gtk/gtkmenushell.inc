// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkMenuShell = ^TGtkMenuShell;
   TGtkMenuShell = record
        container : TGtkContainer;
        children : PGList;
        active_menu_item : PGtkWidget;
        parent_menu_shell : PGtkWidget;
        button : guint;
        activate_time : guint32;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkMenuShellClass = ^TGtkMenuShellClass;
   TGtkMenuShellClass = record
        parent_class : TGtkContainerClass;
        flag0 : word;
        deactivate : procedure (menu_shell:PGtkMenuShell); cdecl;
        selection_done : procedure (menu_shell:PGtkMenuShell); cdecl;
        move_current : procedure (menu_shell:PGtkMenuShell; direction:TGtkMenuDirectionType); cdecl;
        activate_current : procedure (menu_shell:PGtkMenuShell; force_hide:gboolean); cdecl;
        cancel : procedure (menu_shell:PGtkMenuShell); cdecl;
        select_item : procedure (menu_shell:PGtkMenuShell; menu_item:PGtkWidget); cdecl;
        insert : procedure (menu_shell:PGtkMenuShell; child:PGtkWidget; position:gint); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkMenuShell_active = $1;
   bp_TGtkMenuShell_active = 0;
   bm_TGtkMenuShell_have_grab = $2;
   bp_TGtkMenuShell_have_grab = 1;
   bm_TGtkMenuShell_have_xgrab = $4;
   bp_TGtkMenuShell_have_xgrab = 2;
   bm_TGtkMenuShell_ignore_leave = $8;
   bp_TGtkMenuShell_ignore_leave = 3;
   bm_TGtkMenuShell_menu_flag = $10;
   bp_TGtkMenuShell_menu_flag = 4;
   bm_TGtkMenuShell_ignore_enter = $20;
   bp_TGtkMenuShell_ignore_enter = 5;

   bm_TGtkMenuShellClass_submenu_placement = $1;
   bp_TGtkMenuShellClass_submenu_placement = 0;


function GTK_TYPE_MENU_SHELL : GType;
function GTK_MENU_SHELL(obj: pointer) : PGtkMenuShell;
function GTK_MENU_SHELL_CLASS(klass: pointer) : PGtkMenuShellClass;
function GTK_IS_MENU_SHELL(obj: pointer) : boolean;
function GTK_IS_MENU_SHELL_CLASS(klass: pointer) : boolean;
function GTK_MENU_SHELL_GET_CLASS(obj: pointer) : PGtkMenuShellClass;

function active(var a : TGtkMenuShell) : guint; overload;
procedure set_active(var a : TGtkMenuShell; __active : guint); overload;
function have_grab(var a : TGtkMenuShell) : guint; overload;
procedure set_have_grab(var a : TGtkMenuShell; __have_grab : guint); overload;
function have_xgrab(var a : TGtkMenuShell) : guint;
procedure set_have_xgrab(var a : TGtkMenuShell; __have_xgrab : guint);
function ignore_leave(var a : TGtkMenuShell) : guint;
procedure set_ignore_leave(var a : TGtkMenuShell; __ignore_leave : guint);
function menu_flag(var a : TGtkMenuShell) : guint;
procedure set_menu_flag(var a : TGtkMenuShell; __menu_flag : guint);
function ignore_enter(var a : TGtkMenuShell) : guint;
procedure set_ignore_enter(var a : TGtkMenuShell; __ignore_enter : guint);

function submenu_placement(var a : TGtkMenuShellClass) : guint; overload;
procedure set_submenu_placement(var a : TGtkMenuShellClass; __submenu_placement : guint); overload;

function gtk_menu_shell_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_menu_shell_append(menu_shell:PGtkMenuShell; child:PGtkWidget); cdecl; external gtklib;
procedure gtk_menu_shell_prepend(menu_shell:PGtkMenuShell; child:PGtkWidget); cdecl; external gtklib;
procedure gtk_menu_shell_insert(menu_shell:PGtkMenuShell; child:PGtkWidget; position:gint); cdecl; external gtklib;
procedure gtk_menu_shell_deactivate(menu_shell:PGtkMenuShell); cdecl; external gtklib;
procedure gtk_menu_shell_select_item(menu_shell:PGtkMenuShell; menu_item:PGtkWidget); cdecl; external gtklib;
procedure gtk_menu_shell_deselect(menu_shell:PGtkMenuShell); cdecl; external gtklib;
procedure gtk_menu_shell_activate_item(menu_shell:PGtkMenuShell; menu_item:PGtkWidget; force_deactivate:gboolean); cdecl; external gtklib;
procedure _gtk_menu_shell_select_first(menu_shell:PGtkMenuShell); cdecl; external gtklib;
procedure _gtk_menu_shell_activate(menu_shell:PGtkMenuShell); cdecl; external gtklib;

{$IFDEF HasGTK2_4}
procedure gtk_menu_shell_select_first(menu_shell: PGtkMenuShell; search_sensitive: gboolean);  cdecl; external gtklib;
procedure gtk_menu_shell_cancel(menu_shell: PGtkMenuShell); cdecl; external gtklib;
{$ENDIF HasGTK2_4}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MENU_SHELL : GType;
begin
   GTK_TYPE_MENU_SHELL:=gtk_menu_shell_get_type;
end;

function GTK_MENU_SHELL(obj: pointer) : PGtkMenuShell;
begin
   GTK_MENU_SHELL:=PGtkMenuShell(GTK_CHECK_CAST(obj,GTK_TYPE_MENU_SHELL));
end;

function GTK_MENU_SHELL_CLASS(klass: pointer) : PGtkMenuShellClass;
begin
   GTK_MENU_SHELL_CLASS:=PGtkMenuShellClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_MENU_SHELL));
end;

function GTK_IS_MENU_SHELL(obj: pointer) : boolean;
begin
   GTK_IS_MENU_SHELL:=GTK_CHECK_TYPE(obj,GTK_TYPE_MENU_SHELL);
end;

function GTK_IS_MENU_SHELL_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_MENU_SHELL_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_MENU_SHELL);
end;

function GTK_MENU_SHELL_GET_CLASS(obj: pointer) : PGtkMenuShellClass;
begin
   GTK_MENU_SHELL_GET_CLASS:=PGtkMenuShellClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_MENU_SHELL));
end;

function active(var a : TGtkMenuShell) : guint;
begin
   active:=(a.flag0 and bm_TGtkMenuShell_active) shr bp_TGtkMenuShell_active;
end;

procedure set_active(var a : TGtkMenuShell; __active : guint);
begin
   a.flag0:=a.flag0 or ((__active shl bp_TGtkMenuShell_active) and bm_TGtkMenuShell_active);
end;

function have_grab(var a : TGtkMenuShell) : guint;
begin
   have_grab:=(a.flag0 and bm_TGtkMenuShell_have_grab) shr bp_TGtkMenuShell_have_grab;
end;

procedure set_have_grab(var a : TGtkMenuShell; __have_grab : guint);
begin
   a.flag0:=a.flag0 or ((__have_grab shl bp_TGtkMenuShell_have_grab) and bm_TGtkMenuShell_have_grab);
end;

function have_xgrab(var a : TGtkMenuShell) : guint;
begin
   have_xgrab:=(a.flag0 and bm_TGtkMenuShell_have_xgrab) shr bp_TGtkMenuShell_have_xgrab;
end;

procedure set_have_xgrab(var a : TGtkMenuShell; __have_xgrab : guint);
begin
   a.flag0:=a.flag0 or ((__have_xgrab shl bp_TGtkMenuShell_have_xgrab) and bm_TGtkMenuShell_have_xgrab);
end;

function ignore_leave(var a : TGtkMenuShell) : guint;
begin
   ignore_leave:=(a.flag0 and bm_TGtkMenuShell_ignore_leave) shr bp_TGtkMenuShell_ignore_leave;
end;

procedure set_ignore_leave(var a : TGtkMenuShell; __ignore_leave : guint);
begin
   a.flag0:=a.flag0 or ((__ignore_leave shl bp_TGtkMenuShell_ignore_leave) and bm_TGtkMenuShell_ignore_leave);
end;

function menu_flag(var a : TGtkMenuShell) : guint;
begin
   menu_flag:=(a.flag0 and bm_TGtkMenuShell_menu_flag) shr bp_TGtkMenuShell_menu_flag;
end;

procedure set_menu_flag(var a : TGtkMenuShell; __menu_flag : guint);
begin
   a.flag0:=a.flag0 or ((__menu_flag shl bp_TGtkMenuShell_menu_flag) and bm_TGtkMenuShell_menu_flag);
end;

function ignore_enter(var a : TGtkMenuShell) : guint;
begin
   ignore_enter:=(a.flag0 and bm_TGtkMenuShell_ignore_enter) shr bp_TGtkMenuShell_ignore_enter;
end;

procedure set_ignore_enter(var a : TGtkMenuShell; __ignore_enter : guint);
begin
   a.flag0:=a.flag0 or ((__ignore_enter shl bp_TGtkMenuShell_ignore_enter) and bm_TGtkMenuShell_ignore_enter);
end;

function submenu_placement(var a : TGtkMenuShellClass) : guint;
begin
   submenu_placement:=(a.flag0 and bm_TGtkMenuShellClass_submenu_placement) shr bp_TGtkMenuShellClass_submenu_placement;
end;

procedure set_submenu_placement(var a : TGtkMenuShellClass; __submenu_placement : guint);
begin
   a.flag0:=a.flag0 or ((__submenu_placement shl bp_TGtkMenuShellClass_submenu_placement) and bm_TGtkMenuShellClass_submenu_placement);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

