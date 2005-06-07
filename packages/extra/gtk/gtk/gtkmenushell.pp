{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkMenuShell = ^TGtkMenuShell;
     TGtkMenuShell = record
          container : TGtkContainer;
          children : PGList;
          active_menu_item : PGtkWidget;
          parent_menu_shell : PGtkWidget;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          activate_time : guint32;
       end;

  const
     bm_TGtkMenuShell_active = $1;
     bp_TGtkMenuShell_active = 0;
     bm_TGtkMenuShell_have_grab = $2;
     bp_TGtkMenuShell_have_grab = 1;
     bm_TGtkMenuShell_have_xgrab = $4;
     bp_TGtkMenuShell_have_xgrab = 2;
     bm_TGtkMenuShell_button = $18;
     bp_TGtkMenuShell_button = 3;
     bm_TGtkMenuShell_ignore_leave = $20;
     bp_TGtkMenuShell_ignore_leave = 5;
     bm_TGtkMenuShell_menu_flag = $40;
     bp_TGtkMenuShell_menu_flag = 6;
     bm_TGtkMenuShell_ignore_enter = $80;
     bp_TGtkMenuShell_ignore_enter = 7;
function  active(var a : TGtkMenuShell) : guint;
procedure set_active(var a : TGtkMenuShell; __active : guint);
function  have_grab(var a : TGtkMenuShell) : guint;
procedure set_have_grab(var a : TGtkMenuShell; __have_grab : guint);
function  have_xgrab(var a : TGtkMenuShell) : guint;
procedure set_have_xgrab(var a : TGtkMenuShell; __have_xgrab : guint);
function  button(var a : TGtkMenuShell) : guint;
procedure set_button(var a : TGtkMenuShell; __button : guint);
function  ignore_leave(var a : TGtkMenuShell) : guint;
procedure set_ignore_leave(var a : TGtkMenuShell; __ignore_leave : guint);
function  menu_flag(var a : TGtkMenuShell) : guint;
procedure set_menu_flag(var a : TGtkMenuShell; __menu_flag : guint);
function  ignore_enter(var a : TGtkMenuShell) : guint;
procedure set_ignore_enter(var a : TGtkMenuShell; __ignore_enter : guint);

  type
     PGtkMenuShellClass = ^TGtkMenuShellClass;
     TGtkMenuShellClass = record
          parent_class : TGtkContainerClass;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          deactivate : procedure (menu_shell:PGtkMenuShell); cdecl;
          selection_done : procedure (menu_shell:PGtkMenuShell);cdecl;
          move_current : procedure (menu_shell:PGtkMenuShell; direction:TGtkMenuDirectionType);cdecl;
          activate_current : procedure (menu_shell:PGtkMenuShell; force_hide:gboolean);cdecl;
          cancel : procedure (menu_shell:PGtkMenuShell);cdecl;
       end;

  const
     bm_TGtkMenuShellClass_submenu_placement = $1;
     bp_TGtkMenuShellClass_submenu_placement = 0;
function  submenu_placement(var a : TGtkMenuShellClass) : guint;
procedure set_submenu_placement(var a : TGtkMenuShellClass; __submenu_placement : guint);

Type
  GTK_MENU_SHELL=PGtkMenuShell;
  GTK_MENU_SHELL_CLASS=PGtkMenuShellClass;

function  GTK_MENU_SHELL_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_menu_shell_get_type';
function  GTK_IS_MENU_SHELL(obj:pointer):boolean;
function  GTK_IS_MENU_SHELL_CLASS(klass:pointer):boolean;

function  gtk_menu_shell_get_type:TGtkType;cdecl;external gtkdll name 'gtk_menu_shell_get_type';
procedure gtk_menu_shell_append(menu_shell:PGtkMenuShell; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_shell_append';
procedure gtk_menu_shell_prepend(menu_shell:PGtkMenuShell; child:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_shell_prepend';
procedure gtk_menu_shell_insert(menu_shell:PGtkMenuShell; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_menu_shell_insert';
procedure gtk_menu_shell_deactivate(menu_shell:PGtkMenuShell);cdecl;external gtkdll name 'gtk_menu_shell_deactivate';
procedure gtk_menu_shell_select_item(menu_shell:PGtkMenuShell; menu_item:PGtkWidget);cdecl;external gtkdll name 'gtk_menu_shell_select_item';
procedure gtk_menu_shell_deselect(menu_shell:PGtkMenuShell);cdecl;external gtkdll name 'gtk_menu_shell_deselect';
procedure gtk_menu_shell_activate_item(menu_shell:PGtkMenuShell; menu_item:PGtkWidget; force_deactivate:gboolean);cdecl;external gtkdll name 'gtk_menu_shell_activate_item';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  active(var a : TGtkMenuShell) : guint;
    begin
       active:=(a.flag0 and bm_TGtkMenuShell_active) shr bp_TGtkMenuShell_active;
    end;

procedure set_active(var a : TGtkMenuShell; __active : guint);
    begin
       a.flag0:=a.flag0 or ((__active shl bp_TGtkMenuShell_active) and bm_TGtkMenuShell_active);
    end;

function  have_grab(var a : TGtkMenuShell) : guint;
    begin
       have_grab:=(a.flag0 and bm_TGtkMenuShell_have_grab) shr bp_TGtkMenuShell_have_grab;
    end;

procedure set_have_grab(var a : TGtkMenuShell; __have_grab : guint);
    begin
       a.flag0:=a.flag0 or ((__have_grab shl bp_TGtkMenuShell_have_grab) and bm_TGtkMenuShell_have_grab);
    end;

function  have_xgrab(var a : TGtkMenuShell) : guint;
    begin
       have_xgrab:=(a.flag0 and bm_TGtkMenuShell_have_xgrab) shr bp_TGtkMenuShell_have_xgrab;
    end;

procedure set_have_xgrab(var a : TGtkMenuShell; __have_xgrab : guint);
    begin
       a.flag0:=a.flag0 or ((__have_xgrab shl bp_TGtkMenuShell_have_xgrab) and bm_TGtkMenuShell_have_xgrab);
    end;

function  button(var a : TGtkMenuShell) : guint;
    begin
       button:=(a.flag0 and bm_TGtkMenuShell_button) shr bp_TGtkMenuShell_button;
    end;

procedure set_button(var a : TGtkMenuShell; __button : guint);
    begin
       a.flag0:=a.flag0 or ((__button shl bp_TGtkMenuShell_button) and bm_TGtkMenuShell_button);
    end;

function  ignore_leave(var a : TGtkMenuShell) : guint;
    begin
       ignore_leave:=(a.flag0 and bm_TGtkMenuShell_ignore_leave) shr bp_TGtkMenuShell_ignore_leave;
    end;

procedure set_ignore_leave(var a : TGtkMenuShell; __ignore_leave : guint);
    begin
       a.flag0:=a.flag0 or ((__ignore_leave shl bp_TGtkMenuShell_ignore_leave) and bm_TGtkMenuShell_ignore_leave);
    end;

function  menu_flag(var a : TGtkMenuShell) : guint;
    begin
       menu_flag:=(a.flag0 and bm_TGtkMenuShell_menu_flag) shr bp_TGtkMenuShell_menu_flag;
    end;

procedure set_menu_flag(var a : TGtkMenuShell; __menu_flag : guint);
    begin
       a.flag0:=a.flag0 or ((__menu_flag shl bp_TGtkMenuShell_menu_flag) and bm_TGtkMenuShell_menu_flag);
    end;

function  ignore_enter(var a : TGtkMenuShell) : guint;
    begin
       ignore_enter:=(a.flag0 and bm_TGtkMenuShell_ignore_enter) shr bp_TGtkMenuShell_ignore_enter;
    end;

procedure set_ignore_enter(var a : TGtkMenuShell; __ignore_enter : guint);
    begin
       a.flag0:=a.flag0 or ((__ignore_enter shl bp_TGtkMenuShell_ignore_enter) and bm_TGtkMenuShell_ignore_enter);
    end;

function  submenu_placement(var a : TGtkMenuShellClass) : guint;
    begin
       submenu_placement:=(a.flag0 and bm_TGtkMenuShellClass_submenu_placement) shr bp_TGtkMenuShellClass_submenu_placement;
    end;

procedure set_submenu_placement(var a : TGtkMenuShellClass; __submenu_placement : guint);
    begin
       a.flag0:=a.flag0 or ((__submenu_placement shl bp_TGtkMenuShellClass_submenu_placement) and bm_TGtkMenuShellClass_submenu_placement);
    end;

function  GTK_IS_MENU_SHELL(obj:pointer):boolean;
begin
  GTK_IS_MENU_SHELL:=(obj<>nil) and GTK_IS_MENU_SHELL_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_MENU_SHELL_CLASS(klass:pointer):boolean;
begin
  GTK_IS_MENU_SHELL_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_MENU_SHELL_TYPE);
end;

{$endif read_implementation}


