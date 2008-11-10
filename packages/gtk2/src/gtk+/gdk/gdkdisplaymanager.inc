// included by gdk2.pp

{$IFDEF read_forward_definitions}
   PGdkDisplayManager = ^TGdkDisplayManager;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< private > }
   TGdkDisplayManager = record
     parent_instance: TGObject;
   end;

   PGdkDisplayManagerClass = ^TGdkDisplayManagerClass;
   TGdkDisplayManagerClass = record
     parent_class : TGObjectClass;
     display_opened: procedure(display_manager: PGdkDisplayManager; display: PGdkDisplay); cdecl;
   end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GDK_TYPE_DISPLAY_MANAGER: GType;
function GDK_DISPLAY_MANAGER(anObject: pointer): PGdkDisplayManager;
function GDK_DISPLAY_MANAGER_CLASS(klass: pointer): PGdkDisplayManagerClass;
function GDK_IS_DISPLAY_MANAGER(anObject: pointer): Boolean;
function GDK_IS_DISPLAY_MANAGER_CLASS(klass: pointer): Boolean;
function GDK_DISPLAY_MANAGER_GET_CLASS(obj: pointer): PGdkDisplayManagerClass;

//function gdk_display_manager_get_type: GType; cdecl; external gdklib;

function gdk_display_manager_get: PGdkDisplayManager; cdecl; external gdklib;
function gdk_display_manager_get_default_display(display_manager: PGdkDisplayManager): PGdkDisplay; cdecl; external gdklib;
procedure gdk_display_manager_set_default_display(display_manager: PGdkDisplayManager; display: PGdkDisplay); cdecl; external gdklib;
function gdk_display_manager_list_displays(display_manager: PGdkDisplayManager): PGSList; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_DISPLAY_MANAGER: GType;
begin
  GDK_TYPE_DISPLAY_MANAGER := 0; //gdk_display_manager_get_type;
end;

function GDK_DISPLAY_MANAGER(anObject : pointer) : PGdkDisplayManager;
begin
  GDK_DISPLAY_MANAGER:=PGdkDisplayManager(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                             GDK_TYPE_DISPLAY_MANAGER));
end;

function GDK_DISPLAY_MANAGER_CLASS(klass : pointer) : PGdkDisplayManagerClass;
begin
  GDK_DISPLAY_MANAGER_CLASS:=PGdkDisplayManagerClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                             GDK_TYPE_DISPLAY_MANAGER));
end;

function GDK_IS_DISPLAY_MANAGER(anObject : pointer) : boolean;
begin
  GDK_IS_DISPLAY_MANAGER:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_DISPLAY_MANAGER);
end;

function GDK_IS_DISPLAY_MANAGER_CLASS(klass : pointer) : boolean;
begin
  GDK_IS_DISPLAY_MANAGER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DISPLAY_MANAGER);
end;

function GDK_DISPLAY_MANAGER_GET_CLASS(obj : pointer) : PGdkDisplayManagerClass;
begin
  GDK_DISPLAY_MANAGER_GET_CLASS:=PGdkDisplayManagerClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                             GDK_TYPE_DISPLAY_MANAGER));
end;

{$ENDIF}
