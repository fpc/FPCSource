// included by gdk2.pp

{$IFDEF read_forward_definitions}
   PGdkDevice = ^TGdkDevice;
   PGdkTimeCoord = ^TGdkTimeCoord;
   PPGdkTimeCoord = ^PGdkTimeCoord;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGdkExtensionMode = ^TGdkExtensionMode;
   TGdkExtensionMode = (
     GDK_EXTENSION_EVENTS_NONE,
     GDK_EXTENSION_EVENTS_ALL,
     GDK_EXTENSION_EVENTS_CURSOR
   );

   PGdkInputSource = ^TGdkInputSource;
   TGdkInputSource = (
     GDK_SOURCE_MOUSE,
     GDK_SOURCE_PEN,
     GDK_SOURCE_ERASER,
     GDK_SOURCE_CURSOR
   );

   PGdkInputMode = ^TGdkInputMode;
   TGdkInputMode = (
     GDK_MODE_DISABLED,
     GDK_MODE_SCREEN,
     GDK_MODE_WINDOW
     );

   PGdkAxisUse = ^TGdkAxisUse;
   TGdkAxisUse = longint;

   PGdkDeviceKey = ^TGdkDeviceKey;
   TGdkDeviceKey = record
        keyval : guint;
        modifiers : TGdkModifierType;
     end;

   PGdkDeviceAxis = ^TGdkDeviceAxis;
   TGdkDeviceAxis = record
        use : TGdkAxisUse;
        min : gdouble;
        max : gdouble;
     end;

{ All fields are read-only  }
{ TRUE if the X pointer follows device motion  }
   TGdkDevice = record
        parent_instance : TGObject;
        name : Pgchar;
        source : TGdkInputSource;
        mode : TGdkInputMode;
        has_cursor : gboolean;
        num_axes : gint;
        axes : PGdkDeviceAxis;
        num_keys : gint;
        keys : PGdkDeviceKey;
     end;

{ We don't allocate each coordinate this big, but we use it to
   be ANSI compliant and avoid accessing past the defined limits.
  }

   TGdkTimeCoord = record
        time : guint32;
        axes : array[0..(GDK_MAX_TIMECOORD_AXES)-1] of gdouble;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GDK_AXIS_IGNORE = 0;
   GDK_AXIS_X = 1;
   GDK_AXIS_Y = 2;
   GDK_AXIS_PRESSURE = 3;
   GDK_AXIS_XTILT = 4;
   GDK_AXIS_YTILT = 5;
   GDK_AXIS_WHEEL = 6;
   GDK_AXIS_LAST = 7;


function GDK_TYPE_DEVICE : GType;
function GDK_DEVICE(anObject : Pointer) : PGdkDevice;
function GDK_DEVICE_CLASS(klass : Pointer) : PGdkDeviceClass;
function GDK_IS_DEVICE(anObject : Pointer) : boolean;
function GDK_IS_DEVICE_CLASS(klass : Pointer) : boolean;
function GDK_DEVICE_GET_CLASS(obj : Pointer) : PGdkDeviceClass;


function gdk_device_get_type:GType; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
{ Returns a list of GdkDevice    }
function gdk_devices_list:PGList; cdecl; external gdklib;
{$endif}
{ GDK_MULTIHEAD_SAFE  }


{ Functions to configure a device  }

procedure gdk_device_set_source(device:PGdkDevice; source:TGdkInputSource); cdecl; external gdklib;
function gdk_device_set_mode(device:PGdkDevice; mode:TGdkInputMode):gboolean; cdecl; external gdklib;
procedure gdk_device_set_key(device:PGdkDevice; index:guint; keyval:guint; modifiers:TGdkModifierType); cdecl; external gdklib;
procedure gdk_device_set_axis_use(device:PGdkDevice; index:guint; use:TGdkAxisUse); cdecl; external gdklib;
procedure gdk_device_get_state(device:PGdkDevice; window:PGdkWindow; axes:Pgdouble; mask:PGdkModifierType); cdecl; external gdklib;
function gdk_device_get_history(device:PGdkDevice; window:PGdkWindow; start:guint32; stop:guint32; var events:PPGdkTimeCoord;
           n_events:Pgint):gboolean; cdecl; external gdklib;
procedure gdk_device_free_history(events:PPGdkTimeCoord; n_events:gint); cdecl; external gdklib;
function gdk_device_get_axis(device:PGdkDevice; axes:Pgdouble; use:TGdkAxisUse; value:Pgdouble):gboolean; cdecl; external gdklib;
procedure gdk_input_set_extension_events(window:PGdkWindow; mask:gint; mode:TGdkExtensionMode); cdecl; external gdklib;
function gdk_device_get_core_pointer:PGdkDevice; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_DEVICE : GType;
begin
  GDK_TYPE_DEVICE:=gdk_device_get_type;
end;

function GDK_DEVICE(anObject : Pointer) : PGdkDevice;
begin
  GDK_DEVICE:=PGdkDevice(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_DEVICE));
end;

function GDK_DEVICE_CLASS(klass : Pointer) : PGdkDeviceClass;
begin
  GDK_DEVICE_CLASS:=PGdkDeviceClass(G_TYPE_CHECK_CLASS_CAST(klass,GDK_TYPE_DEVICE));
end;

function GDK_IS_DEVICE(anObject : Pointer) : boolean;
begin
  GDK_IS_DEVICE:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_DEVICE);
end;

function GDK_IS_DEVICE_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_DEVICE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DEVICE);
end;

function GDK_DEVICE_GET_CLASS(obj : Pointer) : PGdkDeviceClass;
begin
  GDK_DEVICE_GET_CLASS:=PGdkDeviceClass(G_TYPE_INSTANCE_GET_CLASS(obj,GDK_TYPE_DEVICE));
end;
{$ENDIF}

