// included by gdk2.pp

{$IFDEF read_forward_definitions}
   PGdkDisplay = ^TGdkDisplay;
   PGdkDisplayPointerHooks = ^TGdkDisplayPointerHooks;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< private > }
{ Information for determining if the latest button click
     is part of a double-click or triple-click
    }
{ The last 2 button click times.  }
{ The last 2 windows to receive button presses.  }
{ The last 2 buttons to be pressed.  }
{ Maximum time between clicks in msecs  }
   TGdkDisplay = record
        parent_instance : TGObject;
        queued_events : PGList;
        queued_tail : PGList;
        button_click_time : array[0..1] of guint32;
        button_window : array[0..1] of PGdkWindow;
        button_number : array[0..1] of guint;
        double_click_time : guint;
        core_pointer: PGdkDevice;
        pointer_hooks: PGdkDisplayPointerHooks;  
        closed: guint;
        double_click_distance: guint;
        button_x: array[0..1] of guint;
        button_y: array[0..1] of guint;
     end;

   PGdkDisplayClass = ^TGdkDisplayClass;
   TGdkDisplayClass = record
        parent_class : TGObjectClass;
        get_display_name : function (display:PGdkDisplay):Pgchar; cdecl;
        get_n_screens : function (display:PGdkDisplay):gint; cdecl;
        get_screen : function (display:PGdkDisplay; screen_num:gint):PGdkScreen; cdecl;
        get_default_screen : function (display:PGdkDisplay):PGdkScreen; cdecl;
     end;

   TGdkDisplayPointerHooks = record
      get_pointer: procedure(display: PGdkDisplay; screen: PPGdkScreen; x: Pgint; y: Pgint; mask: PGdkModifierType); cdecl;
      window_get_pointer: function(display: PGdkDisplay; window: PGdkWindow; x: Pgint; y: Pgint; mask: PGdkModifierType): PGdkWindow; cdecl;
      window_at_pointer: function(display: PGdkDisplay; win_x: Pgint; win_y: Pgint): PGdkWindow; cdecl;
    end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GDK_TYPE_DISPLAY : GType;
function GDK_DISPLAY_OBJECT(anObject : pointer) : PGdkDisplay;
function GDK_DISPLAY_CLASS(klass : pointer) : PGdkDisplayClass;
function GDK_IS_DISPLAY(anObject : pointer) : boolean;
function GDK_IS_DISPLAY_CLASS(klass : pointer) : boolean;
function GDK_DISPLAY_GET_CLASS(obj : pointer) : PGdkDisplayClass;

//function gdk_display_get_type:GType; cdecl; external gdklib;
function gdk_display_get_default: PGdkDisplay; cdecl; external gdklib;
function gdk_display_open(display_name:Pgchar):PGdkDisplay; cdecl; external gdklib;
function gdk_display_get_name(display:PGdkDisplay):Pgchar; cdecl; external gdklib;
function gdk_display_get_n_screens(display:PGdkDisplay):gint; cdecl; external gdklib;
function gdk_display_get_screen(display:PGdkDisplay; screen_num:gint):PGdkScreen; cdecl; external gdklib;
function gdk_display_get_default_screen(display:PGdkDisplay):PGdkScreen; cdecl; external gdklib;
procedure gdk_display_pointer_ungrab(display:PGdkDisplay; time:guint32); cdecl; external gdklib;
procedure gdk_display_keyboard_ungrab(display:PGdkDisplay; time:guint32); cdecl; external gdklib;
function gdk_display_pointer_is_grabbed(display:PGdkDisplay):gboolean; cdecl; external gdklib;
procedure gdk_display_beep(display:PGdkDisplay); cdecl; external gdklib;
procedure gdk_display_sync(display:PGdkDisplay); cdecl; external gdklib;
procedure gdk_display_close(display:PGdkDisplay); cdecl; external gdklib;
function gdk_display_list_devices(display:PGdkDisplay):PGList; cdecl; external gdklib;
function gdk_display_get_event(display:PGdkDisplay):PGdkEvent; cdecl; external gdklib;
function gdk_display_peek_event(display:PGdkDisplay):PGdkEvent; cdecl; external gdklib;
procedure gdk_display_put_event(display:PGdkDisplay; event:PGdkEvent); cdecl; external gdklib;
procedure gdk_display_add_client_message_filter(display:PGdkDisplay; message_type:TGdkAtom; func:TGdkFilterFunc; data:gpointer); cdecl; external gdklib;
procedure gdk_display_set_double_click_time(display:PGdkDisplay; msec:guint); cdecl; external gdklib;
procedure gdk_display_set_sm_client_id(display:PGdkDisplay; sm_client_id:Pgchar); cdecl; external gdklib;
procedure gdk_set_default_display(display:PGdkDisplay); cdecl; external gdklib;
function gdk_get_default_display:PGdkDisplay; cdecl; external gdklib;
function gdk_display_get_core_pointer(display:PGdkDisplay): PGdkDevice; cdecl; external gdklib;
procedure gdk_display_get_pointer(display:PGdkDisplay; screen :PGdkScreen; x :Pgint; y : Pgint; mask : PGdkModifierType); cdecl; external gdklib;
function gdk_display_get_window_at_pointer(display:PGdkDisplay; win_x: Pgint; win_y: Pgint): PGdkWindow; cdecl; external gdklib;
function gdk_display_set_pointer_hooks(display: PGdkDisplay; new_hooks: PGdkDisplayPointerHooks): PGdkDisplayPointerHooks; cdecl; external gdklib;
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_DISPLAY : GType;
begin
  GDK_TYPE_DISPLAY := 0; //gdk_display_get_type;
end;

function GDK_DISPLAY_OBJECT(anObject : pointer) : PGdkDisplay;
begin
  GDK_DISPLAY_OBJECT:=PGdkDisplay(G_TYPE_CHECK_INSTANCE_CAST(anObject,
                                                             GDK_TYPE_DISPLAY));
end;

function GDK_DISPLAY_CLASS(klass : pointer) : PGdkDisplayClass;
begin
  GDK_DISPLAY_CLASS:=PGdkDisplayClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                             GDK_TYPE_DISPLAY));
end;

function GDK_IS_DISPLAY(anObject : pointer) : boolean;
begin
  GDK_IS_DISPLAY:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_DISPLAY);
end;

function GDK_IS_DISPLAY_CLASS(klass : pointer) : boolean;
begin
  GDK_IS_DISPLAY_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_DISPLAY);
end;

function GDK_DISPLAY_GET_CLASS(obj : pointer) : PGdkDisplayClass;
begin
  GDK_DISPLAY_GET_CLASS:=PGdkDisplayClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                             GDK_TYPE_DISPLAY));
end;

{$ENDIF}
