// included by atk.pp

{$IFDEF read_forward_definitions}
{
  AtkCoordType:
  @ATK_XY_SCREEN: specifies xy coordinates relative to the screen
  @ATK_XY_WINDOW: specifies xy coordinates relative to the widgets
   top-level window

  Specifies how xy coordinates are to be interpreted. Used by functions such
  as atk_component_get_position() and atk_text_get_character_extents()
   }
   PAtkCoordType = ^TAtkCoordType;
   TAtkCoordType = (
     ATK_XY_SCREEN,
     ATK_XY_WINDOW
   );
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{
   A focus tracker is a function which is called when an anObject
   receives focus.
  }
   TAtkEventListener = procedure (para1:PAtkObject); cdecl;

   TAtkEventListenerInitProc = procedure;
   TAtkEventListenerInit = procedure (para1: TAtkEventListenerInitProc); cdecl;

   PAtkKeyEventStruct = ^TAtkKeyEventStruct;
   TAtkKeyEventStruct = record
        _type : gint;
        state : guint;
        keyval : guint;
        length : gint;
        _string : Pgchar;
        keycode : guint16;
        timestamp : guint32;
     end;
   TAtkKeySnoopFunc = function (event:PAtkKeyEventStruct; func_data:gpointer):gint; cdecl;


   PAtkKeyEventType = ^TAtkKeyEventType;
   TAtkKeyEventType = (
     ATK_KEY_EVENT_PRESS,
     ATK_KEY_EVENT_RELEASE,
     ATK_KEY_EVENT_LAST_DEFINED
   );

   PAtkUtil = ^TAtkUtil;
   TAtkUtil = record
        parent : TGObject;
     end;

   PAtkUtilClass = ^TAtkUtilClass;
   TAtkUtilClass = record
        parent : TGObjectClass;
        add_global_event_listener : function (listener:TGSignalEmissionHook; event_type:Pgchar):guint; cdecl;
        remove_global_event_listener : procedure (listener_id:guint); cdecl;
        add_key_event_listener : function (listener:TAtkKeySnoopFunc; data:gpointer):guint; cdecl;
        remove_key_event_listener : procedure (listener_id:guint); cdecl;
        get_root : function :PAtkObject; cdecl;
        get_toolkit_name : function :Pgchar; cdecl;
        get_toolkit_version : function :Pgchar; cdecl;
     end;



{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_UTIL : GType;
function ATK_IS_UTIL(obj: pointer) : boolean;
function ATK_UTIL(obj: pointer) : PAtkUtil;
function ATK_UTIL_CLASS(klass: pointer) : PAtkUtilClass;
function ATK_IS_UTIL_CLASS(klass: pointer) : boolean;
function ATK_UTIL_GET_CLASS(obj: pointer) : PAtkUtilClass;


function atk_util_get_type:GType; cdecl; external atklib;

{
   Adds the specified function to the list of functions to be called
   when an anObject receives focus.
  }

function atk_add_focus_tracker(focus_tracker:TAtkEventListener):guint; cdecl; external atklib;
{
   Removes the specified focus tracker from the list of function
   to be called when any anObject receives focus
  }
procedure atk_remove_focus_tracker(tracker_id:guint); cdecl; external atklib;
{
   Specifies the function to be called for focus tracker initialization.
   removal. This function should be called by an implementation of the
   ATK interface if any specific work needs to be done to enable
   focus tracking.
  }
procedure atk_focus_tracker_init(add_function:TAtkEventListenerInit); cdecl; external atklib;
{
   Cause the focus tracker functions which have been specified to be
   executed for the anObject.
  }
procedure atk_focus_tracker_notify(anObject:PAtkObject); cdecl; external atklib;
{
   Adds the specified function to the list of functions to be called
   when an event of type event_type occurs.
  }
function atk_add_global_event_listener(listener:TGSignalEmissionHook; event_type:Pgchar):guint; cdecl; external atklib;
{
   Removes the specified event listener
  }
procedure atk_remove_global_event_listener(listener_id:guint); cdecl; external atklib;
{
   Adds the specified function to the list of functions to be called
   when an keyboard event occurs.
  }
function atk_add_key_event_listener(listener:TAtkKeySnoopFunc; data:gpointer):guint; cdecl; external atklib;
{
   Removes the specified event listener
  }
procedure atk_remove_key_event_listener(listener_id:guint); cdecl; external atklib;
{
   Returns the root accessible container for the current application.
  }
function atk_get_root:PAtkObject; cdecl; external atklib;
{
   Returns name string for the GUI toolkit.
  }
function atk_get_toolkit_name:Pgchar; cdecl; external atklib;
{
   Returns version string for the GUI toolkit.
  }
function atk_get_toolkit_version:Pgchar; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_UTIL : GType;
begin
   ATK_TYPE_UTIL:=atk_util_get_type;
end;

function ATK_IS_UTIL(obj: pointer) : boolean;
begin
   ATK_IS_UTIL:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_UTIL);
end;

function ATK_UTIL(obj: pointer) : PAtkUtil;
begin
   ATK_UTIL:=PAtkUtil(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_UTIL));
end;

function ATK_UTIL_CLASS(klass: pointer) : PAtkUtilClass;
begin
   ATK_UTIL_CLASS:=PAtkUtilClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_UTIL));
end;

function ATK_IS_UTIL_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_UTIL_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_UTIL);
end;

function ATK_UTIL_GET_CLASS(obj: pointer) : PAtkUtilClass;
begin
   ATK_UTIL_GET_CLASS:=PAtkUtilClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_UTIL));
end;

{$ENDIF read_implementation}

