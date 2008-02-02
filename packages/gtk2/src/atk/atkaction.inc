// included by atk.pp

{
   The interface AtkAction should be supported by any anObject that can
   perform one or more actions. The interface provides the standard
   mechanism for an assistive technology to determine what those actions
   are as well as tell the anObject to perform them. Any anObject that can
   be manipulated should support this interface.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkActionIface = ^TAtkActionIface;
   TAtkActionIface = record
        parent : TGTypeInterface;
        do_action : function (action:PAtkAction; i:gint):gboolean; cdecl;
        get_n_actions : function (action:PAtkAction):gint; cdecl;
        get_description : function (action:PAtkAction; i:gint):Pgchar; cdecl;
        get_name : function (action:PAtkAction; i:gint):Pgchar; cdecl;
        get_keybinding : function (action:PAtkAction; i:gint):Pgchar; cdecl;
        set_description : function (action:PAtkAction; i:gint; desc:Pgchar):gboolean; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_ACTION : GType;
function ATK_IS_ACTION(obj: pointer) : boolean;
function ATK_ACTION(obj: pointer) : PAtkAction;
function ATK_ACTION_GET_IFACE(obj: pointer) : PAtkActionIface;


{
   These are the function which would be called by an application with
   the argument being a AtkObject anObject cast to (AtkAction).

   The function will just check that   the corresponding
   function pointer is not NULL and will call it.

   The "real" implementation of the function for accessible will be
   provided in a support library
  }
function atk_action_get_type:GType; cdecl; external atklib;
function atk_action_do_action(action:PAtkAction; i:gint):gboolean; cdecl; external atklib;
function atk_action_get_n_actions(action:PAtkAction):gint; cdecl; external atklib;
function atk_action_get_description(action:PAtkAction; i:gint):Pgchar; cdecl; external atklib;
function atk_action_get_name(action:PAtkAction; i:gint):Pgchar; cdecl; external atklib;
function atk_action_get_keybinding(action:PAtkAction; i:gint):Pgchar; cdecl; external atklib;
function atk_action_set_description(action:PAtkAction; i:gint; desc:Pgchar):gboolean; cdecl; external atklib;
{
   Additional GObject properties exported by AtkAction:
      "accessible_action"
         (an accessible action, or the list of actions, has changed)
  }
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_ACTION : GType;
begin
   ATK_TYPE_ACTION:=atk_action_get_type;
end;

function ATK_IS_ACTION(obj: pointer) : boolean;
begin
   ATK_IS_ACTION:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_ACTION);
end;

function ATK_ACTION(obj: pointer) : PAtkAction;
begin
   ATK_ACTION:=PAtkAction(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_ACTION));
end;

function ATK_ACTION_GET_IFACE(obj: pointer) : PAtkActionIface;
begin
   ATK_ACTION_GET_IFACE:=PAtkActionIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_ACTION));
end;

{$ENDIF read_implementation}

