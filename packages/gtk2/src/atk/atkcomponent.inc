// included by atk.pp

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TAtkFocusHandler = procedure (para1:PAtkObject; para2:gboolean); cdecl;
   PAtkComponentIface = ^TAtkComponentIface;
   TAtkComponentIface = record
        parent : TGTypeInterface;
        add_focus_handler : function (component:PAtkComponent; handler:TAtkFocusHandler):guint; cdecl;
        contains : function (component:PAtkComponent; x:gint; y:gint; coord_type:TAtkCoordType):gboolean; cdecl;
        ref_accessible_at_point : function (component:PAtkComponent; x:gint; y:gint; coord_type:TAtkCoordType):PAtkObject; cdecl;
        get_extents : procedure (component:PAtkComponent; x:Pgint; y:Pgint; width:Pgint; height:Pgint;
                      coord_type:TAtkCoordType); cdecl;
        get_position : procedure (component:PAtkComponent; x:Pgint; y:Pgint; coord_type:TAtkCoordType); cdecl;
        get_size : procedure (component:PAtkComponent; width:Pgint; height:Pgint); cdecl;
        grab_focus : function (component:PAtkComponent):gboolean; cdecl;
        remove_focus_handler : procedure (component:PAtkComponent; handler_id:guint); cdecl;
        set_extents : function (component:PAtkComponent; x:gint; y:gint; width:gint; height:gint;
                     coord_type:TAtkCoordType):gboolean; cdecl;
        set_position : function (component:PAtkComponent; x:gint; y:gint; coord_type:TAtkCoordType):gboolean; cdecl;
        set_size : function (component:PAtkComponent; width:gint; height:gint):gboolean; cdecl;
        get_layer : function (component:PAtkComponent):TAtkLayer; cdecl;
        get_mdi_zorder : function (component:PAtkComponent):gint; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{
   The AtkComponent interface should be supported by any anObject that is
   rendered on the screen. The interface provides the standard mechanism
   for an assistive technology to determine and set the graphical
   representation of an anObject.
  }
function ATK_TYPE_COMPONENT : GType;
function ATK_IS_COMPONENT(obj: pointer) : boolean;
function ATK_COMPONENT(obj: pointer) : PAtkComponent;
function ATK_COMPONENT_GET_IFACE(obj: pointer) : PAtkComponentIface;


{ convenience functions  }
function atk_component_get_type:GType; cdecl; external atklib;
function atk_component_add_focus_handler(component:PAtkComponent; handler:TAtkFocusHandler):guint; cdecl; external atklib;
function atk_component_contains(component:PAtkComponent; x:gint; y:gint; coord_type:TAtkCoordType):gboolean; cdecl; external atklib;
function atk_component_ref_accessible_at_point(component:PAtkComponent; x:gint; y:gint; coord_type:TAtkCoordType):PAtkObject; cdecl; external atklib;
procedure atk_component_get_extents(component:PAtkComponent; x:Pgint; y:Pgint; width:Pgint; height:Pgint;
            coord_type:TAtkCoordType); cdecl; external atklib;
procedure atk_component_get_position(component:PAtkComponent; x:Pgint; y:Pgint; coord_type:TAtkCoordType); cdecl; external atklib;
procedure atk_component_get_size(component:PAtkComponent; width:Pgint; height:Pgint); cdecl; external atklib;
function atk_component_get_layer(component:PAtkComponent):TAtkLayer; cdecl; external atklib;
function atk_component_get_mdi_zorder(component:PAtkComponent):gint; cdecl; external atklib;
function atk_component_grab_focus(component:PAtkComponent):gboolean; cdecl; external atklib;
procedure atk_component_remove_focus_handler(component:PAtkComponent; handler_id:guint); cdecl; external atklib;
function atk_component_set_extents(component:PAtkComponent; x:gint; y:gint; width:gint; height:gint;
           coord_type:TAtkCoordType):gboolean; cdecl; external atklib;
function atk_component_set_position(component:PAtkComponent; x:gint; y:gint; coord_type:TAtkCoordType):gboolean; cdecl; external atklib;
function atk_component_set_size(component:PAtkComponent; width:gint; height:gint):gboolean; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_COMPONENT : GType;
begin
   ATK_TYPE_COMPONENT:=atk_component_get_type;
end;

function ATK_IS_COMPONENT(obj: pointer) : boolean;
begin
   ATK_IS_COMPONENT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_COMPONENT);
end;

function ATK_COMPONENT(obj: pointer) : PAtkComponent;
begin
   ATK_COMPONENT:=PAtkComponent(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_COMPONENT));
end;

function ATK_COMPONENT_GET_IFACE(obj: pointer) : PAtkComponentIface;
begin
   ATK_COMPONENT_GET_IFACE:=PAtkComponentIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_COMPONENT));
end;

{$ENDIF read_implementation}

