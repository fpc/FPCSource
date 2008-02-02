// included by atk.pp

{
   The AtkValue interface should be supported by any anObject that
   supports a numerical value (e.g., a scroll bar). This interface
   provides the standard mechanism for an assistive technology to
   determine and set the numerical value as well as get the minimum
   and maximum values.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkValueIface = ^TAtkValueIface;
   TAtkValueIface = record
        parent : TGTypeInterface;
        get_current_value : procedure (obj:PAtkValue; value:PGValue); cdecl;
        get_maximum_value : procedure (obj:PAtkValue; value:PGValue); cdecl;
        get_minimum_value : procedure (obj:PAtkValue; value:PGValue); cdecl;
        set_current_value : function (obj:PAtkValue; value:PGValue):gboolean; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_VALUE : GType;
function ATK_IS_VALUE(obj: pointer) : boolean;
function ATK_VALUE(obj: pointer) : PAtkValue;
function ATK_VALUE_GET_IFACE(obj: pointer) : PAtkValueIface;


function atk_value_get_type:GType; cdecl; external atklib;
procedure atk_value_get_current_value(obj:PAtkValue; value:PGValue); cdecl; external atklib;
procedure atk_value_get_maximum_value(obj:PAtkValue; value:PGValue); cdecl; external atklib;
procedure atk_value_get_minimum_value(obj:PAtkValue; value:PGValue); cdecl; external atklib;
function atk_value_set_current_value(obj:PAtkValue; value:PGValue):gboolean; cdecl; external atklib;
{
   Additional GObject properties exported by GaccessibleValue:
      "accessible_value"
         (the accessible value has changed)
  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_VALUE : GType;
begin
   ATK_TYPE_VALUE:=atk_value_get_type;
end;

function ATK_IS_VALUE(obj: pointer) : boolean;
begin
   ATK_IS_VALUE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_VALUE);
end;

function ATK_VALUE(obj: pointer) : PAtkValue;
begin
   ATK_VALUE:=PAtkValue(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_VALUE));
end;

function ATK_VALUE_GET_IFACE(obj: pointer) : PAtkValueIface;
begin
   ATK_VALUE_GET_IFACE:=PAtkValueIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_VALUE));
end;

{$ENDIF read_implementation}

