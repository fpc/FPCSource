// included by atk.pp

{$IFDEF read_forward_definitions}
   PAtkStateSet = ^TAtkStateSet;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TAtkStateSet = record
        parent : TGObject;
     end;

   PAtkStateSetClass = ^TAtkStateSetClass;
   TAtkStateSetClass = record
        parent : TGObjectClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_STATE_SET : GType;
function ATK_STATE_SET(obj: pointer) : PAtkStateSet;
function ATK_STATE_SET_CLASS(klass: pointer) : PAtkStateSetClass;
function ATK_IS_STATE_SET(obj: pointer) : boolean;
function ATK_IS_STATE_SET_CLASS(klass: pointer) : boolean;
function ATK_STATE_SET_GET_CLASS(obj: pointer) : PAtkStateSetClass;


function atk_state_set_get_type:GType; cdecl; external atklib;
function atk_state_set_new:PAtkStateSet; cdecl; external atklib;
function atk_state_set_is_empty(StateSet:PAtkStateSet):gboolean; cdecl; external atklib;
function atk_state_set_add_state(StateSet:PAtkStateSet; _type:TAtkStateType):gboolean; cdecl; external atklib;
procedure atk_state_set_add_states(StateSet:PAtkStateSet; types:PAtkStateType; n_types:gint); cdecl; external atklib;
procedure atk_state_set_clear_states(StateSet:PAtkStateSet); cdecl; external atklib;
function atk_state_set_contains_state(StateSet:PAtkStateSet; _type:TAtkStateType):gboolean; cdecl; external atklib;
function atk_state_set_contains_states(StateSet:PAtkStateSet; types:PAtkStateType; n_types:gint):gboolean; cdecl; external atklib;
function atk_state_set_remove_state(StateSet:PAtkStateSet; _type:TAtkStateType):gboolean; cdecl; external atklib;
function atk_state_set_and_sets(StateSet:PAtkStateSet; compare_set:PAtkStateSet):PAtkStateSet; cdecl; external atklib;
function atk_state_set_or_sets(StateSet:PAtkStateSet; compare_set:PAtkStateSet):PAtkStateSet; cdecl; external atklib;
function atk_state_set_xor_sets(StateSet:PAtkStateSet; compare_set:PAtkStateSet):PAtkStateSet; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_STATE_SET : GType;
begin
   ATK_TYPE_STATE_SET:=atk_state_set_get_type;
end;

function ATK_STATE_SET(obj: pointer) : PAtkStateSet;
begin
   ATK_STATE_SET:=PAtkStateSet(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_STATE_SET));
end;

function ATK_STATE_SET_CLASS(klass: pointer) : PAtkStateSetClass;
begin
   ATK_STATE_SET_CLASS:=PAtkStateSetClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_STATE_SET));
end;

function ATK_IS_STATE_SET(obj: pointer) : boolean;
begin
   ATK_IS_STATE_SET:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_STATE_SET);
end;

function ATK_IS_STATE_SET_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_STATE_SET_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_STATE_SET);
end;

function ATK_STATE_SET_GET_CLASS(obj: pointer) : PAtkStateSetClass;
begin
   ATK_STATE_SET_GET_CLASS:=PAtkStateSetClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_STATE_SET));
end;

{$ENDIF read_implementation}

