// included by atk.pp

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkRegistry = ^TAtkRegistry;
   TAtkRegistry = record
        parent : TGObject;
        factory_type_registry : PGHashTable;
        factory_singleton_cache : PGHashTable;
     end;

   PAtkRegistryClass = ^TAtkRegistryClass;
   TAtkRegistryClass = record
        parent_class : TGObjectClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_REGISTRY : GType;
function ATK_REGISTRY(obj: pointer) : PAtkRegistry;
function ATK_REGISTRY_CLASS(klass: pointer) : PAtkRegistryClass;
function ATK_IS_REGISTRY(obj: pointer) : boolean;
function ATK_IS_REGISTRY_CLASS(klass: pointer) : boolean;
function ATK_REGISTRY_GET_CLASS(obj: pointer) : PAtkRegistryClass;


function atk_registry_get_type:GType; cdecl; external atklib;
procedure atk_registry_set_factory_type(registry:PAtkRegistry; _type:GType; factory_type:GType); cdecl; external atklib;
function atk_registry_get_factory_type(registry:PAtkRegistry; _type:GType):GType; cdecl; external atklib;
function atk_registry_get_factory(registry:PAtkRegistry; _type:GType):PAtkObjectFactory; cdecl; external atklib;
function atk_get_default_registry:PAtkRegistry; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_REGISTRY : GType;
begin
   ATK_TYPE_REGISTRY:=atk_registry_get_type;
end;

function ATK_REGISTRY(obj: pointer) : PAtkRegistry;
begin
   ATK_REGISTRY:=PAtkRegistry(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_REGISTRY));
end;

function ATK_REGISTRY_CLASS(klass: pointer) : PAtkRegistryClass;
begin
   ATK_REGISTRY_CLASS:=PAtkRegistryClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_REGISTRY));
end;

function ATK_IS_REGISTRY(obj: pointer) : boolean;
begin
   ATK_IS_REGISTRY:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_REGISTRY);
end;

function ATK_IS_REGISTRY_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_REGISTRY_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_REGISTRY);
end;

function ATK_REGISTRY_GET_CLASS(obj: pointer) : PAtkRegistryClass;
begin
   ATK_REGISTRY_GET_CLASS:=PAtkRegistryClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_REGISTRY));
end;

{$ENDIF read_implementation}

