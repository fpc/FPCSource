// included by atk.pp

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkObjectFactory = ^TAtkObjectFactory;
   TAtkObjectFactory = record
        parent : TGObject;
     end;

   PAtkObjectFactoryClass = ^TAtkObjectFactoryClass;
   TAtkObjectFactoryClass = record
        parent_class : TGObjectClass;
        create_accessible : function (obj:PGObject):PAtkObject; cdecl;
        invalidate : procedure (factory:PAtkObjectFactory); cdecl;
        get_accessible_type : function :GType; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_OBJECT_FACTORY : GType;
function ATK_OBJECT_FACTORY(obj: pointer) : PAtkObjectFactory;
function ATK_OBJECT_FACTORY_CLASS(klass: pointer) : PAtkObjectFactoryClass;
function ATK_IS_OBJECT_FACTORY(obj: pointer) : boolean;
function ATK_IS_OBJECT_FACTORY_CLASS(klass: pointer) : boolean;
function ATK_OBJECT_FACTORY_GET_CLASS(obj: pointer) : PAtkObjectFactoryClass;


function atk_object_factory_get_type:GType; cdecl; external atklib;
function atk_object_factory_create_accessible(factory:PAtkObjectFactory; obj:PGObject):PAtkObject; cdecl; external atklib;
procedure atk_object_factory_invalidate(factory:PAtkObjectFactory); cdecl; external atklib;
function atk_object_factory_get_accessible_type(factory:PAtkObjectFactory):GType; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_OBJECT_FACTORY : GType;
begin
   ATK_TYPE_OBJECT_FACTORY:=atk_object_factory_get_type;
end;

function ATK_OBJECT_FACTORY(obj: pointer) : PAtkObjectFactory;
begin
   ATK_OBJECT_FACTORY:=PAtkObjectFactory(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_OBJECT_FACTORY));
end;

function ATK_OBJECT_FACTORY_CLASS(klass: pointer) : PAtkObjectFactoryClass;
begin
   ATK_OBJECT_FACTORY_CLASS:=PAtkObjectFactoryClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_OBJECT_FACTORY));
end;

function ATK_IS_OBJECT_FACTORY(obj: pointer) : boolean;
begin
   ATK_IS_OBJECT_FACTORY:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_OBJECT_FACTORY);
end;

function ATK_IS_OBJECT_FACTORY_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_OBJECT_FACTORY_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_OBJECT_FACTORY);
end;

function ATK_OBJECT_FACTORY_GET_CLASS(obj: pointer) : PAtkObjectFactoryClass;
begin
   ATK_OBJECT_FACTORY_GET_CLASS:=PAtkObjectFactoryClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_OBJECT_FACTORY));
end;

{$ENDIF read_implementation}

