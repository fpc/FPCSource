// included by atk.pp

{
   The AtkGObjectAccessible class is provided as a basis for implementing
   accessibility support for objects which are not GTK+ widgets
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkGObjectAccessible = ^TAtkGObjectAccessible;
   TAtkGObjectAccessible = record
        parent : TAtkObject;
     end;

   PAtkGObjectAccessibleClass = ^TAtkGObjectAccessibleClass;
   TAtkGObjectAccessibleClass = record
        parent_class : TAtkObjectClass;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_GOBJECT_ACCESSIBLE : GType;
function ATK_GOBJECT_ACCESSIBLE(obj: pointer) : PAtkGObjectAccessible;
function ATK_GOBJECT_ACCESSIBLE_CLASS(klass: pointer) : PAtkGObjectAccessibleClass;
function ATK_IS_GOBJECT_ACCESSIBLE(obj: pointer) : boolean;
function ATK_IS_GOBJECT_ACCESSIBLE_CLASS(klass: pointer) : boolean;
function ATK_GOBJECT_ACCESSIBLE_GET_CLASS(obj: pointer) : PAtkGObjectAccessibleClass;


function atk_gobject_accessible_get_type:GType; cdecl; external atklib;

function atk_gobject_accessible_for_object(obj:PGObject):PAtkObject; cdecl; external atklib;
function atk_gobject_accessible_get_object(obj:PAtkGObjectAccessible):PGObject; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_GOBJECT_ACCESSIBLE : GType;
begin
   ATK_TYPE_GOBJECT_ACCESSIBLE:=atk_gobject_accessible_get_type;
end;

function ATK_GOBJECT_ACCESSIBLE(obj: pointer) : PAtkGObjectAccessible;
begin
   ATK_GOBJECT_ACCESSIBLE:=PAtkGObjectAccessible(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_GOBJECT_ACCESSIBLE));
end;

function ATK_GOBJECT_ACCESSIBLE_CLASS(klass: pointer) : PAtkGObjectAccessibleClass;
begin
   ATK_GOBJECT_ACCESSIBLE_CLASS:=PAtkGObjectAccessibleClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_GOBJECT_ACCESSIBLE));
end;

function ATK_IS_GOBJECT_ACCESSIBLE(obj: pointer) : boolean;
begin
   ATK_IS_GOBJECT_ACCESSIBLE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_GOBJECT_ACCESSIBLE);
end;

function ATK_IS_GOBJECT_ACCESSIBLE_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_GOBJECT_ACCESSIBLE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_GOBJECT_ACCESSIBLE);
end;

function ATK_GOBJECT_ACCESSIBLE_GET_CLASS(obj: pointer) : PAtkGObjectAccessibleClass;
begin
   ATK_GOBJECT_ACCESSIBLE_GET_CLASS:=PAtkGObjectAccessibleClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_GOBJECT_ACCESSIBLE));
end;

{$ENDIF read_implementation}

