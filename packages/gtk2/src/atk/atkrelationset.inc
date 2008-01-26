// included by atk.pp

{$IFDEF read_forward_definitions}
   PAtkRelationSet = ^TAtkRelationSet;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   TAtkRelationSet = record
        parent : TGObject;
        relations : PGPtrArray;
     end;

   PAtkRelationSetClass = ^TAtkRelationSetClass;
   TAtkRelationSetClass = record
        parent : TGObjectClass;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_RELATION_SET : GType;
function ATK_RELATION_SET(obj: pointer) : PAtkRelationSet;
function ATK_RELATION_SET_CLASS(klass: pointer) : PAtkRelationSetClass;
function ATK_IS_RELATION_SET(obj: pointer) : boolean;
function ATK_IS_RELATION_SET_CLASS(klass: pointer) : boolean;
function ATK_RELATION_SET_GET_CLASS(obj: pointer) : PAtkRelationSetClass;


function atk_relation_set_get_type:GType; cdecl; external atklib;
function atk_relation_set_new:PAtkRelationSet; cdecl; external atklib;
function atk_relation_set_contains(RelationSet:PAtkRelationSet; relationship:TAtkRelationType):gboolean; cdecl; external atklib;
procedure atk_relation_set_remove(RelationSet:PAtkRelationSet; relation:PAtkRelation); cdecl; external atklib;
procedure atk_relation_set_add(RelationSet:PAtkRelationSet; relation:PAtkRelation); cdecl; external atklib;
function atk_relation_set_get_n_relations(RelationSet:PAtkRelationSet):gint; cdecl; external atklib;
function atk_relation_set_get_relation(RelationSet:PAtkRelationSet; i:gint):PAtkRelation; cdecl; external atklib;
function atk_relation_set_get_relation_by_type(RelationSet:PAtkRelationSet; relationship:TAtkRelationType):PAtkRelation; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_RELATION_SET : GType;
begin
   ATK_TYPE_RELATION_SET:=atk_relation_set_get_type;
end;

function ATK_RELATION_SET(obj: pointer) : PAtkRelationSet;
begin
   ATK_RELATION_SET:=PAtkRelationSet(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_RELATION_SET));
end;

function ATK_RELATION_SET_CLASS(klass: pointer) : PAtkRelationSetClass;
begin
   ATK_RELATION_SET_CLASS:=PAtkRelationSetClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_RELATION_SET));
end;

function ATK_IS_RELATION_SET(obj: pointer) : boolean;
begin
   ATK_IS_RELATION_SET:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_RELATION_SET);
end;

function ATK_IS_RELATION_SET_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_RELATION_SET_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_RELATION_SET);
end;

function ATK_RELATION_SET_GET_CLASS(obj: pointer) : PAtkRelationSetClass;
begin
   ATK_RELATION_SET_GET_CLASS:=PAtkRelationSetClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_RELATION_SET));
end;

{$ENDIF read_implementation}

