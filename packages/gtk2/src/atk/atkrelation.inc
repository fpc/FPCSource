// included by atk.pp

{
   An AtkRelation describes a relation between the anObject and one or more
   other objects. The actual relations that an anObject has with other objects
   are defined as an AtkRelationSet, which is a set of AtkRelations.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{
  AtkRelationType:
  @ATK_RELATION_NULL:
  @ATK_RELATION_CONTROLLED_BY: Indicates an anObject controlled by one or more target objects.
  @ATK_RELATION_CONTROLLER_FOR: Indicates an anObject is an controller for one or more target objects.
  @ATK_RELATION_LABEL_FOR: Indicates an anObject is a label for one or more target objects.
  @ATK_RELATION_LABELLED_BY: Indicates an anObject is labelled by one or more target objects.
  @ATK_RELATION_MEMBER_OF: Indicates an anObject is a member of a group of one or more target objects.
  @ATK_RELATION_NODE_CHILD_OF: Indicates an anObject is a cell in a treetable which is displayed because a cell in the same column is expanded and identifies that cell.
  @ATK_RELATION_LAST_DEFINED:

  Describes the type of the relation
   }
   PAtkRelationType = ^TAtkRelationType;
   TAtkRelationType = (
     ATK_RELATION_NULL {:= 0},
     ATK_RELATION_CONTROLLED_BY,
     ATK_RELATION_CONTROLLER_FOR,
     ATK_RELATION_LABEL_FOR,
     ATK_RELATION_LABELLED_BY,
     ATK_RELATION_MEMBER_OF,
     ATK_RELATION_NODE_CHILD_OF,
     ATK_RELATION_LAST_DEFINED
   );


   PAtkRelation = ^TAtkRelation;
   TAtkRelation = record
        parent : TGObject;
        target : PGPtrArray;
        relationship : TAtkRelationType;
     end;

   PAtkRelationClass = ^TAtkRelationClass;
   TAtkRelationClass = record
        parent : TGObjectClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

function ATK_TYPE_RELATION : GType;
function ATK_RELATION(obj: pointer) : PAtkRelation;
function ATK_RELATION_CLASS(klass: pointer) : PAtkRelationClass;
function ATK_IS_RELATION(obj: pointer) : boolean;
function ATK_IS_RELATION_CLASS(klass: pointer) : boolean;
function ATK_RELATION_GET_CLASS(obj: pointer) : PAtkRelationClass;


function atk_relation_get_type:GType; cdecl; external atklib;
function atk_relation_type_register(name:Pgchar):TAtkRelationType; cdecl; external atklib;
function atk_relation_type_get_name(_type:TAtkRelationType):Pgchar; cdecl; external atklib;
function atk_relation_type_for_name(name:Pgchar):TAtkRelationType; cdecl; external atklib;
{ Create a new relation for the specified key and the specified list of targets. }
function atk_relation_new(targets:PPAtkObject; n_targets:gint; relationship:TAtkRelationType):PAtkRelation; cdecl; external atklib;
{ Returns the type of a relation. }
function atk_relation_get_relation_type(relation:PAtkRelation):TAtkRelationType; cdecl; external atklib;
{ Returns the target list of a relation. }
function atk_relation_get_target(relation:PAtkRelation):PGPtrArray; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_RELATION : GType;
begin
   ATK_TYPE_RELATION:=atk_relation_get_type;
end;

function ATK_RELATION(obj: pointer) : PAtkRelation;
begin
   ATK_RELATION:=PAtkRelation(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_RELATION));
end;

function ATK_RELATION_CLASS(klass: pointer) : PAtkRelationClass;
begin
   ATK_RELATION_CLASS:=PAtkRelationClass(G_TYPE_CHECK_CLASS_CAST(klass,ATK_TYPE_RELATION));
end;

function ATK_IS_RELATION(obj: pointer) : boolean;
begin
   ATK_IS_RELATION:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_RELATION);
end;

function ATK_IS_RELATION_CLASS(klass: pointer) : boolean;
begin
   ATK_IS_RELATION_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,ATK_TYPE_RELATION);
end;

function ATK_RELATION_GET_CLASS(obj: pointer) : PAtkRelationClass;
begin
   ATK_RELATION_GET_CLASS:=PAtkRelationClass(G_TYPE_INSTANCE_GET_CLASS(obj,ATK_TYPE_RELATION));
end;

{$ENDIF read_implementation}

