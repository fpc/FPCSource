(*
 * Summary: interface for the XML entities handling
 * Description: this module provides some of the entity API needed
 *              for the parser and applications.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF TYPE}
(*
 * The different valid entity types.
 *)
  xmlEntityType = (
    XML_INTERNAL_GENERAL_ENTITY = 1,
    XML_EXTERNAL_GENERAL_PARSED_ENTITY = 2,
    XML_EXTERNAL_GENERAL_UNPARSED_ENTITY = 3,
    XML_INTERNAL_PARAMETER_ENTITY = 4,
    XML_EXTERNAL_PARAMETER_ENTITY = 5,
    XML_INTERNAL_PREDEFINED_ENTITY = 6
  );

(*
 * An unit of storage for an entity, contains the string, the value
 * and the linkind data needed for the linking in the hash table.
 *)
  xmlEntity = record
    _private      : pointer;	        (* application data *)
    _type         : xmlElementType;       (* XML_ENTITY_DECL, must be second ! *)
    name          : xmlCharPtr;	(* Entity name *)
    children      : xmlNodePtr;	(* First child link *)
    last          : xmlNodePtr;	(* Last child link *)
    parent        : xmlDtdPtr;	(* -> DTD *)
    next          : xmlNodePtr;	(* next sibling link  *)
    prev          : xmlNodePtr;	(* previous sibling link  *)
    doc           : xmlDocPtr;       (* the containing document *)

    orig          : xmlCharPtr;	(* content without ref substitution *)
    content       : xmlCharPtr;	(* content or ndata if unparsed *)
    length        : cint;	(* the content length *)
    etype         : xmlEntityType;	(* The entity type *)
    ExternalID    : xmlCharPtr;	(* External identifier for PUBLIC *)
    SystemID      : xmlCharPtr;	(* URI for a SYSTEM or PUBLIC Entity *)

    nexte         : xmlEntityPtr;	(* unused *)
    URI           : xmlCharPtr;	(* the full URI as computed *)
    owner         : cint;	(* does the entity own the childrens *)
    checked       : cint;	(* was the entity content checked *)
  end;

(*
 * All entities are stored in an hash table.
 * There is 2 separate hash tables for global and parameter entities.
 *)
  xmlEntitiesTable = record
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * External functions:
 *)

{$IFDEF LIBXML_LEGACY_ENABLED}
procedure xmlInitializePredefinedEntities; cdecl; external;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
function xmlAddDocEntity(doc: xmlDocPtr; name: xmlCharPtr; _type: cint; ExternalID, SystemID, content: xmlCharPtr): xmlEntityPtr; cdecl; external;
function xmlAddDtdEntity(doc: xmlDocPtr; name: xmlCharPtr; _type: cint; ExternalID, SystemID, content: xmlCharPtr): xmlEntityPtr; cdecl; external;
function xmlGetPredefinedEntity(name: xmlCharPtr): xmlEntityPtr; cdecl; external;
function xmlGetDocEntity(doc: xmlDocPtr; name: xmlCharPtr): xmlEntityPtr; cdecl; external;
function xmlGetDtdEntity(doc: xmlDocPtr; name: xmlCharPtr): xmlEntityPtr; cdecl; external;
function xmlGetParameterEntity(doc: xmlDocPtr; name: xmlCharPtr): xmlEntityPtr; cdecl; external;
{$IFDEF LIBXML_LEGACY_ENABLED}
function xmlEncodeEntities(doc: xmlDocPtr; input: xmlCharPtr): xmlCharPtr; cdecl; external;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
function xmlEncodeEntitiesReentrant(doc: xmlDocPtr; input: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlEncodeSpecialChars(doc: xmlDocPtr; input: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlCreateEntitiesTable: xmlEntitiesTablePtr; cdecl; external;
{$IFDEF LIBXML_TREE_ENABLED}
function xmlCopyEntitiesTable(table: xmlEntitiesTablePtr): xmlEntitiesTablePtr; cdecl; external;
{$ENDIF} (* LIBXML_TREE_ENABLED *)
procedure xmlFreeEntitiesTable(table: xmlEntitiesTablePtr); cdecl; external;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlDumpEntitiesTable(buf: xmlBufferPtr; table: xmlEntitiesTablePtr); cdecl; external;
procedure xmlDumpEntityDecl(buf: xmlBufferPtr; ent: xmlEntityPtr); cdecl; external;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
{$IFDEF LIBXML_LEGACY_ENABLED}
procedure xmlCleanupPredefinedEntities; cdecl; external;
{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
{$ENDIF}