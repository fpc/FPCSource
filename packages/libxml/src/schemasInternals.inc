(*
 * Summary: internal interfaces for XML Schemas
 * Description: internal interfaces for the XML Schemas handling
 *              and schema validity checking
 *		The Schemas development is a Work In Progress.
 *              Some of those interfaces are not garanteed to be API or ABI stable !
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_SCHEMAS_ENABLED}

{$IFDEF TYPE}
  xmlSchemaVal = record end;

  xmlSchemaValType = (
    XML_SCHEMAS_UNKNOWN = 0,
    XML_SCHEMAS_STRING,
    XML_SCHEMAS_NORMSTRING,
    XML_SCHEMAS_DECIMAL,
    XML_SCHEMAS_TIME,
    XML_SCHEMAS_GDAY,
    XML_SCHEMAS_GMONTH,
    XML_SCHEMAS_GMONTHDAY,
    XML_SCHEMAS_GYEAR,
    XML_SCHEMAS_GYEARMONTH,
    XML_SCHEMAS_DATE,
    XML_SCHEMAS_DATETIME,
    XML_SCHEMAS_DURATION,
    XML_SCHEMAS_FLOAT,
    XML_SCHEMAS_DOUBLE,
    XML_SCHEMAS_BOOLEAN,
    XML_SCHEMAS_TOKEN,
    XML_SCHEMAS_LANGUAGE,
    XML_SCHEMAS_NMTOKEN,
    XML_SCHEMAS_NMTOKENS,
    XML_SCHEMAS_NAME,
    XML_SCHEMAS_QNAME,
    XML_SCHEMAS_NCNAME,
    XML_SCHEMAS_ID,
    XML_SCHEMAS_IDREF,
    XML_SCHEMAS_IDREFS,
    XML_SCHEMAS_ENTITY,
    XML_SCHEMAS_ENTITIES,
    XML_SCHEMAS_NOTATION,
    XML_SCHEMAS_ANYURI,
    XML_SCHEMAS_INTEGER,
    XML_SCHEMAS_NPINTEGER,
    XML_SCHEMAS_NINTEGER,
    XML_SCHEMAS_NNINTEGER,
    XML_SCHEMAS_PINTEGER,
    XML_SCHEMAS_INT,
    XML_SCHEMAS_UINT,
    XML_SCHEMAS_LONG,
    XML_SCHEMAS_ULONG,
    XML_SCHEMAS_SHORT,
    XML_SCHEMAS_USHORT,
    XML_SCHEMAS_BYTE,
    XML_SCHEMAS_UBYTE,
    XML_SCHEMAS_HEXBINARY,
    XML_SCHEMAS_BASE64BINARY,
    XML_SCHEMAS_ANYTYPE,
    XML_SCHEMAS_ANYSIMPLETYPE
  );

(*
 * XML Schemas defines multiple type of types.
 *)
  xmlSchemaTypeType = (
    XML_SCHEMA_TYPE_BASIC = 1, (* A built-in datatype *)
    XML_SCHEMA_TYPE_ANY,
    XML_SCHEMA_TYPE_FACET,
    XML_SCHEMA_TYPE_SIMPLE,
    XML_SCHEMA_TYPE_COMPLEX,
    XML_SCHEMA_TYPE_SEQUENCE = 6,
    XML_SCHEMA_TYPE_CHOICE,
    XML_SCHEMA_TYPE_ALL,
    XML_SCHEMA_TYPE_SIMPLE_CONTENT,
    XML_SCHEMA_TYPE_COMPLEX_CONTENT,
    XML_SCHEMA_TYPE_UR,
    XML_SCHEMA_TYPE_RESTRICTION,
    XML_SCHEMA_TYPE_EXTENSION,
    XML_SCHEMA_TYPE_ELEMENT,
    XML_SCHEMA_TYPE_ATTRIBUTE,
    XML_SCHEMA_TYPE_ATTRIBUTEGROUP,
    XML_SCHEMA_TYPE_GROUP,
    XML_SCHEMA_TYPE_NOTATION,
    XML_SCHEMA_TYPE_LIST,
    XML_SCHEMA_TYPE_UNION,
    XML_SCHEMA_TYPE_ANY_ATTRIBUTE,
    XML_SCHEMA_TYPE_IDC_UNIQUE,
    XML_SCHEMA_TYPE_IDC_KEY,
    XML_SCHEMA_TYPE_IDC_KEYREF,
    XML_SCHEMA_TYPE_PARTICLE = 25,
    XML_SCHEMA_TYPE_ATTRIBUTE_USE,
    XML_SCHEMA_FACET_MININCLUSIVE = 1000,
    XML_SCHEMA_FACET_MINEXCLUSIVE,
    XML_SCHEMA_FACET_MAXINCLUSIVE,
    XML_SCHEMA_FACET_MAXEXCLUSIVE,
    XML_SCHEMA_FACET_TOTALDIGITS,
    XML_SCHEMA_FACET_FRACTIONDIGITS,
    XML_SCHEMA_FACET_PATTERN,
    XML_SCHEMA_FACET_ENUMERATION,
    XML_SCHEMA_FACET_WHITESPACE,
    XML_SCHEMA_FACET_LENGTH,
    XML_SCHEMA_FACET_MAXLENGTH,
    XML_SCHEMA_FACET_MINLENGTH,
    XML_SCHEMA_EXTRA_QNAMEREF = 2000,
    XML_SCHEMA_EXTRA_ATTR_USE_PROHIB
  );

  xmlSchemaContentType = (
    XML_SCHEMA_CONTENT_UNKNOWN = 0,
    XML_SCHEMA_CONTENT_EMPTY = 1,
    XML_SCHEMA_CONTENT_ELEMENTS,
    XML_SCHEMA_CONTENT_MIXED,
    XML_SCHEMA_CONTENT_SIMPLE,
    XML_SCHEMA_CONTENT_MIXED_OR_ELEMENTS, (* Obsolete *)
    XML_SCHEMA_CONTENT_BASIC,
    XML_SCHEMA_CONTENT_ANY
  );

(**
 * Annotation
 *)
  xmlSchemaAnnot = record
    next    : xmlSchemaAnnotPtr;
    content : xmlNodePtr; (* the annotation *)
  end;

(**
 * XML_SCHEMAS_ANYATTR_SKIP:
 *
 * Skip unknown attribute from validation
 * Obsolete, not used anymore.
 *)
{$DEFINE XML_SCHEMAS_ANYATTR_SKIP :=       1}
(**
 * XML_SCHEMAS_ANYATTR_LAX:
 *
 * Ignore validation non definition on attributes
 * Obsolete, not used anymore.
 *)
{$DEFINE XML_SCHEMAS_ANYATTR_LAX  :=              2}
(**
 * XML_SCHEMAS_ANYATTR_STRICT:
 *
 * Apply strict validation rules on attributes
 * Obsolete, not used anymore.
 *)
{$DEFINE XML_SCHEMAS_ANYATTR_STRICT :=       3}
(**
 * XML_SCHEMAS_ANY_SKIP:
 *
 * Skip unknown attribute from validation
 *)
{$DEFINE XML_SCHEMAS_ANY_SKIP :=       1}
(**
 * XML_SCHEMAS_ANY_LAX:
 *
 * Used by wildcards.
 * Validate if type found, don't worry if not found
 *)
{$DEFINE XML_SCHEMAS_ANY_LAX :=               2}
(**
 * XML_SCHEMAS_ANY_STRICT:
 *
 * Used by wildcards.
 * Apply strict validation rules
 *)
{$DEFINE XML_SCHEMAS_ANY_STRICT :=       3}
(**
 * XML_SCHEMAS_ATTR_USE_PROHIBITED:
 *
 * Used by wildcards.
 * The attribute is prohibited.
 *)
{$DEFINE XML_SCHEMAS_ATTR_USE_PROHIBITED := 0}
(**
 * XML_SCHEMAS_ATTR_USE_REQUIRED:
 *
 * The attribute is required.
 *)
{$DEFINE XML_SCHEMAS_ATTR_USE_REQUIRED := 1}
(**
 * XML_SCHEMAS_ATTR_USE_OPTIONAL:
 *
 * The attribute is optional.
 *)
{$DEFINE XML_SCHEMAS_ATTR_USE_OPTIONAL := 2}
(**
 * XML_SCHEMAS_ATTR_GLOBAL:
 *
 * allow elements in no namespace
 *)
{$DEFINE XML_SCHEMAS_ATTR_GLOBAL :=       (1 shl 0)}
(**
 * XML_SCHEMAS_ATTR_NSDEFAULT:
 *
 * allow elements in no namespace
 *)
{$DEFINE XML_SCHEMAS_ATTR_NSDEFAULT :=       (1 shl 7)}
(**
 * XML_SCHEMAS_ATTR_INTERNAL_RESOLVED:
 *
 * this is set when the "type" and "ref" references
 * have been resolved.
 *)
{$DEFINE XML_SCHEMAS_ATTR_INTERNAL_RESOLVED :=       (1 shl 8)}
(**
 * XML_SCHEMAS_ATTR_FIXED:
 *
 * the attribute has a fixed value
 *)
{$DEFINE XML_SCHEMAS_ATTR_FIXED     :=   (1 shl 9)}

(**
 * xmlSchemaAttribute:
 * An attribute definition.
 *)
  xmlSchemaAttribute = record
    _type           : xmlSchemaTypeType;
    next            : xmlSchemaAttributePtr; (* the next attribute (not used?) *)
    name            : xmlCharPtr; (* the name of the declaration *)
    id              : xmlCharPtr; (* Deprecated; not used *)
    ref             : xmlCharPtr; (* Deprecated; not used *)
    refNs           : xmlCharPtr; (* Deprecated; not used *)
    typeName        : xmlCharPtr; (* the local name of the type definition *)
    typeNs          : xmlCharPtr; (* the ns URI of the type definition *)
    annot           : xmlSchemaAnnotPtr;

    base            : xmlSchemaTypePtr; (* Deprecated; not used *)
    occurs          : cint; (* Deprecated; not used *)
    defValue        : xmlCharPtr; (* The initial value of the value constraint *)
    subtypes        : xmlSchemaTypePtr; (* the type definition *)
    node            : xmlNodePtr;
    targetNamespace : xmlCharPtr;
    flags           : cint;
    refPrefix       : xmlCharPtr; (* Deprecated; not used *)
    defVal          : xmlSchemaValPtr; (* The compiled value constraint *)
    refDecl         : xmlSchemaAttributePtr; (* Deprecated; not used *)
  end;

(**
 * xmlSchemaAttributeLink:
 * Used to build a list of attribute uses on complexType definitions.
 * WARNING: Deprecated; not used.
 *)
  xmlSchemaAttributeLink = record
    next  : xmlSchemaAttributeLinkPtr;(* the next attribute link ... *)
    attr  : xmlSchemaAttributePtr;(* the linked attribute *)
  end;

(**
 * XML_SCHEMAS_WILDCARD_COMPLETE:
 *
 * If the wildcard is complete.
 *)
{$DEFINE XML_SCHEMAS_WILDCARD_COMPLETE := (1 shl 0)}

(**
 * xmlSchemaCharValueLink:
 * Used to build a list of namespaces on wildcards.
 *)
  xmlSchemaWildcardNs = record
    next  : xmlSchemaWildcardNsPtr;(* the next constraint link ... *)
    value : xmlCharPtr;(* the value *)
  end;

(**
 * xmlSchemaWildcard.
 * A wildcard.
 *)
  xmlSchemaWildcard = record
    _type           : xmlSchemaTypeType;        (* The kind of type *)
    id              : xmlCharPtr; (* Deprecated; not used *)
    annot           : xmlSchemaAnnotPtr;
    node            : xmlNodePtr;
    minOccurs       : cint; (* Deprecated; not used *)
    maxOccurs       : cint; (* Deprecated; not used *)
    processContents : cint;
    any             : cint; (* Indicates if the ns constraint is of ##any *)
    nsSet           : xmlSchemaWildcardNsPtr; (* The list of allowed namespaces *)
    negNsSet        : xmlSchemaWildcardNsPtr; (* The negated namespace *)
    flags           : cint;
  end;


(**
 * XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED:
 *
 * The attribute wildcard has been already builded.
 *)
{$DEFINE XML_SCHEMAS_ATTRGROUP_WILDCARD_BUILDED := (1 shl 0)}
(**
 * XML_SCHEMAS_ATTRGROUP_GLOBAL:
 *
 * The attribute wildcard has been already builded.
 *)
{$DEFINE XML_SCHEMAS_ATTRGROUP_GLOBAL := (1 shl 1)}
(**
 * XML_SCHEMAS_ATTRGROUP_MARKED:
 *
 * Marks the attr group as marked; used for circular checks.
 *)
{$DEFINE XML_SCHEMAS_ATTRGROUP_MARKED := (1 shl 2)}

(**
 * XML_SCHEMAS_ATTRGROUP_REDEFINED:
 *
 * The attr group was redefined.
 *)
{$DEFINE XML_SCHEMAS_ATTRGROUP_REDEFINED := (1 shl 3)}
(**
 * XML_SCHEMAS_ATTRGROUP_HAS_REFS:
 *
 * Whether this attr. group contains attr. group references.
 *)
{$DEFINE XML_SCHEMAS_ATTRGROUP_HAS_REFS := (1 shl 4)}

(**
 * An attribute group definition.
 *
 * xmlSchemaAttribute and xmlSchemaAttributeGroup start of structures
 * must be kept similar
 *)
  xmlSchemaAttributeGroup = record
    _type           : xmlSchemaTypeType;        (* The kind of type *)
    next            : xmlSchemaAttributePtr;(* the next attribute if in a group ... *)
    name            : xmlCharPtr;
    id              : xmlCharPtr;
    ref             : xmlCharPtr; (* Deprecated; not used *)
    refNs           : xmlCharPtr; (* Deprecated; not used *)
    annot           : xmlSchemaAnnotPtr;

    attributes      : xmlSchemaAttributePtr; (* Deprecated; not used *)
    node            : xmlNodePtr;
    flags           : cint;
    attributeWildcard : xmlSchemaWildcardPtr;
    refPrefix       : xmlCharPtr; (* Deprecated; not used *)
    refItem         : xmlSchemaAttributeGroupPtr; (* Deprecated; not used *)
    targetNamespace : xmlCharPtr;
    attrUses        : pointer;
  end;

(**
 * xmlSchemaTypeLink:
 * Used to build a list of types (e.g. member types of
 * simpleType with variety "union").
 *)
  xmlSchemaTypeLink = record
    next  : xmlSchemaTypeLinkPtr;(* the next type link ... *)
    _type : xmlSchemaTypePtr;(* the linked type *)
  end;

(**
 * xmlSchemaFacetLink:
 * Used to build a list of facets.
 *)
  xmlSchemaFacetLink = record
    next  : xmlSchemaFacetLinkPtr;(* the next facet link ... *)
    facet : xmlSchemaFacetPtr;(* the linked facet *)
  end;

(**
 * XML_SCHEMAS_TYPE_MIXED:
 *
 * the element content type is mixed
 *)
{$DEFINE XML_SCHEMAS_TYPE_MIXED                := (1 shl 0)}
(**
 * XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION:
 *
 * the simple or complex type has a derivation method of "extension".
 *)
{$DEFINE XML_SCHEMAS_TYPE_DERIVATION_METHOD_EXTENSION                := (1 shl 1)}
(**
 * XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION:
 *
 * the simple or complex type has a derivation method of "restriction".
 *)
{$DEFINE XML_SCHEMAS_TYPE_DERIVATION_METHOD_RESTRICTION                := (1 shl 2)}
(**
 * XML_SCHEMAS_TYPE_GLOBAL:
 *
 * the type is global
 *)
{$DEFINE XML_SCHEMAS_TYPE_GLOBAL                := (1 shl 3)}
(**
 * XML_SCHEMAS_TYPE_OWNED_ATTR_WILDCARD:
 *
 * the complexType owns an attribute wildcard, i.e.
 * it can be freed by the complexType
 *)
{$DEFINE XML_SCHEMAS_TYPE_OWNED_ATTR_WILDCARD    := (1 shl 4)} (* Obsolete. *)
(**
 * XML_SCHEMAS_TYPE_VARIETY_ABSENT:
 *
 * the simpleType has a variety of "absent".
 * TODO: Actually not necessary :-/, since if
 * none of the variety flags occur then it's
 * automatically absent.
 *)
{$DEFINE XML_SCHEMAS_TYPE_VARIETY_ABSENT    := (1 shl 5)}
(**
 * XML_SCHEMAS_TYPE_VARIETY_LIST:
 *
 * the simpleType has a variety of "list".
 *)
{$DEFINE XML_SCHEMAS_TYPE_VARIETY_LIST    := (1 shl 6)}
(**
 * XML_SCHEMAS_TYPE_VARIETY_UNION:
 *
 * the simpleType has a variety of "union".
 *)
{$DEFINE XML_SCHEMAS_TYPE_VARIETY_UNION    := (1 shl 7)}
(**
 * XML_SCHEMAS_TYPE_VARIETY_ATOMIC:
 *
 * the simpleType has a variety of "union".
 *)
{$DEFINE XML_SCHEMAS_TYPE_VARIETY_ATOMIC    := (1 shl 8)}
(**
 * XML_SCHEMAS_TYPE_FINAL_EXTENSION:
 *
 * the complexType has a final of "extension".
 *)
{$DEFINE XML_SCHEMAS_TYPE_FINAL_EXTENSION    := (1 shl 9)}
(**
 * XML_SCHEMAS_TYPE_FINAL_RESTRICTION:
 *
 * the simpleType/complexType has a final of "restriction".
 *)
{$DEFINE XML_SCHEMAS_TYPE_FINAL_RESTRICTION    := (1 shl 10)}
(**
 * XML_SCHEMAS_TYPE_FINAL_LIST:
 *
 * the simpleType has a final of "list".
 *)
{$DEFINE XML_SCHEMAS_TYPE_FINAL_LIST    := (1 shl 11)}
(**
 * XML_SCHEMAS_TYPE_FINAL_UNION:
 *
 * the simpleType has a final of "union".
 *)
{$DEFINE XML_SCHEMAS_TYPE_FINAL_UNION    := (1 shl 12)}
(**
 * XML_SCHEMAS_TYPE_FINAL_DEFAULT:
 *
 * the simpleType has a final of "default".
 *)
{$DEFINE XML_SCHEMAS_TYPE_FINAL_DEFAULT    := (1 shl 13)}
(**
 * XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE:
 *
 * Marks the item as a builtin primitive.
 *)
{$DEFINE XML_SCHEMAS_TYPE_BUILTIN_PRIMITIVE    := (1 shl 14)}
(**
 * XML_SCHEMAS_TYPE_MARKED:
 *
 * Marks the item as marked; used for circular checks.
 *)
{$DEFINE XML_SCHEMAS_TYPE_MARKED        := (1 shl 16)}
(**
 * XML_SCHEMAS_TYPE_BLOCK_DEFAULT:
 *
 * the complexType did not specify 'block' so use the default of the
 * <schema> item.
 *)
{$DEFINE XML_SCHEMAS_TYPE_BLOCK_DEFAULT    := (1 shl 17)}
(**
 * XML_SCHEMAS_TYPE_BLOCK_EXTENSION:
 *
 * the complexType has a 'block' of "extension".
 *)
{$DEFINE XML_SCHEMAS_TYPE_BLOCK_EXTENSION    := (1 shl 18)}
(**
 * XML_SCHEMAS_TYPE_BLOCK_RESTRICTION:
 *
 * the complexType has a 'block' of "restriction".
 *)
{$DEFINE XML_SCHEMAS_TYPE_BLOCK_RESTRICTION    := (1 shl 19)}
(**
 * XML_SCHEMAS_TYPE_ABSTRACT:
 *
 * the simple/complexType is abstract.
 *)
{$DEFINE XML_SCHEMAS_TYPE_ABSTRACT    := (1 shl 20)}
(**
 * XML_SCHEMAS_TYPE_FACETSNEEDVALUE:
 *
 * indicates if the facets need a computed value
 *)
{$DEFINE XML_SCHEMAS_TYPE_FACETSNEEDVALUE    := (1 shl 21)}
(**
 * XML_SCHEMAS_TYPE_INTERNAL_RESOLVED:
 *
 * indicates that the type was typefixed
 *)
{$DEFINE XML_SCHEMAS_TYPE_INTERNAL_RESOLVED    := (1 shl 22)}
(**
 * XML_SCHEMAS_TYPE_INTERNAL_INVALID:
 *
 * indicates that the type is invalid
 *)
{$DEFINE XML_SCHEMAS_TYPE_INTERNAL_INVALID    := (1 shl 23)}
(**
 * XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE:
 *
 * a whitespace-facet value of "preserve"
 *)
{$DEFINE XML_SCHEMAS_TYPE_WHITESPACE_PRESERVE    := (1 shl 24)}
(**
 * XML_SCHEMAS_TYPE_WHITESPACE_REPLACE:
 *
 * a whitespace-facet value of "replace"
 *)
{$DEFINE XML_SCHEMAS_TYPE_WHITESPACE_REPLACE    := (1 shl 25)}
(**
 * XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE:
 *
 * a whitespace-facet value of "collapse"
 *)
{$DEFINE XML_SCHEMAS_TYPE_WHITESPACE_COLLAPSE    := (1 shl 26)}
(**
 * XML_SCHEMAS_TYPE_HAS_FACETS:
 *
 * has facets
 *)
{$DEFINE XML_SCHEMAS_TYPE_HAS_FACETS    := (1 shl 27)}
(**
 * XML_SCHEMAS_TYPE_NORMVALUENEEDED:
 *
 * indicates if the facets (pattern) need a normalized value
 *)
{$DEFINE XML_SCHEMAS_TYPE_NORMVALUENEEDED    := (1 shl 28)}

(**
 * XML_SCHEMAS_TYPE_FIXUP_1:
 *
 * First stage of fixup was done.
 *)
{$DEFINE XML_SCHEMAS_TYPE_FIXUP_1    := (1 shl 29)}

(**
 * XML_SCHEMAS_TYPE_REDEFINED:
 *
 * The type was redefined.
 *)
{$DEFINE XML_SCHEMAS_TYPE_REDEFINED    := (1 shl 30)}
(**
 * XML_SCHEMAS_TYPE_REDEFINING:
 *
 * The type redefines an other type.
 *)
(* {$DEFINE XML_SCHEMAS_TYPE_REDEFINING    := (1 shl 31)} *)

(**
 * _xmlSchemaType:
 *
 * Schemas type definition.
 *)
  xmlSchemaType = record
    _type             : xmlSchemaTypeType; (* The kind of type *)
    next              : xmlSchemaTypePtr; (* the next type if in a sequence ... *)
    name              : xmlCharPtr;
    id                : xmlCharPtr; (* Deprecated; not used *)
    ref               : xmlCharPtr; (* Deprecated; not used *)
    refNs             : xmlCharPtr; (* Deprecated; not used *)
    annot             : xmlSchemaAnnotPtr;
    subtypes          : xmlSchemaTypePtr;
    attributes        : xmlSchemaAttributePtr; (* Deprecated; not used *)
    node              : xmlNodePtr;
    minOccurs         : cint; (* Deprecated; not used *)
    maxOccurs         : cint; (* Deprecated; not used *)

    flags             : cint;
    contentType       : xmlSchemaContentType;
    base              : xmlCharPtr; (* Base type's local name *)
    baseNs            : xmlCharPtr; (* Base type's target namespace *)
    baseType          : xmlSchemaTypePtr; (* The base type component *)
    facets            : xmlSchemaFacetPtr; (* Local facets *)
    redef             : xmlSchemaTypePtr; (* Deprecated; not used *)
    recurse           : cint; (* Obsolete *)
    attributeUses     : xmlSchemaAttributeLinkPtrPtr; (* Deprecated; not used *)
    attributeWildcard : xmlSchemaWildcardPtr;
    builtInType       : cint; (* Type of built-in types. *)
    memberTypes       : xmlSchemaTypeLinkPtr; (* member-types if a union type. *)
    facetSet          : xmlSchemaFacetLinkPtr; (* All facets (incl. inherited) *)
    refPrefix         : xmlCharPtr; (* Deprecated; not used *)
    contentTypeDef    : xmlSchemaTypePtr; (* Used for the simple content of complex types.
                                        Could we use @subtypes for this? *)
    contModel         : xmlRegexpPtr; (* Holds the automaton of the content model *)
    targetNamespace   : xmlCharPtr;
    attrUses          : pointer;
  end;

(*
 * xmlSchemaElement:
 * An element definition.
 *
 * xmlSchemaType, xmlSchemaFacet and xmlSchemaElement start of
 * structures must be kept similar
 *)
(**
 * XML_SCHEMAS_ELEM_NILLABLE:
 *
 * the element is nillable
 *)
{$DEFINE XML_SCHEMAS_ELEM_NILLABLE        := (1 shl 0)}
(**
 * XML_SCHEMAS_ELEM_GLOBAL:
 *
 * the element is global
 *)
{$DEFINE XML_SCHEMAS_ELEM_GLOBAL                := (1 shl 1)}
(**
 * XML_SCHEMAS_ELEM_DEFAULT:
 *
 * the element has a default value
 *)
{$DEFINE XML_SCHEMAS_ELEM_DEFAULT        := (1 shl 2)}
(**
 * XML_SCHEMAS_ELEM_FIXED:
 *
 * the element has a fixed value
 *)
{$DEFINE XML_SCHEMAS_ELEM_FIXED                := (1 shl 3)}
(**
 * XML_SCHEMAS_ELEM_ABSTRACT:
 *
 * the element is abstract
 *)
{$DEFINE XML_SCHEMAS_ELEM_ABSTRACT        := (1 shl 4)}
(**
 * XML_SCHEMAS_ELEM_TOPLEVEL:
 *
 * the element is top level
 * obsolete: use XML_SCHEMAS_ELEM_GLOBAL instead
 *)
{$DEFINE XML_SCHEMAS_ELEM_TOPLEVEL        := (1 shl 5)}
(**
 * XML_SCHEMAS_ELEM_REF:
 *
 * the element is a reference to a type
 *)
{$DEFINE XML_SCHEMAS_ELEM_REF                := (1 shl 6)}
(**
 * XML_SCHEMAS_ELEM_NSDEFAULT:
 *
 * allow elements in no namespace
 * Obsolete, not used anymore.
 *)
{$DEFINE XML_SCHEMAS_ELEM_NSDEFAULT        := (1 shl 7)}
(**
 * XML_SCHEMAS_ELEM_INTERNAL_RESOLVED:
 *
 * this is set when "type", "ref", "substitutionGroup"
 * references have been resolved.
 *)
{$DEFINE XML_SCHEMAS_ELEM_INTERNAL_RESOLVED        := (1 shl 8)}
 (**
 * XML_SCHEMAS_ELEM_CIRCULAR:
 *
 * a helper flag for the search of circular references.
 *)
{$DEFINE XML_SCHEMAS_ELEM_CIRCULAR        := (1 shl 9)}
(**
 * XML_SCHEMAS_ELEM_BLOCK_ABSENT:
 *
 * the "block" attribute is absent
 *)
{$DEFINE XML_SCHEMAS_ELEM_BLOCK_ABSENT        := (1 shl 10)}
(**
 * XML_SCHEMAS_ELEM_BLOCK_EXTENSION:
 *
 * disallowed substitutions are absent
 *)
{$DEFINE XML_SCHEMAS_ELEM_BLOCK_EXTENSION        := (1 shl 11)}
(**
 * XML_SCHEMAS_ELEM_BLOCK_RESTRICTION:
 *
 * disallowed substitutions: "restriction"
 *)
{$DEFINE XML_SCHEMAS_ELEM_BLOCK_RESTRICTION        := (1 shl 12)}
(**
 * XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION:
 *
 * disallowed substitutions: "substituion"
 *)
{$DEFINE XML_SCHEMAS_ELEM_BLOCK_SUBSTITUTION        := (1 shl 13)}
(**
 * XML_SCHEMAS_ELEM_FINAL_ABSENT:
 *
 * substitution group exclusions are absent
 *)
{$DEFINE XML_SCHEMAS_ELEM_FINAL_ABSENT        := (1 shl 14)}
(**
 * XML_SCHEMAS_ELEM_FINAL_EXTENSION:
 *
 * substitution group exclusions: "extension"
 *)
{$DEFINE XML_SCHEMAS_ELEM_FINAL_EXTENSION        := (1 shl 15)}
(**
 * XML_SCHEMAS_ELEM_FINAL_RESTRICTION:
 *
 * substitution group exclusions: "restriction"
 *)
{$DEFINE XML_SCHEMAS_ELEM_FINAL_RESTRICTION        := (1 shl 16)}
(**
 * XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD:
 *
 * the declaration is a substitution group head
 *)
{$DEFINE XML_SCHEMAS_ELEM_SUBST_GROUP_HEAD        := (1 shl 17)}
(**
 * XML_SCHEMAS_ELEM_INTERNAL_CHECKED:
 *
 * this is set when the elem decl has been checked against
 * all constraints
 *)
{$DEFINE XML_SCHEMAS_ELEM_INTERNAL_CHECKED        := (1 shl 18)}

  xmlSchemaElement = record
    _type             : xmlSchemaTypeType; (* The kind of type *)
    next              : xmlSchemaTypePtr; (* Not used? *)
    name              : xmlCharPtr;
    id                : xmlCharPtr; (* Deprecated; not used *)
    ref               : xmlCharPtr; (* Deprecated; not used *)
    refNs             : xmlCharPtr; (* Deprecated; not used *)
    annot             : xmlSchemaAnnotPtr;
    subtypes          : xmlSchemaTypePtr; (* the type definition *)
    attributes        : xmlSchemaAttributePtr;
    node              : xmlNodePtr;
    minOccurs         : cint; (* Deprecated; not used *)
    maxOccurs         : cint; (* Deprecated; not used *)

    flags             : cint;
    targetNamespace   : xmlCharPtr;
    namedType         : xmlCharPtr;
    namedTypeNs       : xmlCharPtr;
    substGroup        : xmlCharPtr;
    substGroupNs      : xmlCharPtr;
    scope             : xmlCharPtr;
    value             : xmlCharPtr; (* The original value of the value constraint. *)
    refDecl           : xmlSchemaElementPtr; (* This will now be used for the
                                          substitution group affiliation *)
    contModel         : xmlRegexpPtr; (* Obsolete for WXS, maybe used for RelaxNG *)
    contentType       : xmlSchemaContentType;
    refPrefix         : xmlCharPtr; (* Deprecated; not used *)
    defVal            : xmlSchemaValPtr; (* The compiled value contraint. *)
    idcs              : pointer; (* The identity-constraint defs *)
  end;

(*
 * XML_SCHEMAS_FACET_UNKNOWN:
 *
 * unknown facet handling
 *)
{$DEFINE XML_SCHEMAS_FACET_UNKNOWN        := (0)}
(*
 * XML_SCHEMAS_FACET_PRESERVE:
 *
 * preserve the type of the facet
 *)
{$DEFINE XML_SCHEMAS_FACET_PRESERVE        := (1)}
(*
 * XML_SCHEMAS_FACET_REPLACE:
 *
 * replace the type of the facet
 *)
{$DEFINE XML_SCHEMAS_FACET_REPLACE        := (2)}
(*
 * XML_SCHEMAS_FACET_COLLAPSE:
 *
 * collapse the types of the facet
 *)
{$DEFINE XML_SCHEMAS_FACET_COLLAPSE        := (3)}
(**
 * A facet definition.
 *)
  xmlSchemaFacet = record
    _type         : xmlSchemaTypeType;        (* The kind of type *)
    next          : xmlSchemaFacetPtr;(* the next type if in a sequence ... *)
    value         : xmlCharPtr; (* The original value *)
    id            : xmlCharPtr; (* Obsolete *)
    annot         : xmlSchemaAnnotPtr;
    node          : xmlNodePtr;
    fixed         : cint; (* XML_SCHEMAS_FACET_PRESERVE, etc. *)
    whitespace    : cint;
    val           : xmlSchemaValPtr; (* The compiled value *)
    regexp        : xmlRegexpPtr; (* The regex for patterns *)
  end;

(**
 * A notation definition.
 *)
  xmlSchemaNotation = record
    _type         : xmlSchemaTypeType; (* The kind of type *)
    name          : xmlCharPtr;
    annot         : xmlSchemaAnnotPtr;
    identifier    : xmlCharPtr;
    targetNamespace : xmlCharPtr;
  end;

(*
* TODO: Actually all those flags used for the schema should sit
* on the schema parser context, since they are used only
* during parsing an XML schema document, and not available
* on the component level as per spec.
*)
(**
 * XML_SCHEMAS_QUALIF_ELEM:
 *
 * Reflects elementFormDefault == qualified in
 * an XML schema document.
 *)
{$DEFINE XML_SCHEMAS_QUALIF_ELEM                := (1 shl 0)}
(**
 * XML_SCHEMAS_QUALIF_ATTR:
 *
 * Reflects attributeFormDefault == qualified in
 * an XML schema document.
 *)
{$DEFINE XML_SCHEMAS_QUALIF_ATTR            := (1 shl 1)}
(**
 * XML_SCHEMAS_FINAL_DEFAULT_EXTENSION:
 *
 * the schema has "extension" in the set of finalDefault.
 *)
{$DEFINE XML_SCHEMAS_FINAL_DEFAULT_EXTENSION        := (1 shl 2)}
(**
 * XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION:
 *
 * the schema has "restriction" in the set of finalDefault.
 *)
{$DEFINE XML_SCHEMAS_FINAL_DEFAULT_RESTRICTION            := (1 shl 3)}
(**
 * XML_SCHEMAS_FINAL_DEFAULT_LIST:
 *
 * the cshema has "list" in the set of finalDefault.
 *)
{$DEFINE XML_SCHEMAS_FINAL_DEFAULT_LIST            := (1 shl 4)}
(**
 * XML_SCHEMAS_FINAL_DEFAULT_UNION:
 *
 * the schema has "union" in the set of finalDefault.
 *)
{$DEFINE XML_SCHEMAS_FINAL_DEFAULT_UNION            := (1 shl 5)}
(**
 * XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION:
 *
 * the schema has "extension" in the set of blockDefault.
 *)
{$DEFINE XML_SCHEMAS_BLOCK_DEFAULT_EXTENSION            := (1 shl 6)}
(**
 * XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION:
 *
 * the schema has "restriction" in the set of blockDefault.
 *)
{$DEFINE XML_SCHEMAS_BLOCK_DEFAULT_RESTRICTION            := (1 shl 7)}
(**
 * XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION:
 *
 * the schema has "substitution" in the set of blockDefault.
 *)
{$DEFINE XML_SCHEMAS_BLOCK_DEFAULT_SUBSTITUTION            := (1 shl 8)}
(**
 * XML_SCHEMAS_INCLUDING_CONVERT_NS:
 *
 * the schema is currently including an other schema with
 * no target namespace.
 *)
{$DEFINE XML_SCHEMAS_INCLUDING_CONVERT_NS            := (1 shl 9)}
(**
 * _xmlSchema:
 *
 * A Schemas definition
 *)
  xmlSchema = record
    name            : xmlCharPtr; (* schema name *)
    targetNamespace : xmlCharPtr; (* the target namespace *)
    version         : xmlCharPtr;
    id              : xmlCharPtr; (* Obsolete *)
    doc             : xmlDocPtr;
    annot           : xmlSchemaAnnotPtr;
    flags           : cint;

    typeDecl        : xmlHashTablePtr;
    attrDecl        : xmlHashTablePtr;
    attrgrpDecl     : xmlHashTablePtr;
    elemDecl        : xmlHashTablePtr;
    notaDecl        : xmlHashTablePtr;

    schemasImports  : xmlHashTablePtr;

    _private        : pointer;        (* unused by the library for users or bindings *)
    groupDecl       : xmlHashTablePtr;
    dict            : xmlDictPtr;
    includes        : pointer;     (* the includes, this is opaque for now *)
    preserve        : cint;        (* whether to free the document *)
    counter         : cint; (* used to give ononymous components unique names *)
    idcDef          : xmlHashTablePtr; (* All identity-constraint defs. *)
    volatiles       : pointer; (* Obsolete *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
procedure xmlSchemaFreeType(_type: xmlSchemaTypePtr); XMLCALL; XMLPUBFUN;
procedure xmlSchemaFreeWildcard(wildcard: xmlSchemaWildcardPtr); XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
