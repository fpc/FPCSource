(*
 * Summary: implementation of XML Schema Datatypes
 * Description: module providing the XML Schema Datatypes implementation
 *              both definition and validity checking
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_SCHEMAS_ENABLED}

{$IFDEF TYPE}
  xmlSchemaWhitespaceValueType = (
    XML_SCHEMA_WHITESPACE_UNKNOWN = 0,
    XML_SCHEMA_WHITESPACE_PRESERVE = 1,
    XML_SCHEMA_WHITESPACE_REPLACE = 2,
    XML_SCHEMA_WHITESPACE_COLLAPSE = 3
  );
{$ENDIF}

{$IFDEF FUNCTION_}
procedure xmlSchemaInitTypes; cdecl; external;
procedure xmlSchemaCleanupTypes; cdecl; external;
function xmlSchemaGetPredefinedType(name, ns: xmlCharPtr): xmlSchemaTypePtr; cdecl; external;
function xmlSchemaValidatePredefinedType(_type: xmlSchemaTypePtr; value: xmlCharPtr; var val: xmlSchemaValPtr): cint; cdecl; external;
function xmlSchemaValPredefTypeNode(_type: xmlSchemaTypePtr; value: xmlCharPtr; var val: xmlSchemaValPtr; node: xmlNodePtr): cint; cdecl; external;
function xmlSchemaValidateFacet(base: xmlSchemaTypePtr; facet: xmlSchemaFacetPtr; value: xmlCharPtr; val: xmlSchemaValPtr): cint; cdecl; external;
function xmlSchemaValidateFacetWhtsp(facet: xmlSchemaFacetPtr; fws: xmlSchemaWhitespaceValueType; valType: xmlSchemaValType;
  value: xmlCharPtr; val: xmlSchemaValPtr; ws: xmlSchemaWhitespaceValueType): cint; cdecl; external;
procedure xmlSchemaFreeValue(val: xmlSchemaValPtr); cdecl; external;
function xmlSchemaNewFacet: xmlSchemaFacetPtr; cdecl; external;
function xmlSchemaCheckFacet(facet: xmlSchemaFacetPtr; typeDecl: xmlSchemaTypePtr; ctxt: xmlSchemaParserCtxtPtr; name: xmlCharPtr): cint; cdecl; external;
procedure xmlSchemaFreeFacet(facet: xmlSchemaFacetPtr); cdecl; external;
function xmlSchemaCompareValues(x, y: xmlSchemaValPtr): cint; cdecl; external;
function xmlSchemaGetBuiltInListSimpleTypeItemType(_type: xmlSchemaTypePtr): xmlSchemaTypePtr; cdecl; external;
function xmlSchemaValidateListSimpleTypeFacet(facet: xmlSchemaFacetPtr; value: xmlCharPtr; actualLen: culong; expectedLen: pculong): cint; cdecl; external;
function xmlSchemaGetBuiltInType(_type: xmlSchemaValType): xmlSchemaTypePtr; cdecl; external;
function xmlSchemaIsBuiltInTypeFacet(_type: xmlSchemaTypePtr; faceType: cint): xmlSchemaTypePtr; cdecl; external;
function xmlSchemaCollapseString(value: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlSchemaWhiteSpaceReplace(value: xmlCharPtr): xmlCharPtr; cdecl; external;
function xmlSchemaGetFacetValueAsULong(facet: xmlSchemaFacetPtr): culong; cdecl; external;
function xmlSchemaValidateLengthFacet(_type: xmlSchemaTypePtr; facet: xmlSchemaFacetPtr; value: xmlCharPtr; val: xmlSchemaValPtr; length: pculong): cint; cdecl; external;
function xmlSchemaValidateLengthFacetWhtsp(facet: xmlSchemaFacetPtr; valType: xmlSchemaValType; value: xmlCharPtr; val: xmlSchemaValPtr; length: pculong; ws: xmlSchemaWhitespaceValueType): cint; cdecl; external;


XMLPUBFUN int XMLCALL
		xmlSchemaValPredefTypeNodeNoNorm(xmlSchemaTypePtr type, 
						 xmlChar *value,
						 xmlSchemaValPtr *val, 
						 xmlNodePtr node);
XMLPUBFUN int XMLCALL
		xmlSchemaGetCanonValue		(xmlSchemaValPtr val,
						 xmlChar **retValue);
XMLPUBFUN int XMLCALL
		xmlSchemaGetCanonValueWhtsp	(xmlSchemaValPtr val,						 
						 xmlChar **retValue,
						 xmlSchemaWhitespaceValueType ws);
XMLPUBFUN int XMLCALL
		xmlSchemaValueAppend		(xmlSchemaValPtr prev,
						 xmlSchemaValPtr cur);
XMLPUBFUN xmlSchemaValPtr XMLCALL
		xmlSchemaValueGetNext		(xmlSchemaValPtr cur);
XMLPUBFUN xmlChar * XMLCALL
		xmlSchemaValueGetAsString	(xmlSchemaValPtr val);
XMLPUBFUN int XMLCALL
		xmlSchemaValueGetAsBoolean	(xmlSchemaValPtr val);
XMLPUBFUN xmlSchemaValPtr XMLCALL
		xmlSchemaNewStringValue		(xmlSchemaValType type,
						 xmlChar *value);
XMLPUBFUN xmlSchemaValPtr XMLCALL
		xmlSchemaNewNOTATIONValue	(xmlChar *name,
						 xmlChar *ns);
XMLPUBFUN xmlSchemaValPtr XMLCALL
		xmlSchemaNewQNameValue		(xmlChar *namespaceName,
						 xmlChar *localName);
XMLPUBFUN int XMLCALL
		xmlSchemaCompareValuesWhtsp	(xmlSchemaValPtr x,
						 xmlSchemaWhitespaceValueType xws,
						 xmlSchemaValPtr y,
						 xmlSchemaWhitespaceValueType yws);
XMLPUBFUN xmlSchemaValPtr XMLCALL
		xmlSchemaCopyValue		(xmlSchemaValPtr val);
XMLPUBFUN xmlSchemaValType XMLCALL
		xmlSchemaGetValType		(xmlSchemaValPtr val);
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
