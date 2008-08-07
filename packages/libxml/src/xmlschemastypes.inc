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
procedure xmlSchemaInitTypes; XMLCALL; XMLPUBFUN;
procedure xmlSchemaCleanupTypes; XMLCALL; XMLPUBFUN;
function xmlSchemaGetPredefinedType(name, ns: xmlCharPtr): xmlSchemaTypePtr; XMLCALL; XMLPUBFUN;
function xmlSchemaValidatePredefinedType(_type: xmlSchemaTypePtr; value: xmlCharPtr; var val: xmlSchemaValPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValPredefTypeNode(_type: xmlSchemaTypePtr; value: xmlCharPtr; var val: xmlSchemaValPtr; node: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateFacet(base: xmlSchemaTypePtr; facet: xmlSchemaFacetPtr; value: xmlCharPtr; val: xmlSchemaValPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateFacetWhtsp(facet: xmlSchemaFacetPtr; fws: xmlSchemaWhitespaceValueType; valType: xmlSchemaValType;
  value: xmlCharPtr; val: xmlSchemaValPtr; ws: xmlSchemaWhitespaceValueType): cint; XMLCALL; XMLPUBFUN;
procedure xmlSchemaFreeValue(val: xmlSchemaValPtr); XMLCALL; XMLPUBFUN;
function xmlSchemaNewFacet: xmlSchemaFacetPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaCheckFacet(facet: xmlSchemaFacetPtr; typeDecl: xmlSchemaTypePtr; ctxt: xmlSchemaParserCtxtPtr; name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
procedure xmlSchemaFreeFacet(facet: xmlSchemaFacetPtr); XMLCALL; XMLPUBFUN;
function xmlSchemaCompareValues(x, y: xmlSchemaValPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaGetBuiltInListSimpleTypeItemType(_type: xmlSchemaTypePtr): xmlSchemaTypePtr; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateListSimpleTypeFacet(facet: xmlSchemaFacetPtr; value: xmlCharPtr; actualLen: culong; expectedLen: pculong): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaGetBuiltInType(_type: xmlSchemaValType): xmlSchemaTypePtr; XMLCALL; XMLPUBFUN;
function xmlSchemaIsBuiltInTypeFacet(_type: xmlSchemaTypePtr; faceType: cint): xmlSchemaTypePtr; XMLCALL; XMLPUBFUN;
function xmlSchemaCollapseString(value: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaWhiteSpaceReplace(value: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaGetFacetValueAsULong(facet: xmlSchemaFacetPtr): culong; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateLengthFacet(_type: xmlSchemaTypePtr; facet: xmlSchemaFacetPtr; value: xmlCharPtr; val: xmlSchemaValPtr; length: pculong): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValidateLengthFacetWhtsp(facet: xmlSchemaFacetPtr; valType: xmlSchemaValType; value: xmlCharPtr; val: xmlSchemaValPtr; length: pculong; ws: xmlSchemaWhitespaceValueType): cint; XMLCALL; XMLPUBFUN;


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
