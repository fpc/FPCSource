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

{$IFDEF FUNCTION}
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
function xmlSchemaValPredefTypeNodeNoNorm(_type: xmlSchemaTypePtr; value: xmlCharPtr; val: xmlSchemaValPtrPtr; node: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaGetCanonValue(val: xmlSchemaValPtr; var retValue: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaGetCanonValueWhtsp(val: xmlSchemaValPtr; var retValue: xmlCharPtr; ws: xmlSchemaWhitespaceValueType): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValueAppend(prev, cur: xmlSchemaValPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaValueGetNext(cur: xmlSchemaValPtr): xmlSchemaValPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaValueGetAsString(val: xmlSchemaValPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaValueGetAsBoolean(val: xmlSchemaValPtr): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaNewStringValue(_type: xmlSchemaValType; value: xmlCharPtr): xmlSchemaValPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaNewNOTATIONValue(name, ns: xmlCharPtr): xmlSchemaValPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaNewQNameValue(namespaceName, localName: xmlCharPtr): xmlSchemaValPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaCompareValuesWhtsp(x: xmlSchemaValPtr; xws: xmlSchemaWhitespaceValueType; y: xmlSchemaValPtr; yws: xmlSchemaWhitespaceValueType): cint; XMLCALL; XMLPUBFUN;
function xmlSchemaCopyValue(val: xmlSchemaValPtr): xmlSchemaValPtr; XMLCALL; XMLPUBFUN;
function xmlSchemaGetValType(val: xmlSchemaValPtr): xmlSchemaValType; XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_SCHEMAS_ENABLED *)
