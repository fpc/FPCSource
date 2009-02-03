(*
 * Summary: internal interfaces for XML Path Language implementation
 * Description: internal interfaces for XML Path Language implementation
 *              used to build new modules on top of XPath like XPointer and
 *              XSLT
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_XPATH_ENABLED}

{$IFDEF _FUNCTION}

(************************************************************************
 *									*
 *			Helpers						*
 *									*
 ************************************************************************)

(*
 * Many of these macros may later turn into functions. They
 * shouldn't be used in {'s preprocessor instructions.
 *)
(**
 * xmlXPathSetError:
 * @ctxt:  an XPath parser context
 * @err:  an xmlXPathError code
 *
 * Raises an error.
 *)
#define xmlXPathSetError(ctxt, err)					\
    { xmlXPatherror((ctxt), __FILE__, __LINE__, (err));			\
      if ((ctxt) != NULL) (ctxt)->error = (err); }

(**
 * xmlXPathSetArityError:
 * @ctxt:  an XPath parser context
 *
 * Raises an XPATH_INVALID_ARITY error.
 *)
#define xmlXPathSetArityError(ctxt)					\
    xmlXPathSetError((ctxt), XPATH_INVALID_ARITY)

(**
 * xmlXPathSetTypeError:
 * @ctxt:  an XPath parser context
 *
 * Raises an XPATH_INVALID_TYPE error.
 *)
#define xmlXPathSetTypeError(ctxt)					\
    xmlXPathSetError((ctxt), XPATH_INVALID_TYPE)

(**
 * xmlXPathGetError:
 * @ctxt:  an XPath parser context
 *
 * Get the error code of an XPath context.
 *
 * Returns the context error.
 *)
#define xmlXPathGetError(ctxt)	  ((ctxt)->error)

(**
 * xmlXPathCheckError:
 * @ctxt:  an XPath parser context
 *
 * Check if an XPath error was raised.
 *
 * Returns true if an error has been raised, false otherwise.
 *)
#define xmlXPathCheckError(ctxt)  ((ctxt)->error != XPATH_EXPRESSION_OK)

(**
 * xmlXPathGetDocument:
 * @ctxt:  an XPath parser context
 *
 * Get the document of an XPath context.
 *
 * Returns the context document.
 *)
#define xmlXPathGetDocument(ctxt)	((ctxt)->context->doc)

(**
 * xmlXPathGetContextNode:
 * @ctxt: an XPath parser context
 *
 * Get the context node of an XPath context.
 *
 * Returns the context node.
 *)
#define xmlXPathGetContextNode(ctxt)	((ctxt)->context->node)

external xml2lib int EXTDECL		
		xmlXPathPopBoolean	(xmlXPathParserContextPtr ctxt);
external xml2lib double EXTDECL		
    		xmlXPathPopNumber	(xmlXPathParserContextPtr ctxt);
external xml2lib xmlChar * EXTDECL	
    		xmlXPathPopString	(xmlXPathParserContextPtr ctxt);
external xml2lib xmlNodeSetPtr EXTDECL	
    		xmlXPathPopNodeSet	(xmlXPathParserContextPtr ctxt);
external xml2lib void * EXTDECL		
    		xmlXPathPopExternal	(xmlXPathParserContextPtr ctxt);

(**
 * xmlXPathReturnBoolean:
 * @ctxt:  an XPath parser context
 * @val:  a boolean
 *
 * Pushes the boolean @val on the context stack.
 *)
#define xmlXPathReturnBoolean(ctxt, val)				\
    valuePush((ctxt), xmlXPathNewBoolean(val))

(**
 * xmlXPathReturnTrue:
 * @ctxt:  an XPath parser context
 *
 * Pushes true on the context stack.
 *)
#define xmlXPathReturnTrue(ctxt)   xmlXPathReturnBoolean((ctxt), 1)

(**
 * xmlXPathReturnFalse:
 * @ctxt:  an XPath parser context
 *
 * Pushes false on the context stack.
 *)
#define xmlXPathReturnFalse(ctxt)  xmlXPathReturnBoolean((ctxt), 0)

(**
 * xmlXPathReturnNumber:
 * @ctxt:  an XPath parser context
 * @val:  a double
 *
 * Pushes the double @val on the context stack.
 *)
#define xmlXPathReturnNumber(ctxt, val)					\
    valuePush((ctxt), xmlXPathNewFloat(val))

(**
 * xmlXPathReturnString:
 * @ctxt:  an XPath parser context
 * @str:  a string
 *
 * Pushes the string @str on the context stack.
 *)
#define xmlXPathReturnString(ctxt, str)					\
    valuePush((ctxt), xmlXPathWrapString(str))

(**
 * xmlXPathReturnEmptyString:
 * @ctxt:  an XPath parser context
 *
 * Pushes an empty string on the stack.
 *)
#define xmlXPathReturnEmptyString(ctxt)					\
    valuePush((ctxt), xmlXPathNewCString(""))

(**
 * xmlXPathReturnNodeSet:
 * @ctxt:  an XPath parser context
 * @ns:  a node-set
 *
 * Pushes the node-set @ns on the context stack.
 *)
#define xmlXPathReturnNodeSet(ctxt, ns)					\
    valuePush((ctxt), xmlXPathWrapNodeSet(ns))

(**
 * xmlXPathReturnEmptyNodeSet:
 * @ctxt:  an XPath parser context
 *
 * Pushes an empty node-set on the context stack.
 *)
#define xmlXPathReturnEmptyNodeSet(ctxt)				\
    valuePush((ctxt), xmlXPathNewNodeSet(NULL))

(**
 * xmlXPathReturnExternal:
 * @ctxt:  an XPath parser context
 * @val:  user data
 *
 * Pushes user data on the context stack.
 *)
#define xmlXPathReturnExternal(ctxt, val)				\
    valuePush((ctxt), xmlXPathWrapExternal(val))

(**
 * xmlXPathStackIsNodeSet:
 * @ctxt: an XPath parser context
 *
 * Check if the current value on the XPath stack is a node set or
 * an XSLT value tree.
 *
 * Returns true if the current object on the stack is a node-set.
 *)
#define xmlXPathStackIsNodeSet(ctxt)					\
    (((ctxt)->value != NULL)						\
     && (((ctxt)->value->type == XPATH_NODESET)				\
         || ((ctxt)->value->type == XPATH_XSLT_TREE)))

(**
 * xmlXPathStackIsExternal:
 * @ctxt: an XPath parser context
 *
 * Checks if the current value on the XPath stack is an external
 * object.
 *
 * Returns true if the current object on the stack is an external
 * object.
 *)
#define xmlXPathStackIsExternal(ctxt)					\
	((ctxt->value != NULL) && (ctxt->value->type == XPATH_USERS))

(**
 * xmlXPathEmptyNodeSet:
 * @ns:  a node-set
 *
 * Empties a node-set.
 *)
#define xmlXPathEmptyNodeSet(ns)					\
    { while ((ns)->nodeNr > 0) (ns)->nodeTab[(ns)->nodeNr--] = NULL; }

(**
 * CHECK_ERROR:
 *
 * Macro to return from the function if an XPath error was detected.
 *)
#define CHECK_ERROR							\
    if (ctxt->error != XPATH_EXPRESSION_OK) return

(**
 * CHECK_ERROR0:
 *
 * Macro to return 0 from the function if an XPath error was detected.
 *)
#define CHECK_ERROR0							\
    if (ctxt->error != XPATH_EXPRESSION_OK) return(0)

(**
 * XP_ERROR:
 * @X:  the error code
 *
 * Macro to raise an XPath error and return.
 *)
#define XP_ERROR(X)							\
    { xmlXPathErr(ctxt, X); return; }

(**
 * XP_ERROR0:
 * @X:  the error code
 *
 * Macro to raise an XPath error and return 0.
 *)
#define XP_ERROR0(X)							\
    { xmlXPathErr(ctxt, X); return(0); }

(**
 * CHECK_TYPE:
 * @typeval:  the XPath type
 *
 * Macro to check that the value on top of the XPath stack is of a given
 * type.
 *)
#define CHECK_TYPE(typeval)						\
    if ((ctxt->value == NULL) || (ctxt->value->type != typeval))	\
        XP_ERROR(XPATH_INVALID_TYPE)

(**
 * CHECK_TYPE0:
 * @typeval:  the XPath type
 *
 * Macro to check that the value on top of the XPath stack is of a given
 * type. Return(0) in case of failure
 *)
#define CHECK_TYPE0(typeval)						\
    if ((ctxt->value == NULL) || (ctxt->value->type != typeval))	\
        XP_ERROR0(XPATH_INVALID_TYPE)

(**
 * CHECK_ARITY:
 * @x:  the number of expected args
 *
 * Macro to check that the number of args passed to an XPath function matches.
 *)
#define CHECK_ARITY(x)							\
    if (ctxt == NULL) return;						\
    if (nargs != (x))							\
        XP_ERROR(XPATH_INVALID_ARITY);

(**
 * CAST_TO_STRING:
 *
 * Macro to try to cast the value on the top of the XPath stack to a string.
 *)
#define CAST_TO_STRING							\
    if ((ctxt->value != NULL) && (ctxt->value->type != XPATH_STRING))	\
        xmlXPathStringFunction(ctxt, 1);

(**
 * CAST_TO_NUMBER:
 *
 * Macro to try to cast the value on the top of the XPath stack to a number.
 *)
#define CAST_TO_NUMBER							\
    if ((ctxt->value != NULL) && (ctxt->value->type != XPATH_NUMBER))	\
        xmlXPathNumberFunction(ctxt, 1);

(**
 * CAST_TO_BOOLEAN:
 *
 * Macro to try to cast the value on the top of the XPath stack to a boolean.
 *)
#define CAST_TO_BOOLEAN							\
    if ((ctxt->value != NULL) && (ctxt->value->type != XPATH_BOOLEAN))	\
        xmlXPathBooleanFunction(ctxt, 1);
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Variable Lookup forwarding.
 *)
procedure xmlXPathRegisterVariableLookup(ctxt: xmlXPathContextPtr; f: xmlXPathVariableLookupFunc; data: pointer); EXTDECL; external xml2lib;

(*
 * Function Lookup forwarding.
 *)
procedure xmlXPathRegisterFuncLookup(ctxt: xmlXPathContextPtr; f: xmlXPathFuncLookupFunc; funcCtxt: pointer); EXTDECL; external xml2lib;

(*
 * Error reporting.
 *)
//procedure __xmlXPatherror(ctxt: xmlXPathParserContextPtr; _file: pchar; line, no: cint); EXTDECL; external xml2lib name 'xmlXPatherror';
procedure xmlXPathErr(ctxt: xmlXPathParserContextPtr; error: cint); EXTDECL; external xml2lib;

{$IFDEF LIBXML_DEBUG_ENABLED}
procedure xmlXPathDebugDumpObject(output: PFILE; cur: xmlXPathObjectPtr; depth: cint); EXTDECL; external xml2lib;
procedure xmlXPathDebugDumpCompExpr(output: PFILE; comp: xmlXPathCompExprPtr; depth: cint); EXTDECL; external xml2lib;
{$ENDIF}

(**
 * NodeSet handling.
 *)
function xmlXPathNodeSetContains(cur: xmlNodeSetPtr; val: xmlNodePtr): cint; EXTDECL; external xml2lib;
function xmlXPathDifference(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathIntersection(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathDistinctSorted(nodes: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathDistinct(nodes: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathHasSameNodes(nodes1, nodes2: xmlNodeSetPtr): cint; EXTDECL; external xml2lib;
function xmlXPathNodeLeadingSorted(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathLeadingSorted(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathNodeLeading(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathLeading(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathNodeTrailingSorted(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathTrailingSorted(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathNodeTrailing(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
function xmlXPathTrailing(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; EXTDECL; external xml2lib;

(**
 * Extending a context.
 *)
function xmlXPathRegisterNs(ctxt: xmlXPathContextPtr; prefix, ns_uri: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlXPathNsLookup(ctxt: xmlXPathContextPtr; prefix: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
procedure xmlXPathDebugDumpCompExpr(ctxt: xmlXPathContextPtr); EXTDECL; external xml2lib;
function xmlXPathRegisterFunc(ctxt: xmlXPathContextPtr; name: xmlCharPtr; f: xmlXPathFunction): cint; EXTDECL; external xml2lib;
function xmlXPathRegisterFuncNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr; f: xmlXPathFunction): cint; EXTDECL; external xml2lib;
function xmlXPathRegisterVariable(ctxt: xmlXPathContextPtr; name: xmlCharPtr; value: xmlXPathObjectPtr): cint; EXTDECL; external xml2lib;
function xmlXPathRegisterVariableNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr; value: xmlXPathObjectPtr): cint; EXTDECL; external xml2lib;
function xmlXPathFunctionLookup(ctxt: xmlXPathContextPtr; name: xmlCharPtr): xmlXPathFunction; EXTDECL; external xml2lib;
function xmlXPathFunctionLookupNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr): xmlXPathFunction; EXTDECL; external xml2lib;
procedure xmlXPathRegisteredFuncsCleanup(ctxt: xmlXPathContextPtr); EXTDECL; external xml2lib;
function xmlXPathVariableLookup(ctxt: xmlXPathContextPtr; name: xmlCharPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathVariableLookupNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
procedure xmlXPathRegisteredVariablesCleanup(ctxt: xmlXPathContextPtr); EXTDECL; external xml2lib;

(**
 * Utilities to extend XPath.
 *)
function xmlXPathNewParserContext(str: xmlCharPtr; ctxt: xmlXPathContextPtr): xmlXPathParserContextPtr; EXTDECL; external xml2lib;
procedure xmlXPathFreeParserContext(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;

(* TODO: remap to xmlXPathValuePop and Push. *)
function valuePop(ctxt: xmlXPathParserContextPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function valuePush(ctxt: xmlXPathParserContextPtr): cint; EXTDECL; external xml2lib;

function xmlXPathNewString(val: xmlCharPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathNewCString(val: pchar): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathWrapString(val: xmlCharPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathWrapCString(val: pchar): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathNewFloat(val: cdouble): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathNewBoolean(val: cint): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathNewNodeSet(val: xmlNodePtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathNewValueTree(val: xmlNodePtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
procedure xmlXPathNodeSetAddUnique(cur: xmlNodeSetPtr; val: xmlNodePtr); EXTDECL; external xml2lib;
procedure xmlXPathNodeSetAdd(cur: xmlNodeSetPtr; val: xmlNodePtr); EXTDECL; external xml2lib;
procedure xmlXPathNodeSetAddNs(cur: xmlNodeSetPtr; node: xmlNodePtr; ns: xmlNsPtr); EXTDECL; external xml2lib;
procedure xmlXPathNodeSetSort(_set: xmlNodeSetPtr); EXTDECL; external xml2lib;
procedure xmlXPathRoot(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
procedure xmlXPathEvalExpr(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
function xmlXPathParseName(ctxt: xmlXPathParserContextPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlXPathParseNCName(ctxt: xmlXPathParserContextPtr): xmlCharPtr; EXTDECL; external xml2lib;

(*
 * Existing functions.
 *)
{external xml2lib double EXTDECL 
		xmlXPathStringEvalNumber	(xmlChar *str);
external xml2lib int EXTDECL 
		xmlXPathEvaluatePredicateResult (xmlXPathParserContextPtr ctxt, 
						 xmlXPathObjectPtr res);
external xml2lib void EXTDECL 
		xmlXPathRegisterAllFunctions	(xmlXPathContextPtr ctxt);
external xml2lib xmlNodeSetPtr EXTDECL 
		xmlXPathNodeSetMerge		(xmlNodeSetPtr val1, 
						 xmlNodeSetPtr val2);
external xml2lib void EXTDECL 
		xmlXPathNodeSetDel		(xmlNodeSetPtr cur, 
						 xmlNodePtr val);
external xml2lib void EXTDECL 
		xmlXPathNodeSetRemove		(xmlNodeSetPtr cur, 
						 int val);
external xml2lib xmlXPathObjectPtr EXTDECL 
		xmlXPathNewNodeSetList		(xmlNodeSetPtr val);
external xml2lib xmlXPathObjectPtr EXTDECL 
		xmlXPathWrapNodeSet		(xmlNodeSetPtr val);
external xml2lib xmlXPathObjectPtr EXTDECL 
		xmlXPathWrapExternal		(void *val);}

function xmlXPathEqualValues(ctxt: xmlXPathParserContextPtr): cint; EXTDECL; external xml2lib;
function xmlXPathNotEqualValues(ctxt: xmlXPathParserContextPtr): cint; EXTDECL; external xml2lib;
function xmlXPathCompareValues(ctxt: xmlXPathParserContextPtr; inf, strict: cint): cint; EXTDECL; external xml2lib;
procedure xmlXPathValueFlipSign(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
procedure xmlXPathAddValues(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
procedure xmlXPathSubValues(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
procedure xmlXPathMultValues(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
procedure xmlXPathDivValues(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;
procedure xmlXPathModValues(ctxt: xmlXPathParserContextPtr); EXTDECL; external xml2lib;

function xmlXPathIsNodeType(name: xmlCharPtr): cint; EXTDECL; external xml2lib;


(*
 * Some of the axis navigation routines.
 *)
function xmlXPathNextSelf(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextChild(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextDescendant(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextDescendantOrSelf(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextParent(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextAncestorOrSelf(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextFollowingSibling(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextFollowing(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextNamespace(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextAttribute(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextPreceding(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextAncestor(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;
function xmlXPathNextPrecedingSibling(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; EXTDECL; external xml2lib;

(*
 * The official core of XPath functions.
 *)
procedure xmlXPathLastFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathPositionFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathCountFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathIdFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathLocalNameFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathNamespaceURIFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathStringFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathStringLengthFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathConcatFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathContainsFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathStartsWithFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathSubstringFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathSubstringBeforeFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathSubstringAfterFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathNormalizeFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathTranslateFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathNotFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathTrueFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathFalseFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathLangFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathNumberFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathSumFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathFloorFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathCeilingFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathRoundFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;
procedure xmlXPathBooleanFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL; external xml2lib;

(**
 * Really internal functions
 *)
procedure xmlXPathNodeSetFreeNs(ns: xmlNsPtr); EXTDECL; external xml2lib;
{$ENDIF}

{$ENDIF} (* LIBXML_XPATH_ENABLED *)
