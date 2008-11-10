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

XMLPUBFUN int XMLCALL		
		xmlXPathPopBoolean	(xmlXPathParserContextPtr ctxt);
XMLPUBFUN double XMLCALL		
    		xmlXPathPopNumber	(xmlXPathParserContextPtr ctxt);
XMLPUBFUN xmlChar * XMLCALL	
    		xmlXPathPopString	(xmlXPathParserContextPtr ctxt);
XMLPUBFUN xmlNodeSetPtr XMLCALL	
    		xmlXPathPopNodeSet	(xmlXPathParserContextPtr ctxt);
XMLPUBFUN void * XMLCALL		
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
procedure xmlXPathRegisterVariableLookup(ctxt: xmlXPathContextPtr; f: xmlXPathVariableLookupFunc; data: pointer); XMLCALL; XMLPUBFUN;

(*
 * Function Lookup forwarding.
 *)
procedure xmlXPathRegisterFuncLookup(ctxt: xmlXPathContextPtr; f: xmlXPathFuncLookupFunc; funcCtxt: pointer); XMLCALL; XMLPUBFUN;

(*
 * Error reporting.
 *)
//procedure __xmlXPatherror(ctxt: xmlXPathParserContextPtr; _file: pchar; line, no: cint); XMLCALL; XMLPUBFUN name 'xmlXPatherror';
procedure xmlXPathErr(ctxt: xmlXPathParserContextPtr; error: cint); XMLCALL; XMLPUBFUN;

{$IFDEF LIBXML_DEBUG_ENABLED}
procedure xmlXPathDebugDumpObject(output: PFILE; cur: xmlXPathObjectPtr; depth: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathDebugDumpCompExpr(output: PFILE; comp: xmlXPathCompExprPtr; depth: cint); XMLCALL; XMLPUBFUN;
{$ENDIF}

(**
 * NodeSet handling.
 *)
function xmlXPathNodeSetContains(cur: xmlNodeSetPtr; val: xmlNodePtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathDifference(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathIntersection(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathDistinctSorted(nodes: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathDistinct(nodes: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathHasSameNodes(nodes1, nodes2: xmlNodeSetPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathNodeLeadingSorted(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathLeadingSorted(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNodeLeading(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathLeading(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNodeTrailingSorted(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathTrailingSorted(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNodeTrailing(nodes: xmlNodeSetPtr; node: xmlNodePtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;
function xmlXPathTrailing(nodes1, nodes2: xmlNodeSetPtr): xmlNodeSetPtr; XMLCALL; XMLPUBFUN;

(**
 * Extending a context.
 *)
function xmlXPathRegisterNs(ctxt: xmlXPathContextPtr; prefix, ns_uri: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathNsLookup(ctxt: xmlXPathContextPtr; prefix: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPathDebugDumpCompExpr(ctxt: xmlXPathContextPtr); XMLCALL; XMLPUBFUN;
function xmlXPathRegisterFunc(ctxt: xmlXPathContextPtr; name: xmlCharPtr; f: xmlXPathFunction): cint; XMLCALL; XMLPUBFUN;
function xmlXPathRegisterFuncNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr; f: xmlXPathFunction): cint; XMLCALL; XMLPUBFUN;
function xmlXPathRegisterVariable(ctxt: xmlXPathContextPtr; name: xmlCharPtr; value: xmlXPathObjectPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathRegisterVariableNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr; value: xmlXPathObjectPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathFunctionLookup(ctxt: xmlXPathContextPtr; name: xmlCharPtr): xmlXPathFunction; XMLCALL; XMLPUBFUN;
function xmlXPathFunctionLookupNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr): xmlXPathFunction; XMLCALL; XMLPUBFUN;
procedure xmlXPathRegisteredFuncsCleanup(ctxt: xmlXPathContextPtr); XMLCALL; XMLPUBFUN;
function xmlXPathVariableLookup(ctxt: xmlXPathContextPtr; name: xmlCharPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathVariableLookupNS(ctxt: xmlXPathContextPtr; name, ns_uri: xmlCharPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPathRegisteredVariablesCleanup(ctxt: xmlXPathContextPtr); XMLCALL; XMLPUBFUN;

(**
 * Utilities to extend XPath.
 *)
function xmlXPathNewParserContext(str: xmlCharPtr; ctxt: xmlXPathContextPtr): xmlXPathParserContextPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPathFreeParserContext(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;

(* TODO: remap to xmlXPathValuePop and Push. *)
function valuePop(ctxt: xmlXPathParserContextPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function valuePush(ctxt: xmlXPathParserContextPtr): cint; XMLCALL; XMLPUBFUN;

function xmlXPathNewString(val: xmlCharPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNewCString(val: pchar): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathWrapString(val: xmlCharPtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathWrapCString(val: pchar): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNewFloat(val: cdouble): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNewBoolean(val: cint): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNewNodeSet(val: xmlNodePtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
function xmlXPathNewValueTree(val: xmlNodePtr): xmlXPathObjectPtr; XMLCALL; XMLPUBFUN;
procedure xmlXPathNodeSetAddUnique(cur: xmlNodeSetPtr; val: xmlNodePtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathNodeSetAdd(cur: xmlNodeSetPtr; val: xmlNodePtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathNodeSetAddNs(cur: xmlNodeSetPtr; node: xmlNodePtr; ns: xmlNsPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathNodeSetSort(_set: xmlNodeSetPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathRoot(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathEvalExpr(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
function xmlXPathParseName(ctxt: xmlXPathParserContextPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlXPathParseNCName(ctxt: xmlXPathParserContextPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;

(*
 * Existing functions.
 *)
{XMLPUBFUN double XMLCALL 
		xmlXPathStringEvalNumber	(xmlChar *str);
XMLPUBFUN int XMLCALL 
		xmlXPathEvaluatePredicateResult (xmlXPathParserContextPtr ctxt, 
						 xmlXPathObjectPtr res);
XMLPUBFUN void XMLCALL 
		xmlXPathRegisterAllFunctions	(xmlXPathContextPtr ctxt);
XMLPUBFUN xmlNodeSetPtr XMLCALL 
		xmlXPathNodeSetMerge		(xmlNodeSetPtr val1, 
						 xmlNodeSetPtr val2);
XMLPUBFUN void XMLCALL 
		xmlXPathNodeSetDel		(xmlNodeSetPtr cur, 
						 xmlNodePtr val);
XMLPUBFUN void XMLCALL 
		xmlXPathNodeSetRemove		(xmlNodeSetPtr cur, 
						 int val);
XMLPUBFUN xmlXPathObjectPtr XMLCALL 
		xmlXPathNewNodeSetList		(xmlNodeSetPtr val);
XMLPUBFUN xmlXPathObjectPtr XMLCALL 
		xmlXPathWrapNodeSet		(xmlNodeSetPtr val);
XMLPUBFUN xmlXPathObjectPtr XMLCALL 
		xmlXPathWrapExternal		(void *val);}

function xmlXPathEqualValues(ctxt: xmlXPathParserContextPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathNotEqualValues(ctxt: xmlXPathParserContextPtr): cint; XMLCALL; XMLPUBFUN;
function xmlXPathCompareValues(ctxt: xmlXPathParserContextPtr; inf, strict: cint): cint; XMLCALL; XMLPUBFUN;
procedure xmlXPathValueFlipSign(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathAddValues(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathSubValues(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathMultValues(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathDivValues(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;
procedure xmlXPathModValues(ctxt: xmlXPathParserContextPtr); XMLCALL; XMLPUBFUN;

function xmlXPathIsNodeType(name: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;


(*
 * Some of the axis navigation routines.
 *)
function xmlXPathNextSelf(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextChild(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextDescendant(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextDescendantOrSelf(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextParent(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextAncestorOrSelf(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextFollowingSibling(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextFollowing(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextNamespace(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextAttribute(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextPreceding(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextAncestor(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;
function xmlXPathNextPrecedingSibling(ctxt: xmlXPathParserContextPtr; cur: xmlNodePtr): xmlNodePtr; XMLCALL; XMLPUBFUN;

(*
 * The official core of XPath functions.
 *)
procedure xmlXPathLastFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathPositionFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathCountFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathIdFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathLocalNameFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathNamespaceURIFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathStringFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathStringLengthFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathConcatFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathContainsFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathStartsWithFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathSubstringFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathSubstringBeforeFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathSubstringAfterFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathNormalizeFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathTranslateFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathNotFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathTrueFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathFalseFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathLangFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathNumberFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathSumFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathFloorFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathCeilingFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathRoundFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;
procedure xmlXPathBooleanFunction(ctxt: xmlXPathParserContextPtr; nargs: cint); XMLCALL; XMLPUBFUN;

(**
 * Really internal functions
 *)
procedure xmlXPathNodeSetFreeNs(ns: xmlNsPtr); XMLCALL; XMLPUBFUN;
{$ENDIF}

{$ENDIF} (* LIBXML_XPATH_ENABLED *)
