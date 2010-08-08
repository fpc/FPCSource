(*
 * Summary: XML Path Language implementation
 * Description: API for the XML Path Language implementation
 *
 * XML Path Language implementation
 * XPath is a language for addressing parts of an XML document,
 * designed to be used by both XSLT and XPointer
 *     http://www.w3.org/TR/xpath
 *
 * Implements
 * W3C Recommendation 16 November 1999
 *     http://www.w3.org/TR/1999/REC-xpath-19991116
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_XPATH_ENABLED}

{$IFDEF POINTER}
  xmlXPathContextPtr = ^xmlXPathContext;
  xmlXPathParserContextPtr = ^xmlXPathParserContext;
  xmlNodeSetPtr = ^xmlNodeSet;
  xmlXPathObjectPtr = ^xmlXPathObject;
  xmlXPathObjectPtrPtr = ^xmlXPathObjectPtr;
  xmlXPathTypePtr = ^xmlXPathType;
  xmlXPathVariablePtr = ^xmlXPathVariable;
  xmlXPathFuncPtr = ^xmlXPathFunc;
  xmlXPathAxisPtr = ^xmlXPathAxis;
  xmlXPathCompExprPtr = ^xmlXPathCompExpr;
{$ENDIF}

{$IFDEF TYPE}
(**
 * The set of XPath error codes.
 *)

  xmlXPathError = (
    XPATH_EXPRESSION_OK = 0,
    XPATH_NUMBER_ERROR,
    XPATH_UNFINISHED_LITERAL_ERROR,
    XPATH_START_LITERAL_ERROR,
    XPATH_VARIABLE_REF_ERROR,
    XPATH_UNDEF_VARIABLE_ERROR,
    XPATH_INVALID_PREDICATE_ERROR,
    XPATH_EXPR_ERROR,
    XPATH_UNCLOSED_ERROR,
    XPATH_UNKNOWN_FUNC_ERROR,
    XPATH_INVALID_OPERAND,
    XPATH_INVALID_TYPE,
    XPATH_INVALID_ARITY,
    XPATH_INVALID_CTXT_SIZE,
    XPATH_INVALID_CTXT_POSITION,
    XPATH_MEMORY_ERROR,
    XPTR_SYNTAX_ERROR,
    XPTR_RESOURCE_ERROR,
    XPTR_SUB_RESOURCE_ERROR,
    XPATH_UNDEF_PREFIX_ERROR,
    XPATH_ENCODING_ERROR,
    XPATH_INVALID_CHAR_ERROR,
    XPATH_INVALID_CTXT
  );

(*
 * A node-set (an unordered collection of nodes without duplicates).
 *)
  xmlNodeSet = record
    nodeNr  : cint;			(* number of nodes in the set *)
    nodeMax : cint;		(* size of the array as allocated *)
    nodeTab : xmlNodePtrPtr;	(* array of nodes in no particular order *)
    (* @@ with_ns to check wether namespace nodes should be looked at @@ *)
  end;

(*
 * An expression is evaluated to yield an object, which
 * has one of the following four basic types:
 *   - node-set
 *   - boolean
 *   - number
 *   - string
 *
 * @@ XPointer will add more types !
 *)

  xmlXPathObjectType = (
    XPATH_UNDEFINED = 0,
    XPATH_NODESET = 1,
    XPATH_BOOLEAN = 2,
    XPATH_NUMBER = 3,
    XPATH_STRING = 4,
    XPATH_POINT = 5,
    XPATH_RANGE = 6,
    XPATH_LOCATIONSET = 7,
    XPATH_USERS = 8,
    XPATH_XSLT_TREE = 9  (* An XSLT value tree, non modifiable *)
  );

  xmlXPathObject = record
    _type       : xmlXPathObjectType;
    nodesetval  : xmlNodeSetPtr;
    boolval     : cint;
    floatval    : cdouble;
    stringval   : xmlCharPtr;
    user        : pointer;
    index       : cint;
    user2       : pointer;
    index2      : cint;
  end;

(**
 * xmlXPathConvertFunc:
 * @obj:  an XPath object
 * @type:  the number of the target type
 *
 * A conversion function is associated to a type and used to cast
 * the new type to primitive values.
 *
 * Returns -1 in case of error, 0 otherwise
 *)
  xmlXPathConvertFunc = function(obj: xmlXPathObjectPtr; _type: cint): cint; EXTDECL;

(*
 * Extra type: a name and a conversion function.
 *)
  xmlXPathType = record
    name  : xmlCharPtr;		(* the type name *)
    func  : xmlXPathConvertFunc;		(* the conversion function *)
  end;

(*
 * Extra variable: a name and a value.
 *)
  xmlXPathVariable = record
    name  : xmlCharPtr;		(* the variable name *)
    value : xmlXPathObjectPtr;		(* the value *)
  end;

(**
 * xmlXPathEvalFunc:
 * @ctxt: an XPath parser context
 * @nargs: the number of arguments passed to the function
 *
 * An XPath evaluation function, the parameters are on the XPath context stack.
 *)

  xmlXPathEvalFunc = procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;

(*
 * Extra function: a name and a evaluation function.
 *)

  xmlXPathFunc = record
    name  : xmlCharPtr;		(* the function name *)
    func  : xmlXPathEvalFunc;		(* the evaluation function *)
  end;

(**
 * xmlXPathAxisFunc:
 * @ctxt:  the XPath interpreter context
 * @cur:  the previous node being explored on that axis
 *
 * An axis traversal function. To traverse an axis, the engine calls
 * the first time with cur == NULL and repeat until the function returns
 * NULL indicating the end of the axis traversal.
 *
 * Returns the next node in that axis or NULL if at the end of the axis.
 *)

  xmlXPathAxisFunc = function(ctxt: xmlXPathParserContextPtr; cur: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL;

(*
 * Extra axis: a name and an axis function.
 *)
  xmlXPathAxis = record
    name  : xmlCharPtr;		(* the axis name *)
    func  : xmlXPathAxisFunc;		(* the search function *)
  end;

(**
 * xmlXPathFunction:
 * @ctxt:  the XPath interprestation context
 * @nargs:  the number of arguments
 *
 * An XPath function.
 * The arguments (if any) are popped out from the context stack
 * and the result is pushed on the stack.
 *)
  xmlXPathFunction = procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); EXTDECL;

(*
 * Function and Variable Lookup.
 *)

(**
 * xmlXPathVariableLookupFunc:
 * @ctxt:  an XPath context
 * @name:  name of the variable
 * @ns_uri:  the namespace name hosting this variable
 *
 * Prototype for callbacks used to plug variable lookup in the XPath
 * engine.
 *
 * Returns the XPath object value or NULL if not found.
 *)
  xmlXPathVariableLookupFunc = function(ctxt: pointer; name, ns_uri: xmlCharPtr): xmlXPathObjectPtr; EXTDECL;

(**
 * xmlXPathFuncLookupFunc:
 * @ctxt:  an XPath context
 * @name:  name of the function
 * @ns_uri:  the namespace name hosting this function
 *
 * Prototype for callbacks used to plug function lookup in the XPath
 * engine.
 *
 * Returns the XPath function or NULL if not found.
 *)
  xmlXPathFuncLookupFunc = function(ctxt: pointer; name, ns_uri: xmlCharPtr): xmlXPathFunction; EXTDECL;
{$ENDIF}

{$IFDEF CONST}
(**
 * xmlXPathFlags:
 * Flags for XPath engine compilation and runtime
 *)
(**
 * XML_XPATH_CHECKNS:
 *
 * check namespaces at compilation
 *)
  XML_XPATH_CHECKNS = (1 shl 0);
(**
 * XML_XPATH_NOVAR:
 *
 * forbid variables in expression
 *)
  XML_XPATH_NOVAR	 = (1 shl 1);
{$ENDIF}

{$IFDEF TYPE}
(**
 * xmlXPathContext:
 *
 * Expression evaluation occurs with respect to a context.
 * he context consists of:
 *    - a node (the context node) 
 *    - a node list (the context node list) 
 *    - a set of variable bindings 
 *    - a function library 
 *    - the set of namespace declarations in scope for the expression 
 * Following the switch to hash tables, this need to be trimmed up at
 * the next binary incompatible release.
 * The node may be modified when the context is passed to libxml2
 * for an XPath evaluation so you may need to initialize it again
 * before the next call.
 *)

  xmlXPathContext = record
    doc                 : xmlDocPtr;			(* The current document *)
    node                : xmlNodePtr;			(* The current node *)

    nb_variables_unused : cint;		(* unused (hash table) *)
    max_variables_unused: cint;		(* unused (hash table) *)
    varHash             : xmlHashTablePtr;		(* Hash table of defined variables *)

    nb_types            : cint;			(* number of defined types *)
    max_types           : cint;			(* max number of types *)
    types               : xmlXPathTypePtr;		(* Array of defined types *)

    nb_funcs_unused     : cint;		(* unused (hash table) *)
    max_funcs_unused    : cint;		(* unused (hash table) *)
    funcHash            : xmlHashTablePtr;		(* Hash table of defined funcs *)

    nb_axis             : cint;			(* number of defined axis *)
    max_axis            : cint;			(* max number of axis *)
    axis                : xmlXPathAxisPtr;		(* Array of defined axis *)

    (* the namespace nodes of the context node *)
    namespaces          : xmlNsPtrPtr;		(* Array of namespaces *)
    nsNr                : cint;				(* number of namespace in scope *)
    user                : pointer;				(* function to free *)

    (* extra variables *)
    contextSize         : cint;			(* the context size *)
    proximityPosition   : cint;		(* the proximity position *)

    (* extra stuff for XPointer *)
    xptr                : cint;				(* is this an XPointer context? *)
    here                : xmlNodePtr;			(* for here() *)
    origin              : xmlNodePtr;			(* for origin() *)

    (* the set of namespace declarations in scope for the expression *)
    nsHash              : xmlHashTablePtr;		(* The namespaces hash table *)
    varLookupFunc       : xmlXPathVariableLookupFunc;(* variable lookup func *)
    varLookupData       : pointer;		(* variable lookup data *)

    (* Possibility to link in an extra item *)
    extra               : pointer;                        (* needed for XSLT *)

    (* The function name and URI when calling a function *)
    _function           : xmlCharPtr;
    functionURI         : xmlCharPtr;

    (* function lookup function and data *)
    funcLookupFunc      : xmlXPathFuncLookupFunc;(* function lookup func *)
    funcLookupData      : pointer;		(* function lookup data *)

    (* temporary namespace lists kept for walking the namespace axis *)
    tmpNsList           : xmlNsPtr;		(* Array of namespaces *)
    tmpNsNr             : cint;			(* number of namespaces in scope *)

    (* error reporting mechanism *)
    userData            : pointer;                     (* user specific data block *)
    error               : xmlStructuredErrorFunc;       (* the callback in case of errors *)
    lastError           : xmlError;			(* the last error *)
    debugNode           : xmlNodePtr;		(* the source node XSLT *)

    (* dictionary *)
    dict                : xmlDictPtr;			(* dictionary if any *)

    flags               : cint;				(* flags to control compilation *)

    (* Cache for reusal of XPath objects *)
    cache               : pointer;
  end;

(*
 * The structure of a compiled expression form is not public.
 *)
  xmlXPathCompExpr = record end;

(**
 * xmlXPathParserContext:
 *
 * An XPath parser context. It contains pure parsing informations,
 * an xmlXPathContext, and the stack of objects.
 *)
  xmlXPathParserContext = record
    cur           : xmlCharPtr;			(* the current char being parsed *)
    base          : xmlCharPtr;			(* the full expression *)

    error         : cint;				(* error code *)

    context       : xmlXPathContextPtr;	(* the evaluation context *)
    value         : xmlXPathObjectPtr;	(* the current value *)
    valueNr       : cint;	(* number of values stacked *)
    valueMax      : cint;	(* max number of values stacked *)
    valueTab      : xmlXPathObjectPtrPtr;	(* stack of values *)

    comp          : xmlXPathCompExprPtr;		(* the precompiled expression *)
    xptr          : cint;				(* it this an XPointer expression *)
    ancestor      : xmlNodePtr;	(* used for walking preceding axis *)
  end;
{$ENDIF}

{$IFDEF FUNCTION}
(************************************************************************
 *									*
 *			Public API					*
 *									*
 ************************************************************************)

(**
 * Objects and Nodesets handling
 *)

{$IFNDEF NO_EXTERNAL_VARS}
var
  xmlXPathNAN: cdouble; cvar; external;
  xmlXPathPINF: cdouble; cvar; external;
  xmlXPathNINF: cdouble; cvar; external;
{$ENDIF}

(* These macros may later turn into functions *)
(**
 * xmlXPathNodeSetGetLength:
 * @ns:  a node-set
 *
 * Implement a functionality similar to the DOM NodeList.length.
 *
 * Returns the number of nodes in the node-set.
 *)
function xmlXPathNodeSetGetLength(ns: xmlNodeSetPtr): cint;

(**
 * xmlXPathNodeSetItem:
 * @ns:  a node-set
 * @index:  index of a node in the set
 *
 * Implements a functionality similar to the DOM NodeList.item().
 *
 * Returns the xmlNodePtr at the given @index in @ns or NULL if
 *         @index is out of range (0 to length-1)
 *)
function xmlXPathNodeSetItem(ns: xmlNodeSetPtr; index: cint): xmlNodePtr;

(**
 * xmlXPathNodeSetIsEmpty:
 * @ns: a node-set
 *
 * Checks whether @ns is empty or not.
 *
 * Returns %TRUE if @ns is an empty node-set.
 *)
function xmlXPathNodeSetIsEmpty(ns: xmlNodeSetPtr): boolean;


procedure xmlXPathFreeObject(obj: xmlXPathObjectPtr); EXTDECL; external xml2lib;
function xmlXPathNodeSetCreate(val: xmlNodePtr): xmlNodeSetPtr; EXTDECL; external xml2lib;
procedure xmlXPathFreeNodeSetList(obj: xmlXPathObjectPtr); EXTDECL; external xml2lib;
procedure xmlXPathFreeNodeSet(obj: xmlNodeSetPtr); EXTDECL; external xml2lib;
function xmlXPathObjectCopy(val: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathCmpNodes(node1, node2: xmlNodePtr): cint; EXTDECL; external xml2lib;

(**
 * Conversion functions to basic types.
 *)
function xmlXPathCastNumberToBoolean(val: cdouble): cint; EXTDECL; external xml2lib;
function xmlXPathCastStringToBoolean(val: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlXPathCastNodeSetToBoolean(ns: xmlNodeSetPtr): cint; EXTDECL; external xml2lib;
function xmlXPathCastToBoolean(ns: xmlXPathObjectPtr): cint; EXTDECL; external xml2lib;

function xmlXPathCastBooleanToNumber(val: cint): cdouble; EXTDECL; external xml2lib;
function xmlXPathCastStringToNumber(val: xmlCharPtr): cdouble; EXTDECL; external xml2lib;
function xmlXPathCastNodeToNumber(val: xmlNodePtr): cdouble; EXTDECL; external xml2lib;
function xmlXPathCastNodeSetToNumber(val: xmlNodeSetPtr): cdouble; EXTDECL; external xml2lib;
function xmlXPathCastToNumber(val: xmlXPathObjectPtr): cdouble; EXTDECL; external xml2lib;

function xmlXPathCastBooleanToString(val: cint): xmlCharPtr; EXTDECL; external xml2lib;
function xmlXPathCastNumberToString(val: cdouble): xmlCharPtr; EXTDECL; external xml2lib;
function xmlXPathCastNodeToString(val: xmlNodePtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlXPathCastNodeSetToString(val: xmlNodeSetPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlXPathCastToString(val: xmlXPathObjectPtr): xmlCharPtr; EXTDECL; external xml2lib;

function xmlXPathConvertBoolean(val: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathConvertNumber(val: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathConvertString(val: xmlXPathObjectPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;

(**
 * Context handling.
 *)
function xmlXPathNewContext(doc: xmlDocPtr): xmlXPathContextPtr; EXTDECL; external xml2lib;
procedure xmlXPathFreeContext(ctxt: xmlXPathContextPtr); EXTDECL; external xml2lib;
function xmlXPathContextSetCache(ctxt: xmlXPathContextPtr; active, value, options: cint): cint; EXTDECL; external xml2lib;

(**
 * Evaluation functions.
 *)
function xmlXPathOrderDocElems(doc: xmlDocPtr): clong; EXTDECL; external xml2lib;
function xmlXPathEval(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathEvalExpression(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathEvalPredicate(ctxt: xmlXPathContextPtr; res: xmlXPathObjectPtr): cint; EXTDECL; external xml2lib;

(**
 * Separate compilation/evaluation entry points.
 *)
function xmlXPathCompile(str: xmlCharPtr): xmlXPathCompExprPtr; EXTDECL; external xml2lib;
function xmlXPathCtxtCompile(ctxt: xmlXPathContextPtr; str: xmlCharPtr): xmlXPathCompExprPtr; EXTDECL; external xml2lib;
function xmlXPathCompiledEval(comp: xmlXPathCompExprPtr; ctxt: xmlXPathContextPtr): xmlXPathObjectPtr; EXTDECL; external xml2lib;
function xmlXPathCompiledEvalToBoolean(comp: xmlXPathCompExprPtr; ctxt: xmlXPathContextPtr): cint; EXTDECL; external xml2lib;
procedure xmlXPathFreeCompExpr(comp: xmlXPathCompExprPtr); EXTDECL; external xml2lib;
{$ENDIF}
{$ENDIF} (* LIBXML_XPATH_ENABLED *)

{$if defined(LIBXML_XPATH_ENABLED) or defined(LIBXML_SCHEMAS_ENABLED)}
{$IFDEF FUNCTION}
procedure xmlXPathInit; EXTDECL; external xml2lib;
function xmlXPathIsNaN(val: cdouble): cint; EXTDECL; external xml2lib;
function xmlXPathIsInf(val: cdouble): cint; EXTDECL; external xml2lib;
{$ENDIF}
{$ENDIF} (* LIBXML_XPATH_ENABLED or LIBXML_SCHEMAS_ENABLED*)
