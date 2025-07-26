(*
 * Summary: internal data structures, constants and functions
 * Description: Internal data structures, constants and functions used
 *              by the XSLT engine.
 *              They are not part of the API or ABI, i.e. they can change
 *              without prior notice, use carefully.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF POINTER}
  xsltRuntimeExtraPtr = ^xsltRuntimeExtra;
  xsltTemplatePtr = ^xsltTemplate;
  xsltTemplatePtrPtr = ^xsltTemplatePtr;
  xsltDecimalFormatPtr = ^xsltDecimalFormat;
  xsltDocumentPtr = ^xsltDocument;
  xsltKeyDefPtr = ^xsltKeyDef;
  xsltKeyTablePtr = ^xsltKeyTable;
  xsltElemPreCompPtr = ^xsltElemPreComp;
  xsltStackElemPtrPtr = ^xsltStackElemPtr;
  xsltStylesheetPtr = ^xsltStylesheet;
  xsltTransformContextPtr = ^xsltTransformContext;
  {$IFDEF XSLT_REFACTORED}
  xsltPointerListPtr = ^xsltPointerList;
  xsltPrincipalStylesheetDataPtr = ^xsltPrincipalStylesheetData;
  xsltStylePreCompPtr = ^xsltStylePreComp;
  xsltNsListContainerPtr = ^xsltNsListContainer;
  {$ELSE}
  xsltNsListContainerPtr = Pointer;
  xsltStylePreCompPtr = Pointer;
  {$ENDIF}
{$ENDIF} {POINTER}

{
/* #define XSLT_DEBUG_PROFILE_CACHE */

/**
 * XSLT_IS_TEXT_NODE:
 *
 * check if the argument is a text node
 */
#define XSLT_IS_TEXT_NODE(n) ((n != NULL) && \
    (((n)->type == XML_TEXT_NODE) || \
     ((n)->type == XML_CDATA_SECTION_NODE)))


/**
 * XSLT_MARK_RES_TREE_FRAG:
 *
 * internal macro to set up tree fragments
 */
#define XSLT_MARK_RES_TREE_FRAG(n) \
    (n)->name = (char *) xmlStrdup(BAD_CAST " fake node libxslt");

/**
 * XSLT_IS_RES_TREE_FRAG:
 *
 * internal macro to test tree fragments
 */
#define XSLT_IS_RES_TREE_FRAG(n) \
    ((n != NULL) && ((n)->type == XML_DOCUMENT_NODE) && \
     ((n)->name != NULL) && ((n)->name[0] == ' '))

/**
 * XSLT_REFACTORED_KEYCOMP:
 *
 * Internal define to enable on-demand xsl:key computation.
 * That's the only mode now but the define is kept for compatibility
 */
#define XSLT_REFACTORED_KEYCOMP

/**
 * XSLT_FAST_IF:
 *
 * Internal define to enable usage of xmlXPathCompiledEvalToBoolean()
 * for XSLT "tests"; e.g. in <xsl:if test="/foo/bar">
 */
#define XSLT_FAST_IF

/**
 * XSLT_REFACTORED:
 *
 * Internal define to enable the refactored parts of Libxslt.
 */
/* #define XSLT_REFACTORED */
/* ==================================================================== */

/**
 * XSLT_REFACTORED_VARS:
 *
 * Internal define to enable the refactored variable part of libxslt
 */
#define XSLT_REFACTORED_VARS

#ifdef XSLT_REFACTORED

extern const xmlChar *xsltXSLTAttrMarker;


/* TODO: REMOVE: #define XSLT_REFACTORED_EXCLRESNS */

/* TODO: REMOVE: #define XSLT_REFACTORED_NSALIAS */

/**
 * XSLT_REFACTORED_XSLT_NSCOMP
 *
 * Internal define to enable the pointer-comparison of
 * namespaces of XSLT elements.
 */
/* #define XSLT_REFACTORED_XSLT_NSCOMP */

#ifdef XSLT_REFACTORED_XSLT_NSCOMP

extern const xmlChar *xsltConstNamespaceNameXSLT;

/**
 * IS_XSLT_ELEM_FAST:
 *
 * quick test to detect XSLT elements
 */
#define IS_XSLT_ELEM_FAST(n) \
    (((n) != NULL) && ((n)->ns != NULL) && \
    ((n)->ns->href == xsltConstNamespaceNameXSLT))

/**
 * IS_XSLT_ATTR_FAST:
 *
 * quick test to detect XSLT attributes
 */
#define IS_XSLT_ATTR_FAST(a) \
    (((a) != NULL) && ((a)->ns != NULL) && \
    ((a)->ns->href == xsltConstNamespaceNameXSLT))

/**
 * XSLT_HAS_INTERNAL_NSMAP:
 *
 * check for namespace mapping
 */
#define XSLT_HAS_INTERNAL_NSMAP(s) \
    (((s) != NULL) && ((s)->principal) && \
     ((s)->principal->principalData) && \
     ((s)->principal->principalData->nsMap))

/**
 * XSLT_GET_INTERNAL_NSMAP:
 *
 * get pointer to namespace map
 */
#define XSLT_GET_INTERNAL_NSMAP(s) ((s)->principal->principalData->nsMap)

#else /* XSLT_REFACTORED_XSLT_NSCOMP */

/**
 * IS_XSLT_ELEM_FAST:
 *
 * quick check whether this is an xslt element
 */
#define IS_XSLT_ELEM_FAST(n) \
    (((n) != NULL) && ((n)->ns != NULL) && \
     (xmlStrEqual((n)->ns->href, XSLT_NAMESPACE)))

/**
 * IS_XSLT_ATTR_FAST:
 *
 * quick check for xslt namespace attribute
 */
#define IS_XSLT_ATTR_FAST(a) \
    (((a) != NULL) && ((a)->ns != NULL) && \
     (xmlStrEqual((a)->ns->href, XSLT_NAMESPACE)))


#endif /* XSLT_REFACTORED_XSLT_NSCOMP */


/**
 * XSLT_REFACTORED_MANDATORY_VERSION:
 *
 * TODO: Currently disabled to surpress regression test failures, since
 *  the old behaviour was that a missing version attribute
 *  produced a only a warning and not an error, which was incerrect.
 *  So the regression tests need to be fixed if this is enabled.
 */
/* #define XSLT_REFACTORED_MANDATORY_VERSION */
}

{$IFDEF TYPE}
{$IFDEF XSLT_REFACTORED}
  (**
   * xsltPointerList:
   *
   * Pointer-list for various purposes.
   *)

  xsltPointerList = record
    items: PPointer;
    number: cint;
    size: cint;
  end;
{$ENDIF} {XSLT_REFACTORED}
{$ENDIF} {TYPE}

(**
 * XSLT_REFACTORED_PARSING:
 *
 * Internal define to enable the refactored parts of Libxslt
 * related to parsing.
 *)
(* #define XSLT_REFACTORED_PARSING *)

{$IFDEF CONST}
  (**
   * XSLT_MAX_SORT:
   *
   * Max number of specified xsl:sort on an element.
   *)
  XSLT_MAX_SORT = 15;

  (**
   * XSLT_PAT_NO_PRIORITY:
   *
   * Specific value for pattern without priority expressed.
   *)
  XSLT_PAT_NO_PRIORITY = -12345789;
{$ENDIF} {CONST}

{$IFDEF TYPE}
  (**
   * xsltRuntimeExtra:
   *
   * Extra information added to the transformation context.
   *)
  xsltRuntimeExtra = record
    info: Pointer;              (* pointer to the extra data *)
    deallocate: xmlFreeFunc;    (* pointer to the deallocation routine *)
    val: record                 (* dual-purpose field *)
      case Boolean of
        True: (
          ptr: Pointer;         (* data not needing deallocation *)
        );
        False: (
          ival: cint;        (* integer value storage *)
        )
    end;
  end;

{
/**
 * XSLT_RUNTIME_EXTRA_LST:
 * @ctxt: the transformation context
 * @nr: the index
 *
 * Macro used to access extra information stored in the context
 */
#define XSLT_RUNTIME_EXTRA_LST(ctxt, nr) (ctxt)->extras[(nr)].info
/**
 * XSLT_RUNTIME_EXTRA_FREE:
 * @ctxt: the transformation context
 * @nr: the index
 *
 * Macro used to free extra information stored in the context
 */
#define XSLT_RUNTIME_EXTRA_FREE(ctxt, nr) (ctxt)->extras[(nr)].deallocate
/**
 * XSLT_RUNTIME_EXTRA:
 * @ctxt: the transformation context
 * @nr: the index
 *
 * Macro used to define extra information stored in the context
 */
#define	XSLT_RUNTIME_EXTRA(ctxt, nr, typ) (ctxt)->extras[(nr)].val.typ
}

  (**
   * xsltTemplate:
   *
   * The in-memory structure corresponding to an XSLT Template.
   *)
  xsltTemplate = record
    next: xsltTemplatePtr;    (* chained list sorted by priority *)
    style: xsltStylesheetPtr; (* the containing stylesheet *)
    match: xmlCharPtr;        (* the matching string *)
    priority: Single;         (* as given from the stylesheet, not computed *)
    name: xmlCharPtr;         (* the local part of the name QName *)
    nameURI: xmlCharPtr;      (* the URI part of the name QName *)
    mode: xmlCharPtr;         (* the local part of the mode QName *)
    modeURI: xmlCharPtr;      (* the URI part of the mode QName *)
    content: xmlNodePtr;      (* the template replacement value *)
    elem: xmlNodePtr;         (* the source element *)

    (*
    * TODO: @inheritedNsNr and @inheritedNs won't be used in the
    *  refactored code.
    *)
    inheritedNsNr: cint;   (* number of inherited namespaces *)
    inheritedNs: xmlNsPtrPtr; (* inherited non-excluded namespaces *)

    (* Profiling information *)
    nbCalls: cint;         (* the number of time the template was called *)
    time: LongWord;           (* the time spent in this template *)
    params: Pointer;          (* xsl:param instructions *)

    templNr: cint;         (* Nb of templates in the stack *)
    templMax: cint;        (* Size of the templtes stack *)
    templCalledTab: xsltTemplatePtrPtr; (* templates called *)
    templCountTab: PLongint;  (* .. and how often *)

    (* Conflict resolution *)
    position: cint;
  end;

  (**
   * xsltDecimalFormat:
   *
   * Data structure of decimal-format.
   *)
  xsltDecimalFormat = record
    next: xsltDecimalFormatPtr; (* chained list *)
    name: xmlCharPtr;
    (* Used for interpretation of pattern *)
    digit: xmlCharPtr;
    patternSeparator: xmlCharPtr;
    (* May appear in result *)
    minusSign: xmlCharPtr;
    infinity: xmlCharPtr;
    noNumber: xmlCharPtr; (* Not-a-number *)
    (* Used for interpretation of pattern and may appear in result *)
    decimalPoint: xmlCharPtr;
    grouping: xmlCharPtr;
    percent: xmlCharPtr;
    permille: xmlCharPtr;
    zeroDigit: xmlCharPtr;
    nsUri: xmlCharPtr;
  end;

  (**
   * xsltDocument:
   *
   * Data structure associated to a parsed document.
   *)
  xsltDocument = record
    next: xsltDocumentPtr;     (* documents are kept in a chained list *)
    main: cint;              (* is this the main document *)
    doc: xmlDocPtr;             (* the parsed document *)
    keys: Pointer;              (* key tables storage *)
    includes: xsltDocumentPtr;  (* subsidiary includes *)
    preproc: cint;           (* pre-processing already done *)
    nbKeysComputed: cint;
  end;

  (**
   * xsltKeyDef:
   *
   * Representation of an xsl:key.
   *)
  xsltKeyDef = record
    next: xsltKeyDefPtr;
    inst: xmlNodePtr;
    name: xmlCharPtr;
    nameURI: xmlCharPtr;
    match: xmlCharPtr;
    use: xmlCharPtr;
    comp: xmlXPathCompExprPtr;
    usecomp: xmlXPathCompExprPtr;
    nsList: xmlNsPtrPtr;          (* the namespaces in scope *)
    nsNr: cint;                (* the number of namespaces in scope *)
  end;

  (**
   * xsltKeyTable:
   *
   * Holds the computed keys for key definitions of the same QName.
   * Is owned by an xsltDocument.
   *)
  xsltKeyTable = record
    next: xsltKeyTablePtr;
    name: xmlCharPtr;
    nameURI: xmlCharPtr;
    keys: xmlHashTablePtr;
  end;

  (**
   * xsltTransformFunction:
   * @ctxt: the XSLT transformation context
   * @node: the input node
   * @inst: the stylesheet node
   * @comp: the compiled information from the stylesheet
   *
   * Signature of the function associated to elements part of the
   * stylesheet language like xsl:if or xsl:apply-templates.
   *)
  xsltTransformFunction = procedure(ctxt: xsltTransformContextPtr;
    node: xmlNodePtr; inst: xmlNodePtr; comp: xsltElemPreCompPtr); EXTDECL;

  (**
   * xsltSortFunc:
   * @ctxt:    a transformation context
   * @sorts:   the node-set to sort
   * @nbsorts: the number of sorts
   *
   * Signature of the function to use during sorting
   *)
  xsltSortFunc = procedure(ctxt: xsltTransformContextPtr; sorts: xmlNodePtrPtr;
    nbsorts: cint); EXTDECL;

  xsltStyleType = (
    XSLT_FUNC_COPY = 1,
    XSLT_FUNC_SORT,
    XSLT_FUNC_TEXT,
    XSLT_FUNC_ELEMENT,
    XSLT_FUNC_ATTRIBUTE,
    XSLT_FUNC_COMMENT,
    XSLT_FUNC_PI,
    XSLT_FUNC_COPYOF,
    XSLT_FUNC_VALUEOF,
    XSLT_FUNC_NUMBER,
    XSLT_FUNC_APPLYIMPORTS,
    XSLT_FUNC_CALLTEMPLATE,
    XSLT_FUNC_APPLYTEMPLATES,
    XSLT_FUNC_CHOOSE,
    XSLT_FUNC_IF,
    XSLT_FUNC_FOREACH,
    XSLT_FUNC_DOCUMENT,
    XSLT_FUNC_WITHPARAM,
    XSLT_FUNC_PARAM,
    XSLT_FUNC_VARIABLE,
    XSLT_FUNC_WHEN,
    XSLT_FUNC_EXTENSION
    {$IFDEF XSLT_REFACTORED}
    ,
    XSLT_FUNC_OTHERWISE,
    XSLT_FUNC_FALLBACK,
    XSLT_FUNC_MESSAGE,
    XSLT_FUNC_INCLUDE,
    XSLT_FUNC_ATTRSET,
    XSLT_FUNC_LITERAL_RESULT_ELEMENT,
    XSLT_FUNC_UNKOWN_FORWARDS_COMPA
    {$ENDIF}
   );

  (**
   * xsltElemPreCompDeallocator:
   * @comp:  the #xsltElemPreComp to free up
   *
   * Deallocates an #xsltElemPreComp structure.
   *)
  xsltElemPreCompDeallocator = procedure(comp: xsltElemPreCompPtr); EXTDECL;

  (**
   * xsltElemPreComp:
   *
   * The basic structure for compiled items of the AST of the XSLT processor.
   * This structure is also intended to be extended by extension implementors.
   * TODO: This is somehow not nice, since it has a "free" field, which
   *   derived stylesheet-structs do not have.
   *)
  xsltElemPreComp = record
    next: xsltElemPreCompPtr;     (* next item in the global chained
                                     list held by xsltStylesheet. *)
    &type: xsltStyleType;         (* type of the element *)
    func: xsltTransformFunction;  (* handling function *)
    inst: xmlNodePtr;             (* the node in the stylesheet's tree
  					 corresponding to this item *)

    (* end of common part *)
    free: xsltElemPreCompDeallocator; (* the deallocator *)
  end;

{$ENDIF} {TYPE}

{$IFDEF XSLT_REFACTORED}

{$IFDEF FUNCTION}
(*
* Some pointer-list utility functions.
*)
function xsltPointerListCreate(initialSize: cint): xsltPointerListPtr; EXTDECL; external xsltlib;
procedure xsltPointerListFree(list: xsltPointerListPtr); EXTDECL; external xsltlib;
procedure xsltPointerListClear(list: xsltPointerListPtr); EXTDECL; external xsltlib;
function xsltPointerListAddSize(list: xsltPointerListPtr; item: Pointer; initialSize: cint): cint; EXTDECL; external xsltlib;
{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
(*
* Some pointer-list utility functions.
*)
  xsltPointerListCreate: function(initialSize: cint): xsltPointerListPtr; EXTDECL;
  xsltPointerListFree: procedure(list: xsltPointerListPtr); EXTDECL;
  xsltPointerListClear: procedure(list: xsltPointerListPtr); EXTDECL;
  xsltPointerListAddSize: function(list: xsltPointerListPtr; item: Pointer; initialSize: cint): cint; EXTDECL;
{$ENDIF} {FUNCTION}

{$IFDEF TYPE}
(************************************************************************
 *									*
 * Refactored structures                                                *
 *									*
 ************************************************************************)

  xsltNsListContainer = record
    list: xmlNsPtrPtr;
    totalNumber: cint;
    xpathNumber: cint;
  end;

  (**
   * _xsltStylePreComp:
   *
   * The abstract basic structure for items of the XSLT processor.
   * This includes:
   * 1) compiled forms of XSLT instructions (e.g. xsl:if, xsl:attribute, etc.)
   * 2) compiled forms of literal result elements
   * 3) various properties for XSLT instructions (e.g. xsl:when,
   *    xsl:with-param)
   *
   * REVISIT TODO: Keep this structure equal to the fields
   *   defined by XSLT_ITEM_COMMON_FIELDS
   *)
  xsltStylePreComp = record
    next: xsltElemPreCompPtr;   (* next item in the global chained
  				 list held by xsltStylesheet *)
    &type: xsltStyleType;       (* type of the item *)
    func: xsltTransformFunction; (* handling function *)
    inst: xmlNodePtr;           (* the node in the stylesheet's tree
  				 corresponding to this item. *)
    (* Currently no navigational fields. *)
    inScopeNs: xsltNsListContainerPtr;
  end;

  (**
   * xsltStyleBasicEmptyItem:
   *
   * Abstract structure only used as a short-cut for
   * XSLT items with no extra fields.
   * NOTE that it is intended that this structure looks the same as
   *  _xsltStylePreComp.
   *)
  xsltStyleBasicEmptyItemPtr = ^xsltStyleBasicEmptyItem;
  xsltStyleBasicEmptyItem = record
    {$I xslt_item_common_fields.inc}
  end;

  (**
   * xsltStyleBasicExpressionItem:
   *
   * Abstract structure only used as a short-cut for
   * XSLT items with just an expression.
   *)

  xsltStyleBasicExpressionItemPtr = ^xsltStyleBasicExpressionItem;
  xsltStyleBasicExpressionItem = record
    {$I xslt_item_common_fields.inc}
    select: xmlCharPtr;        (* TODO: Change this to "expression". *)
    comp: xmlXPathCompExprPtr; (* TODO: Change this to compExpr. *)
  end;

(************************************************************************
 *									*
 * XSLT-instructions/declarations                                       *
 *									*
 ************************************************************************)

  (**
   * xsltStyleItemElement:
   *
   * <!-- Category: instruction -->
   * <xsl:element
   *  name = { qname }
   *  namespace = { uri-reference }
   *  use-attribute-sets = qnames>
   *  <!-- Content: template -->
   * </xsl:element>
   *)
  xsltStyleItemElementPtr = ^xsltStyleItemElement;
  xsltStyleItemElement = record
    {$I xslt_item_common_fields.inc}
    use: xmlCharPtr;
    has_use: cint;
    name: xmlCharPtr;
    has_name: cint;
    ns: xmlCharPtr;
    nsPrefix: xmlCharPtr;
    has_ns: cint;
  end;

  (**
   * xsltStyleItemAttribute:
   *
   * <!-- Category: instruction -->
   * <xsl:attribute
   *  name = { qname }
   *  namespace = { uri-reference }>
   *  <!-- Content: template -->
   * </xsl:attribute>
   *)
  xsltStyleItemAttributePtr = ^xsltStyleItemAttribute;
  xsltStyleItemAttribute = record
    {$I xslt_item_common_fields.inc}
    name: xmlCharPtr;
    has_name: cint;
    ns: xmlCharPtr;
    nsPrefix: xmlCharPtr;
    has_ns: cint;
  end;

  (**
   * xsltStyleItemText:
   *
   * <!-- Category: instruction -->
   * <xsl:text
   *  disable-output-escaping = "yes" | "no">
   *  <!-- Content: #PCDATA -->
   * </xsl:text>
   *)
  xsltStyleItemTextPtr = ^xsltStyleItemText;
  xsltStyleItemText = record
    {$I xslt_item_common_fields.inc}
    noescape: cint;                (* text *)
  end;

  (**
   * xsltStyleItemComment:
   *
   * <!-- Category: instruction -->
   *  <xsl:comment>
   *  <!-- Content: template -->
   * </xsl:comment>
   *)
  xsltStyleItemComment = xsltStyleBasicEmptyItem;
  xsltStyleItemCommentPtr = ^xsltStyleItemComment;

  (**
   * xsltStyleItemPI:
   *
   * <!-- Category: instruction -->
   *  <xsl:processing-instruction
   *  name = { ncname }>
   *  <!-- Content: template -->
   * </xsl:processing-instruction>
   *)
  xsltStyleItemPIPtr = ^xsltStyleItemPI;
  xsltStyleItemPI = record
    {$I xslt_item_common_fields.inc}
    name: xmlCharPtr;
    has_name: cint;
  end;

  (**
   * xsltStyleItemApplyImports:
   *
   * <!-- Category: instruction -->
   * <xsl:apply-imports />
   *)
  xsltStyleItemApplyImports = xsltStyleBasicEmptyItem;
  xsltStyleItemApplyImportsPtr = ^xsltStyleItemApplyImports;

  (**
   * xsltStyleItemApplyTemplates:
   *
   * <!-- Category: instruction -->
   *  <xsl:apply-templates
   *  select = node-set-expression
   *  mode = qname>
   *  <!-- Content: (xsl:sort | xsl:with-param)* -->
   * </xsl:apply-templates>
   *)
  xsltStyleItemApplyTemplatesPtr = ^xsltStyleItemApplyTemplates;
  xsltStyleItemApplyTemplates = record
    {$I xslt_item_common_fields.inc}
    mode: xmlCharPtr;           (* apply-templates *)
    modeURI: xmlCharPtr;        (* apply-templates *)
    select: xmlCharPtr;         (* sort, copy-of, value-of, apply-templates *)
    comp: xmlXPathCompExprPtr;  (* a precompiled XPath expression *)
    (* TODO: with-params *)
  end;

  (**
   * xsltStyleItemCallTemplate:
   *
   * <!-- Category: instruction -->
   *  <xsl:call-template
   *  name = qname>
   *  <!-- Content: xsl:with-param* -->
   * </xsl:call-template>
   *)
  xsltStyleItemCallTemplatePtr = ^xsltStyleItemCallTemplate;
  xsltStyleItemCallTemplate = record
    {$I xslt_item_common_fields.inc}
    templ: xsltTemplatePtr;              (* call-template *)
    name: xmlCharPtr;                    (* element, attribute, pi *)
    has_name: cint;                   (* element, attribute, pi *)
    ns: xmlCharPtr;                      (* element *)
    has_ns: cint;                     (* element *)
    (* TODO: with-params *)
  end;

  (**
   * xsltStyleItemCopy:
   *
   * <!-- Category: instruction -->
   * <xsl:copy
   *  use-attribute-sets = qnames>
   *  <!-- Content: template -->
   * </xsl:copy>
   *)
  xsltStyleItemCopyPtr = ^xsltStyleItemCopy;
  xsltStyleItemCopy = record
    {$I xslt_item_common_fields.inc}
    use: xmlCharPtr;            (* copy, element *)
    has_use: cint;           (* copy, element *)
  end;

  (**
   * xsltStyleItemIf:
   *
   * <!-- Category: instruction -->
   *  <xsl:if
   *  test = boolean-expression>
   *  <!-- Content: template -->
   * </xsl:if>
   *)
  xsltStyleItemIfPtr = ^xsltStyleItemIf;
  xsltStyleItemIf = record
    {$I xslt_item_common_fields.inc}
    test: xmlCharPtr;            (* if *)
    comp: xmlXPathCompExprPtr;   (* a precompiled XPath expression *)
  end;

  (**
   * xsltStyleItemCopyOf:
   *
   * <!-- Category: instruction -->
   * <xsl:copy-of
   *  select = expression />
   *)
  xsltStyleItemCopyOf = xsltStyleBasicExpressionItem;
  xsltStyleItemCopyOfPtr = ^xsltStyleItemCopyOf;

  (**
   * xsltStyleItemValueOf:
   *
   * <!-- Category: instruction -->
   * <xsl:value-of
   *  select = string-expression
   *  disable-output-escaping = "yes" | "no" />
   *)
  xsltStyleItemValueOfPtr = ^xsltStyleItemValueOf;
  xsltStyleItemValueOf = record
    {$I xslt_item_common_fields.inc}
    select: xmlCharPtr;
    comp: xmlXPathCompExprPtr;   (* a precompiled XPath expression *)
    noescape: cint;
  end;

  (**
   * xsltStyleItemNumber:
   *
   * <!-- Category: instruction -->
   *  <xsl:number
   *  level = "single" | "multiple" | "any"
   *  count = pattern
   *  from = pattern
   *  value = number-expression
   *  format = { string }
   *  lang = { nmtoken }
   *  letter-value = { "alphabetic" | "traditional" }
   *  grouping-separator = { char }
   *  grouping-size = { number } />
   *)
  xsltStyleItemNumberPtr = ^xsltStyleItemNumber;
  xsltStyleItemNumber = record
    {$I xslt_item_common_fields.inc}
    numdata: xsltNumberData;    (* number *)
  end;

  (**
   * xsltStyleItemChoose:
   *
   * <!-- Category: instruction -->
   *  <xsl:choose>
   *  <!-- Content: (xsl:when+, xsl:otherwise?) -->
   * </xsl:choose>
   *)
  xsltStyleItemChoose = xsltStyleBasicEmptyItem;
  xsltStyleItemChoosePtr = ^xsltStyleItemChoose;

  (**
   * xsltStyleItemFallback:
   *
   * <!-- Category: instruction -->
   *  <xsl:fallback>
   *  <!-- Content: template -->
   * </xsl:fallback>
   *)
  xsltStyleItemFallback = xsltStyleBasicEmptyItem;
  xsltStyleItemFallbackPtr = ^xsltStyleItemFallback;

  (**
   * xsltStyleItemForEach:
   *
   * <!-- Category: instruction -->
   * <xsl:for-each
   *   select = node-set-expression>
   *   <!-- Content: (xsl:sort*, template) -->
   * </xsl:for-each>
   *)
  xsltStyleItemForEach = xsltStyleBasicExpressionItem;
  xsltStyleItemForEachPtr = ^xsltStyleItemForEach;

  (**
   * xsltStyleItemMessage:
   *
   * <!-- Category: instruction -->
   * <xsl:message
   *   terminate = "yes" | "no">
   *   <!-- Content: template -->
   * </xsl:message>
   *)
  xsltStyleItemMessagePtr = ^xsltStyleItemMessage;
  xsltStyleItemMessage = record
    {$I xslt_item_common_fields.inc}
    terminate: cint;
  end;

  (**
   * xsltStyleItemDocument:
   *
   * NOTE: This is not an instruction of XSLT 1.0.
   *)
  xsltStyleItemDocumentPtr = ^xsltStyleItemDocument;
  xsltStyleItemDocument = record
    {$I xslt_item_common_fields.inc}
    ver11: cint;              (* assigned: in xsltDocumentComp;
                                    read: nowhere;
                                    TODO: Check if we need. *)
    filename: xmlCharPtr;        (* document URL *)
    has_filename: cint;
  end;

(************************************************************************
 *									*
 * Non-instructions (actually properties of instructions/declarations)  *
 *									*
 ************************************************************************)

  (**
   * xsltStyleBasicItemVariable:
   *
   * Basic struct for xsl:variable, xsl:param and xsl:with-param.
   * It's currently important to have equal fields, since
   * xsltParseStylesheetCallerParam() is used with xsl:with-param from
   * the xslt side and with xsl:param from the exslt side (in
   * exsltFuncFunctionFunction()).
   *
   * FUTURE NOTE: In XSLT 2.0 xsl:param, xsl:variable and xsl:with-param
   *   have additional different fields.
   *)
  xsltStyleBasicItemVariablePtr = ^xsltStyleBasicItemVariable;
  xsltStyleBasicItemVariable = record
    {$I xslt_item_common_fields.inc}
    select: xmlCharPtr;
    comp: xmlXPathCompExprPtr;

    name: xmlCharPtr;
    has_name: cint;
    ns: xmlCharPtr;
    has_ns: cint;
  end;

  (**
   * xsltStyleItemVariable:
   *
   * <!-- Category: top-level-element -->
   * <xsl:param
   *   name = qname
   *   select = expression>
   *   <!-- Content: template -->
   * </xsl:param>
   *)
  xsltStyleItemVariable = xsltStyleBasicItemVariable;
  xsltStyleItemVariablePtr = ^xsltStyleItemVariable;

  (**
   * xsltStyleItemParam:
   *
   * <!-- Category: top-level-element -->
   * <xsl:param
   *   name = qname
   *   select = expression>
   *   <!-- Content: template -->
   * </xsl:param>
   *)
  xsltStyleItemParamPtr = ^xsltStyleItemParam;
  xsltStyleItemParam = record
    {$I xslt_item_common_fields.inc}
    select: xmlCharPtr;
    comp: xmlXPathCompExprPtr;

    name: xmlCharPtr;
    has_name: cint;
    ns: xmlCharPtr;
    has_ns: cint;
  end;

  (**
   * xsltStyleItemWithParam:
   *
   * <xsl:with-param
   *  name = qname
   *  select = expression>
   *  <!-- Content: template -->
   * </xsl:with-param>
   *)
  xsltStyleItemWithParam = xsltStyleBasicItemVariable;
  xsltStyleItemWithParamPtr = ^xsltStyleItemWithParam;

  (**
   * xsltStyleItemSort:
   *
   * Reflects the XSLT xsl:sort item.
   * Allowed parents: xsl:apply-templates, xsl:for-each
   * <xsl:sort
   *   select = string-expression
   *   lang = { nmtoken }
   *   data-type = { "text" | "number" | qname-but-not-ncname }
   *   order = { "ascending" | "descending" }
   *   case-order = { "upper-first" | "lower-first" } />
   *)
  xsltStyleItemSortPtr = ^xsltStyleItemSort;
  xsltStyleItemSort = record
    {$I xslt_item_common_fields.inc}
    stype: xmlCharPtr;          (* sort *)
    has_stype: cint;         (* sort *)
    number: cint;            (* sort *)
    order: xmlCharPtr;          (* sort *)
    has_order: cint;         (* sort *)
    descending: cint;        (* sort *)
    lang: xmlCharPtr;           (* sort *)
    has_lang: cint;          (* sort *)
    locale: xsltLocale;         (* sort *)
    case_order: xmlCharPtr;     (* sort *)
    lower_first: cint;       (* sort *)

    use: xmlCharPtr;
    has_use: cint;

    select: xmlCharPtr;         (* sort, copy-of, value-of, apply-templates *)

    comp: xmlXPathCompExprPtr;  (* a precompiled XPath expression *)
  end;


  (**
   * xsltStyleItemWhen:
   *
   * <xsl:when
   *   test = boolean-expression>
   *   <!-- Content: template -->
   * </xsl:when>
   * Allowed parent: xsl:choose
   *)
  xsltStyleItemWhenPtr = ^xsltStyleItemWhen;
  xsltStyleItemWhen = record
    {$I xslt_item_common_fields.inc}
    test: xmlCharPtr;
    comp: xmlXPathCompExprPtr;
  end;

  (**
   * xsltStyleItemOtherwise:
   *
   * Allowed parent: xsl:choose
   * <xsl:otherwise>
   *   <!-- Content: template -->
   * </xsl:otherwise>
   *)
  xsltStyleItemOtherwisePtr = ^xsltStyleItemOtherwise;
  xsltStyleItemOtherwise = record
    {$I xslt_item_common_fields.inc}
  end;

  xsltStyleItemIncludePtr = ^xsltStyleItemInclude;
  xsltStyleItemInclude = record
    {$I xslt_item_common_fields.inc}
    include: xsltDocumentPtr;
  end;

(************************************************************************
 *                                                                      *
 *  XSLT elements in forwards-compatible mode                           *
 *                                                                      *
 ************************************************************************)

  xsltStyleItemUknownPtr = ^xsltStyleItemUknown;
  xsltStyleItemUknown = record
    {$I xslt_item_common_fields.inc}
  end;

(************************************************************************
 *									*
 *  Extension elements                                                  *
 *									*
 ************************************************************************)

  (*
   * xsltStyleItemExtElement:
   *
   * Reflects extension elements.
   *
   * NOTE: Due to the fact that the structure xsltElemPreComp is most
   * probably already heavily in use out there by users, so we cannot
   * easily change it, we'll create an intermediate structure which will
   * hold an xsltElemPreCompPtr.
   * BIG NOTE: The only problem I see here is that the user processes the
   *  content of the stylesheet tree, possibly he'll lookup the node->psvi
   *  fields in order to find subsequent extension functions.
   *  In this case, the user's code will break, since the node->psvi
   *  field will hold now the xsltStyleItemExtElementPtr and not
   *  the xsltElemPreCompPtr.
   *  However the place where the structure is anchored in the node-tree,
   *  namely node->psvi, has beed already once been moved from node->_private
   *  to node->psvi, so we have a precedent here, which, I think, should allow
   *  us to change such semantics without headaches.
   *)
  xsltStyleItemExtElementPtr = ^xsltStyleItemExtElement;
  xsltStyleItemExtElement = record
    {$I xslt_item_common_fields.inc}
    item: xsltElemPreCompPtr;
  end;

(************************************************************************
 *									*
 *  Literal result elements                                             *
 *									*
 ************************************************************************)

  xsltEffectiveNsPtr = ^xsltEffectiveNs;
  xsltEffectiveNs = record
    nextInStore: xsltEffectiveNsPtr; (* storage next *)
    next: xsltEffectiveNsPtr;        (* next item in the list *)
    prefix: xmlCharPtr;
    nsName: xmlCharPtr;
    (*
    * Indicates if eclared on the literal result element; dunno if really
    * needed.
    *)
    holdByElem: cint;
  end;

  (*
   * Info for literal result elements.
   * This will be set on the elem->psvi field and will be
   * shared by literal result elements, which have the same
   * excluded result namespaces; i.e., this *won't* be created uniquely
   * for every literal result element.
   *)
  xsltStyleItemLRElementInfoPtr = ^xsltStyleItemLRElementInfo;
  xsltStyleItemLRElementInfo = record
    {$I xslt_item_common_fields.inc}
    (*
    * @effectiveNs is the set of effective ns-nodes
    *  on the literal result element, which will be added to the result
    *  element if not already existing in the result tree.
    *  This means that excluded namespaces (via exclude-result-prefixes,
    *  extension-element-prefixes and the XSLT namespace) not added
    *  to the set.
    *  Namespace-aliasing was applied on the @effectiveNs.
    *)
    effectiveNs: xsltEffectiveNsPtr;
  end;

  xsltNsAliasPtr = ^xsltNsAlias;
  xsltNsAlias = record
    next: xsltNsAliasPtr; (* next in the list *)
    literalNs: xmlNsPtr;
    targetNs: xmlNsPtr;
    docOfTargetNs: xmlDocPtr;
  end;

  {$ifdef XSLT_REFACTORED_XSLT_NSCOMP}

  xsltNsMapPtr = ^xsltNsMap;
  xsltNsMap = record
    next: xsltNsMapPtr; (* next in the list *)
    doc: xmlDocPtr;
    elem: xmlNodePtr; (* the element holding the ns-decl *)
    ns: xmlNsPtr; (* the xmlNs structure holding the XML namespace name *)
    origNsName: xmlCharPtr; (* the original XML namespace name *)
    newNsName: xmlCharPtr; (* the mapped XML namespace name *)
  end;

  {$endif}

(************************************************************************
 *									*
 *  Compile-time structures for *internal* use only                     *
 *									*
 ************************************************************************)

  xsltNsListPtr = ^xsltNsList;
  xsltNsList = record
    next: xsltNsListPtr; (* next in the list *)
    ns: xmlNsPtr;
  end;

  (*
  * xsltVarInfo:
  *
  * Used at compilation time for parameters and variables.
  *)
  xsltVarInfoPtr = ^xsltVarInfo;
  xsltVarInfo = record
    next: xsltVarInfoPtr; (* next in the list *)
    prev: xsltVarInfoPtr;
    depth: cint; (* the depth in the tree *)
    name: xmlCharPtr;
    nsName: xmlCharPtr;
  end;

  (**
   * xsltCompilerNodeInfo:
   *
   * Per-node information during compile-time.
   *)
  xsltCompilerNodeInfoPtr = ^xsltCompilerNodeInfo;
  xsltCompilerNodeInfo = record
    next: xsltCompilerNodeInfoPtr;
    prev: xsltCompilerNodeInfoPtr;
    node: xmlNodePtr;
    depth: cint;
    templ: xsltTemplatePtr;   (* The owning template *)
    category: cint;        (* XSLT element, LR-element or
                                extension element *)
    &type: xsltStyleType;
    item: xsltElemPreCompPtr; (* The compiled information *)
    (* The current in-scope namespaces *)
    inScopeNs: xsltNsListContainerPtr;
    (* The current excluded result namespaces *)
    exclResultNs: xsltPointerListPtr;
    (* The current extension instruction namespaces *)
    extElemNs: xsltPointerListPtr;

    (* The current info for literal result elements. *)
    litResElemInfo: xsltStyleItemLRElementInfoPtr ;
    (*
    * Set to 1 if in-scope namespaces changed,
    *  or excluded result namespaces changed,
    *  or extension element namespaces changed.
    * This will trigger creation of new infos
    *  for literal result elements.
    *)
    nsChanged: cint;
    preserveWhitespace: cint;
    stripWhitespace: cint;
    isRoot: cint; (* whether this is the stylesheet's root node *)
    forwardsCompat: cint; (* whether forwards-compatible mode is enabled *)
    (* whether the content of an extension element was processed *)
    extContentHandled: cint;
    (* the type of the current child *)
    curChildType: xsltStyleType;
  end;

  (**
   * XSLT_CCTXT:
   *
   * get pointer to compiler context
   */
  #define XSLT_CCTXT(style) ((xsltCompilerCtxtPtr) style->compCtxt)
  *)

  xsltErrorSeverityType = (
    XSLT_ERROR_SEVERITY_ERROR = 0,
    XSLT_ERROR_SEVERITY_WARNING
  );

  xsltCompilerCtxtPtr = ^xsltCompilerCtxt;
  xsltCompilerCtxt = record
    errorCtxt: Pointer;            (* user specific error context *)
    (*
     * used for error/warning reports; e.g. XSLT_ERROR_SEVERITY_WARNING *)
    errSeverity: xsltErrorSeverityType;
    warnings: cint;             (* TODO: number of warnings found at
                                      compilation *)
    errors: cint;               (* TODO: number of errors found at
                                      compilation *)
    dict: xmlDictPtr;
    style: xsltStylesheetPtr;
    simplified: cint; (* whether this is a simplified stylesheet *)
    (* TODO: structured/unstructured error contexts. *)
    depth: cint; (* Current depth of processing *)

    inode: xsltCompilerNodeInfoPtr;
    inodeList: xsltCompilerNodeInfoPtr;
    inodeLast: xsltCompilerNodeInfoPtr;
    tmpList: xsltPointerListPtr; (* Used for various purposes *)
    (*
    * The XSLT version as specified by the stylesheet's root element.
    *)
    isInclude: cint;
    hasForwardsCompat: cint; (* whether forwards-compatible mode was used
                                   in a parsing episode *)
    maxNodeInfos: cint; (* TEMP TODO: just for the interest *)
    maxLREs: cint;      (* TEMP TODO: just for the interest *)
    (*
    * In order to keep the old behaviour, applying strict rules of
    * the spec can be turned off. This has effect only on special
    * mechanisms like whitespace-stripping in the stylesheet.
    *)
    strict: cint;
    psData: xsltPrincipalStylesheetDataPtr;
    unknownItem: xsltStyleItemUknownPtr;
    hasNsAliases: cint; (* Indicator if there was an xsl:namespace-alias. *)
    nsAliases: xsltNsAliasPtr;
    ivars: xsltVarInfoPtr; (* Storage of local in-scope variables/params. *)
    ivar: xsltVarInfoPtr;  (* topmost local variable/param. *)
  end;

{$ENDIF} {TYPE}

{$ELSE} (* XSLT_REFACTORED *)

{$IFDEF TYEP}
  (*
  * The old structures before refactoring.
  *)

  (**
   * _xsltStylePreComp:
   *
   * The in-memory structure corresponding to XSLT stylesheet constructs
   * precomputed data.
   *)
  xsltStylePreComp = record
    next: xsltElemPreCompPtr;   (* chained list *)
    &type: xsltStyleType;       (* type of the element *)
    func: xsltTransformFunction;(* handling function *)
    inst: xmlNodePtr;		(* the instruction *)

    (*
     * Pre computed values.
     *)

    stype: xmlCharPtr;          (* sort *)
    has_stype: cint;         (* sort *)
    number: cint;            (* sort *)
    order: xmlCharPtr;          (* sort *)
    has_order: cint;         (* sort *)
    descending: cint;        (* sort *)
    lang: xmlCharPtr;           (* sort *)
    has_lang: cint;          (* sort *)
    locale: xsltLocale;         (* sort *)
    case_order: xmlCharPtr;     (* sort *)
    lower_first: cint;       (* sort *)

    use: xmlCharPtr;            (* copy, element *)
    has_use: cint;           (* copy, element *)

    noescape: cint;          (* text *)

    name: xmlCharPtr;           (* element, attribute, pi *)
    has_name: cint;          (* element, attribute, pi *)
    ns: xmlCharPtr;             (* element *)
    has_ns: cint;            (* element *)

    mode: xmlCharPtr;           (* apply-templates *)
    modeURI: xmlCharPtr;        (* apply-templates *)

    test: xmlCharPtr;           (* if *)

    templ: xsltTemplatePtr;     (* call-template *)

    select: xmlCharPtr;         (* sort, copy-of, value-of, apply-templates *)

    ver11: cint;                (* document *)
    filename: xmlCharPtr;       (* document URL *)
    has_filename: cint;         (* document *)

    numdata: xsltNumberData;    (* number *)

    comp: xmlXPathCompExprPtr;  (* a precompiled XPath expression *)
    nsList: xmlNsPtrPtr;        (* the namespaces in scope *)
    nsNr: cint;                 (* the number of namespaces in scope *)
  end;
{$ENDIF} {TYPE}

{$ENDIF} (* XSLT_REFACTORED *)

{$IFDEF TYPE}
  (*
   * The in-memory structure corresponding to an XSLT Variable
   * or Param.
   *)
  xsltStackElemPtr = ^xsltStackElem;
  xsltStackElem = record
    next: xsltStackElemPtr;     (* chained list *)
    comp: xsltStylePreCompPtr;  (* the compiled form *)
    computed: cint;             (* was the evaluation done *)
    name: xmlCharPtr;           (* the local part of the name QName *)
    nameURI: xmlCharPtr;        (* the URI part of the name QName *)
    select: xmlCharPtr;         (* the eval string *)
    tree: xmlNodePtr;           (* the sequence constructor if no eval
                                   string or the location *)
    value: xmlXPathObjectPtr;   (* The value if computed *)
    fragment: xmlDocPtr;        (* The Result Tree Fragments (needed for XSLT 1.0)
                                   which are bound to the variable's lifetime. *)
    level: cint;                (* the depth in the tree;
                                   -1 if persistent (e.g. a given xsl:with-param) *)
    context: xsltTransformContextPtr; (* The transformation context; needed to cache
                                         the variables *)
    flags: cint;
  end;

{$IFDEF XSLT_REFACTORED}

  xsltPrincipalStylesheetData = record
    (*
    * Namespace dictionary for ns-prefixes and ns-names:
    * TODO: Shared between stylesheets, and XPath mechanisms.
    *   Not used yet.
    *)
    namespaceDict: xmlDictPtr;
    (*
    * Global list of in-scope namespaces.
    *)
    inScopeNamespaces: xsltPointerListPtr;
    (*
    * Global list of information for [xsl:]excluded-result-prefixes.
    *)
    exclResultNamespaces: xsltPointerListPtr;
    (*
    * Global list of information for [xsl:]extension-element-prefixes.
    *)
    extElemNamespaces: xsltPointerListPtr;
    effectiveNs: xsltEffectiveNsPtr;
{$IFDEF XSLT_REFACTORED_XSLT_NSCOMP}
    (*
    * Namespace name map to get rid of string comparison of namespace names.
    *)
    nsMap: xsltNsMapPtr;
{$ENDIF}
  end;

{$ENDIF} {XSLT_REFACTORED}

  (*
   * Note that we added a @compCtxt field to anchor an stylesheet compilation
   * context, since, due to historical reasons, various compile-time function
   * take only the stylesheet as argument and not a compilation context.
   *)
  xsltStylesheet = record
    (*
     * The stylesheet import relation is kept as a tree.
     *)
    parent: xsltStylesheetPtr;
    next: xsltStylesheetPtr;
    imports: xsltStylesheetPtr;

    docList: xsltDocumentPtr;         (* the include document list *)

    (*
     * General data on the style sheet document.
     *)
    doc: xmlDocPtr;                   (* the parsed XML stylesheet *)
    stripSpaces: xmlHashTablePtr;     (* the hash table of the strip-space and
                                         preserve space elements *)
    stripAll: cint;                   (* strip-space * (1) preserve-space * (-1) *)
    cdataSection: xmlHashTablePtr;    (* the hash table of the cdata-section *)

    (*
     * Global variable or parameters.
     *)
    variables: xsltStackElemPtr;     (* linked list of param and variables *)

    (*
     * Template descriptions.
     *)
    templates: xsltTemplatePtr;           (* the ordered list of templates *)
    templatesHash: xmlHashTablePtr;       (* hash table or wherever compiled
                                             templates information is stored *)
    rootMatch: xsltCompMatchPtr;    (* template based on / *)
    keyMatch: xsltCompMatchPtr;     (* template based on key() *)
    elemMatch: xsltCompMatchPtr;    (* template based on * *)
    attrMatch: xsltCompMatchPtr;    (* template based on @* *)
    parentMatch: xsltCompMatchPtr;  (* template based on .. *)
    textMatch: xsltCompMatchPtr;    (* template based on text() *)
    piMatch: xsltCompMatchPtr;      (* template based on
                                       processing-instruction() *)
    commentMatch: xsltCompMatchPtr; (* template based on comment() *)

    (*
     * Namespace aliases.
     * NOTE: Not used in the refactored code.
     *)
    nsAliases: xmlHashTablePtr;     (* the namespace alias hash tables *)

    (*
     * Attribute sets.
     *)
    attributeSets: xmlHashTablePtr; (* the attribute sets hash tables *)

    (*
     * Namespaces.
     * TODO: Eliminate this.
     *)
    nsHash: xmlHashTablePtr;        (* the set of namespaces in use:
                                       ATTENTION: This is used for
                                       execution of XPath expressions; unfortunately
                                       it restricts the stylesheet to have distinct
                                       prefixes.
                                       TODO: We need to get rid of this.
                                    *)
    nsDefs: Pointer;                (* ATTENTION TODO: This is currently used to store
                                       xsltExtDefPtr (in extensions.c) and
                                       *not* xmlNsPtr.
                                    *)

    (*
     * Key definitions.
     *)
    keys: Pointer;                  (* key definitions *)

    (*
     * Output related stuff.
     *)
    method: xmlCharPtr;             (* the output method *)
    methodURI: xmlCharPtr;          (* associated namespace if any *)
    version: xmlCharPtr;            (* version string *)
    encoding: xmlCharPtr;           (* encoding string *)
    omitXmlDeclaration: cint;       (* omit-xml-declaration = "yes" | "no" *)

    (*
     * Number formatting.
     *)
    decimalFormat: xsltDecimalFormatPtr;
    standalone: cint;               (* standalone = "yes" | "no" *)
    doctypePublic: xmlCharPtr;      (* doctype-public string *)
    doctypeSystem: xmlCharPtr;      (* doctype-system string *)
    indent: cint;                   (* should output being indented *)
    mediaType: xmlCharPtr;          (* media-type string *)

    (*
     * Precomputed blocks.
     *)
    preComps: xsltElemPreCompPtr ;  (* list of precomputed blocks *)
    warnings: cint;                 (* number of warnings found at compilation *)
    errors: cint;                   (* number of errors found at compilation *)

    exclPrefix: xmlCharPtr;         (* last excluded prefixes *)
    exclPrefixTab: xmlCharPtrPtr;   (* array of excluded prefixes *)
    exclPrefixNr: cint;             (* number of excluded prefixes in scope *)
    exclPrefixMax: cint;            (* size of the array *)

    _private: Pointer;              (* user defined data *)

    (*
     * Extensions.
     *)
    extInfos: xmlHashTablePtr;      (* the extension data *)
    extrasNr: cint;                 (* the number of extras required *)

    (*
     * For keeping track of nested includes
     *)
    includes: xsltDocumentPtr;     (* points to last nested include *)

    (*
     * dictionary: shared between stylesheet, context and documents.
     *)
    dict: xmlDictPtr;
    (*
     * precompiled attribute value templates.
     *)
    attVTs: Pointer;
    (*
     * if namespace-alias has an alias for the default stylesheet prefix
     * NOTE: Not used in the refactored code.
     *)
    defaultAlias: xmlCharPtr;
    (*
     * bypass pre-processing (already done) (used in imports)
     *)
    nopreproc: cint;
    (*
     * all document text strings were internalized
     *)
    internalized: cint;
    (*
     * Literal Result Element as Stylesheet c.f. section 2.3
     *)
    literal_result: cint;
    (*
    * The principal stylesheet
    *)
    principal: xsltStylesheetPtr;
{$IFDEF XSLT_REFACTORED}
    (*
    * Compilation context used during compile-time.
    *)
    compCtxt: xsltCompilerCtxtPtr; (* TODO: Change this to (void * ). *)

    principalData: xsltPrincipalStylesheetDataPtr;
{$ENDIF}
    {*
     * Forwards-compatible processing
     *}
    forwards_compatible: cint;

    namedTemplates: xmlHashTablePtr; (* hash table of named templates *)

    xpathCtxt: xmlXPathContextPtr;
  end;

  xsltTransformCachePtr = ^xsltTransformCache;
  xsltTransformCache = record
    RVT: xmlDocPtr;
    nbRVT: cint;
    stackItems: xsltStackElemPtr;
    nbStackItems: cint;
{$IFDEF XSLT_DEBUG_PROFILE_CACHE}
    dbgCachedRVTs: cint;
    dbgReusedRVTs: cint;
    dbgCachedVars: cint;
    dbgReusedVars: cint;
{$ENDIF}
  end;

  (*
   * The in-memory structure corresponding to an XSLT Transformation.
   *)
  xsltOutputType = (
    XSLT_OUTPUT_XML = 0,
    XSLT_OUTPUT_HTML,
    XSLT_OUTPUT_TEXT
  );

  xsltTransformState = (
    XSLT_STATE_OK = 0,
    XSLT_STATE_ERROR,
    XSLT_STATE_STOPPED
  );

  xsltTransformContext = record
    style: xsltStylesheetPtr;           (* the stylesheet used *)
    &type: xsltOutputType;              (* the type of output *)

    templ: xsltTemplatePtr;             (* the current template *)
    templNr: cint;                      (* Nb of templates in the stack *)
    templMax: cint;                     (* Size of the templtes stack *)
    templTab: xsltTemplatePtrPtr;       (* the template stack *)

    vars: xsltStackElemPtr;             (* the current variable list *)
    varsNr: cint;                       (* Nb of variable list in the stack *)
    varsMax: cint;                       (* Size of the variable list stack *)
    varsTab: xsltStackElemPtrPtr;       (* the variable list stack *)
    varsBase: cint;                     (* the var base for current templ *)

    (*
     * Extensions
     *)
    extFunctions: xmlHashTablePtr;      (* the extension functions *)
    extElements: xmlHashTablePtr;       (* the extension elements *)
    extInfos: xmlHashTablePtr;          (* the extension data *)

    mode: xmlCharPtr;                   (* the current mode *)
    modeURI: xmlCharPtr;                (* the current mode URI *)

    docList: xsltDocumentPtr;           (* the document list *)

    document: xsltDocumentPtr;          (* the current source document; can be NULL if an RTF *)
    node: xmlNodePtr;                   (* the current node being processed *)
    nodeList: xmlNodeSetPtr;            (* the current node list *)
    (* xmlNodePtr current;			the node *)

    output: xmlDocPtr;                  (* the resulting document *)
    insert: xmlNodePtr;                 (* the insertion node *)

    xpathCtxt: xmlXPathContextPtr;      (* the XPath context *)
    state: xsltTransformState;          (* the current state *)

    (*
     * Global variables
     *)
    globalVars: xmlHashTablePtr;        (* the global variables and params *)

    inst: xmlNodePtr;                   (* the instruction in the stylesheet *)

    xinclude: cint;                     (* should XInclude be processed *)

    outputFile: PAnsiChar;              (* the output URI if known *)

    profile: cint;                      (* is this run profiled *)
    prof: clong;                        (* the current profiled value *)
    profNr: cint;                       (* Nb of templates in the stack *)
    profMax: cint;                      (* Size of the templtaes stack *)
    profTab: pclong;                    (* the profile template stack *)

    _private: Pointer;                  (* user defined data *)

    extrasNr: cint;                     (* the number of extras used *)
    extrasMax: cint;                    (* the number of extras allocated *)
    extras: xsltRuntimeExtraPtr;        (* extra per runtime information *)

    styleList: xsltDocumentPtr;         (* the stylesheet docs list *)
    sec: Pointer;                       (* the security preferences if any *)

    error: xmlGenericErrorFunc;         (* a specific error handler *)
    errctx: Pointer;                    (* context for the error handler *)

    sortfunc: xsltSortFunc;             (* a ctxt specific sort routine *)

    (*
     * handling of temporary Result Value Tree
     * (XSLT 1.0 term: "Result Tree Fragment")
     *)
    tmpRVT: xmlDocPtr;                  (* list of RVT without persistance *)
    persistRVT: xmlDocPtr;              (* list of persistant RVTs *)
    ctxtflags: cint;                    (* context processing flags *)

    (*
     * Speed optimization when coalescing text nodes
     *)
    lasttext: xmlCharPtr;               (* last text node content *)
    lasttsize: cint;                    (* last text node size *)
    lasttuse: cint;                     (* last text node use *)
    (*
     * Per Context Debugging
     *)
    debugStatus: cint;                  (* the context level debug status *)
    traceCode: pculong;                  (* pointer to the variable holding the mask *)

    parserOptions: cint;                (* parser options xmlParserOption *)

    (*
     * dictionary: shared between stylesheet, context and documents.
     *)
    dict: xmlDictPtr;
    tmpDoc: xmlDocPtr;                  (* Obsolete; not used in the library. *)
    (*
     * all document text strings are internalized
     *)
    internalized: cint;
    nbKeys: cint;
    hasTemplKeyPatterns: cint;
    currentTemplateRule: xsltTemplatePtr; (* the Current Template Rule *)
    initialContextNode: xmlNodePtr;
    initialContextDoc: xmlDocPtr;
    cache: xsltTransformCachePtr;
    contextVariable: Pointer; (* the current variable item *)
    localRVT: xmlDocPtr; (* list of local tree fragments; will be freed when
                            the instruction which created the fragment
                            exits *)
    localRVTBase: xmlDocPtr; (* Obsolete *)
    keyInitLevel: cint;   (* Needed to catch recursive keys issues *)
    depth: cint;          (* Needed to catch recursions *)
    maxTemplateDepth: cint;
    maxTemplateVars: cint;
    opLimit: culong;
    opCount: culong;
  end;
{$ENDIF} {TYPE}

{$IFDEF FUNCTION}
(*
 * Functions associated to the internal types
xsltDecimalFormatPtr	xsltDecimalFormatGetByName(xsltStylesheetPtr sheet,
  						 xmlChar *name);
 *)
function xsltNewStylesheet: xsltStylesheetPtr; EXTDECL; external xsltlib;
function xsltParseStylesheetFile(const filename: xmlCharPtr): xsltStylesheetPtr; EXTDECL; external xsltlib;
procedure xsltFreeStylesheet(style: xsltStylesheetPtr); EXTDECL; external xsltlib;
function xsltIsBlank(str: xmlCharPtr): cint; EXTDECL; external xsltlib;
procedure xsltFreeStackElemList(elem: xsltStackElemPtr); EXTDECL; external xsltlib;
function xsltDecimalFormatGetByName(style: xsltStylesheetPtr; name: xmlCharPtr): xsltDecimalFormatPtr; EXTDECL; external xsltlib;
function xsltDecimalFormatGetByQName(style: xsltStylesheetPtr; const nsUri: xmlCharPtr; const name: xmlCharPtr): xsltDecimalFormatPtr; EXTDECL; external xsltlib;
function xsltParseStylesheetProcess(ret: xsltStylesheetPtr; doc: xmlDocPtr): xsltStylesheetPtr; EXTDECL; external xsltlib;
procedure xsltParseStylesheetOutput(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL; external xsltlib;
function xsltParseStylesheetDoc(doc: xmlDocPtr): xsltStylesheetPtr; EXTDECL; external xsltlib;
function xsltParseStylesheetImportedDoc(doc: xmlDocPtr; style: xsltStylesheetPtr): xsltStylesheetPtr; EXTDECL; external xsltlib;
function xsltParseStylesheetUser(style: xsltStylesheetPtr; doc: xmlDocPtr): cint; EXTDECL; external xsltlib;
function xsltLoadStylesheetPI(doc: xmlDocPtr): xsltStylesheetPtr; EXTDECL; external xsltlib;
procedure xsltNumberFormat(ctxt: xsltTransformContextPtr; data: xsltNumberDataPtr; node: xmlNodePtr); EXTDECL; external xsltlib;
function xsltFormatNumberConversion(self: xsltDecimalFormatPtr; format: xmlCharPtr; number: double; result: xmlCharPtrPtr): xmlXPathError; EXTDECL; external xsltlib;
procedure xsltParseTemplateContent(style: xsltStylesheetPtr; templ: xmlNodePtr); EXTDECL; external xsltlib;
function xsltAllocateExtra(style: xsltStylesheetPtr): cint; EXTDECL; external xsltlib;
function xsltAllocateExtraCtxt(ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;

(*
 * Extra functions for Result Value Trees
 *)
function xsltCreateRVT(ctxt: xsltTransformContextPtr): xmlDocPtr; EXTDECL; external xsltlib;
function xsltRegisterTmpRVT(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr): cint; EXTDECL; external xsltlib;
function xsltRegisterLocalRVT(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr): cint; EXTDECL; external xsltlib;
function xsltRegisterPersistRVT(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr): cint; EXTDECL; external xsltlib;
function xsltExtensionInstructionResultRegister(ctxt: xsltTransformContextPtr; obj: xmlXPathObjectPtr): cint; EXTDECL; external xsltlib;
function xsltExtensionInstructionResultFinalize(ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;
function xsltFlagRVTs(ctxt: xsltTransformContextPtr; obj: xmlXPathObjectPtr; val: Pointer): cint; EXTDECL; external xsltlib;
procedure xsltFreeRVTs(ctxt: xsltTransformContextPtr); EXTDECL; external xsltlib;
procedure xsltReleaseRVT(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr); EXTDECL; external xsltlib;

(*
 * Extra functions for Attribute Value Templates
 *)
procedure xsltCompileAttr(style: xsltStylesheetPtr; attr: xmlAttrPtr); EXTDECL; external xsltlib;
function xsltEvalAVT(ctxt: xsltTransformContextPtr; avt: Pointer; node: xmlNodePtr): xmlCharPtr; EXTDECL; external xsltlib;
procedure xsltFreeAVTList(avt: Pointer); EXTDECL; external xsltlib;

(*
 * Extra function for successful xsltCleanupGlobals / xsltInit sequence.
 *)

procedure xsltUninit; EXTDECL; external xsltlib;

(************************************************************************
 *									*
 *  Compile-time functions for *internal* use only                      *
 *									*
 ************************************************************************)

{$IFDEF XSLT_REFACTORED}
procedure xsltParseSequenceConstructor(cctxt: xsltCompilerCtxtPtr; start: xmlNodePtr); EXTDECL; external xsltlib;
function xsltParseAnyXSLTElem(cctxt: xsltCompilerCtxtPtr; elem: xmlNodePtr): cint; EXTDECL; external xsltlib;
{$IFDEF XSLT_REFACTORED_XSLT_NSCOMP}
function xsltRestoreDocumentNamespaces(ns: xsltNsMapPtr; doc: xmlDocPtr): cint; EXTDECL; external xsltlib;
{$ENDIF}
{$ENDIF} (* XSLT_REFACTORED *)

(************************************************************************
 *									*
 *  Transformation-time functions for *internal* use only               *
 *									*
 ************************************************************************)
function xsltInitCtxtKey(ctxt: xsltTransformContextPtr; doc: xsltDocumentPtr; keyd: xsltKeyDefPtr): cint; EXTDECL; external xsltlib;
function xsltInitAllDocKeys(ctxt: xsltTransformContextPtr): cint; EXTDECL; external xsltlib;

{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}
(*
 * Functions associated to the internal types
xsltDecimalFormatPtr	xsltDecimalFormatGetByName(xsltStylesheetPtr sheet,
  						 xmlChar *name);
 *)
  xsltNewStylesheet: function: xsltStylesheetPtr; EXTDECL;
  xsltParseStylesheetFile: function(const filename: xmlCharPtr): xsltStylesheetPtr; EXTDECL;
  xsltFreeStylesheet: procedure(style: xsltStylesheetPtr); EXTDECL;
  xsltIsBlank: function(str: xmlCharPtr): cint; EXTDECL;
  xsltFreeStackElemList: procedure(elem: xsltStackElemPtr); EXTDECL;
  xsltDecimalFormatGetByName: function(style: xsltStylesheetPtr; name: xmlCharPtr): xsltDecimalFormatPtr; EXTDECL;
  xsltDecimalFormatGetByQName: function(style: xsltStylesheetPtr; const nsUri: xmlCharPtr; const name: xmlCharPtr): xsltDecimalFormatPtr; EXTDECL;
  xsltParseStylesheetProcess: function(ret: xsltStylesheetPtr; doc: xmlDocPtr): xsltStylesheetPtr; EXTDECL;
  xsltParseStylesheetOutput: procedure(style: xsltStylesheetPtr; cur: xmlNodePtr); EXTDECL;
  xsltParseStylesheetDoc: function(doc: xmlDocPtr): xsltStylesheetPtr; EXTDECL;
  xsltParseStylesheetImportedDoc: function(doc: xmlDocPtr; style: xsltStylesheetPtr): xsltStylesheetPtr; EXTDECL;
  xsltParseStylesheetUser: function(style: xsltStylesheetPtr; doc: xmlDocPtr): cint; EXTDECL;
  xsltLoadStylesheetPI: function(doc: xmlDocPtr): xsltStylesheetPtr; EXTDECL;
  xsltNumberFormat: procedure(ctxt: xsltTransformContextPtr; data: xsltNumberDataPtr; node: xmlNodePtr); EXTDECL;
  xsltFormatNumberConversion: function(self: xsltDecimalFormatPtr; format: xmlCharPtr; number: double; result: xmlCharPtrPtr): xmlXPathError; EXTDECL;
  xsltParseTemplateContent: procedure(style: xsltStylesheetPtr; templ: xmlNodePtr); EXTDECL;
  xsltAllocateExtra: function(style: xsltStylesheetPtr): cint; EXTDECL;
  xsltAllocateExtraCtxt: function(ctxt: xsltTransformContextPtr): cint; EXTDECL;

(*
 * Extra functions for Result Value Trees
 *)
  xsltCreateRVT: function(ctxt: xsltTransformContextPtr): xmlDocPtr; EXTDECL;
  xsltRegisterTmpRVT: function(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr): cint; EXTDECL;
  xsltRegisterLocalRVT: function(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr): cint; EXTDECL;
  xsltRegisterPersistRVT: function(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr): cint; EXTDECL;
  xsltExtensionInstructionResultRegister: function(ctxt: xsltTransformContextPtr; obj: xmlXPathObjectPtr): cint; EXTDECL;
  xsltExtensionInstructionResultFinalize: function(ctxt: xsltTransformContextPtr): cint; EXTDECL;
  xsltFlagRVTs: function(ctxt: xsltTransformContextPtr; obj: xmlXPathObjectPtr; val: Pointer): cint; EXTDECL;
  xsltFreeRVTs: procedure(ctxt: xsltTransformContextPtr); EXTDECL;
  xsltReleaseRVT: procedure(ctxt: xsltTransformContextPtr; RVT: xmlDocPtr); EXTDECL;

(*
 * Extra functions for Attribute Value Templates
 *)
  xsltCompileAttr: procedure(style: xsltStylesheetPtr; attr: xmlAttrPtr); EXTDECL;
  xsltEvalAVT: function(ctxt: xsltTransformContextPtr; avt: Pointer; node: xmlNodePtr): xmlCharPtr; EXTDECL;
  xsltFreeAVTList: procedure(avt: Pointer); EXTDECL;

(*
 * Extra function for successful xsltCleanupGlobals / xsltInit sequence.
 *)

  xsltUninit: procedure; EXTDECL;

(************************************************************************
 *									*
 *  Compile-time functions for *internal* use only                      *
 *									*
 ************************************************************************)

{$IFDEF XSLT_REFACTORED}
  xsltParseSequenceConstructor: procedure(cctxt: xsltCompilerCtxtPtr; start: xmlNodePtr); EXTDECL;
  xsltParseAnyXSLTElem: function(cctxt: xsltCompilerCtxtPtr; elem: xmlNodePtr): cint; EXTDECL;
{$IFDEF XSLT_REFACTORED_XSLT_NSCOMP}
  xsltRestoreDocumentNamespaces: function(ns: xsltNsMapPtr; doc: xmlDocPtr): cint; EXTDECL;
{$ENDIF}
{$ENDIF} (* XSLT_REFACTORED *)

(************************************************************************
 *									*
 *  Transformation-time functions for *internal* use only               *
 *									*
 ************************************************************************)
  xsltInitCtxtKey: function(ctxt: xsltTransformContextPtr; doc: xsltDocumentPtr; keyd: xsltKeyDefPtr): cint; EXTDECL;
  xsltInitAllDocKeys: function(ctxt: xsltTransformContextPtr): cint; EXTDECL;

{$ENDIF} {FUNCTIONVAR}
