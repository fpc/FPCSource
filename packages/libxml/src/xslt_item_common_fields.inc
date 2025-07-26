(**
 * XSLT_ITEM_COMPATIBILITY_FIELDS:
 *
 * Fields for API compatibility to the structure
 * _xsltElemPreComp which is used for extension functions.
 * Note that @next is used for storage; it does not reflect a next
 * sibling in the tree.
 * TODO: Evaluate if we really need such a compatibility.
 *)

    next: xsltElemPreCompPtr;
    &type: xsltStyleType;
    func: xsltTransformFunction;
    inst: xmlNodePtr;

(**
 * XSLT_ITEM_NAVIGATION_FIELDS:
 *
 * Currently empty.
 * TODO: It is intended to hold navigational fields in the future.
 *)
(*
    parent: xsltStylePreCompPtr;
    children: xsltStylePreCompPtr;
    nextItem: xsltStylePreCompPtr;
*)

(**
 * XSLT_ITEM_NSINSCOPE_FIELDS:
 *
 * The in-scope namespaces.
 *)
    inScopeNs: xsltNsListContainerPtr;

(**
 * XSLT_ITEM_COMMON_FIELDS:
 *
 * Common fields used for all items.
 */
#define XSLT_ITEM_COMMON_FIELDS \
    XSLT_ITEM_COMPATIBILITY_FIELDS \
    XSLT_ITEM_NAVIGATION_FIELDS \
    XSLT_ITEM_NSINSCOPE_FIELDS
*)

