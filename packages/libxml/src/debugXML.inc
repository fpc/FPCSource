(*
 * Summary: Tree debugging APIs
 * Description: Interfaces to a set of routines used for debugging the tree
 *              produced by the XML parser.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

#ifndef __DEBUG_XML__
#define __DEBUG_XML__
#include <stdio.h>
#include <libxml/xmlversion.h>
#include <libxml/tree.h>

{ LIBXML_DEBUG_ENABLED

#include <libxml/xpath.h>

{ __cplusplus
extern "C" {
#endif

(*
 * The standard Dump routines.
 *)
external xml2lib void EXTDECL	
	xmlDebugDumpString	(FILE *output,
				 xmlChar *str);
external xml2lib void EXTDECL	
	xmlDebugDumpAttr	(FILE *output,
				 xmlAttrPtr attr,
				 int depth);
external xml2lib void EXTDECL	
	xmlDebugDumpAttrList	(FILE *output,
				 xmlAttrPtr attr,
				 int depth);
external xml2lib void EXTDECL	
	xmlDebugDumpOneNode	(FILE *output,
				 xmlNodePtr node,
				 int depth);
external xml2lib void EXTDECL
	xmlDebugDumpNode	(FILE *output,
				 xmlNodePtr node,
				 int depth);
external xml2lib void EXTDECL
	xmlDebugDumpNodeList	(FILE *output,
				 xmlNodePtr node,
				 int depth);
external xml2lib void EXTDECL
	xmlDebugDumpDocumentHead(FILE *output,
				 xmlDocPtr doc);
external xml2lib void EXTDECL
	xmlDebugDumpDocument	(FILE *output,
				 xmlDocPtr doc);
external xml2lib void EXTDECL	
	xmlDebugDumpDTD		(FILE *output,
				 xmlDtdPtr dtd);
external xml2lib void EXTDECL	
	xmlDebugDumpEntities	(FILE *output,
				 xmlDocPtr doc);

(****************************************************************
 *								*
 *	 		Checking routines			*
 *								*
 ****************************************************************)

external xml2lib int EXTDECL
	xmlDebugCheckDocument	(FILE * output,
				 xmlDocPtr doc);

(****************************************************************
 *								*
 *	 		XML shell helpers			*
 *								*
 ****************************************************************)

external xml2lib void EXTDECL	
	xmlLsOneNode		(FILE *output, xmlNodePtr node);
external xml2lib int EXTDECL	
	xmlLsCountNode		(xmlNodePtr node);

external xml2lib char * EXTDECL 
	xmlBoolToText		(int boolval);

(****************************************************************
 *								*
 *	 The XML shell related structures and functions		*
 *								*
 ****************************************************************)

{ LIBXML_XPATH_ENABLED
(**
 * xmlShellReadlineFunc:
 * @prompt:  a string prompt
 *
 * This is a generic signature for the XML shell input function.
 *
 * Returns a string which will be freed by the Shell.
 *)
typedef char * (* xmlShellReadlineFunc)(char *prompt);

(**
 * xmlShellCtxt:
 *
 * A debugging shell context.
 * TODO: add the defined function tables.
 *)
typedef struct _xmlShellCtxt xmlShellCtxt;
typedef xmlShellCtxt *xmlShellCtxtPtr;
struct _xmlShellCtxt {
    char *filename;
    xmlDocPtr doc;
    xmlNodePtr node;
    xmlXPathContextPtr pctxt;
    int loaded;
    FILE *output;
    xmlShellReadlineFunc input;
};

(**
 * xmlShellCmd:
 * @ctxt:  a shell context
 * @arg:  a string argument
 * @node:  a first node
 * @node2:  a second node
 *
 * This is a generic signature for the XML shell functions.
 *
 * Returns an int, negative returns indicating errors.
 *)
typedef int (* xmlShellCmd) (xmlShellCtxtPtr ctxt,
                             char *arg,
			     xmlNodePtr node,
			     xmlNodePtr node2);

external xml2lib void EXTDECL	
	xmlShellPrintXPathError	(int errorType,
				 char *arg);
external xml2lib void EXTDECL	
	xmlShellPrintXPathResult(xmlXPathObjectPtr list);
external xml2lib int EXTDECL	
	xmlShellList		(xmlShellCtxtPtr ctxt,
				 char *arg,
				 xmlNodePtr node,
				 xmlNodePtr node2);
external xml2lib int EXTDECL	
	xmlShellBase		(xmlShellCtxtPtr ctxt,
				 char *arg,
				 xmlNodePtr node,
				 xmlNodePtr node2);
external xml2lib int EXTDECL	
	xmlShellDir		(xmlShellCtxtPtr ctxt,
				 char *arg,
				 xmlNodePtr node,
				 xmlNodePtr node2);
external xml2lib int EXTDECL	
	xmlShellLoad		(xmlShellCtxtPtr ctxt,
				 char *filename,
				 xmlNodePtr node,
				 xmlNodePtr node2);
{ LIBXML_OUTPUT_ENABLED
external xml2lib void EXTDECL	
	xmlShellPrintNode	(xmlNodePtr node);
external xml2lib int EXTDECL	
	xmlShellCat		(xmlShellCtxtPtr ctxt,
				 char *arg,
				 xmlNodePtr node,
				 xmlNodePtr node2);
external xml2lib int EXTDECL	
	xmlShellWrite		(xmlShellCtxtPtr ctxt,
				 char *filename,
				 xmlNodePtr node,
				 xmlNodePtr node2);
external xml2lib int EXTDECL	
	xmlShellSave		(xmlShellCtxtPtr ctxt,
				 char *filename,
				 xmlNodePtr node,
				 xmlNodePtr node2);
#endif (* LIBXML_OUTPUT_ENABLED *)
{ LIBXML_VALID_ENABLED
external xml2lib int EXTDECL	
	xmlShellValidate	(xmlShellCtxtPtr ctxt,
				 char *dtd,
				 xmlNodePtr node,
				 xmlNodePtr node2);
#endif (* LIBXML_VALID_ENABLED *)
external xml2lib int EXTDECL	
	xmlShellDu		(xmlShellCtxtPtr ctxt,
				 char *arg,
				 xmlNodePtr tree,
				 xmlNodePtr node2);
external xml2lib int EXTDECL	
	xmlShellPwd		(xmlShellCtxtPtr ctxt,
				 char *buffer,
				 xmlNodePtr node,
				 xmlNodePtr node2);

(*
 * The Shell interface.
 *)
external xml2lib void EXTDECL	
	xmlShell		(xmlDocPtr doc,
				 char *filename,
				 xmlShellReadlineFunc input,
				 FILE *output);
			 
#endif (* LIBXML_XPATH_ENABLED *)

{ __cplusplus
}
#endif

#endif (* LIBXML_DEBUG_ENABLED *)
#endif (* __DEBUG_XML__ *)
