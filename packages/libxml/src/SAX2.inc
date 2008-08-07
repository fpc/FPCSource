(*
 * Summary: SAX2 parser interface used to build the DOM tree
 * Description: those are the default SAX2 interfaces used by
 *              the library when building DOM tree.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


#ifndef __XML_SAX2_H__
#define __XML_SAX2_H__

#include <stdio.h>
#include <stdlib.h>
#include <libxml/xmlversion.h>
#include <libxml/parser.h>
#include <libxml/xlink.h>

{ __cplusplus
extern "C" {
#endif
XMLPUBFUN xmlChar * XMLCALL
		xmlSAX2GetPublicId		(void *ctx);
XMLPUBFUN xmlChar * XMLCALL	
		xmlSAX2GetSystemId		(void *ctx);
XMLPUBFUN void XMLCALL		
		xmlSAX2SetDocumentLocator	(void *ctx,
						 xmlSAXLocatorPtr loc);
    
XMLPUBFUN int XMLCALL		
		xmlSAX2GetLineNumber		(void *ctx);
XMLPUBFUN int XMLCALL		
		xmlSAX2GetColumnNumber		(void *ctx);

XMLPUBFUN int XMLCALL		
		xmlSAX2IsStandalone		(void *ctx);
XMLPUBFUN int XMLCALL		
		xmlSAX2HasInternalSubset	(void *ctx);
XMLPUBFUN int XMLCALL		
		xmlSAX2HasExternalSubset	(void *ctx);

XMLPUBFUN void XMLCALL		
		xmlSAX2InternalSubset		(void *ctx,
						 xmlChar *name,
						 xmlChar *ExternalID,
						 xmlChar *SystemID);
XMLPUBFUN void XMLCALL		
		xmlSAX2ExternalSubset		(void *ctx,
						 xmlChar *name,
						 xmlChar *ExternalID,
						 xmlChar *SystemID);
XMLPUBFUN xmlEntityPtr XMLCALL	
		xmlSAX2GetEntity		(void *ctx,
						 xmlChar *name);
XMLPUBFUN xmlEntityPtr XMLCALL	
		xmlSAX2GetParameterEntity	(void *ctx,
						 xmlChar *name);
XMLPUBFUN xmlParserInputPtr XMLCALL 
		xmlSAX2ResolveEntity		(void *ctx,
						 xmlChar *publicId,
						 xmlChar *systemId);

XMLPUBFUN void XMLCALL		
		xmlSAX2EntityDecl		(void *ctx,
						 xmlChar *name,
						 int type,
						 xmlChar *publicId,
						 xmlChar *systemId,
						 xmlChar *content);
XMLPUBFUN void XMLCALL		
		xmlSAX2AttributeDecl		(void *ctx,
						 xmlChar *elem,
						 xmlChar *fullname,
						 int type,
						 int def,
						 xmlChar *defaultValue,
						 xmlEnumerationPtr tree);
XMLPUBFUN void XMLCALL		
		xmlSAX2ElementDecl		(void *ctx,
						 xmlChar *name,
						 int type,
						 xmlElementContentPtr content);
XMLPUBFUN void XMLCALL		
		xmlSAX2NotationDecl		(void *ctx,
						 xmlChar *name,
						 xmlChar *publicId,
						 xmlChar *systemId);
XMLPUBFUN void XMLCALL		
		xmlSAX2UnparsedEntityDecl	(void *ctx,
						 xmlChar *name,
						 xmlChar *publicId,
						 xmlChar *systemId,
						 xmlChar *notationName);

XMLPUBFUN void XMLCALL		
		xmlSAX2StartDocument		(void *ctx);
XMLPUBFUN void XMLCALL		
		xmlSAX2EndDocument		(void *ctx);
#if defined(LIBXML_SAX1_ENABLED) || defined(LIBXML_HTML_ENABLED) || defined(LIBXML_WRITER_ENABLED) || defined(LIBXML_DOCB_ENABLED)
XMLPUBFUN void XMLCALL		
		xmlSAX2StartElement		(void *ctx,
						 xmlChar *fullname,
						 xmlChar **atts);
XMLPUBFUN void XMLCALL		
		xmlSAX2EndElement		(void *ctx,
						 xmlChar *name);
#endif (* LIBXML_SAX1_ENABLED or LIBXML_HTML_ENABLED *)
XMLPUBFUN void XMLCALL
		xmlSAX2StartElementNs		(void *ctx,
						 xmlChar *localname,
						 xmlChar *prefix,
						 xmlChar *URI,
						 int nb_namespaces,
						 xmlChar **namespaces,
						 int nb_attributes,
						 int nb_defaulted,
						 xmlChar **attributes);
XMLPUBFUN void XMLCALL
		xmlSAX2EndElementNs		(void *ctx,
						 xmlChar *localname,
						 xmlChar *prefix,
						 xmlChar *URI);
XMLPUBFUN void XMLCALL		
		xmlSAX2Reference		(void *ctx,
						 xmlChar *name);
XMLPUBFUN void XMLCALL		
		xmlSAX2Characters		(void *ctx,
						 xmlChar *ch,
						 int len);
XMLPUBFUN void XMLCALL		
		xmlSAX2IgnorableWhitespace	(void *ctx,
						 xmlChar *ch,
						 int len);
XMLPUBFUN void XMLCALL		
		xmlSAX2ProcessingInstruction	(void *ctx,
						 xmlChar *target,
						 xmlChar *data);
XMLPUBFUN void XMLCALL		
		xmlSAX2Comment			(void *ctx,
						 xmlChar *value);
XMLPUBFUN void XMLCALL		
		xmlSAX2CDataBlock		(void *ctx,
						 xmlChar *value,
						 int len);

{ LIBXML_SAX1_ENABLED
XMLPUBFUN int XMLCALL
		xmlSAXDefaultVersion		(int version);
#endif (* LIBXML_SAX1_ENABLED *)

XMLPUBFUN int XMLCALL
		xmlSAXVersion			(xmlSAXHandler *hdlr,
						 int version);
XMLPUBFUN void XMLCALL		
		xmlSAX2InitDefaultSAXHandler    (xmlSAXHandler *hdlr,
						 int warning);
{ LIBXML_HTML_ENABLED
XMLPUBFUN void XMLCALL		
		xmlSAX2InitHtmlDefaultSAXHandler(xmlSAXHandler *hdlr);
XMLPUBFUN void XMLCALL		
		htmlDefaultSAXHandlerInit	(void);
#endif
{ LIBXML_DOCB_ENABLED
XMLPUBFUN void XMLCALL		
		xmlSAX2InitDocbDefaultSAXHandler(xmlSAXHandler *hdlr);
XMLPUBFUN void XMLCALL		
		docbDefaultSAXHandlerInit	(void);
#endif
XMLPUBFUN void XMLCALL		
		xmlDefaultSAXHandlerInit	(void);
{ __cplusplus
}
#endif
#endif (* __XML_SAX2_H__ *)
