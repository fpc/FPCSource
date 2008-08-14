(*
 * Summary: Old SAX version 1 handler, deprecated
 * Description: DEPRECATED set of SAX version 1 interfaces used to
 *              build the DOM tree.
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF LIBXML_LEGACY_ENABLED}

{$IFDEF FUNCTION_}

XMLPUBFUN xmlChar * XMLCALL
		getPublicId			(void *ctx);
XMLPUBFUN xmlChar * XMLCALL	
		getSystemId			(void *ctx);
XMLPUBFUN void XMLCALL		
		setDocumentLocator		(void *ctx,
						 xmlSAXLocatorPtr loc);
    
XMLPUBFUN int XMLCALL		
		getLineNumber			(void *ctx);
XMLPUBFUN int XMLCALL		
		getColumnNumber			(void *ctx);

XMLPUBFUN int XMLCALL		
		isStandalone			(void *ctx);
XMLPUBFUN int XMLCALL		
		hasInternalSubset		(void *ctx);
XMLPUBFUN int XMLCALL		
		hasExternalSubset		(void *ctx);

XMLPUBFUN void XMLCALL		
		internalSubset			(void *ctx,
						 xmlChar *name,
						 xmlChar *ExternalID,
						 xmlChar *SystemID);
XMLPUBFUN void XMLCALL		
		externalSubset			(void *ctx,
						 xmlChar *name,
						 xmlChar *ExternalID,
						 xmlChar *SystemID);
XMLPUBFUN xmlEntityPtr XMLCALL	
		getEntity			(void *ctx,
						 xmlChar *name);
XMLPUBFUN xmlEntityPtr XMLCALL	
		getParameterEntity		(void *ctx,
						 xmlChar *name);
XMLPUBFUN xmlParserInputPtr XMLCALL 
		resolveEntity			(void *ctx,
						 xmlChar *publicId,
						 xmlChar *systemId);

XMLPUBFUN void XMLCALL		
		entityDecl			(void *ctx,
						 xmlChar *name,
						 int type,
						 xmlChar *publicId,
						 xmlChar *systemId,
						 xmlChar *content);
XMLPUBFUN void XMLCALL		
		attributeDecl			(void *ctx,
						 xmlChar *elem,
						 xmlChar *fullname,
						 int type,
						 int def,
						 xmlChar *defaultValue,
						 xmlEnumerationPtr tree);
XMLPUBFUN void XMLCALL		
		elementDecl			(void *ctx,
						 xmlChar *name,
						 int type,
						 xmlElementContentPtr content);
XMLPUBFUN void XMLCALL		
		notationDecl			(void *ctx,
						 xmlChar *name,
						 xmlChar *publicId,
						 xmlChar *systemId);
XMLPUBFUN void XMLCALL		
		unparsedEntityDecl		(void *ctx,
						 xmlChar *name,
						 xmlChar *publicId,
						 xmlChar *systemId,
						 xmlChar *notationName);

XMLPUBFUN void XMLCALL		
		startDocument			(void *ctx);
XMLPUBFUN void XMLCALL		
		endDocument			(void *ctx);
XMLPUBFUN void XMLCALL		
		attribute			(void *ctx,
						 xmlChar *fullname,
						 xmlChar *value);
XMLPUBFUN void XMLCALL		
		startElement			(void *ctx,
						 xmlChar *fullname,
						 xmlChar **atts);
XMLPUBFUN void XMLCALL		
		endElement			(void *ctx,
						 xmlChar *name);
XMLPUBFUN void XMLCALL		
		reference			(void *ctx,
						 xmlChar *name);
XMLPUBFUN void XMLCALL		
		characters			(void *ctx,
						 xmlChar *ch,
						 int len);
XMLPUBFUN void XMLCALL		
		ignorableWhitespace		(void *ctx,
						 xmlChar *ch,
						 int len);
XMLPUBFUN void XMLCALL		
		processingInstruction		(void *ctx,
						 xmlChar *target,
						 xmlChar *data);
XMLPUBFUN void XMLCALL		
		globalNamespace			(void *ctx,
						 xmlChar *href,
						 xmlChar *prefix);
XMLPUBFUN void XMLCALL		
		setNamespace			(void *ctx,
						 xmlChar *name);
XMLPUBFUN xmlNsPtr XMLCALL	
		getNamespace			(void *ctx);
XMLPUBFUN int XMLCALL		
		checkNamespace			(void *ctx,
						 xmlChar *nameSpace);
XMLPUBFUN void XMLCALL		
		namespaceDecl			(void *ctx,
						 xmlChar *href,
						 xmlChar *prefix);
XMLPUBFUN void XMLCALL		
		comment				(void *ctx,
						 xmlChar *value);
XMLPUBFUN void XMLCALL		
		cdataBlock			(void *ctx,
						 xmlChar *value,
						 int len);

{$IFDEF LIBXML_SAX1_ENABLED}
XMLPUBFUN void XMLCALL		
		initxmlDefaultSAXHandler	(xmlSAXHandlerV1 *hdlr,
						 int warning);
{$IFDEF LIBXML_HTML_ENABLED}
XMLPUBFUN void XMLCALL		
		inithtmlDefaultSAXHandler	(xmlSAXHandlerV1 *hdlr);
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
XMLPUBFUN void XMLCALL		
		initdocbDefaultSAXHandler	(xmlSAXHandlerV1 *hdlr);
{$ENDIF}
{$ENDIF} (* LIBXML_SAX1_ENABLED *)
{$ENDIF}

{$ENDIF} (* LIBXML_LEGACY_ENABLED *)
