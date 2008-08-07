(**
 * Summary: interfaces to the Catalog handling system
 * Description: the catalog module implements the support for
 * XML Catalogs and SGML catalogs
 *
 * SGML Open Technical Resolution TR9401:1997.
 * http://www.jclark.com/sp/catalog.htm
 *
 * XML Catalogs Working Draft 06 August 2001
 * http://www.oasis-open.org/committees/entity/spec-2001-08-06.html
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

#ifndef __XML_CATALOG_H__
#define __XML_CATALOG_H__

#include <stdio.h>

#include <libxml/xmlversion.h>
#include <libxml/xmlstring.h>
#include <libxml/tree.h>

{ LIBXML_CATALOG_ENABLED

{ __cplusplus
extern "C" {
#endif

(**
 * XML_CATALOGS_NAMESPACE:
 *
 * The namespace for the XML Catalogs elements.
 *)
#define XML_CATALOGS_NAMESPACE					\
    (xmlChar *) "urn:oasis:names:tc:entity:xmlns:xml:catalog"
(**
 * XML_CATALOG_PI:
 *
 * The specific XML Catalog Processing Instuction name.
 *)
#define XML_CATALOG_PI						\
    (xmlChar *) "oasis-xml-catalog"

(*
 * The API is voluntarily limited to general cataloging.
 *)
typedef enum {
    XML_CATA_PREFER_NONE = 0,
    XML_CATA_PREFER_PUBLIC = 1,
    XML_CATA_PREFER_SYSTEM
} xmlCatalogPrefer;

typedef enum {
    XML_CATA_ALLOW_NONE = 0,
    XML_CATA_ALLOW_GLOBAL = 1,
    XML_CATA_ALLOW_DOCUMENT = 2,
    XML_CATA_ALLOW_ALL = 3
} xmlCatalogAllow;

typedef struct _xmlCatalog xmlCatalog;
typedef xmlCatalog *xmlCatalogPtr;

(*
 * Operations on a given catalog.
 *)
XMLPUBFUN xmlCatalogPtr XMLCALL
		xmlNewCatalog		(int sgml);
XMLPUBFUN xmlCatalogPtr XMLCALL	
		xmlLoadACatalog		(char *filename);
XMLPUBFUN xmlCatalogPtr XMLCALL	
		xmlLoadSGMLSuperCatalog	(char *filename);
XMLPUBFUN int XMLCALL		
		xmlConvertSGMLCatalog	(xmlCatalogPtr catal);
XMLPUBFUN int XMLCALL		
		xmlACatalogAdd		(xmlCatalogPtr catal,
					 xmlChar *type,
					 xmlChar *orig,
					 xmlChar *replace);
XMLPUBFUN int XMLCALL		
		xmlACatalogRemove	(xmlCatalogPtr catal,
					 xmlChar *value);
XMLPUBFUN xmlChar * XMLCALL	
		xmlACatalogResolve	(xmlCatalogPtr catal,
					 xmlChar *pubID,
	                                 xmlChar *sysID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlACatalogResolveSystem(xmlCatalogPtr catal,
					 xmlChar *sysID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlACatalogResolvePublic(xmlCatalogPtr catal,
					 xmlChar *pubID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlACatalogResolveURI	(xmlCatalogPtr catal,
					 xmlChar *URI);
{ LIBXML_OUTPUT_ENABLED
XMLPUBFUN void XMLCALL		
		xmlACatalogDump		(xmlCatalogPtr catal,
					 FILE *out);
#endif (* LIBXML_OUTPUT_ENABLED *)
XMLPUBFUN void XMLCALL		
		xmlFreeCatalog		(xmlCatalogPtr catal);
XMLPUBFUN int XMLCALL		
		xmlCatalogIsEmpty	(xmlCatalogPtr catal);

(*
 * Global operations.
 *)
XMLPUBFUN void XMLCALL		
		xmlInitializeCatalog	(void);
XMLPUBFUN int XMLCALL		
		xmlLoadCatalog		(char *filename);
XMLPUBFUN void XMLCALL		
		xmlLoadCatalogs		(char *paths);
XMLPUBFUN void XMLCALL		
		xmlCatalogCleanup	(void);
{ LIBXML_OUTPUT_ENABLED
XMLPUBFUN void XMLCALL		
		xmlCatalogDump		(FILE *out);
#endif (* LIBXML_OUTPUT_ENABLED *)
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogResolve	(xmlChar *pubID,
	                                 xmlChar *sysID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogResolveSystem	(xmlChar *sysID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogResolvePublic	(xmlChar *pubID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogResolveURI	(xmlChar *URI);
XMLPUBFUN int XMLCALL		
		xmlCatalogAdd		(xmlChar *type,
					 xmlChar *orig,
					 xmlChar *replace);
XMLPUBFUN int XMLCALL		
		xmlCatalogRemove	(xmlChar *value);
XMLPUBFUN xmlDocPtr XMLCALL	
		xmlParseCatalogFile	(char *filename);
XMLPUBFUN int XMLCALL		
		xmlCatalogConvert	(void);

(*
 * Strictly minimal interfaces for per-document catalogs used
 * by the parser.
 *)
XMLPUBFUN void XMLCALL		
		xmlCatalogFreeLocal	(void *catalogs);
XMLPUBFUN void * XMLCALL		
		xmlCatalogAddLocal	(void *catalogs,
					 xmlChar *URL);
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogLocalResolve	(void *catalogs,
					 xmlChar *pubID,
	                                 xmlChar *sysID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogLocalResolveURI(void *catalogs,
					 xmlChar *URI);
(*
 * Preference settings.
 *)
XMLPUBFUN int XMLCALL		
		xmlCatalogSetDebug	(int level);
XMLPUBFUN xmlCatalogPrefer XMLCALL 
		xmlCatalogSetDefaultPrefer(xmlCatalogPrefer prefer);
XMLPUBFUN void XMLCALL		
		xmlCatalogSetDefaults	(xmlCatalogAllow allow);
XMLPUBFUN xmlCatalogAllow XMLCALL	
		xmlCatalogGetDefaults	(void);


(* DEPRECATED interfaces *)
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogGetSystem	(xmlChar *sysID);
XMLPUBFUN xmlChar * XMLCALL	
		xmlCatalogGetPublic	(xmlChar *pubID);

{ __cplusplus
}
#endif
#endif (* LIBXML_CATALOG_ENABLED *)
#endif (* __XML_CATALOG_H__ *)
