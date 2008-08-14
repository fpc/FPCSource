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

{$IFDEF LIBXML_CATALOG_ENABLED}

{$IFDEF CONST}
(**
 * XML_CATALOGS_NAMESPACE:
 *
 * The namespace for the XML Catalogs elements.
 *)
  XML_CATALOGS_NAMESPACE: xmlCharPtr = 'urn:oasis:names:tc:entity:xmlns:xml:catalog';

(**
 * XML_CATALOG_PI:
 *
 * The specific XML Catalog Processing Instuction name.
 *)
  XML_CATALOG_PI: xmlCharPtr = 'urn:oasis:names:tc:entity:xmlns:xml:catalog';
{$ENDIF}

{$IFDEF POINTER}
  xmlCatalogPtr = ^xmlCatalog;
{$ENDIF}

{$IFDEF TYPE}
(*
 * The API is voluntarily limited to general cataloging.
 *)
  xmlCatalogPrefer = (
    XML_CATA_PREFER_NONE = 0,
    XML_CATA_PREFER_PUBLIC = 1,
    XML_CATA_PREFER_SYSTEM
  );

  xmlCatalogAllow = (
    XML_CATA_ALLOW_NONE = 0,
    XML_CATA_ALLOW_GLOBAL = 1,
    XML_CATA_ALLOW_DOCUMENT = 2,
    XML_CATA_ALLOW_ALL = 3
  );

  xmlCatalog = record end;
{$ENDIF}

{$IFDEF FUNCTION}
(*
 * Operations on a given catalog.
 *)
function xmlNewCatalog(sgml: cint): xmlCatalogPtr; XMLCALL; XMLPUBFUN;
function xmlLoadACatalog(filename: pchar): xmlCatalogPtr; XMLCALL; XMLPUBFUN;
function xmlLoadSGMLSuperCatalog(filename: pchar): xmlCatalogPtr; XMLCALL; XMLPUBFUN;
function xmlLoadACatalog(catal: xmlCatalogPtr): cint; XMLCALL; XMLPUBFUN;
function xmlACatalogAdd(catal: xmlCatalogPtr; _type, orig, replace: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlACatalogRemove(catal: xmlCatalogPtr; value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlACatalogResolve(catal: xmlCatalogPtr; pubID, sysID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlACatalogResolveSystem(catal: xmlCatalogPtr; sysID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlACatalogResolvePublic(catal: xmlCatalogPtr; pubID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlACatalogResolveURI(catal: xmlCatalogPtr; URI: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlACatalogDump(catal: xmlCatalogPtr; _out: PFILE); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
procedure xmlFreeCatalog(catal: xmlCatalogPtr); XMLCALL; XMLPUBFUN;
function xmlCatalogIsEmpty(catal: xmlCatalogPtr): cint; XMLCALL; XMLPUBFUN;

(*
 * Global operations.
 *)
procedure xmlInitializeCatalog; XMLCALL; XMLPUBFUN;
function xmlLoadCatalog(filename: pchar): cint; XMLCALL; XMLPUBFUN;
function xmlLoadCatalogs(paths: pchar): cint; XMLCALL; XMLPUBFUN;
procedure xmlCatalogCleanup; XMLCALL; XMLPUBFUN;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlCatalogDump(_out: PFILE); XMLCALL; XMLPUBFUN;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function xmlCatalogResolve(pubID, sysID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCatalogResolveSystem(sysID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCatalogResolvePublic(pubID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCatalogResolveURI(URI: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCatalogAdd(_type, orig, replace: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlCatalogRemove(value: xmlCharPtr): cint; XMLCALL; XMLPUBFUN;
function xmlParseCatalogFile(filename: pchar): xmlDocPtr; XMLCALL; XMLPUBFUN;
function xmlCatalogConvert: cint; XMLCALL; XMLPUBFUN;

(*
 * Strictly minimal interfaces for per-document catalogs used
 * by the parser.
 *)
procedure xmlCatalogFreeLocal(catalogs: pointer); XMLCALL; XMLPUBFUN;
function xmlCatalogAddLocal(catalogs: pointer; URL: xmlCharPtr): pointer; XMLCALL; XMLPUBFUN;
function xmlCatalogLocalResolve(catalogs: pointer; pubID, sysID: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;
function xmlCatalogLocalResolveURI(catalogs: pointer; URI: xmlCharPtr): xmlCharPtr; XMLCALL; XMLPUBFUN;

(*
 * Preference settings.
 *)
function xmlCatalogSetDebug(level: cint): cint; XMLCALL; XMLPUBFUN;
function xmlCatalogSetDefaultPrefer(prefer: xmlCatalogPrefer): cint; XMLCALL; XMLPUBFUN;
procedure xmlCatalogSetDefaults(allow: xmlCatalogAllow); XMLCALL; XMLPUBFUN;
function xmlCatalogGetDefaults: xmlCatalogAllow; XMLCALL; XMLPUBFUN;
{$ENDIF}
{$ENDIF} (* LIBXML_CATALOG_ENABLED *)
