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
  __XML_CATALOGS_NAMESPACE: String = 'urn:oasis:names:tc:entity:xmlns:xml:catalog';
  XML_CATALOGS_NAMESPACE: xmlCharPtr = @__XML_CATALOGS_NAMESPACE[1];

(**
 * XML_CATALOG_PI:
 *
 * The specific XML Catalog Processing Instuction name.
 *)
  __XML_CATALOG_PI: String = 'urn:oasis:names:tc:entity:xmlns:xml:catalog';
  XML_CATALOG_PI: xmlCharPtr = @__XML_CATALOG_PI[1];
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
function xmlNewCatalog(sgml: cint): xmlCatalogPtr; EXTDECL; external xml2lib;
function xmlLoadACatalog(filename: pchar): xmlCatalogPtr; EXTDECL; external xml2lib;
function xmlLoadSGMLSuperCatalog(filename: pchar): xmlCatalogPtr; EXTDECL; external xml2lib;
function xmlLoadACatalog(catal: xmlCatalogPtr): cint; EXTDECL; external xml2lib;
function xmlACatalogAdd(catal: xmlCatalogPtr; _type, orig, replace: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlACatalogRemove(catal: xmlCatalogPtr; value: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlACatalogResolve(catal: xmlCatalogPtr; pubID, sysID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlACatalogResolveSystem(catal: xmlCatalogPtr; sysID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlACatalogResolvePublic(catal: xmlCatalogPtr; pubID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlACatalogResolveURI(catal: xmlCatalogPtr; URI: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlACatalogDump(catal: xmlCatalogPtr; _out: PFILE); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
procedure xmlFreeCatalog(catal: xmlCatalogPtr); EXTDECL; external xml2lib;
function xmlCatalogIsEmpty(catal: xmlCatalogPtr): cint; EXTDECL; external xml2lib;

(*
 * Global operations.
 *)
procedure xmlInitializeCatalog; EXTDECL; external xml2lib;
function xmlLoadCatalog(filename: pchar): cint; EXTDECL; external xml2lib;
function xmlLoadCatalogs(paths: pchar): cint; EXTDECL; external xml2lib;
procedure xmlCatalogCleanup; EXTDECL; external xml2lib;
{$IFDEF LIBXML_OUTPUT_ENABLED}
procedure xmlCatalogDump(_out: PFILE); EXTDECL; external xml2lib;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
function xmlCatalogResolve(pubID, sysID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCatalogResolveSystem(sysID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCatalogResolvePublic(pubID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCatalogResolveURI(URI: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCatalogAdd(_type, orig, replace: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlCatalogRemove(value: xmlCharPtr): cint; EXTDECL; external xml2lib;
function xmlParseCatalogFile(filename: pchar): xmlDocPtr; EXTDECL; external xml2lib;
function xmlCatalogConvert: cint; EXTDECL; external xml2lib;

(*
 * Strictly minimal interfaces for per-document catalogs used
 * by the parser.
 *)
procedure xmlCatalogFreeLocal(catalogs: pointer); EXTDECL; external xml2lib;
function xmlCatalogAddLocal(catalogs: pointer; URL: xmlCharPtr): pointer; EXTDECL; external xml2lib;
function xmlCatalogLocalResolve(catalogs: pointer; pubID, sysID: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;
function xmlCatalogLocalResolveURI(catalogs: pointer; URI: xmlCharPtr): xmlCharPtr; EXTDECL; external xml2lib;

(*
 * Preference settings.
 *)
function xmlCatalogSetDebug(level: cint): cint; EXTDECL; external xml2lib;
function xmlCatalogSetDefaultPrefer(prefer: xmlCatalogPrefer): cint; EXTDECL; external xml2lib;
procedure xmlCatalogSetDefaults(allow: xmlCatalogAllow); EXTDECL; external xml2lib;
function xmlCatalogGetDefaults: xmlCatalogAllow; EXTDECL; external xml2lib;
{$ENDIF}
{$ENDIF} (* LIBXML_CATALOG_ENABLED *)
