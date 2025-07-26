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
  XML_CATALOG_PI: xmlCharPtr = 'oasis-xml-catalog';
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
function xmlLoadACatalog(filename: PAnsiChar): xmlCatalogPtr; EXTDECL; external xml2lib;
function xmlLoadSGMLSuperCatalog(filename: PAnsiChar): xmlCatalogPtr; EXTDECL; external xml2lib;
function xmlConvertSGMLCatalog(catal: xmlCatalogPtr): cint; EXTDECL; external xml2lib;
//function xmlLoadACatalog(catal: xmlCatalogPtr): cint; EXTDECL; external xml2lib;
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
function xmlLoadCatalog(filename: PAnsiChar): cint; EXTDECL; external xml2lib;
function xmlLoadCatalogs(paths: PAnsiChar): cint; EXTDECL; external xml2lib;
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
function xmlParseCatalogFile(filename: PAnsiChar): xmlDocPtr; EXTDECL; external xml2lib;
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
{$IFDEF FUNCTIONVAR}
(*
 * Operations on a given catalog.
 *)
  xmlNewCatalog: function(sgml: cint): xmlCatalogPtr; EXTDECL;
  xmlLoadACatalog: function(filename: PAnsiChar): xmlCatalogPtr; EXTDECL;
  xmlLoadSGMLSuperCatalog: function(filename: PAnsiChar): xmlCatalogPtr; EXTDECL;
  xmlConvertSGMLCatalog: function(catal: xmlCatalogPtr): cint; EXTDECL;
  //xmlLoadACatalog: function(catal: xmlCatalogPtr): cint; EXTDECL;
  xmlACatalogAdd: function(catal: xmlCatalogPtr; _type, orig, replace: xmlCharPtr): cint; EXTDECL;
  xmlACatalogRemove: function(catal: xmlCatalogPtr; value: xmlCharPtr): cint; EXTDECL;
  xmlACatalogResolve: function(catal: xmlCatalogPtr; pubID, sysID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlACatalogResolveSystem: function(catal: xmlCatalogPtr; sysID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlACatalogResolvePublic: function(catal: xmlCatalogPtr; pubID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlACatalogResolveURI: function(catal: xmlCatalogPtr; URI: xmlCharPtr): xmlCharPtr; EXTDECL;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlACatalogDump: procedure(catal: xmlCatalogPtr; _out: PFILE); EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlFreeCatalog: procedure(catal: xmlCatalogPtr); EXTDECL;
  xmlCatalogIsEmpty: function(catal: xmlCatalogPtr): cint; EXTDECL;

(*
 * Global operations.
 *)
  xmlInitializeCatalog: procedure; EXTDECL;
  xmlLoadCatalog: function(filename: PAnsiChar): cint; EXTDECL;
  xmlLoadCatalogs: function(paths: PAnsiChar): cint; EXTDECL;
  xmlCatalogCleanup: procedure; EXTDECL;
{$IFDEF LIBXML_OUTPUT_ENABLED}
  xmlCatalogDump: procedure(_out: PFILE); EXTDECL;
{$ENDIF} (* LIBXML_OUTPUT_ENABLED *)
  xmlCatalogResolve: function(pubID, sysID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlCatalogResolveSystem: function(sysID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlCatalogResolvePublic: function (pubID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlCatalogResolveURI: function(URI: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlCatalogAdd: function(_type, orig, replace: xmlCharPtr): cint; EXTDECL;
  xmlCatalogRemove: function(value: xmlCharPtr): cint; EXTDECL;
  xmlParseCatalogFile: function(filename: PAnsiChar): xmlDocPtr; EXTDECL;
  xmlCatalogConvert: function: cint; EXTDECL;

(*
 * Strictly minimal interfaces for per-document catalogs used
 * by the parser.
 *)
  xmlCatalogFreeLocal: procedure(catalogs: pointer); EXTDECL;
  xmlCatalogAddLocal: function(catalogs: pointer; URL: xmlCharPtr): pointer; EXTDECL;
  xmlCatalogLocalResolve: function(catalogs: pointer; pubID, sysID: xmlCharPtr): xmlCharPtr; EXTDECL;
  xmlCatalogLocalResolveURI: function(catalogs: pointer; URI: xmlCharPtr): xmlCharPtr; EXTDECL;

(*
 * Preference settings.
 *)
  xmlCatalogSetDebug: function(level: cint): cint; EXTDECL;
  xmlCatalogSetDefaultPrefer: function(prefer: xmlCatalogPrefer): cint; EXTDECL;
  xmlCatalogSetDefaults: procedure(allow: xmlCatalogAllow); EXTDECL;
  xmlCatalogGetDefaults: function: xmlCatalogAllow; EXTDECL;
{$ENDIF}

{$ENDIF} (* LIBXML_CATALOG_ENABLED *)
