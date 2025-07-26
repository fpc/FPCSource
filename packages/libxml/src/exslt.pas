{$IFNDEF FPC_DOTTEDUNITS}
unit exslt;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}
{$H+}
{$macro on}

{$ALIGN 8}
{$MINENUMSIZE 4}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.XML.Xml2,
  System.CTypes;
{$ELSE}
uses
  xml2, ctypes;
{$ENDIF}

const
{$IF Defined(WINDOWS)}
  exsltlib = 'libexslt.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
  {$DEFINE NO_EXTERNAL_VARS}
{$ELSEIF Defined(UNIX)}
  exsltlib = 'libexslt.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
{$ELSE}
  {$MESSAGE ERROR 'Platform not supported right now'}
{$IFEND}

{$I exslt.inc}

procedure exsltCommonRegister; EXTDECL; external exsltlib;
{$IFDEF EXSLT_CRYPTO_ENABLED}
procedure exsltCryptoRegister; EXTDECL; external exsltlib;
{$ENDIF}
procedure exsltMathRegister; EXTDECL; external exsltlib;
procedure exsltSetsRegister; EXTDECL; external exsltlib;
procedure exsltFuncRegister; EXTDECL; external exsltlib;
procedure exsltStrRegister; EXTDECL; external exsltlib;
procedure exsltDateRegister; EXTDECL; external exsltlib;
procedure exsltSaxonRegister; EXTDECL; external exsltlib;
procedure exsltDynRegister; EXTDECL; external exsltlib;

procedure exsltRegisterAll; EXTDECL; external exsltlib;

function exsltDateXpathCtxtRegister(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL; external exsltlib;
function exsltMathXpathCtxtRegister(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL; external exsltlib;
function exsltSetsXpathCtxtRegister(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL; external exsltlib;
function exsltStrXpathCtxtRegister(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL; external exsltlib;

implementation

end.

