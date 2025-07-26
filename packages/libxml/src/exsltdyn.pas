{$IFNDEF FPC_DOTTEDUNITS}
unit exsltdyn;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}
{$H+}
{$macro on}

{$ALIGN 8}
{$MINENUMSIZE 4}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Api.XML.Xml2Dyn,
  System.CTypes;
{$ELSE}
uses
  xml2dyn, ctypes;
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

{$IFNDEF NO_EXTERNAL_VARS}
{$DEFINE NO_EXTERNAL_VARS}
{$ENDIF}

{$I exslt.inc}

var
  exsltCommonRegister: procedure; EXTDECL;
{$IFDEF EXSLT_CRYPTO_ENABLED}
  exsltCryptoRegister: procedure; EXTDECL;
{$ENDIF}
  exsltMathRegister: procedure; EXTDECL;
  exsltSetsRegister: procedure; EXTDECL;
  exsltFuncRegister: procedure; EXTDECL;
  exsltStrRegister: procedure; EXTDECL;
  exsltDateRegister: procedure; EXTDECL;
  exsltSaxonRegister: procedure; EXTDECL;
  exsltDynRegister: procedure; EXTDECL;

  exsltRegisterAll: procedure; EXTDECL;

  exsltDateXpathCtxtRegister: function(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL;
  exsltMathXpathCtxtRegister: function(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL;
  exsltSetsXpathCtxtRegister: function(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL;
  exsltStrXpathCtxtRegister: function(ctxt: xmlXPathContextPtr; const prefix: xmlCharPtr): cint; EXTDECL;

var
  LibExsltHandle: TLibHandle = NilHandle;

function LoadExsltLib(ALibName: String = ''): Boolean;
procedure FreeLibExslt;

implementation

function LoadExsltLib(ALibName: String): Boolean;
begin
  if ALibName = '' then
    ALibName := exsltlib;

  LibExsltHandle := LoadLibrary(ALibName);

  if LibExsltHandle <> NilHandle then
  begin
    Pointer(exsltCommonRegister) := GetProcAddress(LibExsltHandle, 'exsltCommonRegister');
  {$IFDEF EXSLT_CRYPTO_ENABLED}
    Pointer(exsltCryptoRegister) := GetProcAddress(LibExsltHandle, 'exsltCryptoRegister');
  {$ENDIF}
    Pointer(exsltMathRegister) := GetProcAddress(LibExsltHandle, 'exsltMathRegister');
    Pointer(exsltSetsRegister) := GetProcAddress(LibExsltHandle, 'exsltSetsRegister');
    Pointer(exsltFuncRegister) := GetProcAddress(LibExsltHandle, 'exsltFuncRegister');
    Pointer(exsltStrRegister) := GetProcAddress(LibExsltHandle, 'exsltStrRegister');
    Pointer(exsltDateRegister) := GetProcAddress(LibExsltHandle, 'exsltDateRegister');
    Pointer(exsltSaxonRegister) := GetProcAddress(LibExsltHandle, 'exsltSaxonRegister');
    Pointer(exsltDynRegister) := GetProcAddress(LibExsltHandle, 'exsltDynRegister');

    Pointer(exsltRegisterAll) := GetProcAddress(LibExsltHandle, 'exsltRegisterAll');

    Pointer(exsltDateXpathCtxtRegister) := GetProcAddress(LibExsltHandle, 'exsltDateXpathCtxtRegister');
    Pointer(exsltMathXpathCtxtRegister) := GetProcAddress(LibExsltHandle, 'exsltMathXpathCtxtRegister');
    Pointer(exsltSetsXpathCtxtRegister) := GetProcAddress(LibExsltHandle, 'exsltSetsXpathCtxtRegister');
    Pointer(exsltStrXpathCtxtRegister) := GetProcAddress(LibExsltHandle, 'exsltStrXpathCtxtRegister');

    Result := True;
  end
  else
    Result := False;
end;

procedure FreeLibExslt;
begin
  if LibExsltHandle = NilHandle then
    Exit;
  FreeLibrary(LibExsltHandle);
  LibExsltHandle := NilHandle;

  exsltCommonRegister := nil;
{$IFDEF EXSLT_CRYPTO_ENABLED}
  exsltCryptoRegister := nil;
{$ENDIF}
  exsltMathRegister := nil;
  exsltSetsRegister := nil;
  exsltFuncRegister := nil;
  exsltStrRegister := nil;
  exsltDateRegister := nil;
  exsltSaxonRegister := nil;
  exsltDynRegister := nil;

  exsltRegisterAll := nil;

  exsltDateXpathCtxtRegister := nil;
  exsltMathXpathCtxtRegister := nil;
  exsltSetsXpathCtxtRegister := nil;
  exsltStrXpathCtxtRegister := nil;
end;

end.

