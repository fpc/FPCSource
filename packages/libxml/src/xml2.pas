{
  Translation of the libxml2 headers for FreePascal
  Copyright (C) 2008 by Ivo Steinmann
}

{$IFNDEF FPC_DOTTEDUNITS}
unit xml2;
{$ENDIF FPC_DOTTEDUNITS}

{$i xml2h.inc}

{$i xml2.inc}

implementation

{$i fpcfunctions.inc}

{$i macros.inc}

{$IFDEF NO_EXTERNAL_VARS}
procedure LoadExternalVariables;
var
  libHandle: THandle;
begin
  libHandle := LoadLibrary(xml2lib);
  if libHandle <> 0 then
  begin
  { xmlregexp.inc }
   {__emptyExp := xmlExpNodePtrPtr(GetProcAddress(libHandle, 'emptyExp'));
    __forbiddenExp := xmlExpNodePtrPtr(GetProcAddress(libHandle, 'forbiddenExp'));}

  { paserInternals.inc }
    //__xmlParserMaxDepth := PCardinal(GetProcAddress(libHandle, 'xmlParserMaxDepth'));
   
  {  }
   {xmlStringComment := PAnsiChar(GetProcAddress(libHandle, 'xmlStringComment'));
    xmlStringText := PAnsiChar(GetProcAddress(libHandle, 'xmlStringText'));
    xmlStringTextNoenc := PAnsiChar(GetProcAddress(libHandle, 'xmlStringTextNoenc'));}

  { chvalid.inc }
    __xmlIsBaseCharGroup := xmlChRangeGroupPtr(GetProcAddress(libHandle, 'xmlIsBaseCharGroup'));
    __xmlIsCharGroup := xmlChRangeGroupPtr(GetProcAddress(libHandle, 'xmlIsCharGroup'));
    __xmlIsCombiningGroup := xmlChRangeGroupPtr(GetProcAddress(libHandle, 'xmlIsCombiningGroup'));
    __xmlIsDigitGroup := xmlChRangeGroupPtr(GetProcAddress(libHandle, 'xmlIsDigitGroup'));
    __xmlIsExtenderGroup := xmlChRangeGroupPtr(GetProcAddress(libHandle, 'xmlIsExtenderGroup'));
    __xmlIsIdeographicGroup := xmlChRangeGroupPtr(GetProcAddress(libHandle, 'xmlIsIdeographicGroup'));
    __xmlIsPubidChar_tab := GetProcAddress(libHandle, 'xmlIsPubidChar_tab');
    
  { globals.inc }
    varxmlMalloc := PxmlMallocFunc(GetProcAddress(libHandle, 'xmlMalloc'));
    varxmlMallocAtomic := PxmlMallocFunc(GetProcAddress(libHandle, 'xmlMallocAtomic'));
    varxmlRealloc := PxmlReallocFunc(GetProcAddress(libHandle, 'xmlRealloc'));
    varxmlFree := PxmlFreeFunc(GetProcAddress(libHandle, 'xmlFree'));
    varxmlMemStrdup := PxmlStrdupFunc(GetProcAddress(libHandle, 'xmlMemStrdup'));
    
  { xpath.inc }
   {__xmlXPathNAN := PDouble(GetProcAddress(libHandle, 'xmlXPathNAN'));
    __xmlXPathNINF := PDouble(GetProcAddress(libHandle, 'xmlXPathNINF'));
    __xmlXPathPINF := PDouble(GetProcAddress(libHandle, 'xmlXPathPINF'));}
    
    FreeLibrary(libHandle);
  end;
end;
{$ENDIF}

var
  mask : TFPUExceptionMask;

initialization
{$IFDEF NO_EXTERNAL_VARS}
  LoadExternalVariables;
{$ENDIF}

(*
 * overloading the memory functions
 *)
  xmlMemSetup(@fpcxmlFree, @fpcxmlMalloc, @fpcxmlRealloc, @fpcxmlStrdup);

(*
 * this initialize the library and check potential ABI mismatches
 * between the version it was compiled for and the actual shared
 * library used.
 *)
  mask:=GetExceptionMask;
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);

  LIBXML_TEST_VERSION;

  SetExceptionMask(mask);
(*
 * overloading the error functions
 *)
  //xmlSetGenericErrorFunc(nil, @fpcxmlGenericErrorHandler);
  //xmlSetStructuredErrorFunc(nil, @fpcxmlStructuredErrorHandler);

finalization
(*
 * Cleanup function for the XML library.
 *)
  xmlCleanupParser();

(*
 * this is to debug memory for regression tests
 *)
  //xmlMemoryDump();

end.
