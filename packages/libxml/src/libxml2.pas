unit libxml2;

{$mode objfpc}
{$macro on}

{$ALIGN 8}
{$MINENUMSIZE 4}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  libxml2lib = 'libxml2.dll';
{$ELSEIF Defined(UNIX)}
  libxml2lib = 'libxml2.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB xml2}
{$ENDIF}

{$i xmlexports.inc}
{$i xmlversion.inc}

type
  iconv_t = pointer;
  PFILE = pointer;
  va_list = pointer;
  size_t = {$IF Sizeof(pointer) = 8}qword{$ELSE}longword{$IFEND};


  xmlNodeSetPtr = ^xmlNodeSet;
  xmlNodeSet = record end;

(*
  include pointers (forwarding)
*)
{$DEFINE POINTER}
  {$i dict.inc}
  {$i encoding.inc}
  {$i tree.inc}
  {$i list.inc}
  {$i entities.inc}
  {$i xmlerror.inc}
  {$i xmlmemory.inc}
  {$i hash.inc}
  {$i pattern.inc}
  {$i schemasInternals.inc}
  {$i valid.inc}
  {$i parser.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i xmlautomata.inc}
  {$i xmlIO.inc}
  {$i xmlmodule.inc}
  {$i xmlreader.inc}
  {$i xmlregexp.inc}
  {$i xmlsave.inc}
  {$i xmlschemas.inc}
  {$i xmlschemastypes.inc}
  {$i xmlstring.inc}
  {$i xmlunicode.inc}
  {$i xmlwriter.inc}
  {.$i xpath.inc}
  {$i c14n.inc}
{$UNDEF POINTER}

(*
  include types
*)
{$DEFINE TYPE}
  {$i dict.inc}
  {$i encoding.inc}
  {$i tree.inc}
  {$i list.inc}
  {$i entities.inc}
  {$i xmlerror.inc}
  {$i xmlmemory.inc}
  {$i hash.inc}
  {$i pattern.inc}
  {$i schemasInternals.inc}
  {$i valid.inc}
  {$i parser.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i xmlautomata.inc}
  {$i xmlIO.inc}
  {$i xmlmodule.inc}
  {$i xmlreader.inc}
  {$i xmlregexp.inc}
  {$i xmlsave.inc}
  {$i xmlschemas.inc}
  {$i xmlschemastypes.inc}
  {$i xmlstring.inc}
  {$i xmlunicode.inc}
  {$i xmlwriter.inc}
  {.$i xpath.inc}
  {$i c14n.inc}
{$UNDEF TYPE}

(*
  include constants
*)
{$DEFINE CONST}
const
  {$i dict.inc}
  {$i encoding.inc}
  {$i tree.inc}
  {$i list.inc}
  {$i entities.inc}
  {$i xmlerror.inc}
  {$i xmlmemory.inc}
  {$i pattern.inc}
  {$i schemasInternals.inc}
  {$i hash.inc}
  {$i valid.inc}
  {$i parser.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i xmlautomata.inc}
  {$i xmlIO.inc}
  {$i xmlmodule.inc}
  {$i xmlreader.inc}
  {$i xmlregexp.inc}
  {$i xmlsave.inc}
  {$i xmlschemas.inc}
  {$i xmlschemastypes.inc}
  {$i xmlstring.inc}
  {$i xmlunicode.inc}
  {$i xmlwriter.inc}
  {.$i xpath.inc}
  {$i c14n.inc}
{$UNDEF CONST}

(*
  include functions
*)
{$DEFINE FUNCTION}
  {$i dict.inc}
  {$i encoding.inc}
  {$i tree.inc}
  {$i list.inc}
  {$i entities.inc}
  {$i xmlerror.inc}
  {$i xmlmemory.inc}
  {$i pattern.inc}
  {$i schemasInternals.inc}
  {$i hash.inc}
  {$i valid.inc}
  {$i parser.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i xmlautomata.inc}
  {$i xmlIO.inc}
  {$i xmlmodule.inc}
  {$i xmlreader.inc}
  {$i xmlregexp.inc}
  {$i xmlsave.inc}
  {$i xmlschemas.inc}
  {$i xmlschemastypes.inc}
  {$i xmlstring.inc}
  {$i xmlunicode.inc}
  {$i xmlwriter.inc}
  {.$i xpath.inc}
  {$i c14n.inc}
{$UNDEF FUNCTION}

implementation

procedure fpcxmlFree(mem: pointer); XMLCALL;
begin
  FreeMem(mem);
end;

function fpcxmlMalloc(size: size_t): pointer; XMLCALL;
begin
  GetMem(Result, size);
end;

function fpcxmlRealloc(mem: pointer; size: size_t): pointer; XMLCALL;
begin
  Result := mem;
  ReallocMem(Result, size);
end;

procedure fpcxmlGenericErrorHandler(ctx: pointer; msg: pchar; args: array of const); XMLCDECL;
begin
  writeln(msg);
end;

procedure fpcxmlStructuredErrorHandler(userData: pointer; error: xmlErrorPtr); XMLCALL;
begin
  writeln('struct error');
end;


initialization
(*
 * this initialize the library and check potential ABI mismatches
 * between the version it was compiled for and the actual shared
 * library used.
 *)
  LIBXML_TEST_VERSION;

(*
 * overloading the memory functions
 *)
  xmlMemSetup(@fpcxmlFree, @fpcxmlMalloc, @fpcxmlRealloc, nil);

(*
 * overloading the error functions
 *)
  xmlSetGenericErrorFunc(nil, @fpcxmlGenericErrorHandler);
  xmlSetStructuredErrorFunc(nil, @fpcxmlStructuredErrorHandler);

finalization
(*
 * Cleanup function for the XML library.
 *)
  xmlCleanupParser();

(*
 * this is to debug memory for regression tests
 *)
  xmlMemoryDump();

end.
