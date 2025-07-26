{$DEFINE DEFINES}
  {$i xmlversion.inc}
{$UNDEF DEFINES}

type
  PFILE = pointer;
  va_list = pointer;
  iconv_t = pointer;

(*
  include pointers (forwarding)
*)
{$DEFINE POINTER}
  {$i catalog.inc}
  {$i chvalid.inc}
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
  {$i libxmlparser.inc}
  {$i parserInternals.inc}
  {$i schematron.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i nanoftp.inc}
  {$i nanohttp.inc}
  {$i SAX.inc}
  {$i SAX2.inc}
  {$i HTMLtree.inc}
  {$i HTMLparser.inc}
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
  {$i c14n.inc}
  {$i xpath.inc}
  {$i xpathInternals.inc}
  {$i xlink.inc}
  {$i xinclude.inc}
  {$i xpointer.inc}
{$UNDEF POINTER}

(*
  include types
*)
{$DEFINE TYPE}
  {$i catalog.inc}
  {$i chvalid.inc}
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
  {$i libxmlparser.inc}
  {$i parserInternals.inc}
  {$i schematron.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i nanoftp.inc}
  {$i nanohttp.inc}
  {$i SAX.inc}
  {$i SAX2.inc}
  {$i HTMLtree.inc}
  {$i HTMLparser.inc}
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
  {$i c14n.inc}
  {$i xpath.inc}
  {$i xpathInternals.inc}
  {$i xlink.inc}
  {$i xinclude.inc}
  {$i xpointer.inc}
{$UNDEF TYPE}

(*
  include constants
*)
{$DEFINE CONST}
const
  {$i xmlversion.inc}
  {$i catalog.inc}
  {$i chvalid.inc}
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
  {$i libxmlparser.inc}
  {$i parserInternals.inc}
  {$i schematron.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i nanoftp.inc}
  {$i nanohttp.inc}
  {$i SAX.inc}
  {$i SAX2.inc}
  {$i HTMLtree.inc}
  {$i HTMLparser.inc}
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
  {$i c14n.inc}
  {$i xpath.inc}
  {$i xpathInternals.inc}
  {$i xlink.inc}
  {$i xinclude.inc}
  {$i xpointer.inc}
{$UNDEF CONST}

(*
  include functions
*)
{$IFDEF LIBXML_DYN}
{$DEFINE FUNCTIONVAR}
var
{$ELSE}
{$DEFINE FUNCTION}
{$ENDIF}
  {$i xmlversion.inc}
  {$i catalog.inc}
  {$i chvalid.inc}
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
  {$i libxmlparser.inc}
  {$i parserInternals.inc}
  {$i schematron.inc}
  {$i threads.inc}
  {$i uri.inc}
  {$i relaxng.inc}
  {$i globals.inc}
  {$i nanoftp.inc}
  {$i nanohttp.inc}
  {$i SAX.inc}
  {$i SAX2.inc}
  {$i HTMLtree.inc}
  {$i HTMLparser.inc}
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
  {$i c14n.inc}
  {$i xpath.inc}
  {$i xpathInternals.inc}
  {$i xlink.inc}
  {$i xinclude.inc}
  {$i xpointer.inc}
{$IFDEF LIBXML_DYN}
{$UNDEF FUNCTIONVAR}
{$ELSE}
{$UNDEF FUNCTION}
{$ENDIF}

{$DEFINE MACRO}
  {$i xmlversion.inc}
  {$i xpath.inc}
  {$i HTMLparser.inc}
{$UNDEF MACRO}

{$DEFINE EXTVAR}
  {$i chvalid.inc}
  {$i globals.inc}
  {$i xmlregexp.inc}
  {$i xpath.inc}
{$UNDEF EXTVAR}
