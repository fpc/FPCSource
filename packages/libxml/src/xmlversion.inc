(*
 * Summary: compile-time version informations
 * Description: compile-time version informations for the XML library
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)


(*
 * use those to be sure nothing nasty will happen if
 * your library and includes mismatch
 *)
procedure xmlCheckVersion(version: cint); XMLCALL; XMLPUBFUN;

(**
 * LIBXML_DOTTED_VERSION:
 *
 * the version string like "1.2.3"
 *)
const
  LIBXML_DOTTED_VERSION = '2.6.32';

(**
 * LIBXML_VERSION:
 *
 * the version number: 1.2.3 value is 10203
 *)
  LIBXML_VERSION = 20632;

(**
 * LIBXML_VERSION_STRING:
 *
 * the version number string, 1.2.3 value is "10203"
 *)
  LIBXML_VERSION_STRING = '20632';

(**
 * LIBXML_VERSION_EXTRA:
 *
 * extra version information, used to show a CVS compilation
 *)
  LIBXML_VERSION_EXTRA = '-CVS2831';

(**
 * LIBXML_TEST_VERSION:
 *
 * Macro to check that the libxml version in use is compatible with
 * the version the software has been compiled against
 *)
procedure LIBXML_TEST_VERSION;

(**
 * LIBXML_THREAD_ENABLED:
 *
 * Whether the thread support is configured in
 *)
{#if 1
#if defined(_REENTRANT) || defined(__MT__) || \
    (defined(_POSIX_C_SOURCE) && (_POSIX_C_SOURCE - 0 >= 199506L))
#define LIBXML_THREAD_ENABLED
#endif
#endif}

(**
 * LIBXML_TREE_ENABLED:
 *
 * Whether the DOM like tree manipulation API support is configured in
 *)
{$DEFINE LIBXML_TREE_ENABLED}

(**
 * LIBXML_OUTPUT_ENABLED:
 *
 * Whether the serialization/saving support is configured in
 *)
{$DEFINE LIBXML_OUTPUT_ENABLED}

(**
 * LIBXML_PUSH_ENABLED:
 *
 * Whether the push parsing interfaces are configured in
 *)
{$DEFINE LIBXML_PUSH_ENABLED}

(**
 * LIBXML_READER_ENABLED:
 *
 * Whether the xmlReader parsing interface is configured in
 *)
{$DEFINE LIBXML_READER_ENABLED}

(**
 * LIBXML_PATTERN_ENABLED:
 *
 * Whether the xmlPattern node selection interface is configured in
 *)
{$DEFINE LIBXML_PATTERN_ENABLED}

(**
 * LIBXML_WRITER_ENABLED:
 *
 * Whether the xmlWriter saving interface is configured in
 *)
{$DEFINE LIBXML_WRITER_ENABLED}

(**
 * LIBXML_SAX1_ENABLED:
 *
 * Whether the older SAX1 interface is configured in
 *)
{$DEFINE LIBXML_SAX1_ENABLED}

(**
 * LIBXML_FTP_ENABLED:
 *
 * Whether the FTP support is configured in
 *)
{$DEFINE LIBXML_FTP_ENABLED}

(**
 * LIBXML_HTTP_ENABLED:
 *
 * Whether the HTTP support is configured in
 *)
{$DEFINE LIBXML_HTTP_ENABLED}

(**
 * LIBXML_VALID_ENABLED:
 *
 * Whether the DTD validation support is configured in
 *)
{$DEFINE LIBXML_VALID_ENABLED}

(**
 * LIBXML_HTML_ENABLED:
 *
 * Whether the HTML support is configured in
 *)
{$DEFINE LIBXML_HTML_ENABLED}

(**
 * LIBXML_LEGACY_ENABLED:
 *
 * Whether the deprecated APIs are compiled in for compatibility
 *)
{$DEFINE LIBXML_LEGACY_ENABLED}

(**
 * LIBXML_C14N_ENABLED:
 *
 * Whether the Canonicalization support is configured in
 *)
{$DEFINE LIBXML_C14N_ENABLED}

(**
 * LIBXML_CATALOG_ENABLED:
 *
 * Whether the Catalog support is configured in
 *)
{$DEFINE LIBXML_CATALOG_ENABLED}

(**
 * LIBXML_DOCB_ENABLED:
 *
 * Whether the SGML Docbook support is configured in
 *)
{$DEFINE LIBXML_DOCB_ENABLED}

(**
 * LIBXML_XPATH_ENABLED:
 *
 * Whether XPath is configured in
 *)
{$DEFINE LIBXML_XPATH_ENABLED}

(**
 * LIBXML_XPTR_ENABLED:
 *
 * Whether XPointer is configured in
 *)
{$DEFINE LIBXML_XPTR_ENABLED}

(**
 * LIBXML_XINCLUDE_ENABLED:
 *
 * Whether XInclude is configured in
 *)
{$DEFINE LIBXML_XINCLUDE_ENABLED}

(**
 * LIBXML_ICONV_ENABLED:
 *
 * Whether iconv support is available
 *)
{$DEFINE LIBXML_ICONV_ENABLED}

(**
 * LIBXML_ISO8859X_ENABLED:
 *
 * Whether ISO-8859-* support is made available in case iconv is not
 *)
{$DEFINE LIBXML_ISO8859X_ENABLED}

(**
 * LIBXML_DEBUG_ENABLED:
 *
 * Whether Debugging module is configured in
 *)
{$DEFINE LIBXML_DEBUG_ENABLED}

(**
 * DEBUG_MEMORY_LOCATION:
 *
 * Whether the memory debugging is configured in
 *)
{$DEFINE DEBUG_MEMORY_LOCATION}

(**
 * LIBXML_DEBUG_RUNTIME:
 *
 * Whether the runtime debugging is configured in
 *)
{$DEFINE LIBXML_DEBUG_RUNTIME}

(**
 * LIBXML_UNICODE_ENABLED:
 *
 * Whether the Unicode related interfaces are compiled in
 *)
{$DEFINE LIBXML_UNICODE_ENABLED}

(**
 * LIBXML_REGEXP_ENABLED:
 *
 * Whether the regular expressions interfaces are compiled in
 *)
{$DEFINE LIBXML_REGEXP_ENABLED}

(**
 * LIBXML_AUTOMATA_ENABLED:
 *
 * Whether the automata interfaces are compiled in
 *)
{$DEFINE LIBXML_AUTOMATA_ENABLED}

(**
 * LIBXML_EXPR_ENABLED:
 *
 * Whether the formal expressions interfaces are compiled in
 *)
{$DEFINE LIBXML_EXPR_ENABLED}

(**
 * LIBXML_SCHEMAS_ENABLED:
 *
 * Whether the Schemas validation interfaces are compiled in
 *)
{$DEFINE LIBXML_SCHEMAS_ENABLED}

(**
 * LIBXML_SCHEMATRON_ENABLED:
 *
 * Whether the Schematron validation interfaces are compiled in
 *)
{$DEFINE LIBXML_SCHEMATRON_ENABLED}

(**
 * LIBXML_MODULES_ENABLED:
 *
 * Whether the module interfaces are compiled in
 *)
{$DEFINE LIBXML_MODULES_ENABLED}
(**
 * LIBXML_MODULE_EXTENSION:
 *
 * the string suffix used by dynamic modules (usually shared libraries)
 *)
const
  LIBXML_MODULE_EXTENSION = '.so';

(**
 * LIBXML_ZLIB_ENABLED:
 *
 * Whether the Zlib support is compiled in
 *)
{$DEFINE LIBXML_ZLIB_ENABLED}
