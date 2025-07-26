(*
 * Summary: Interfaces, constants and types related to the XSLT engine
 * Description: Interfaces, constants and types related to the XSLT engine
 *
 * Copy: See Copyright for the status of this software.
 *
 * Author: Daniel Veillard
 *)

{$IFDEF CONST}
  (**
   * XSLT_DEFAULT_VERSION:
   *
   * The default version of XSLT supported.
   *)
  XSLT_DEFAULT_VERSION = '1.0';

  (**
   * XSLT_DEFAULT_VENDOR:
   *
   * The XSLT "vendor" string for this processor.
   *)
  XSLT_DEFAULT_VENDOR = 'libxslt';

  (**
   * XSLT_DEFAULT_URL:
   *
   * The XSLT "vendor" URL for this processor.
   *)
  XSLT_DEFAULT_URL = 'http://xmlsoft.org/XSLT/';

  (**
   * XSLT_NAMESPACE:
   *
   * The XSLT specification namespace.
   *)
  XSLT_NAMESPACE = 'http://www.w3.org/1999/XSL/Transform';

  (**
   * XSLT_PARSE_OPTIONS:
   *
   * The set of options to pass to an xmlReadxxx when loading files for
   * XSLT consumption.
   *)
  XSLT_PARSE_OPTIONS = XML_PARSE_NOENT or XML_PARSE_DTDLOAD or XML_PARSE_DTDATTR or XML_PARSE_NOCDATA;

{$ENDIF} {CONST}

{$IFDEF EXTVAR}

  (**
   * xsltMaxDepth:
   *
   * This value is used to detect templates loops.
   *)
var
  xsltMaxDepth: cint; external xsltlib;

  (**
   *  * xsltMaxVars:
   *   *
   *    * This value is used to detect templates loops.
   *     *)
  xsltMaxVars: cint; external xsltlib;

  (**
   * xsltEngineVersion:
   *
   * The version string for libxslt.
   *)
  xsltEngineVersion: PAnsiChar; external xsltlib;

  (**
   * xsltLibxsltVersion:
   *
   * The version of libxslt compiled.
   *)
  xsltLibxsltVersion: cint; external xsltlib;

  (**
   * xsltLibxmlVersion:
   *
   * The version of libxml libxslt was compiled against.
   *)
  xsltLibxmlVersion: cint; external xsltlib;

{$ENDIF}

{$IFDEF FUNCTION}

(*
 * Global initialization function.
 *)

procedure xsltInit; EXTDECL; external xsltlib;

(*
 * Global cleanup function.
 *)
procedure xsltCleanupGlobals; EXTDECL; external xsltlib;

{$ENDIF} {FUNCTION}

{$IFDEF FUNCTIONVAR}

(*
 * Global initialization function.
 *)

  xsltInit: procedure; EXTDECL;

(*
 * Global cleanup function.
 *)
  xsltCleanupGlobals: procedure; EXTDECL;

{$ENDIF} {FUNCTIONVAR}
