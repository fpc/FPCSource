{$IFNDEF NO_EXTERNAL_VARS}
var
  exsltLibraryVersion: PAnsiChar; external exsltlib;
  exsltLibexsltVersion: cint; external exsltlib;
  exsltLibxsltVersion: cint; external exsltlib;
  exsltLibxmlVersion: cint; external exsltlib;
{$ENDIF}

const
  (**
   * EXSLT_COMMON_NAMESPACE:
   *
   * Namespace for EXSLT common functions
   *)
  EXSLT_COMMON_NAMESPACE = 'http://exslt.org/common';
  (**
   * EXSLT_CRYPTO_NAMESPACE:
   *
   * Namespace for EXSLT crypto functions
   *)
  EXSLT_CRYPTO_NAMESPACE = 'http://exslt.org/crypto';
  (**
   * EXSLT_MATH_NAMESPACE:
   *
   * Namespace for EXSLT math functions
   *)
  EXSLT_MATH_NAMESPACE = 'http://exslt.org/math';
  (**
   * EXSLT_SETS_NAMESPACE:
   *
   * Namespace for EXSLT set functions
   *)
  EXSLT_SETS_NAMESPACE = 'http://exslt.org/sets';
  (**
   * EXSLT_FUNCTIONS_NAMESPACE:
   *
   * Namespace for EXSLT functions extension functions
   *)
  EXSLT_FUNCTIONS_NAMESPACE = 'http://exslt.org/functions';
  (**
   * EXSLT_STRINGS_NAMESPACE:
   *
   * Namespace for EXSLT strings functions
   *)
  EXSLT_STRINGS_NAMESPACE = 'http://exslt.org/strings';
  (**
   * EXSLT_DATE_NAMESPACE:
   *
   * Namespace for EXSLT date functions
   *)
  EXSLT_DATE_NAMESPACE = 'http://exslt.org/dates-and-times';
  (**
   * EXSLT_DYNAMIC_NAMESPACE:
   *
   * Namespace for EXSLT dynamic functions
   *)
  EXSLT_DYNAMIC_NAMESPACE = 'http://exslt.org/dynamic';

  (**
   * SAXON_NAMESPACE:
   *
   * Namespace for SAXON extensions functions
   *)
  SAXON_NAMESPACE = 'http://icl.com/saxon';
