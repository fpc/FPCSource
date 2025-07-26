{$IFNDEF FPC_DOTTEDUNITS}
unit xslt;
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
  System.CTypes
  {$IFDEF WINDOWS}
  , WinApi.Windows
  {$ENDIF}
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  xml2, ctypes
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  ;
{$ENDIF}

{.$DEFINE XSLT_REFACTORED}
{$DEFINE HAVE_STRXFRM_L}

const
{$IF Defined(WINDOWS)}
  xsltlib = 'libxslt.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
  {$DEFINE NO_EXTERNAL_VARS}
{$ELSEIF Defined(UNIX)}
  xsltlib = 'libxslt.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
{$ELSE}
  {$MESSAGE ERROR 'Platform not supported right now'}
{$IFEND}

type
{$DEFINE POINTER}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$UNDEF POINTER}

{$DEFINE TYPE}
{$I xsltpattern.inc}
{$I numbersInternals.inc}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$I documents.inc}
{$I extensions.inc}
{$I security.inc}
{$I xsltutils.inc}
{$UNDEF TYPE}

const
{$DEFINE CONST}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$I documents.inc}
{$I extra.inc}
{$I keys.inc}
{$I namespaces.inc}
{$I variables.inc}
{$I xslt.inc}
{$I xsltconfig.inc}
{$I xsltutils.inc}
{$UNDEF CONST}


{$DEFINE FUNCTION}
{$I xsltpattern.inc}
{$I xsltlocale.inc}
{$I xsltInternals.inc}
{$I attributes.inc}
{$I documents.inc}
{$I extensions.inc}
{$I extra.inc}
{$I functions.inc}
{$I imports.inc}
{$I keys.inc}
{$I namespaces.inc}
{$I preproc.inc}
{$I security.inc}
{$I templates.inc}
{$I transform.inc}
{$I variables.inc}
{$I xslt.inc}
{$I xsltutils.inc}
{$UNDEF FUNCTION}

{$IFNDEF NO_EXTERNAL_VARS}
{$DEFINE EXTVAR}
{$I documents.inc}
{$I preproc.inc}
{$I xslt.inc}
{$I xsltutils.inc}
{$UNDEF EXTVAR}
{$ENDIF}

implementation

end.

