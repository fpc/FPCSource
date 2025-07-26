{$mode objfpc}
{$H+}
{$macro on}

{$ALIGN 8}
{$MINENUMSIZE 4}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.DynLibs,
  System.CTypes,
  System.Math;
{$ELSE FPC_DOTTEDUNITS}
uses
  dynlibs,
  ctypes,
  math;
{$ENDIF FPC_DOTTEDUNITS}

const
{$IF Defined(WINDOWS)}
  xml2lib = 'libxml2.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
  {$DEFINE NO_EXTERNAL_VARS}
{$ELSEIF Defined(UNIX)}
  xml2lib = 'libxml2.'+sharedsuffix;
  {$DEFINE EXTDECL := cdecl}
{$ELSE}
  {$MESSAGE ERROR 'Platform not supported right now'}
{$IFEND}


