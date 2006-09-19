unit openal;

{$mode objfpc}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
  openallib = 'openal32.dll';
{$ELSE}
  {$LINKLIB openal}
{$ENDIF}

{$include alh.inc}
{$include alch.inc}
{$include alexth.inc}

implementation

end.