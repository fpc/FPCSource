unit lSpawnFCGI;

{$mode objfpc}{$H+}

interface

uses
  Sockets, lNet, lCommon;

  function SpawnFCGIProcess(App, Enviro: string; const aPort: Word): Integer;

implementation

{$ifdef UNIX}
  {$i lspawnfcgiunix.inc}
{$else}
  {$i lspawnfcgiwin.inc}
{$endif}

end.

