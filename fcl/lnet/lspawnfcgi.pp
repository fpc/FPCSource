unit lSpawnFCGI;

{$mode objfpc}{$H+}

interface

uses
  Sockets, lNet, lCommon;

  function SpawnFCGIProcess(App, Enviro: string; const aPort: Word): Integer;

implementation

{$ifdef WINDOWS}
  {$i lspawnfcgiwin.inc}
{$else}
  {$i lspawnfcgiunix.inc}
{$endif}

end.

