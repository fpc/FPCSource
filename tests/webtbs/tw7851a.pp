{ %cpu=powerpc,powerpc64 }

program AsmCrash;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

uses
  sysutils;

type
  TMyRecord = record
    rField: Integer;
  end;

  TMyObject = class
  private
    oField: TMyRecord;
  public
    procedure Test;
    procedure Test2;
  end;

{ TMyObject }

var
  l: cardinal;

procedure TMyObject.Test;
var
  retaddr: pointer;
asm
  mflr r0
{$ifndef cpu64}
  stw r0, retaddr
  bl TMyObject.Test2
  lwz r0, retaddr
{$else}
  std r0, retaddr
{$ifdef linux}
  bl .TMyObject.Test2
{$else linux}
  bl TMyObject.Test2
{$endif linux}
  ld r0, retaddr
{$endif}
  mtlr r0
end;

procedure TMyObject.Test2;
begin
  l := $cafebabe;
end;

begin
  with TMyObject.Create do try
    Test;
    if l <> $cafebabe then
      halt(1);
    l := $deadbeef;
  finally
    Free;
  end;
  if l <> $deadbeef then
    halt(2);
end.
