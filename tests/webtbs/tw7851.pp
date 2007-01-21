{ %cpu=i386,x86_64 }

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
asm
  call TMyObject.Test2
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
