program testtypecastintfintfinfoexc;

// Test that typecast interface -> interface generates the needed info for EInvalidCast.
{$mode objfpc}

procedure geterrinfo;
var
  aFrom,aTo : shortstring;

begin
  if ExitCode=219 then
    TObject.GetLastCastErrorInfo(aFrom,aTo);
  Writeln('Got typecast ',aFrom,' to ',aTo,' error');
  if (aFrom<>'') or (aTo<>'') then
    ExitCode:=0;
end;

Type
  TA = Class(TInterfacedObject);
  TB = Class(TObject);
  IB = interface ['{3A4B75D4-76E6-4856-A3F1-0B10454763F6}']
  end;

var
  A : IInterface;
  B : IB;
  F,T : ShortString;

begin
  AddExitProc(@geterrinfo);
  TObject.GetLastCastErrorInfo(F,T);
  if (F<>'') or (T<>'') then
    begin
    Writeln('Error, cast info must be empty at start');
    Halt(1);
    end;
  A:=TA.Create;
  B:=A as IB;
end.

