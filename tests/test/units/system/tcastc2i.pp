program testtypecastintfinfoexc;

// Test that typecast class -> interface generates the needed info for EInvalidCast.
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
  TA = Class(TObject);


var
  A : TObject;
  B : IInterface;
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
  B:=A as IInterface;
end.

