program testtypecastinfo;

// Test that typecast info is available when error 219 is encountered.

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
  TB = Class(TObject);

var
  A : TObject;
  B : TB;
  F,T : ShortString;

begin
  TObject.GetLastCastErrorInfo(F,T);
  if (F<>'') or (T<>'') then
    begin
    Writeln('Error, cast info must be empty at start');
    Halt(1);
    end;
  AddExitProc(@geterrinfo);
  A:=TA.Create;
  B:=A as TB;
end.

