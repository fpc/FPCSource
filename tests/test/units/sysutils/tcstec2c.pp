program testtypecastinfoexc;

{$mode objfpc}
// Test that EInvalidCast picks up the typecast error info.
 
uses SysUtils;

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
  A:=TA.Create;
  try
  B:=A as TB;
  except
    On E : EInvalidCast do
      begin
        if not (Pos('TB',E.Message) > Pos('TA',E.Message)) then
          Halt(1)
        else
          Writeln('TA and TB in error: ',E.Message);
      end
  else
    Halt()
  end;
end.

