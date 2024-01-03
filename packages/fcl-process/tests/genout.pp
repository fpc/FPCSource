program genout;

uses sysutils;

var
  I,aOffset, aCount : Integer;
  UseStdErr : Boolean;

begin
  // number of lines to emit. If negative, use stderr
  aCount:=StrToIntDef(ParamStr(1),3);
  // Offset : start at 1+Offset
  aOffset:=StrToIntDef(ParamStr(2),0);
  UseStdErr:=aCount<0;
  aCount:=Abs(aCount);
  aCount:=aCount+aOffset;
  Inc(aOffset);
  For I:=aOffset to aCount do
    if UseStdErr then
      Writeln(StdErr,'Line ',IntToStr(I))
    else
      Writeln('Line ',IntToStr(I));
end.

