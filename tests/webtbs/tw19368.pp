program Test5_FloatToStr_2Times;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

const
  first_passed : boolean = false;
  second_passed : boolean = false;

Procedure ExitProg;
begin
  if first_passed <> second_passed then
    begin
      Writeln('Error ',ExitCode,' after first passed!');
    end
  else if (ExitCode<>0) then
    begin
      Writeln('Exit code: ',ExitCode);
      if (ExitCode=217) and not first_passed then
        begin
          Writeln('FPU error appeared at first call to FloatToStr(NaN)');
          Writeln('This is OK, it just means that sysutils.FloatToStr');
          Writeln('doesn''t support NaNs.');
          Writeln('Test considered successful.');
          ExitCode:=0;
        end;
    end;
end;

var
  s,s1: string;

begin
  ExitProc:=@ExitProg;
  Writeln('1: ',NaN);
  Writeln('2: ',NaN);
  Writeln('1 with FloatToStr: ',FloatToStr(NaN));
  first_passed:=true;
  Writeln('2 with FloatToStr: ',FloatToStr(NaN));
  second_passed:=true;
end.
