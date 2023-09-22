program md5performancetest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  {$ENDIF}
  SysUtils,Classes,md5,dateutils;

var
  StartTime: TDateTime;
  EndTime: TDateTime;
  i: integer;
  TimeTaken: string;
  s,ss: RawByteString;
begin
  writeln('MD5 of a million "a" symbols');
  Writeln('x86 only: compile md5 unit with -dMD5SLOW to use unoptimized original version');
  SetLength(s, 1000000);
  for i := 1 to 1000000 do s[i] := 'a';

  StartTime:=now;
  for i := 0 to 1000 do
    ss := LowerCase(MDPrint(MDString(s, MD_VERSION_5)));
  EndTime:=now;
  writeln('Performance test finished. Elapsed time:');
  DateTimeToString(TimeTaken, 'S.ZZ', EndTime-StartTime);
  WriteLn('Average time taken = ', TimeTaken, ' ms');
end.

