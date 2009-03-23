program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Sysutils, variants;


procedure TestMatch(ATestNo: Integer; Value1: TDateTime; Value2: Variant; shouldmatch: boolean);
const
  cDateTime = 'yyyy-mm-dd HH:mm:ss.zzz';
var
  result: Boolean;
begin
  writeln('========= ' + IntToStr(ATestNo) + ' =============');
  writeln(' value1=' + FormatDateTime(cDateTime, value1));
  writeln(' value2=', FormatDateTime(cDateTime, value2));
  result := (value1 = value2);
  writeln(' Do they match?  result=', Result);
  writeln(' ');
  if (result<>shouldmatch) then
    halt(1);
end;


var
  lDate: TDateTime;
  lSearch: variant;
begin
  lDate := EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
  lSearch := EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
  TestMatch(1, lDate, lSearch, true);

  lDate := EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
  lSearch := EncodeDate(1999,02,06)+EncodeTime(20,0,0,2);
  TestMatch(2, lDate, lSearch, false);

  lDate := EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
  lSearch := EncodeDate(1999,02,06)+EncodeTime(20,0,0,4);
  TestMatch(3, lDate, lSearch, false);

  lDate := EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
  lSearch := EncodeDate(1999,02,06)+EncodeTime(20,0,0,5);
  TestMatch(4, lDate, lSearch, false);
end.

