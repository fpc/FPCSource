{ ConcatPaths with empty leading elements returns the same path as without them }
program tw40682;

{$mode objfpc}{$h+}

uses
  SysUtils;

procedure Check(aCode: LongInt; const aActual, aExpected: string);

begin
  if aActual<>aExpected then
    begin
    writeln(aCode,': "',aActual,'", expected "',aExpected,'"');
    halt(aCode);
    end;
end;


const
  D = PathDelim;

begin
  Check(1,ConcatPaths(['/1/','2','3/']),'/1/2'+D+'3/');
  Check(2,ConcatPaths(['1/','2','3/']),'1/2'+D+'3/');
  Check(3,ConcatPaths(['1','/2','3']),'1'+D+'2'+D+'3');
  Check(4,ConcatPaths(['1/','','/2/3']),'1/2/3');
  Check(5,ConcatPaths(['','/1/','2','3/']),'/1/2'+D+'3/');
  Check(6,ConcatPaths(['','1/','2','3/']),'1/2'+D+'3/');
  Check(7,ConcatPaths(['','1','/2','3']),'1'+D+'2'+D+'3');
  Check(8,ConcatPaths(['','1/','','/2/3']),'1/2/3');
  Check(9,ConcatPaths(['','','1','2']),'1'+D+'2');
  Check(10,ConcatPaths(['1','2','']),'1'+D+'2');
  Check(11,ConcatPaths(['','']),'');
  Check(12,ConcatPaths(['']),'');
  Check(13,ConcatPaths(['1']),'1');
  writeln('ok');
end.
