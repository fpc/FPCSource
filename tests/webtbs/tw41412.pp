

{ Test recursive tail optimization:
  recursive call without assigning the result
  could return back the wrong value }

var
  level_1, level_2, res : longint;
  level2_1, level2_2, res2 : longint;

function test(again : boolean) : longint;
begin
  if not again then
    begin
      level_2:=2;
      test:=4;
    end
  else
    begin
      level_1:=1;
      test:=3;
      test(false);
    end;
end;

function test2(again : boolean) : longint;
var
  d : longint;
begin
  if not again then
    begin
      level2_2:=6;
      test2:=8;
    end
  else
    begin
      level2_1:=5;
      test2:=7;
      d:=test2(false);
    end;
end;

var
  error : byte;
begin
  res:=test(true);
  if level_1<>1 then
    error:=1
  else if level_2<>2 then
    error:=2
  else if res<>3 then
    error:=3;
  res2:=test2(true);
  if level2_1<>5 then
    error:=4
  else if level2_2<>6 then
    error:=5
  else if res2<>7 then
    error:=3;
  if error<>0 then
    begin
      writeln('Test failed: error=',error);
      writeln('level_1=',level_1,' (should be 1)');
      writeln('level_2=',level_2,' (should be 2)');
      writeln('res=',res,' (should be 3)');
      writeln('level2_1=',level2_1,' (should be 5)');
      writeln('level2_2=',level2_2,' (should be 6)');
      writeln('res2=',res2,' (should be 7)');
      halt(error);
    end
  else
    begin
      writeln('Test OK');
    end;
 end.
