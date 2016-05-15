{ %cpu=i386,x86_64 }
{ %skiptarget=win64 }
{ Target must actually support Extended type }

function test1(x: single): integer;
begin
  test1:=1;
end;

function test1(x: double): integer;
begin
  test1:=2;
end;

function test1(x: extended): integer;
begin
  test1:=3;
end;


function test2(x: single): integer;
begin
  test2:=1;
end;

function test2(x: double): integer;
begin
  test2:=2;
end;


function test3(x: single): integer;
begin
  test3:=1;
end;

function test3(x: double): integer;
begin
  test3:=2;
end;

function test3(x: cextended): integer;
begin
  test3:=3;
end;


var
  a: cextended;
  b: extended;
begin
  a:= 123.456;
  b:= 123.456;
  { test #1: single/double/extended available, passing cextended must select extended }
  if test1(a)<>3 then
    halt(1);

  { test #2: single and double avaiable, passing cextended must select double }
  if test2(a)<>2 then
    halt(2);

  { test #3: single/double/cextended available, passing extended must select cextended }
  if test3(a)<>3 then
    halt(3);
end.
