{ Old file: tbs0188.pp }
{ can't print function result of procedural var that returns a function. Not a bugs : wrong syntax !! See source (PM) }

{ this are no bugs, just wrong
  understanding of FPC syntax }

type testfunc = function:longint;

var f : testfunc;

var test: testfunc;

function test_temp: longint;
begin
  test_temp:=12;
end;

procedure sound(test: testfunc);
begin
  {writeln(test); this is wrong because
   test is the function itself and write does not know how to
   output a function !
   to call test you must use test() !! }
  writeln(test());
end; { proc. sound }

var i : longint;
begin
  i:=test_temp;
  f:=@test_temp;
  if f()<>i then
    begin
       Writeln('error calling f');
       Halt(1);
    end;

  { this works for FPC
   sound(test_temp);
  but the correct syntax would be }
  sound(@test_temp);
  { imagine if a function would return its own type !! }

  { for f var this is correct also ! }
  sound(f);
end.
