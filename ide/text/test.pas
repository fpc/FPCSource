program TestProgram;

uses Test2;

const A =  1234;
      B =  $1234;

var Hello : word;
    X: PRecord;
    T : TRecord;

function Func1: shortint;
var X: word;
begin
  if Hello=0 then X:=0 else X:=1;
  Func1:=X;
end;

BEGIN
  X:=nil;
  writeln('Hello world!');
  writeln(IsOdd(3));
  writeln(Func1);
  Halt;
END.
