program TestProgram;

uses Test2;

const A =  1234;
      B =  $1234;

var Hello : word;
    X: PRecord;
    T : TRecord;

function Func1(x,z : word;y : boolean): shortint;

  procedure test_local(c,f : longint);
   var
      int_loc : longint;
   begin
      Writeln('dummy for browser');
   end;

begin
  if Hello=0 then X:=0 else X:=1;
  test_local(0,2);
  Func1:=X;
end;

var i : longint;

BEGIN
  X:=nil;
  writeln('Hello world!');
  Writeln('ParamCount = ',ParamCount);
  For i:=0 to paramcount do
   writeln('Paramstr(',i,') = ',Paramstr(i));
  writeln(IsOdd(3));
  writeln(Func1(5,5,true));
  Halt;
END.
