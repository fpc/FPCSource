{$L+}
unit Test2;

interface

type
     PRecord = ^TRecord;
     TRecord = record
       Field1: longint;
       Next  : PRecord;
     end;

function IsOdd(X: integer): boolean;

var
  TEST2_X : real;

implementation

function IsOdd(X: integer): boolean;
var Z: byte;
begin
  Z:=0;
  X:=Z*X{$ifdef cpui386}*
   Test8087{$endif};
  IsOdd:=(X mod 2)=1;
end;

procedure static;
begin
end;

END.
