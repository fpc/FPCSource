unit Test2;

interface

type
     PRecord = ^TRecord;
     TRecord = record
       Field1: longint;
       Next  : PRecord;
     end;

function IsOdd(X: integer): boolean;

implementation

function IsOdd(X: integer): boolean;
var Z: byte;
begin
  Z:=0;
  X:=Z*X*Test8087;
  IsOdd:=(X mod 2)=1;
end;

END.
