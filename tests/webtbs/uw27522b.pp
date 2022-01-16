{$MODE OBJFPC}
unit uw27522b;

interface

implementation

type
   TTest2Callback = procedure() of object;

function Test2(): TTest2Callback;
begin
end;

end.
