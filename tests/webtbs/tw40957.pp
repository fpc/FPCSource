program i20241020_01;
{$mode objfpc}

//{$OPTIMIZATION NODFA}
//{$OPTIMIZATION NOFORLOOP}

type
  TObj = object
    arr: array [0..512] of byte;
    constructor init;
    destructor done; virtual;
  end;

const
  cnt=44;

var
  arr: array[0..cnt] of TObj;
  i: int32;

constructor TObj.init;
begin
end;

destructor TObj.done;
begin
end;

begin
  for i:=0 to cnt do arr[i].init;
  for i:=0 to cnt do arr[i].done; // AV here in run-time
end.