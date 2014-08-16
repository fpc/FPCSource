{ %OPT=-gh }
{$mode objfpc}{$h+}
{ Test that exception in object constructor does not cause memory leak }

uses sysutils;

type
  pobj=^obj;
  obj=object
    constructor init;
    destructor done; virtual;
  end;

  pobjnodestructor=^objnodestructor;
  objnodestructor=object
    constructor init;
  end;

constructor obj.init;
begin
  Abort;
end;

destructor obj.done;
begin
end;

constructor objnodestructor.init;
begin
  Abort;
end;

var
  ps: obj;
  ps2: objnodestructor;
  p: pobj;
  p2: pobjnodestructor;

begin
  HaltOnNotReleased:=true;
  { Test 1: object with destructor, dynamically allocated. Must free memory. }
  try
    new(p,init);
  except
    on EAbort do
    else Halt(1);
  end;

  { Test 2: object with destructor, statically allocated. Must not try to free memory. }
  try
    ps.init;
  except
    on EAbort do
    else Halt(2);
  end;

  { Test 3: object without destructor, dynamically allocated. }
  try
    new(p2,init);
  except
    on EAbort do
    else Halt(3);
  end;

  { Test 4: object without desturtor, statically allocated. }
  try
    ps2.init;
  except
    on EAbort do
    else Halt(4);
  end;
end.
