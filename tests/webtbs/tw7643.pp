{$mode objfpc}

type
  TMethod = procedure of object;

  TDummy = class
    procedure Method;
  end;

  tr=record
    i1,i2 : longint;
  end;
  pr=^tr;
  
procedure TDummy.Method;
begin
end;

procedure DoSomething(Method: TMethod);
begin
end;

var
  Dummy: TDummy;
  r : tr;
  i : longint;
begin
  i:=ptrint(@pr(nil)^.i2);
{  Dummy := nil;
  DoSomething(@Dummy.Method);}
  DoSomething(@TDummy(nil).Method);
  
end.