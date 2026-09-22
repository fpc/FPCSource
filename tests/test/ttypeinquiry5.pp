{ mode extendedpascal has the modeswitch typeinquiry on by default.
  It needs an own test, because unit/interface/implementation are no
  keywords in this mode. }
program ttypeinquiry5;

{$mode extendedpascal}

var
  i: int64;
  v: type of i;
  b: byte;
  arr: array[1..3] of type of b;
  p: ^type of i;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

begin
  i:=5;
  v:=i;
  Check(SizeOf(v)=8,1);
  Check(v=5,2);
  b:=1;
  arr[1]:=b;
  Check(SizeOf(arr[1])=1,3);
  Check(arr[1]=1,4);
  p:=@i;
  Check(p^=5,5);
  Check(SizeOf(type of i)=8,6);
  writeln('ok');
end.
