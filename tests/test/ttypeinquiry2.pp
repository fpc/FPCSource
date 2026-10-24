{ The operand of "type of" is only parsed and type checked, never executed.
  So it neither calls functions nor triggers range or overflow checks. }
program ttypeinquiry2;

{$mode objfpc}{$H+}
{$R+}
{$Q+}

var
  CallCount: longint = 0;
  EmptyArr: array of longint;
  FixedArr: array[1..3] of word;
  i: longint = 100;
  b: byte = 0;

function SideEffect: longint;
begin
  Inc(CallCount);
  Result:=1;
end;

function SideEffectPara(x: longint): shortstring;
begin
  Inc(CallCount);
  Result:='';
end;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

var
  { the functions must not be called, not even at run time via an
    initialization of these variables }
  a: type of SideEffect;
  c: type of SideEffectPara(1);
  { reading this would be out of range, but nothing is read }
  d: type of EmptyArr[0];
  e: type of FixedArr[i];
  { this would overflow with the overflow checks on, but it is never computed }
  f: type of (byte(High(byte))+byte(1));
begin
  Check(CallCount=0,1);
  Check(SizeOf(a)=SizeOf(longint),2);
  Check(SizeOf(c)=SizeOf(shortstring),3);
  Check(SizeOf(d)=SizeOf(longint),4);
  Check(SizeOf(e)=SizeOf(word),5);
  Check(SizeOf(f)>0,6);
  { the index was never evaluated, although it is out of range }
  Check(i=100,7);

  { also inside intrinsics and casts }
  Check(SizeOf(type of SideEffect)=SizeOf(longint),10);
  Check(High(type of EmptyArr[0])=High(longint),11);
  Check(Default(type of SideEffect)=0,12);
  Check(type of EmptyArr[0](b)=0,13);
  Check(CallCount=0,14);

  { the functions do work when they are really called }
  Check(SideEffect=1,20);
  Check(CallCount=1,21);

  writeln('ok');
end.
