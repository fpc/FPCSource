program E04;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

type
  T0 = class
    class procedure Foo;
  end;

  TA = class(T0)
    class procedure Foo(A: Integer = 0); overload;
  end;

  TB = class(TA)
    class procedure Foo(A: Integer); overload;
  end;

  TClassB = class of TB;

var
  t0called,
  tacalled,
  tbcalled: boolean;

class procedure T0.Foo();
begin
  WriteLn('T0.Foo');
  t0called:=true;
end;

class procedure TA.Foo(A: Integer = 0);
begin
  WriteLn('TA.Foo');
  tacalled:=true;
end;

class procedure TB.Foo(A: Integer);
begin
  WriteLn('TB.Foo');
  tbcalled:=true;
end;

var
  B: TB;
  ClassB: TClassB;
begin
  TB.Foo; // call TA.Foo (VMT is not used, compiler can determine)
  if t0called then
    halt(1);
  if not tacalled then
    halt(2);
  if tbcalled then
    halt(3);
  tacalled:=false;

  B := TB.Create;
  B.Foo; // call TA.Foo because of VMT rules
  B.Free;
  if t0called then
    halt(4);
  if not tacalled then
    halt(5);
  if tbcalled then
    halt(6);
  tacalled:=false;

  ClassB := TB;
  ClassB.Foo; // call TA.Foo because of VMT rules
  if t0called then
    halt(7);
  if not tacalled then
    halt(8);
  if tbcalled then
    halt(9);
end.
