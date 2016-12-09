program E03;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
{$APPTYPE CONSOLE}

type
  T0 = class
    class procedure Foo;
  end;

  TA = class(T0)
    class procedure Foo(A: Integer = 0); overload; virtual;
  end;

  TB = class(TA)
    class procedure Foo(A: Integer); overload; override;
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
  TB.Foo; // call TA.Foo (VMT is not used, compiler can determine) -- on Delphi
          // on FPC: call TB.Foo because virtual method and VMT specified
  if t0called then
    halt(1);
  if tacalled then
    halt(2);
  if not tbcalled then
    halt(3);
  tbcalled:=false;

  B := TB.Create;
  B.Foo; // call TB.Foo because of VMT rules
  B.Free;
  if t0called then
    halt(4);
  if tacalled then
    halt(5);
  if not tbcalled then
    halt(6);
  tbcalled:=false;

  ClassB := TB;
  ClassB.Foo; // call TB.Foo because of VMT rules
  if t0called then
    halt(7);
  if tacalled then
    halt(8);
  if not tbcalled then
    halt(9);
  tbcalled:=false;
end.