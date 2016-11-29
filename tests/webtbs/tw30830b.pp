{ %NORUN }

program tw30830a;

{$mode delphi}

type
  TBase<T> = class
    procedure Test1(const a: T);
  end;

  TDerived<T> = class(TBase<T>)
    procedure Test2(const a: T);
  end;

procedure TBase<T>.Test1(const a: T);
begin
end;

procedure TDerived<T>.Test2(const a: T);
begin
end;

procedure Test<T>(aIntf: TBase<T>); overload; // works
begin
end;

procedure Test<T>(aIntf: TDerived<T>); overload; // SIGSEGV :(
begin
end;

var
  b: TBase<LongInt>;
  d: TDerived<LongInt>;
begin
  Test<LongInt>(b);
  Test<LongInt>(d);
end.

