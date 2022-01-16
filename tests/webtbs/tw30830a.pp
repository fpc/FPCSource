{ %NORUN }

program tw30830a;

{$mode objfpc}

type
  generic TBase<T> = class
    procedure Test1(const a: T);
  end;

  generic TDerived<T> = class(specialize TBase<T>)
    procedure Test2(const a: T);
  end;

procedure TBase.Test1(const a: T);
begin
end;

procedure TDerived.Test2(const a: T);
begin
end;

generic procedure Test<T>(aIntf: specialize TBase<T>); // works
begin
end;

generic procedure Test<T>(aIntf: specialize TDerived<T>); // SIGSEGV :(
begin
end;

var
  b: specialize TBase<LongInt>;
  d: specialize TDerived<LongInt>;
begin
  specialize Test<LongInt>(b);
  specialize Test<LongInt>(d);
end.

