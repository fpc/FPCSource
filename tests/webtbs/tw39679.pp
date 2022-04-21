{ %NORUN }

program tw39679;

{$mode objfpc}{$H+}
{$ModeSwitch implicitfunctionspecialization}

type
  generic TBase<T> = class(TObject);
  generic TChild<T> = class(specialize TBase<T>);
  TLongIntChild = class(specialize TChild<LongInt>);
  TLongIntBase = class(specialize TBase<LongInt>);

generic procedure Foo<T>(lst: specialize TBase<T>);
begin
end;

var
  lst: specialize TChild<Integer>;
  lst2: TLongIntChild;
  lst3: TLongIntBase;
begin
  specialize Foo<Integer>(lst); // works
  Foo(lst); // Error
  Foo(lst2);
  Foo(lst3);
end.

