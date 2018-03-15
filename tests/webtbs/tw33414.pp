{$mode objfpc}{$H+}
uses Classes, SysUtils;

type
  generic TFoo<SomeEnum> = class
    m: array[SomeEnum] of integer;
  end;

  TEnum = (e0, e1);

var
  a: specialize TFoo<TEnum>;

begin
  if low(a.m)<>e0 then
    halt(1);
  if high(a.m)<>e1 then
    halt(1);
  writeln('ok');
end.
