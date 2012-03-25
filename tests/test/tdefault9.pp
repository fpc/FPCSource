{ default can be used with generic type parameters as well }
program tdefault9;

{$mode objfpc}

type
  generic TTest<T> = class
    f: T;
    constructor Create;
  end;

{ TTest }

constructor TTest.Create;
begin
  f := Default(T);
end;

type
  TLongIntSpez = specialize TTest<LongInt>;
  TAnsiStringSpez = specialize TTest<AnsiString>;
  TObjectSpez = specialize TTest<TObject>;

var
  si: TLongIntSpez;
  sa: TAnsiStringSpez;
  so: TObjectSpez;
begin
  si := TLongIntSpez.Create;
  if si.f <> 0 then
    Halt(1);
  sa := TAnsiStringSpez.Create;
  if sa.f <> '' then
    Halt(2);
  so := TObjectSpez.Create;
  if so.f <> Nil then
    Halt(3);
end.
