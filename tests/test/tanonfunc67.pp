{ %NORUN }

program tanonfunc67;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
{$modeswitch advancedrecords}

{ test advanced generics 

  same as tanonfunc41 but mode delphi
}

type
  M<T> = reference to function (const A: T): M<T>;

type
  G<T> = record
    type M = reference to function (const A: T): M;
  end;

type
  C<T> = class(TInterfacedObject, M<T>)
    function Invoke(const A: T): M<T>;
  end;

function C<T>.Invoke(const A: T): M<T>;
begin
  Writeln(ClassName, '.Invoke(', A, ')');
  Result := Self;
end;

type
  R = record
    procedure Foo;
    procedure Foo<T>;
  end;

procedure R.Foo;
type
  Local<T> = reference to procedure (const X: T);
var
  Z: Local<Char>;
// TODO: var AZ: reference to procedure (const X: T);
begin
  Z := procedure (const C: Char) begin
    WriteLn('nmls: ', C)
  end;
  Z('Z')
end;

procedure R.Foo<T>;
type
  Local = reference to procedure (const X: T);
var
  l: Local;
begin
  l := procedure(const X: T)
       begin
       end;
  l(Default(T));
  // TODO: nameless routines in generics
end;

var
  X: M<Integer>{G<Integer>.M};
  Y: R;
begin
  X := C<Integer>.Create;
  X(42)(777)(1024);

  Y.Foo;
end.
