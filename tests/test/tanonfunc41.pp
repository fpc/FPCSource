{ %NORUN }

program tanonfunc41;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
{$modeswitch advancedrecords}

{ test advanced generics }

type
  generic M<T> = reference to function (const A: T): specialize M<T>;

type
  generic G<T> = record
    type M = reference to function (const A: T): M;
  end;

type
  generic C<T> = class(TInterfacedObject, specialize M<T>)
    function Invoke(const A: T): specialize M<T>;
  end;

function C.Invoke(const A: T): specialize M<T>;
begin
  Writeln(ClassName, '.Invoke(', A, ')');
  Result := Self;
end;

type
  R = record
    procedure Foo;
    generic procedure Foo<T>;
  end;

procedure R.Foo;
type
  generic Local<T> = reference to procedure (const X: T);
var
  Z: specialize Local<Char>;
// TODO: var AZ: reference to procedure (const X: T);
begin
  Z := procedure (const C: Char) begin
    WriteLn('nmls: ', C)
  end;
  Z('Z')
end;

generic procedure R.Foo<T>;
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
  X: specialize M<Integer>{G<Integer>.M};
  Y: R;
begin
  X := specialize C<Integer>.Create;
  X(42)(777)(1024);

  Y.Foo;
end.
