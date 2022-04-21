{ %NORUN }

program tw39675;

{ "private type xyz never used" }
{$warn 5071 error}

{$mode delphi}
{$ModeSwitch implicitfunctionspecialization}

type
  TWingFunc<T> = function(aArg: T): T of object;
  TBird = class
  public
    function Fly<T>(aFunc: TWingFunc<T>; ArgB: T): T;
    function Flap(s: string): string;
  end;

{ TBird }

function TBird.Fly<T>(aFunc: TWingFunc<T>; ArgB: T): T;
begin
  Result:=aFunc(ArgB);
end;

function TBird.Flap(s: string): string;
begin
  Result:='Flap'+s;
end;

var
  Bird: TBird;
begin
  Bird:=TBird.Create;
  writeln(Bird.Fly(Bird.Flap,'Foo'));
end.
