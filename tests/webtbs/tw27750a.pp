{ %NORUN }

program tw27750a;

{$MODE DELPHI}

type

  { TGeneric }

  TGeneric<Foo, Bar> = record
  public type
    TGFB = TGeneric<Foo, Bar>;
  public
    class operator implicit(aFoo : Foo) : TGFB;
    class operator implicit(aBar : Bar) : TGFB;
  end;

{ TGeneric }

class operator TGeneric<Foo, Bar>.implicit(aFoo: Foo): TGFB;
begin

end;

class operator TGeneric<Foo, Bar>.implicit(aBar: Bar): TGFB;
begin

end;

begin
end.

