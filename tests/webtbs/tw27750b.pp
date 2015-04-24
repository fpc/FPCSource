{ %NORUN }

program tw27750b;

{$MODE OBJFPC}
{$modeswitch advancedrecords}

type

  { TGeneric }

  generic TGeneric<Foo, Bar> = record
  public type
    TGFB = specialize TGeneric<Foo, Bar>;
  public
    class operator :=(aFoo : Foo) : TGFB;
    class operator :=(aBar : Bar) : TGFB;
  end;

{ TGeneric }

class operator TGeneric.:=(aFoo: Foo): TGFB;
begin

end;

class operator TGeneric.:=(aBar: Bar): TGFB;
begin

end;

begin
end.

