{ %NORUN }

program tb0677;

{$mode objfpc}

type
  TEnum = (eOne, eTwo, eThree, eFour);
  TSet = set of TEnum;

  generic TTest<SetType, EnumType> = class
    procedure Test;
  end;

procedure TTest.Test;
var
  s1: TSet;
  s2: SetType;
  e1: TEnum;
  e2: EnumType;
begin
  Include(s1, e1);
  Exclude(s1, e1);

  Include(s2, e1);
  Exclude(s2, e1);

  Include(s2, e2);
  Exclude(s2, e2);

  Include(s2, e1);
  Exclude(s2, e2);
end;

type
  TTestTypes = specialize TTest<TSet, TEnum>;

begin

end.
