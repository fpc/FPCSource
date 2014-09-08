{ %NORUN }

program tb0606;

{$mode delphi}

type
  TTest<T> = class
    procedure Test;
  end;

procedure TTest<T>.Test;
var
  r: T;
  i: LongInt;
begin
  r := i div r;
  r := r div i;
  r := i mod r;
  r := r mod i;
  r := i shl r;
  r := r shl i;
  r := i shr r;
  r := r shr i;
  r := - r;
  r := not r;
  r := + r;
end;

begin

end.
