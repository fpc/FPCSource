{ %version=1.1 }
{ %FAIL }

{ in this mode, typed constants are read-only }
{$J-}
const
  w1 : word = 1;

begin
  w1:=2;
end.
