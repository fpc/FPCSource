{ %fail }
{$R+}
Type Directions = (North, East,South,West);

Var Go : Directions;

begin
  Go:=Pred(North); { must give compile time error }
end.
