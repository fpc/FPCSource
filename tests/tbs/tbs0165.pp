Program bug0165;

{ No range check when -Cr given}

Type Directions = (North, East,South,West);

Var Go : Directions;

begin
  Go:=North;
  Go:=Pred(Go); { must give run-time error }
  Go:=Pred(North); { must give compile time error }
end.
