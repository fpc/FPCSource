{$R+}
Program bug0165;

uses
  erroru;

{ No range check when -Cr given}

Type Directions = (North, East,South,West);

Var Go : Directions;


begin
  Require_Error(201);
  Go:=North;
  Go:=Pred(Go); { must give run-time error }
  Go:=Pred(North); { must give compile time error }
end.
