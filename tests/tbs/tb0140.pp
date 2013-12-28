{ %OPT=-Oonoconstprop }
{ Old file: tbs0165.pp }
{ missing range check code for enumerated types.            OK 0.99.9 (PFV) }

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
end.
