{ Old file: tbs0247.pp }
{ var with initial value not supprted (Delphi var x : integer = 5;) allowed in -Sd mode OK 0.99.11 (PM) }

{$mode delphi}

var
 x : integer = 34;
{ this is the way Delphi creates initialized vars
  ++ its much more logical then BP
  typed const !!
  -- its incompatible with BP !! (PM) }

 y : array[0..2] of real = (0.0,1.23,2.56);

{ these are true const in Delphi mode and thus
  it should not be possible to change ! }

const
 z : real = 45.2;

begin
 y[2]:=z;
 { this should be refused ! }
 z:=y[1];
end.
