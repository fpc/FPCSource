Program Example11;

{ Program to demonstrate the Copy function. }

Var S,T : String;

begin
  T:='1234567';
  S:=Copy (T,1,2);   { S:='12'   }
  S:=Copy (T,4,2);   { S:='45'   }
  S:=Copy (T,4,8);   { S:='4567' }
end.
