Program Example14;

{ Program to demonstrate the Dec function. }

Var
  I  : Integer;
  L  : Longint;
  W  : Word;
  B  : Byte;
  Si : ShortInt;

begin
 I:=1;
 L:=2;
 W:=3;
 B:=4;
 Si:=5;
 Dec (i);    { i:=0  }
 Dec (L,2);  { L:=0  }
 Dec (W,2);  { W:=1  }
 Dec (B,-2); { B:=6  }
 Dec (Si,0); { Si:=5 }
end.
