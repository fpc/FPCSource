Program ex1;

{ Program to demonstrate TRect.Empty }

Uses objects;


Var ARect,BRect : TRect;
    P : TPoint;

begin
  With ARect.A do
    begin
    X:=10;
    Y:=10;
    end;
  With ARect.B do
    begin
    X:=20;
    Y:=20;
    end;
  { Offset B by (5,5) }
  With BRect.A do
    begin
    X:=15;
    Y:=15;
    end;
  With BRect.B do
    begin
    X:=25;
    Y:=25;
    end;
  { Point }
  With P do
    begin
    X:=15;
    Y:=15;
    end;
  Writeln ('A empty : ',ARect.Empty);
  Writeln ('B empty : ',BRect.Empty);
  Writeln ('A Equals B : ',ARect.Equals(BRect));
  Writeln ('A Contains (15,15) : ',ARect.Contains(P));
end.