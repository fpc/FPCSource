Program ex5;

{ Program to demonstrate TRect.Move }

Uses objects;


Var ARect,BRect : TRect;

begin
  ARect.Assign(10,10,20,20);
  ARect.Move(5,5);
  // Brect should be where new ARect is.
  BRect.Assign(15,15,25,25);
  If ARect.Equals(BRect) Then
    Writeln ('ARect equals BRect')
  Else
    Writeln ('ARect does not equal BRect !');
end.