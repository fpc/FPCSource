Program ex4;

{ Program to demonstrate TRect.Intersect }

Uses objects;


Var ARect,BRect,CRect : TRect;

begin
  ARect.Assign(10,10,20,20);
  BRect.Assign(15,15,25,25);
  { CRect is intersection of ARect and BRect }
  CRect.Assign(15,15,20,20);
  { Calculate it explicitly}
  ARect.Intersect(BRect);
  If ARect.Equals(CRect) Then
    Writeln ('ARect equals CRect')
  Else
    Writeln ('ARect does not equal CRect !');
  BRect.Assign(25,25,30,30);
  Arect.Intersect(BRect);
  If ARect.Empty Then
    Writeln ('ARect is empty');
end.