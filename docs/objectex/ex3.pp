Program ex3;

{ Program to demonstrate TRect.Union }

Uses objects;


Var ARect,BRect,CRect : TRect;

begin
  ARect.Assign(10,10,20,20);
  BRect.Assign(15,15,25,25);
  { CRect is union of ARect and BRect }
  CRect.Assign(10,10,25,25);
  { Calculate it explicitly}
  ARect.Union(BRect);
  If ARect.Equals(CRect) Then
    Writeln ('ARect equals CRect')
  Else
    Writeln ('ARect does not equal CRect !');
end.