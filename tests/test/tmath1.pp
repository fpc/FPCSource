


Procedure TestDiv;
var
 bx,by: byte;
 ix,iy: integer;
 wx,wy: word;
 lx,ly: longint;
Begin
 { byte test }
 bx:=10;
 by:=5;
 bx:=bx div by;
 if bx = 2 then
  WriteLn('TEST_DIV(1): PASSED.')
 else
  WriteLn('TEST_DIV(1): FAILED.');
 bx:=20;
 bx:=bx div 10;
 if bx = 2 then
  WriteLn('TEST_DIV(2): PASSED.')
 else
  WriteLn('TEST_DIV(2): FAILED.');
 { integer test }
 ix:=-10;
 iy:=5;
 ix:=ix div iy;
 if ix = -2 then
  WriteLn('TEST_DIV(3): PASSED.')
 else
  WriteLn('TEST_DIV(3): FAILED.');
 ix:=-20;
 ix:=ix div 10;
 if ix = -2 then
  WriteLn('TEST_DIV(4): PASSED.')
 else
  WriteLn('TEST_DIV(4): FAILED.');
 { word test }
 wx:=64000;
 wy:=2;
 wx:=wx div wy;
 if wx = 32000 then
  WriteLn('TEST_DIV(5): PASSED.')
 else
  WriteLn('TEST_DIV(5): FAILED.');
 wx:=20;
 wx:=wx div 10;
 if wx = 2 then
  WriteLn('TEST_DIV(6): PASSED.')
 else
  WriteLn('TEST_DIV(6): FAILED.');
 { longint test }
 lx:=-1000000;
 ly:=2;
 lx:=lx div ly;
 if lx = -500000 then
  WriteLn('TEST_DIV(7): PASSED.')
 else
  WriteLn('TEST_DIV(7): FAILED.');
 lx:=-1000000;
 lx:=lx div 10;
 if lx = -100000 then
  WriteLn('TEST_DIV(8): PASSED.')
 else
  WriteLn('TEST_DIV(8): FAILED.')
end;




Begin
 Testdiv;
end.
