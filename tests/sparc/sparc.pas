PROGRAM SparcTest;
VAR
  x,y:Cardinal; 
  z:0..255;
FUNCTION Add(a,b:Cardinal):Cardinal;
  BEGIN
    Add:=a+b;
  END;
BEGIN
  y:=0;
  z:=0;
  x:=1+y;
  x:=Add(x,y);
END.
