{$define FAIL}

var
  x1,y1,z : double;

  function x : double;
  begin
    x :=((x1+y1)*(x1-y1))*((x1+y1)*(x1-y1));
  end;
  function y : double;
  begin
    y :=((x1*y1)/(x1+y1)){$ifdef FAIL}*((x1+y1)*(x1-y1)){$endif};
  end;

begin
  x1:=2;
  y1:=3;
{ Explanation a addnote needs the same number of fpu regs
  that the max fpu need of left and right node, unless
  these two numbers are equal:
  this is the reason of the symmetric form of this test code PM }
  z:=((((x+y)*(x-y))+((x+y)*(x-y)))+(((x+y)*(x-y))+((x+y)*(x-y)))+
      (((x+y)*(x-y))+((x+y)*(x-y)))+(((x+y)*(x-y))+((x+y)*(x-y))))+
     ((((x+y)*(x-y))+((x+y)*(x-y)))+(((x+y)*(x-y))+((x+y)*(x-y)))+
      (((x+y)*(x-y))+((x+y)*(x-y)))+(((x+y)*(x-y))+((x+y)*(x-y))));
  Writeln('z = ',z);
end.
