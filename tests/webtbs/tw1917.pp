{$mode objfpc}

uses SysUtils;

var
  x,y,z : real;

begin
  x:=5.75;
  y:=5.75;
  z:=6;
  try
    z:=z/ln(x/y);
    WriteLn('Error! No runtime error detected');
    Writeln('z = ',z);
  except
     on e : exception do
       begin
         Writeln('Correct, found error: ',e.message);
       end;
  end;
end.
