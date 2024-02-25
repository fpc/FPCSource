{%norun}
{%neededafter}

program dotouch;


var
  F : Text;
  N : String;

begin
  N:=ParamStr(1);
  if N='' then
    N:='touch.txt';
  Assign(F,N);
  Rewrite(F);
  Writeln(F,N);
  Close(F);
end.

