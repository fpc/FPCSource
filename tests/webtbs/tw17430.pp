program Project1;

{$mode delphi}{$H+}

var
  p:pointer;
begin
  returnnilifgrowheapfails:=true;
  GetMem(p,ptruint(-128));
  if assigned(p) then
    halt(1);
end.
