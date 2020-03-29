program Project1;

{$mode delphi}{$H+}

var
  p:pointer;
begin
  returnnilifgrowheapfails:=true;
  { Use a bigger absoulte value to avoid
    getting a overflow inside heaptrc
    if compiled with -gh option:
    -128 changed to -1024,
    which should be larger than typical
    size of extra memory used by heaptrc }
  GetMem(p,ptruint(-1024));
  if assigned(p) then
    halt(1);
end.
