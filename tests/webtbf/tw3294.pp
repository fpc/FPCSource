{ %fail }

{ Source provided for Free Pascal Bug Report 3294 }
{ Submitted by "marco" on  2004-09-05 }
{ e-mail:  }

{$mode delphi}

var i : integer;

begin
  for i:=0 to 10 do
    begin
      i:=20;
    end;
end.
