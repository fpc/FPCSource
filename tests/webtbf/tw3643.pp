{ %fail }
{ %opt=-Sew -vw }

{ Source provided for Free Pascal Bug Report 3643 }
{ Submitted by "Naj Kejah" on  2005-02-08 }
{ e-mail: universario@hotmail.com }
type
  recDep = record
    cuec, cuek: byte
  end;
var
  Dep1Rec : recDep;
BEGIN
  WITH Dep1Rec DO
    begin
      if cuek = 4 then ;
      if cuek = 4 then ;
    end;
END.
