{ %norun }
{ %opt=-FcCP1252 }

program project1;

Type
  WideArray = array [0..3] of Widechar;

var
  Test: WideArray;

begin
  Test := 'hmmm';
end.
