{ %opt=-vew -Sew }
{ %fail }

type
  trec = record
    s: set of byte;
  end;

function f: trec;
begin
  if f.s <>[] then ;
end;

begin
  f;
end.
