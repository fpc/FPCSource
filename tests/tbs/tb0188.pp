{ Old file: tbs0222.pp }
{ an record field can't be the counter index (compiles with TP) OK 0.99.11 (PFV) }

{$mode tp}

type TStruct = record
                 x,y: Integer;
               end;

var i: TStruct;

begin
  for i.x:=1 to 10 do
    writeln(i.x);
end.
