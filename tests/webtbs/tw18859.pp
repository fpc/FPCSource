{ %OPT=-gh }
Program project1;

{$mode objfpc}
{$h+}

type
  trec = record
    s: string;
  end;

procedure test1(values: array of string);
begin
   if paramcount = 0 then
     values[0] := values[0] + '1'
   else
     values[0] := '1';
end;


procedure test2(values: array of trec);
begin
   if paramcount = 0 then
     values[0].s := values[0].s + '1'
   else
     values[0].s := '1';
end;

var
  tr: trec;

begin
  HaltOnNotReleased := True;
  tr.s := 'test';
  uniquestring(tr.s);
  test1([tr.s]);
  test2([tr]);
end.
