{ nested statement expressions }
{$Mode ObjFPC}{$H+}
{$ModeSwitch StatementExpressions}

function Twice(const s: String): String;
begin
  Result := s + s;
end;

var
  i: Integer;
  s: String;
begin
  i := 2;
  s := case i of
    1: 'One';
    2: if Odd(i) then 'OddTwo' else 'EvenTwo';
    else 'Other'
  end;
  WriteLn(s);
  if s<>'EvenTwo' then
    Halt(1);

  s := Twice(if i > 1 then 'A' else 'B');
  WriteLn(s);
  if s<>'AA' then
    Halt(2);

  s := if i = 2 then case i of 2: 'Two'; else 'NotTwo' end else 'Else';
  WriteLn(s);
  if s<>'Two' then
    Halt(3);

  i := (if i = 2 then 10 else 20) + 1;
  WriteLn(i);
  if i<>11 then
    Halt(4);
end.
