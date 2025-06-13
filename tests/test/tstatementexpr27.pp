{$Mode ObjFPC}
{$ModeSwitch StatementExpressions}

function ConditionalThrow(doRaise: Boolean): String;
begin
  Result := 'Foo';
  if doRaise then raise TObject.Create;
end;

var
  s: String;
begin
  s := try ConditionalThrow(False) except 'Error';
  WriteLn(s);
  if (s<>'Foo') then
    Halt(1);
  s := try ConditionalThrow(True) except 'Error';
  WriteLn(s);
  if (s<>'Error') then
    Halt(2);
  s := try ConditionalThrow(True) except on o: TObject do 'TObject' else 'Error';
  WriteLn(s);
  if (s<>'TObject') then
    Halt(3);
end.
