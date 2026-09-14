{ optional semicolons before end, exhaustive boolean case }
{$Mode ObjFPC}{$H+}
{$ModeSwitch StatementExpressions}

function ConditionalThrow(doRaise: Boolean): String;
begin
  Result := 'Foo';
  if doRaise then raise TObject.Create;
end;

var
  s: String;
  b: Boolean;
begin
  s := try ConditionalThrow(True) except 'Error'; end;
  if s<>'Error' then
    Halt(1);
  s := try ConditionalThrow(True) except on o: TObject do 'TObject'; else 'Error'; end;
  if s<>'TObject' then
    Halt(2);
  b := True;
  s := case b of
    False: 'F';
    True: 'T'
  end;
  if s<>'T' then
    Halt(3);
  s := case 3 of
    1: 'One';
    else 'Other';
  end;
  if s<>'Other' then
    Halt(4);
end.
