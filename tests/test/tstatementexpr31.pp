{ statement expressions are enabled by default in mode delphi }
{$Mode Delphi}
type
  TMyEnum = (meFirst, meSecond, meLast);

function ConditionalThrow(doRaise: Boolean): String;
begin
  Result := 'Foo';
  if doRaise then raise TObject.Create;
end;

var
  s: String;
  e: TMyEnum;
begin
  s := if 0 < 1 then 'Foo' else 'Bar';
  if s<>'Foo' then
    Halt(1);
  e := meLast;
  s := case e of
    meFirst: 'First';
    meSecond: 'Second';
    else 'Other'
  end;
  if s<>'Other' then
    Halt(2);
  s := try ConditionalThrow(True) except 'Error' end;
  if s<>'Error' then
    Halt(3);
end.
