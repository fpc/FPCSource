program TFSrch2;

uses
  Dos;

procedure TestWildCard(ADirList: String); //Issue #0024504
var
  FRes: PathStr;
const
  Expected = '';
begin
{$IFDEF DEBUG}
  write('Testing FSearch(''',ADirList, ''', ''', DirectorySeparator, '''): ');
{$ENDIF DEBUG}
  FRes := FSearch(ADirList, DirectorySeparator);
  if (FRes <> Expected) then
  begin
    writeln('Fail: expected ''',Expected,''', but got ''',FRes,'''.');
    Halt(1);
  end
{$IFDEF DEBUG}
  else
    writeln('OK.');
{$ENDIF DEBUG}
end;

begin
  TestWildCard(AllFilesMask);
  TestWildCard('?');
end.
