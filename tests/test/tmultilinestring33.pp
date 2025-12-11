// Multiline string
{$mode objfpc}
const
  s = '''
  this is
  a UTF8 àéèùá
  multiline string
  ''';

  stest = 'this is'+slinebreak+'a UTF8 àéèùá'+sLineBreak+'multiline string';

begin
  if not (s=stest) then
    begin
    writeln('Wrong string, expected "',stest,'" but got: "',s,'"');
    halt(1);
    end;
end.