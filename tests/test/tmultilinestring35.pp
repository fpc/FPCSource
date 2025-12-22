// Multiline string,
{$mode delphi}
{$TEXTBLOCK CRLF}
const
  s = '''
  this is
  a UTF8 àéèùá
  multiline string
  ''';

  CRLF = #13#10;
  stest = 'this is'+CRLF+'a UTF8 àéèùá'+CRLF+'multiline string';

begin
  if not (s=stest) then
    begin
    writeln('Wrong string, expected "',stest,'" but got: "',s,'"');
    halt(1);
    end;
end.