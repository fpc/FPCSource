// Multiline string
{$mode delphi}
{$TEXTBLOCK CRLF}
const 
  s = '''
  this
  is
  a multiline
  string
  ''';
  
  CRLF = #13#10;
  stest = 'this'+CRLF+'is'+CRLF+'a multiline'+CRLF+'string';
  
begin
  if not (s=stest) then
    begin
    writeln('Wrong string, expected "',stest,'" but got: "',s,'"');
    halt(1);
    end;
end.