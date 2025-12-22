{ %fail }
// Multiline string but not properly indented
{$mode objfpc}
const
  s = '''
  this
 is
  a multiline
  string
  ''';

  stest = 'this'+sLineBreak+'is'+slinebreak+'a multiline'+sLineBreak+'string';

begin
  if not (s=stest) then
    begin
    writeln('Wrong string, expected "',stest,'" but got: "',s,'"');
    halt(1);
    end;
end.