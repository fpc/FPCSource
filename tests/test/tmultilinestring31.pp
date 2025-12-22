// Multiline string, 5 quotes, less quotes are preserved in the string
{$mode objfpc}
const
  s = '''''
  this
  is
  a '''multiline'''
  string
  ''''';

  stest = 'this'+sLineBreak+'is'+slinebreak+'a ''''''multiline'''''''+sLineBreak+'string';

begin
  if not (s=stest) then
    begin
    writeln('Wrong string, expected "',stest,'" but got: "',s,'"');
    halt(1);
    end;
end.