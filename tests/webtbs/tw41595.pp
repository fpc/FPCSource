{$mode delphiunicode}
program chars01;
type chars = array[ 0..3] of AnsiChar;
procedure test;
var ch: chars;
begin
ch := '';
if ch <> ''
then writeln( 'error')
end;
begin
test
end.