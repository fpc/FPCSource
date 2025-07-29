program testtick;
// Test corner case backtick 
{$modeswitch multilinestrings}
var
  s:string;
begin 
  s:=
'''
`
'''
;
  writeln('>>>',s,'<<<');
end.