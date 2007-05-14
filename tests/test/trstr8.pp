{ from GPC test suite }

program fjf227a;

type TString = String;

procedure foo (const v : double);
var s : TString;
begin
  repeat
    WriteStr (s, '', v : 0);
    ReadStr (s, s);
  until (s = '') or (s <> '');
  if s = ' 4.2E+001' then writeln ('OK') else writeln ('failed "', s,'"')
end;

begin
  foo (42)
end.
