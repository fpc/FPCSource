uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
  SysUtils;

type
  ts866 = type AnsiString(866);
var
  a866 : ts866;
begin
  a866 := '';
  SetLength(a866,10);
  if (StringCodePage(a866) <> 866) then
    halt(1);
  WriteLn('ok');
end.
