program tcpstr13;

// check that copy operation converts from 866 to DefaultSystemCodePage encoding

{$mode delphi}

{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif}

type
  ts866 = type ansistring(866);

var
  s: ts866;
  a: ansistring;
begin
  s:='abc'#$00A9#$00AE'123';
//  if s[4] <> 'c' then
//    halt(1);
  a:=copy(s,1,4);
  if stringcodepage(a)<>DefaultSystemCodePage then
    halt(2);
end.
