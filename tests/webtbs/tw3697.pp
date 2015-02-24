{ Source provided for Free Pascal Bug Report 3697 }
{ Submitted by "Matthias Hryniszak" on  2005-02-26 }
{ e-mail: matthias@hryniszak.de }
uses
  {$ifdef unix}{$ifdef darwin}iosxwstr{$else}cwstring{$endif}, {$endif}SysUtils;

var
  S: WideString;

begin
  S := WideFormat('Test %s', ['string']);
  if s<>'Test string' then
    halt(1);
  
  writeln('ok');
end.
