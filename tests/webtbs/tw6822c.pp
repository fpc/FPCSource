{ %target=win32,win64,wince,darwin,linux,freebsd,solaris,beos}
uses
  SysUtils;

var
  t: text;
begin
  { see uw6822a.pp }
  assign(t,'uw6822a.txt');
{$i-}
  reset(t);
{$i+}
  if ioresult<>0 then
    halt(1);
  close(t);
  erase(t);
end.
