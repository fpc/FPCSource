{$codepage utf8}
{$mode objfpc}{$h+}

uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  sysutils;

type
  tcpstr866 = type ansistring(866);

procedure error(const s: string);
begin
  writeln('Error: ',s);
  halt(1);
end;


procedure warn(const s: string);
begin
  writeln('Warning: cannot test '+s+' scenario fully because not all characters are supported by DefaultFileSystemCodePage');
end;

{$i-}

procedure testsinglebyte;
var
  u, u2,u3: utf8string;
  c,c2,c3: tcpstr866;
  f: file;
begin
  u:='‹≈©◊';
  assign(f,u);
  rewrite(f);
  if ioresult<>0 then
    Error('Creating utf8string file');
  close(f);
  if ioresult<>0 then
    Error('Closing utf8string file');
  u2:='†∞';
  mkdir(u2);
  if ioresult<>0 then
    Error('Creating utf8string dir');
  u3:=u2+'/‹≈©◊1';
  rename(f,u3);
  if ioresult<>0 then
    Error('Renaming utf8string file');
  erase(f);
  if ioresult<>0 then
    Error('Erasing utf8string file');
  rmdir(u2);
  if ioresult<>0 then
    Error('Removing utf8string dir');

  c:='Русская';
  setcodepage(rawbytestring(c),866);
  assign(f,c);
  rewrite(f);
  if ioresult<>0 then
    Error('Creating cp866 file');
  close(f);
  if ioresult<>0 then
    Error('Closing cp866 file');
  c2:='кая';
  setcodepage(rawbytestring(c2),866);
  mkdir(c2);
  if ioresult<>0 then
    Error('Creating cp866 dir');
  c3:=c2+'/Русская1';
  setcodepage(rawbytestring(c3),866);
  rename(f,c3);
  if ioresult<>0 then
    Error('Renaming cp866 file');
  erase(f);
  if ioresult<>0 then
    Error('Erasing cp866 file');
  rmdir(c2);
  if ioresult<>0 then
    Error('Removing cp866 dir');
end;


begin
{ Changing the DefaultFileSystemCodepage without instructing the operating
  system to expect UTF-8 parameters to its API functions (if that is possible
  for the particular operating system at all) is wrong and it cannot work
  correctly on any operating system not using UTF-8 without this setting anyway
  and not providing direct possibility of Unicode (UTF-16) parameters }
{DefaultFileSystemCodePage:=CP_UTF8;}
  testsinglebyte;
//  testtwobyte;
end.
