{$codepage utf8}
{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cwstring,
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


procedure testsinglebyte;
var
  u: utf8string;
  c: tcpstr866;
  f: THandle;
  r: rawbytestring;
begin
  { can't set code page of an empty string }
  r:=' ';
  setcodepage(r,DefaultFileSystemCodePage,false);
  u:='‹≈©◊';
  r:=u;
  if r=u then
    begin
      f:=FileCreate(u,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
      if f=-1 then
        Error('Creating utf8string');
      FileClose(f);
    end
  else
    warn('utf8string');
  c:='Русская';
  setcodepage(rawbytestring(c),866);
  r:=c;
  setcodepage(r,DefaultFileSystemCodePage);
  if r=c then
    begin
      f:=FileCreate(c,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
      if f=-1 then
        Error('Creating tcpstr866');
      FileClose(f);
    end
  else
    warn('tcpstr866');
end;


procedure testtwobyte;
var
  u: unicodestring;
  f: THandle;
  r: rawbytestring;
begin
  { can't set code page of an empty string }
  r:=' ';
  setcodepage(r,DefaultFileSystemCodePage,false);
  u:='‹≈©◊';
  r:=u;
  if r=u then
    begin
      f:=FileCreate(u,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
      if f=-1 then
        Error('Creating unicodestring 1');
      FileClose(f);
    end
  else
    warn('random unicodestring');
  u:='Русская';
  r:=u;
  if r=u then
    begin
      f:=FileCreate(u,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
      if f=-1 then
        Error('Creating unicodestring 2');
      FileClose(f);
    end
  else
    warn('cp866 unicodestring');
end;


begin
  testsinglebyte;
  testtwobyte;
end.