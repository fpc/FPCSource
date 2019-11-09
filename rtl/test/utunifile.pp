unit utunifile;

{$codepage utf8}
{$mode objfpc}{$h+}

interface

uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  sysutils;
  
implementation  

uses punit,utrtl;

type
  tcpstr866 = type ansistring(866);

procedure error(const s: string);
begin
  writeln('Error: ',s);
  halt(1);
end;


procedure warn(const s: string);
begin
  Ignore('Warning: cannot test '+s+' scenario fully because not all characters are supported by DefaultFileSystemCodePage');
end;


Function testsinglebyteUtf8 : String;

var
  u: utf8string;
  f: THandle;
  r: rawbytestring;
begin
  Result:='';
  u:='‹≈©◊';
  r:=u;
  setcodepage(r,DefaultFileSystemCodePage);
  if r=u then
    begin
    f:=FileCreate(u,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
    if not AssertTrue('Creating utf8string',f<>-1) then exit;
    FileClose(f);
    DeleteFile(u);
    end
  else
    warn('utf8string');
end;

Function testsinglebytecp866 : String;

  var
    c: tcpstr866;
    f: THandle;
    r: rawbytestring;
  begin
    Result:='';
  c:='Русская';
  setcodepage(rawbytestring(c),866);
  r:=c;
  setcodepage(r,DefaultFileSystemCodePage);
  if r=c then
    begin
    f:=FileCreate(c,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
    if not AssertTrue('Creating tcpstr866',f<>-1) then exit;
    FileClose(f);
    DeleteFile(c);
    end
  else
    warn('tcpstr866');
end;


Function testtwobyteutf8 : string;

var
  u: unicodestring;
  f: THandle;
  r: rawbytestring;

begin
  Result:='';
  R:='';
  u:='‹≈©◊';
  widestringmanager.unicode2ansimoveproc(punicodechar(u),r,DefaultFileSystemCodePage,length(u));
  if r=u then
    begin
    f:=FileCreate(u,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
    if not AssertTrue('Creating unicodestring 1',f<>-1) then exit;
    FileClose(f);
    DeleteFile(u);
    end
  else
    warn('random unicodestring');
end;

Function testtwobytecp866 : string;

var
  u: unicodestring;
  f: THandle;
  r: rawbytestring;

begin
  Result:='';
  r:='';
  u:='Русская';
  r:=u;
  if r=u then
    begin
    f:=FileCreate(u,fmShareDenyNone,(6 shl 6) or (4 shl 3) or 4);
    if not AssertTrue('Creating unicodestring 1',f<>-1) then exit;
    FileClose(f);
    DeleteFile(u);
    end
  else
    warn('cp866 unicodestring');
end;


begin
  SysutilsTest('testsinglebyteutf8',@testsinglebyteutf8);
  SysutilsTest('testsinglebytecp866',@testsinglebytecp866);
  SysutilsTest('testtwobyteutf8',@testtwobyteutf8);
  SysutilsTest('testtwobytecp866',@testtwobytecp866);
end.
