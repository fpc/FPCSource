unit utffirst;
{$mode objfpc}
{$h+}
{$codepage utf8}
interface

uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  SysUtils;


implementation

uses punit, utrtl;

Function dotffirstutf8 : string;

const
  FNAME = utf8string('adéfg');
  
var
  f: thandle;
  res: longint;
  fnamecmp,
  fsearch : utf8string;
  rsr: TRawByteSearchRec;

begin
  Result:='';
  DeleteFile(FNAME);
  f:=FileCreate(FNAME);
  if f<=0 then
    Exit('Cannot create file');
  FileClose(f);
  
  { determine how the file system reports the name of the file (with the é
    precomposed or decomposed) so we can pass the correct form to findfirst. We cannot
    deal with this automatically in findfirst itself, because some OSes/file systems
    allow both forms to coexist. }
  if (findfirst('ad*fg',faAnyFile and not(faDirectory),rsr)<>0) then
    Exit('Findfirst 1 did not return result')
  else
    begin
      fnamecmp:=rsr.name;
      findclose(rsr);
    end;

  fsearch:=fnamecmp;
  fsearch[1]:='?';
  res:=findfirst(fsearch,faAnyFile and not(faDirectory),rsr);

  if Not AssertEquals('Findfirst 2: res',0,Res) then
    exit;
  if not AssertEquals('Findfirst 2 : name',fnamecmp,rsr.name) then
    begin
    findclose(rsr);
    exit;
    end;
  fsearch:=fnamecmp;
  fsearch[2]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 3 failed')
  else
    findclose(rsr);

  { must succeed regardless of whether the é is decomposed or not }
  if (findfirst('ad?fg',faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 4 failed')
  else
    findclose(rsr);

  { this should succeed if if the the é is decomposed (at least "ls ade?fg" succeeds
    on Mac OS X) }
  if (fnamecmp[3]='e') then
    if (findfirst('ade?fg',faAnyFile and not(faDirectory),rsr)<>0) then
      Exit('FindFirst 5')
    else
      findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 6')
  else
    findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 7')
  else
    findclose(rsr);

  if (findfirst('a*fg',faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 8')
  else
    findclose(rsr);

  if (findfirst('ad*',faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 9')
  else
    findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='*';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    Exit('FindFirst 10')
  else
    findclose(rsr);
end;

Function tffirstutf8 : string;
const
  FNAME = utf8string('adéfg');

Var
  curdir: utf8string;

begin
  RemoveDir('tffdir');
  if not DirectoryExists('tffdir') then
    if not CreateDir('tffdir') then
      exit('Failed to create test dir tffdir');
  curdir:=utf8string(GetCurrentDir);
  if not SetCurrentDir('tffdir') then
    Exit('Cannot chdir to  test dir');
  Result:=dotffirstutf8;
  DeleteFile(FNAME);
  SetCurrentDir(curdir);
  RemoveDir('tffdir');
end;



Function dotffirstutf16 : string;

const
  FNAME = unicodestring('adéfg');
var
  f: thandle;
  res: longint;
  fnamecmp,
  fsearch,
  curdir: unicodestring;
  usr: TUnicodeSearchRec;
begin
  DeleteFile(FNAME);
  f:=FileCreate(FNAME);
  if f<=0 then
    Exit('Failed to create file');
  FileClose(f);
  
  { determine how the file system reports the name of the file (with the é
    precomposed or decomposed) so we can pass the correct form to findfirst. We cannot
    deal with this automatically in findfirst itself, because some OSes/file systems
    allow both forms to coexist. }
  if (findfirst('ad*fg',faAnyFile and not(faDirectory),usr)<>0) then
    Exit('Failed at 11')
  else
    begin
      fnamecmp:=usr.name;
      findclose(usr);
    end;

  fsearch:=fnamecmp;
  fsearch[1]:='?';
  res:=findfirst(fsearch,faAnyFile and not(faDirectory),usr);
  if Not AssertEquals('Findfirst 2 res',0,Res) then exit;
  if Not AssertEquals('Findfirst 2 name',fnamecmp,usr.name) then
    begin
    findClose(usr);
    exit;
    end;
  findclose(usr);

  fsearch:=fnamecmp;
  fsearch[2]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 13')
  else
    findclose(usr);

  { must succeed regardless of whether the é is decomposed or not }
  if (findfirst('ad?fg',faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 14')
  else
    findclose(usr);

  { this should succeed if if the the é is decomposed (at least "ls ade?fg" succeeds
    on Mac OS X) }
  if (fnamecmp[3]='e') then
    if (findfirst('ade?fg',faAnyFile and not(faDirectory),usr)<>0) then
      Exit('Failed at 15')
    else
      findclose(usr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 16')
  else
    findclose(usr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 17')
  else
    findclose(usr);

  if (findfirst('a*fg',faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 18')
  else
    findclose(usr);

  if (findfirst('ad*',faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 19')
  else
    findclose(usr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='*';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    Exit('Failed at 20')
  else
    findclose(usr);
end;

Function tffirstutf16 : string;
const
  FNAME = unicodestring('adéfg');

Var
  curdir: utf8string;

begin
  RemoveDir('tffdir');
  if not DirectoryExists('tffdir') then
    if not CreateDir('tffdir') then
      exit('Failed to create test dir tffdir');
  curdir:=utf8string(GetCurrentDir);
  if not SetCurrentDir('tffdir') then
    Exit('Cannot chdir to  test dir');
  Result:=Dotffirstutf16;
  DeleteFile(FNAME);
  SetCurrentDir(curdir);
  RemoveDir('tffdir');
end;

begin
  Case GetSysTestOS of
    'linux','freebsd','openbsd','netbsd','win32','win64','darwin','haiku','morphos' :
       begin
       SysutilsTest('TestFFirstUtf8',@tffirstutf8);
       SysutilsTest('TestFFirstUtf16',@tffirstutf16);
       end;
  end;
end.

