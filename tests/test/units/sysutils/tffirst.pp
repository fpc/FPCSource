{ %target=linux,freebsd,openbsd,netbsd,win32,win64,darwin,haiku,morphos }

{$codepage utf8}

uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
  SysUtils;

procedure tffirstutf8;
const
  FNAME = utf8string('adéfg');
var
  f: thandle;
  res: longint;
  fnamecmp,
  fsearch,
  curdir: utf8string;
  rsr: TRawByteSearchRec;
begin
  if not CreateDir('tffdir') then
    halt(100);
  curdir:=utf8string(GetCurrentDir);
  if not SetCurrentDir('tffdir') then
    halt(101);
  f:=FileCreate(FNAME);
  if f<=0 then
    halt(102);
  FileClose(f);
  
  { determine how the file system reports the name of the file (with the é
    precomposed or decomposed) so we can pass the correct form to findfirst. We cannot
    deal with this automatically in findfirst itself, because some OSes/file systems
    allow both forms to coexist. }
  if (findfirst('ad*fg',faAnyFile and not(faDirectory),rsr)<>0) then
    halt(1)
  else
    begin
      fnamecmp:=rsr.name;
      findclose(rsr);
    end;

  fsearch:=fnamecmp;
  fsearch[1]:='?';
  res:=findfirst(fsearch,faAnyFile and not(faDirectory),rsr);
  if (res<>0) or
     (rsr.name<>fnamecmp) then
    begin
      writeln('res: ',res);
      if res=0 then
        writeln('fn: ',rsr.name);
      halt(2)
    end
  else
    findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[2]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(3)
  else
    findclose(rsr);

  { must succeed regardless of whether the é is decomposed or not }
  if (findfirst('ad?fg',faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(4)
  else
    findclose(rsr);

  { this should succeed if if the the é is decomposed (at least "ls ade?fg" succeeds
    on Mac OS X) }
  if (fnamecmp[3]='e') then
    if (findfirst('ade?fg',faAnyFile and not(faDirectory),rsr)<>0) then
      halt(5)
    else
      findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(6)
  else
    findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(7)
  else
    findclose(rsr);

  if (findfirst('a*fg',faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(8)
  else
    findclose(rsr);

  if (findfirst('ad*',faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(9)
  else
    findclose(rsr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='*';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),rsr)<>0) or
     (rsr.name<>fnamecmp) then
    halt(10)
  else
    findclose(rsr);
    
  DeleteFile(FNAME);
  SetCurrentDir(curdir);
  RemoveDir('tffdir');
end;

procedure tffirstutf16;
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
  if not CreateDir('tffdir') then
    halt(200);
  curdir:=unicodestring(GetCurrentDir);
  if not SetCurrentDir('tffdir') then
    halt(201);
  f:=FileCreate(FNAME);
  if f<=0 then
    halt(202);
  FileClose(f);
  
  { determine how the file system reports the name of the file (with the é
    precomposed or decomposed) so we can pass the correct form to findfirst. We cannot
    deal with this automatically in findfirst itself, because some OSes/file systems
    allow both forms to coexist. }
  if (findfirst('ad*fg',faAnyFile and not(faDirectory),usr)<>0) then
    halt(11)
  else
    begin
      fnamecmp:=usr.name;
      findclose(usr);
    end;

  fsearch:=fnamecmp;
  fsearch[1]:='?';
  res:=findfirst(fsearch,faAnyFile and not(faDirectory),usr);
  if (res<>0) or
     (usr.name<>fnamecmp) then
    begin
      writeln('res: ',res);
      if res=0 then
        writeln('fn: ',usr.name);
      halt(12)
    end
  else
    findclose(usr);

  fsearch:=fnamecmp;
  fsearch[2]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(13)
  else
    findclose(usr);

  { must succeed regardless of whether the é is decomposed or not }
  if (findfirst('ad?fg',faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(14)
  else
    findclose(usr);

  { this should succeed if if the the é is decomposed (at least "ls ade?fg" succeeds
    on Mac OS X) }
  if (fnamecmp[3]='e') then
    if (findfirst('ade?fg',faAnyFile and not(faDirectory),usr)<>0) then
      halt(15)
    else
      findclose(usr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(16)
  else
    findclose(usr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)]:='?';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(17)
  else
    findclose(usr);

  if (findfirst('a*fg',faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(18)
  else
    findclose(usr);

  if (findfirst('ad*',faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(19)
  else
    findclose(usr);

  fsearch:=fnamecmp;
  fsearch[length(fsearch)-1]:='*';
  if (findfirst(fsearch,faAnyFile and not(faDirectory),usr)<>0) or
     (usr.name<>fnamecmp) then
    halt(20)
  else
    findclose(usr);
    
  DeleteFile(FNAME);
  SetCurrentDir(curdir);
  RemoveDir('tffdir');
end;

begin
  tffirstutf8;
  tffirstutf16;
end.  