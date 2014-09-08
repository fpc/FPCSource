unit InstantFPTools;

{$mode objfpc}{$H+}

{$define UseFpExecV}

{$ifdef WINDOWS}
  {$undef UseFpExecV}
  {$define HASEXEEXT}
{$endif WINDOWS}
{$ifdef go32v2}
  {$undef UseFpExecV}
  {$define HASEXEEXT}
{$endif go32v2}
{$ifdef watcom}
  {$undef UseFpExecV}
  {$define HASEXEEXT}
{$endif watcom}
{$ifdef os2}
  {$undef UseFpExecV}
  {$define HASEXEEXT}
{$endif go32v2}

{$IFNDEF VER2_4}
{$DEFINE UseExeSearch}
{$ENDIF}

{$if defined(Windows) or defined(darwin) or defined(os2) or defined(go32v2) or defined(watcom)}
{$define CaseInsensitiveFilenames}
{$endif}

interface

uses
  {$IFDEF UseFpExecV}
  Unix,
  {$ENDIF}
  Classes, SysUtils, Process;

procedure CheckSourceName(const Filename: string);
procedure CommentShebang(Src: TStringList);
function GetCacheDir: string;
procedure SetCacheDir(AValue : string);
function IsCacheValid(Src: TStringList;
                      const CachedSrcFile, CachedExeFile: string): boolean;
procedure Compile(const SrcFilename, CacheFilename, OutputFilename: string);
procedure WriteCompilerOutput(SrcFilename, CacheFilename, CompilerOutput: string);
function GetCompiler: string;
procedure SetCompiler(AValue : string);
function GetCompilerParameters(const SrcFilename, OutputDirectory, OutputFilename: string): string;
procedure Run(const Filename: string);

implementation

Var
  CmdCacheDir : String;
  CmdCompiler : String;

procedure AddParam(p: string; var Line: string);
begin
  if p='' then exit;
  if Line<>'' then Line:=Line+' ';
  if (p[1]<>'"') and (System.Pos(' ',p)>0) then
    p:='"'+p+'"';
  Line:=Line+p;
end;

procedure CheckSourceName(const Filename: string);
var
  Ext: String;
begin
  // avoid name clashes
  Ext:=lowercase(ExtractFileExt(Filename));
  if (Ext<>'') and (Ext<>'.pas') and (Ext<>'.pp') and (Ext<>'.p')
  and (Ext<>'.lpr') and (Ext<>'.txt') and (Ext<>'.sh') and (Ext<>'.cgi')
  then begin
    writeln('invalid source extension ',Ext);
    Halt(1);
  end;
end;

procedure CommentShebang(Src: TStringList);
var
  Line: string;
  i: Integer;
begin
  // comment shebang #!
  if (Src.Count=0) then exit;
  Line:=Src[0];
  i:=1;
  if copy(Line,1,3)=#$EF#$BB#$BF then
    inc(i,3);// UTF8 BOM
  if (i>length(Line)) or (Line[i]<>'#') then exit;
  Src[0]:=copy(Line,1,i-1)+'//'+copy(Line,i,length(Line));
end;


procedure SetCacheDir(AValue : string);

begin
  CmdCacheDir:=AValue;
end;

function GetCacheDir: string;
begin
  Result:=CmdCacheDir;
  if (Result='') then
    begin
    Result:=GetEnvironmentVariable('INSTANTFPCCACHE');
    if Result='' then
      begin
      Result:=GetEnvironmentVariable('HOME');
{$ifdef WINDOWS}
      if Result='' then
        Result:=GetEnvironmentVariable('LOCALAPPDATA');
{$endif WINDOWS}
      if Result<>'' then
        Result:=IncludeTrailingPathDelimiter(Result)+'.cache'+PathDelim+'instantfpc';
      end;
    end;
  if Result='' then begin
    writeln('missing environment variable: HOME or INSTANTFPCCACHE or LOCALAPPDATA');
    Halt(1);
  end;
  Result:=IncludeTrailingPathDelimiter(ExpandFileName(Result));
  if not ForceDirectories(Result) then begin
    writeln('unable to create cache directory "'+Result+'"');
    Halt(1);
  end;
end;

function IsCacheValid(Src: TStringList; const CachedSrcFile,
  CachedExeFile: string): boolean;
var
  OldSrc: TStringList;
  i: Integer;
  p: String;
begin
  Result:=false;
  for i:=1 to Paramcount do begin
    p:=ParamStr(i);
    if (p='') or (p[1]<>'-') then break;
    if p='-B' then exit; // always compile
  end;
  if not FileExists(CachedSrcFile) then exit;
  if not FileExists(CachedExeFile) then exit;
  OldSrc:=TStringList.Create;
  OldSrc.LoadFromFile(CachedSrcFile);
  Result:=Src.Equals(OldSrc);
  {$IFDEF IFFreeMem}
  OldSrc.Free;
  {$ENDIF}
end;

procedure SetCompiler(AValue : string);

begin
  CmdCompiler:=AValue;
end;

procedure WriteCompilerOutput(SrcFilename, CacheFilename, CompilerOutput: string);
var
  Lines: TStringList;
  i: Integer;
  Line: String;
  p: SizeInt;
begin
  // replace in compiler output CacheFilename with SrcFilename
  Lines:=TStringList.Create;
  Lines.Text:=CompilerOutput;
  {$IFDEF CaseInsensitiveFilenames}
  CacheFilename:=LowerCase(CacheFilename);
  {$ENDIF}
  for i:=0 to Lines.Count-1 do begin
    repeat
      Line:=Lines[i];
      {$IFDEF CaseInsensitiveFilenames}
      Line:=LowerCase(Line);
      {$ENDIF}
      p:=Pos(CacheFilename,Line);
      if p<1 then break;
      {$IFDEF CaseInsensitiveFilenames}
      Line:=Lines[i];
      {$ENDIF}
      Lines[i]:=copy(Line,1,p-1)+SrcFilename+copy(Line,p+length(CacheFilename),length(Line));
    until false;
  end;

  // write to stdout
  writeln(Lines.Text);
  {$IFDEF IFFreeMem}
  Lines.Free;
  {$ENDIF}
end;

function GetCompiler: string;
var
  CompFile: String;
{$IFNDEF UseExeSearch}
  Path: String;
  p: Integer;
  StartPos: LongInt;
  Dir: String;
{$ENDIF}
begin
  Result:=CmdCompiler;
  if (Result<>'') then
    begin
    Result:=ExpandFileName(Result);
    if not FileExists(Result) then
      begin
      writeln('Error: '+Result+' not found, check the --compiler parameter.');
      Halt(1);
      end;
    exit;
    end;

  {$IFDEF HASEXEEXT}
  CompFile:='fpc.exe';
  {$ELSE}
  CompFile:='fpc';
  {$ENDIF}
  {$IFDEF UseExeSearch}
  Result:=ExeSearch(CompFile);
  {$ELSE}
  Path:=GetEnvironmentVariable('PATH');
  if Path<>'' then begin
    p:=1;
    while p<=length(Path) do begin
      StartPos:=p;
      while (p<=length(Path)) and (Path[p]<>':') do inc(p);
      if StartPos<p then begin
        Dir:=copy(Path,StartPos,p-StartPos);
        Result:=ExpandFileName(IncludeTrailingPathDelimiter(Dir))+CompFile;
        if FileExists(Result) then exit;
      end;
      inc(p);
    end;
  end;
  {$ENDIF}

  if (Result='') then
    begin
    writeln('Error: '+CompFile+' not found in PATH');
    Halt(1);
    end;
end;

procedure DeleteDirectory(Directory: string);
var
  FileInfo: TSearchRec;
  aFilename: String;
begin
  Directory:=ExcludeTrailingPathDelimiter(Directory);
  if not DirectoryExists(Directory) then exit;
  if FindFirst(Directory+PathDelim+AllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
      aFilename:=Directory+PathDelim+FileInfo.Name;
      if (FileInfo.Attr and faDirectory)>0 then
        DeleteDirectory(aFilename)
      else if not DeleteFile(aFilename) then begin
        writeln('unable to delete file "'+aFilename+'"');
        Halt(1);
      end;
    until FindNext(FileInfo)<>0;
    Findclose(FileInfo);
  end;
  if not RemoveDir(Directory) then begin
    writeln('unable to delete directory "'+Directory+'"');
    Halt(1);
  end;
end;

procedure Compile(const SrcFilename, CacheFilename, OutputFilename: string);
var
  Compiler: String;
  CompParams: String;
  Proc: TProcess;
  Count: Int64;
  ss: TStringStream;
  buf : Array[1..4096] of byte;
  pid: SizeUInt;
  BuildDir: String;
  OutputFilenameExe, BuildOutputFilename: String;

  procedure CleanUp;
  begin
    if BuildDir<>'' then begin
      // delete build directory
      DeleteDirectory(BuildDir);
    end;
  end;

begin
  Compiler:=GetCompiler;
  pid:=GetProcessID;
  BuildDir:='';
  OutputFilenameExe:=OutputFilename {$IFDEF HASEXEEXT} + '.exe' {$ENDIF};
  BuildOutputFilename:=OutputFilenameExe;
  if pid>0 then begin
    BuildDir:=ExtractFilePath(OutputFilenameExe)+'__tmp'+IntToStr(pid)+PathDelim;
    BuildOutputFilename:=BuildDir+ExtractFileName(OutputFilenameExe);
  end;
  //writeln('Compiler=',Compiler,' Params=',CompParams);
  if FileExists(OutputFilenameExe) and not DeleteFile(OutputFilenameExe) then begin
    writeln('unable to delete ',OutputFilenameExe);
    Halt(1);
  end;
  if BuildDir<>'' then begin
    if FileExists(BuildOutputFilename) and not DeleteFile(BuildOutputFilename)
    then begin
      writeln('unable to delete ',BuildOutputFilename);
      Halt(1);
    end;
    if not DirectoryExists(BuildDir) and not CreateDir(BuildDir) then begin
      writeln('unable to mkdir ',BuildDir);
      Halt(1);
    end;
  end;
  try
    CompParams:=GetCompilerParameters(CacheFilename,BuildDir,BuildOutputFilename);
    Proc:=TProcess.Create(nil);
    Proc.CommandLine:=Compiler+' '+CompParams;
  {$WARNING Unconditional use of pipes breaks for targets not supporting them}
    Proc.Options:= [poUsePipes, poStdErrToOutput];
    Proc.ShowWindow := swoHide;
    Proc.Execute;
    ss:=TStringStream.Create('');
    repeat
      Count:=Proc.Output.Read(Buf{%H-},4096);
      if Count>0 then
        ss.write(buf,count);
    until Count=0;
    if (not Proc.WaitOnExit) or (Proc.ExitStatus<>0) then begin
      WriteCompilerOutput(SrcFilename,BuildOutputFilename,ss.DataString);
      CleanUp;
      Halt(1);
    end;
    if BuildDir<>'' then begin
      // move from build directory to cache
      if not RenameFile(BuildOutputFilename,OutputFilenameExe) then begin
        writeln('unable to move "',BuildOutputFilename,'" to "',OutputFilenameExe,'"');
        Halt(1);
      end;
    end;
    ss.Free;
    Proc.Free;
  finally
    CleanUp;
  end;
end;

function GetCompilerParameters(const SrcFilename, OutputDirectory,
  OutputFilename: string): string;
{ For example:
    /usr/bin/instantfpc -MObjFpc -Sh ./envvars.pas param1
  The shebang compile parameters: -MObjFpc -Sh
}
var
  p: String;
  i : integer;
begin
  Result:=GetEnvironmentVariable('INSTANTFPCOPTIONS');
  I:=1;
  While (I<=ParamCount) and (Copy(ParamStr(i),1,1)='-') do
    begin
    p:=ParamStr(i);
    if (Copy(p,1,1)='-') and (copy(p,1,2)<>'--') then
      AddParam(P,Result);
    inc(I);
    end;
  if OutputDirectory<>'' then
    AddParam('-FU'+OutputDirectory,Result);
  AddParam('-o'+OutputFilename,Result);
  AddParam(SrcFilename,Result);
end;

procedure Run(const Filename: string);
var
  p : PPChar;
  {$IFNDEF UseFpExecV}
  i : integer;
  args : array of string;
  {$ENDIF}
begin
  p:=argv;
  inc(p);
  while (p<>nil) do begin
    if (p^<>nil) and (p^^<>'-') then begin
      break;
    end;
    inc(p);
  end;
  {$IFDEF UseFpExecV}
    Halt(FpExecV(Filename,p));
  {$ELSE}
    if paramcount>1 then
      begin
        setlength(args,paramcount-1);
        for i:=2 to paramcount do 
          args[i-2]:=paramstr(i);
      end;
    Halt(ExecuteProcess(Filename,args));
  {$ENDIF}
end;

end.

