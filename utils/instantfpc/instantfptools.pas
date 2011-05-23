unit InstantFPTools;

{$mode objfpc}{$H+}

interface

uses
  {$IFNDEF MSWINDOWS}
  Unix,
  {$ENDIF}
  Classes, SysUtils, Process;

procedure CheckSourceName(const Filename: string);
procedure CommentShebang(Src: TStringList);
function GetCacheDir: string;
function IsCacheValid(Src: TStringList;
                      const CachedSrcFile, CachedExeFile: string): boolean;
procedure Compile(const CacheFilename, OutputFilename: string);
function GetCompiler: string;
function GetCompilerParameters(const SrcFilename, OutputFilename: string): string;
procedure Run(const Filename: string);

implementation

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
  and (Ext<>'.lpr') and (Ext<>'.txt') and (Ext<>'.sh')
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

function GetCacheDir: string;
begin
  Result:=GetEnvironmentVariable('INSTANTFPCCACHE');
  if Result='' then begin
    Result:=GetEnvironmentVariable('HOME');
    if Result<>'' then
      Result:=IncludeTrailingPathDelimiter(Result)+'.cache'+PathDelim+'instantfpc';
  end;
  if Result='' then begin
    writeln('missing environment variable: HOME or INSTANTFPCCACHE');
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

function GetCompiler: string;
const
  CompilerParam = '--compiler=';
var
  Path: String;
  p: Integer;
  StartPos: LongInt;
  Dir: String;
  CompFile: String;
  i: Integer;
  Param: String;
begin
  for i:=1 to Paramcount do begin
    Param:=ParamStr(i);
    if (Param='') or (Param[1]<>'-') then break;
    if copy(Param,1,length(CompilerParam))=CompilerParam then begin
      CompFile:=copy(Param,length(CompilerParam)+1,length(Param));
      Result:=ExpandFileName(CompFile);
      if not FileExists(Result) then begin
        writeln('Error: '+CompFile+' not found, check the ',CompilerParam,' parameter.');
        Halt(1);
      end;
      exit;
    end;
  end;

  {$IFDEF Windows}
  CompFile:='fpc.exe';
  {$ELSE}
  CompFile:='fpc';
  {$ENDIF}
  Path:=GetEnvironmentVariable('PATH');
  if PATH<>'' then begin
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
  writeln('Error: '+CompFile+' not found in PATH');
  Halt(1);
end;

procedure Compile(const CacheFilename, OutputFilename: string);
var
  Compiler: String;
  CompParams: String;
  Proc: TProcess;
  Count: Int64;
  ss: TStringStream;
  buf : Array[1..4096] of byte;
begin
  Compiler:=GetCompiler;
  CompParams:=GetCompilerParameters(CacheFilename,OutputFilename);
  //writeln('Compiler=',Compiler,' Params=',CompParams);
  if FileExists(OutputFilename) and not DeleteFile(OutputFilename) then begin
    writeln('unable to delete ',OutputFilename);
    Halt(1);
  end;
  Proc:=TProcess.Create(nil);
  Proc.CommandLine:=Compiler+' '+CompParams;
  Proc.Options:= [poUsePipes, poStdErrToOutput];
  Proc.ShowWindow := swoHide;
  Proc.Execute;
  ss:=TStringStream.Create('');
  repeat
    Count:=Proc.Output.Read(Buf,4096);
    if Count>0 then
      ss.write(buf,count);
  until Count=0;
  if (not Proc.WaitOnExit) or (Proc.ExitStatus<>0) then begin
    write(ss.DataString);
    Halt(1);
  end;
  ss.Free;
  Proc.Free;
end;

function GetCompilerParameters(const SrcFilename, OutputFilename: string): string;
{ For example:
    /usr/bin/instantfpc -MObjFpc -Sh ./envvars.pas param1
  The shebang compile parameters: -MObjFpc -Sh
}
var
  p: String;
begin
  Result:='';
  if (Paramcount>0) then begin
    p:=ParamStr(1);
    if (p<>'') and (p[1]='-') then
      Result:=p; // copy compile params from the script
  end;
  AddParam('-o'+OutputFilename {$IFDEF MSWINDOWS} + '.exe' {$ENDIF},Result);
  AddParam(SrcFilename,Result);
end;

procedure Run(const Filename: string);
var
  p: PPChar;
begin
  p:=argv;
  inc(p);
  while (p<>nil) do begin
    if (p^<>nil) and (p^^<>'-') then begin
      break;
    end;
    inc(p);
  end;
  {$IFDEF MSWINDOWS}
    Inc(p); //lose the first command-line argument with the the script filename
    Halt(ExecuteProcess(Filename,[p^]));
  {$ELSE}
    Halt(FpExecV(Filename,p));
  {$ENDIF}
end;

end.

