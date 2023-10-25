{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Michael Van Canneyt

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    Pas2JS compiler Postprocessor support. Can depend on filesystem.
}
{$IFNDEF CPUWASM}
{$DEFINE HAS_PROCESS}
{$ENDIF}

{$IFNDEF FPC_DOTTEDUNITS}
unit Pas2JSCompilerPP;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Pas2Js.Compiler.Base, Js.Writer, Pas2Js.SrcMap, System.Contnrs;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, pas2jscompiler, jswriter, FPPJSSrcMap, contnrs;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TPas2JSFSPostProcessorSupport }

  TPas2JSFSPostProcessorSupport = Class(TPas2JSPostProcessorSupport)
  Private
    FPostProcs: TObjectList;
    function CmdListAsStr(CmdList: TStrings): string;
  Public
    Constructor Create(aCompiler: TPas2JSCompiler); override;
    Destructor Destroy; override;
    Procedure Clear; override;
    Procedure WriteUsedTools; override;
    Procedure AddPostProcessor(const Cmd: String); override;
    Procedure CallPostProcessors(const JSFileName: String; aWriter: TPas2JSMapper); override;
    function Execute(const JSFilename: String; Cmd: TStringList; JS: TJSWriterString): TJSWriterString;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses {$IFDEF HAS_PROCESS}System.Process,{$ENDIF} Pas2Js.Logger, Pas2Js.Utils, Pas2Js.Files.Utils;
{$ELSE FPC_DOTTEDUNITS}
uses {$IFDEF HAS_PROCESS}process, {$ENDIF} pas2jslogger, pas2jsutils, pas2jsfileutils;
{$ENDIF FPC_DOTTEDUNITS}

function TPas2JSFSPostProcessorSupport.CmdListAsStr(CmdList: TStrings): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to CmdList.Count-1 do
  begin
    if Result<>'' then Result+=' ';
    Result+=QuoteStr(CmdList[i]);
  end;
end;

constructor TPas2JSFSPostProcessorSupport.Create(aCompiler: TPas2JSCompiler);
begin
  inherited Create(aCompiler);
  FPostProcs:=TObjectList.Create; // Owns objects
end;

destructor TPas2JSFSPostProcessorSupport.Destroy;
begin
  FreeAndNil(FPostProcs);
  inherited Destroy;
end;

procedure TPas2JSFSPostProcessorSupport.Clear;
begin
  FPostProcs.Clear;
end;

procedure TPas2JSFSPostProcessorSupport.WriteUsedTools;

Var
  I : integer;
  PostProc : TStringList;

begin
  // post processors
  for i:=0 to FPostProcs.Count-1 do
  begin
    PostProc:=TStringList(FPostProcs[i]);
    Compiler.Log.LogMsgIgnoreFilter(nPostProcessorInfoX,[CmdListAsStr(PostProc)]);
  end;
end;

procedure TPas2JSFSPostProcessorSupport.AddPostProcessor(const Cmd: String);

Var
  PostProc : TStringList;
  S : String;

begin
  PostProc:=TStringList.Create;
  FPostProcs.Add(PostProc);
  SplitCmdLineParams(Cmd,PostProc);
  if PostProc.Count<1 then
    Compiler.ParamFatal('-Jpcmd executable missing');
  // check executable
  S:=Compiler.FS.ExpandExecutable(PostProc[0]);
  if (S='') then
    Compiler.ParamFatal('-Jpcmd executable "'+S+'" not found');
  PostProc[0]:=S;
end;

procedure TPas2JSFSPostProcessorSupport.CallPostProcessors(const JSFileName: String; aWriter: TPas2JSMapper);

var
  i: Integer;
  JS, OrigJS: TJSWriterString;

begin
  if FPostProcs.Count=0 then exit;
  OrigJS:=aWriter.AsString;
  JS:=OrigJS;
  for i:=0 to FPostProcs.Count-1 do
    JS:=Execute(JSFilename,TStringList(FPostProcs[i]),JS);
  if JS<>OrigJS then
  begin
    aWriter.AsString:=JS;
    if aWriter.SrcMap<>nil then
      aWriter.SrcMap.Clear;
  end;

end;

{$IFDEF HAS_PROCESS}
function TPas2JSFSPostProcessorSupport.Execute(const JSFilename: String; Cmd: TStringList; JS: TJSWriterString): TJSWriterString;

const
  BufSize = 65536;
var
  Exe: String;
  TheProcess: TProcess;
  WrittenBytes, ReadBytes: LongInt;
  Buf, s, ErrBuf: string;
  OutputChunks: TStringList;
  CurExitCode, i, InPos: Integer;
begin
  Result:='';
  Buf:='';
  Exe:=Cmd[0];
  if Compiler.ShowDebug or Compiler.ShowUsedTools then
    Compiler.Log.LogMsgIgnoreFilter(nPostProcessorRunX,[QuoteStr(JSFilename)+' | '+CmdListAsStr(Cmd)]);
  if Compiler.FS.DirectoryExists(Exe) then
    raise EFOpenError.Create('post processor "'+Exe+'" is a directory');
  if not FileIsExecutable(Exe) then
    raise EFOpenError.Create('post processor "'+Exe+'" is a not executable');
  try
    TheProcess := TProcess.Create(nil);
    OutputChunks:=TStringList.Create;
    try
      TheProcess.Executable := Exe;
      for i:=1 to Cmd.Count-1 do
        TheProcess.Parameters.Add(Cmd[i]);
      TheProcess.Options:= [poUsePipes];
      TheProcess.ShowWindow := swoHide;
      //TheProcess.CurrentDirectory:=WorkingDirectory;
      TheProcess.Execute;
      ErrBuf:='';
      SetLength(Buf,BufSize);
      InPos:=1;
      repeat
        // read stderr and log immediately as warnings
        repeat
          if TheProcess.Stderr.NumBytesAvailable=0 then break;
          ReadBytes:=TheProcess.Stderr.Read(Buf[1],BufSize);
          if ReadBytes=0 then break;
          ErrBuf+=LeftStr(Buf,ReadBytes);
          repeat
            i:=1;
            while (i<=length(ErrBuf)) and (i<128) and not (ErrBuf[i] in [#10,#13]) do
              inc(i);
            if i>length(ErrBuf) then break;
            Compiler.Log.LogMsg(nPostProcessorWarnX,[LeftStr(ErrBuf,i)]);
            if (i<=length(ErrBuf)) and (ErrBuf[i] in [#10,#13]) then
            begin
              // skip linebreak
              if (i<length(ErrBuf)) and (ErrBuf[i+1] in [#10,#13])
                  and (ErrBuf[i]<>ErrBuf[i+1]) then
                inc(i,2)
              else
                inc(i);
            end;
            Delete(ErrBuf,1,i-1);
          until false;
        until false;
        // write to stdin
        if InPos<length(JS) then
        begin
          i:=length(JS)-InPos+1;
          if i>BufSize then i:=BufSize;
          WrittenBytes:=TheProcess.Input.Write(JS[InPos],i);
          inc(InPos,WrittenBytes);
          if InPos>length(JS) then
            TheProcess.CloseInput;
        end else
          WrittenBytes:=0;
        // read stdout
        if TheProcess.Output.NumBytesAvailable=0 then
          ReadBytes:=0
        else
          ReadBytes:=TheProcess.Output.Read(Buf[1],BufSize);
        if ReadBytes>0 then
          OutputChunks.Add(LeftStr(Buf,ReadBytes));

        if (WrittenBytes=0) and (ReadBytes=0) then
        begin
          if not TheProcess.Running then break;
          Sleep(10); // give tool some time
        end;
      until false;
      TheProcess.WaitOnExit;
      CurExitCode:=TheProcess.ExitCode;

      // concatenate output chunks
      ReadBytes:=0;
      for i:=0 to OutputChunks.Count-1 do
        inc(ReadBytes,length(OutputChunks[i]));
      SetLength(Result,ReadBytes);
      ReadBytes:=0;
      for i:=0 to OutputChunks.Count-1 do
      begin
        s:=OutputChunks[i];
        if s='' then continue;
        System.Move(s[1],Result[ReadBytes+1],length(s));
        inc(ReadBytes,length(s));
      end;
    finally
      OutputChunks.Free;
      TheProcess.Free;
    end;
  except
    on E: Exception do begin
      if Compiler.ShowDebug then
        Compiler.Log.LogExceptionBackTrace(E);
      Compiler.Log.LogPlain('Error: '+E.Message);
      Compiler.Log.LogMsg(nPostProcessorFailX,[CmdListAsStr(Cmd)]);
      Compiler.Terminate(ExitCodeToolError);
    end
    {$IFDEF Pas2js}
    else HandleJSException('[20181118170506] TPas2jsCompiler.CallPostProcessor Cmd: '+CmdListAsStr(Cmd),JSExceptValue,true);
    {$ENDIF}
  end;
  if CurExitCode<>0 then
  begin
    Compiler.Log.LogMsg(nPostProcessorFailX,[CmdListAsStr(Cmd)]);
    Compiler.Terminate(ExitCodeToolError);
  end;
  if Compiler.ShowDebug or Compiler.ShowUsedTools then
    Compiler.Log.LogMsgIgnoreFilter(nPostProcessorFinished,[]);
end;
{$ELSE}
function TPas2JSFSPostProcessorSupport.Execute(const JSFilename: String; Cmd: TStringList; JS: TJSWriterString): TJSWriterString;

begin
  raise EFOpenError.Create('post processor "'+Cmd[0]+'" cannot be executed, no process support');
end;
{$ENDIF}

end.

