{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Misc routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}

unit FPIntf;

{$mode objfpc}

interface

{ Run }
function  GetRunParameters: string;
procedure SetRunParameters(const Params: string);
function GetRunDir: string;
procedure SetRunDir(const Params: string);

{ Compile }
procedure Compile(const FileName, ConfigFile: string);
procedure SetPrimaryFile(const fn:string);
function LinkAfter : boolean;
{$ifdef USE_EXTERNAL_COMPILER}
function version_string : string;
function full_version_string : string;
{$endif USE_EXTERNAL_COMPILER}


implementation

uses
  Compiler,Comphook,
  sysutils,
{$ifndef NODEBUG}
  FPDebug,
{$endif NODEBUG}
  FPRedir,FPVars,FpCompil,
  FPUtils,FPSwitch,WUtils;

{****************************************************************************
                                   Run
****************************************************************************}

var
  RunDir,
  RunParameters : string;

function LinkAfter : boolean;
begin
  LinkAfter:=LinkAfterSwitches^.GetBooleanItem(0);
end;

function GetRunParameters: string;
begin
  GetRunParameters:=RunParameters;
end;

procedure SetRunParameters(const Params: string);
begin
  RunParameters:=Params;
{$ifndef NODEBUG}
  If assigned(Debugger) then
    Debugger^.SetArgs(RunParameters);
{$endif}
end;

function GetRunDir: string;
begin
  GetRunDir:=RunDir;
end;

procedure SetRunDir(const Params: string);
begin
  RunDir:=Params;
{$ifndef NODEBUG}
  If assigned(Debugger) then
    Debugger^.SetDir(RunDir);
{$endif}
end;


{****************************************************************************
                                   Compile
****************************************************************************}

var
  CatchErrorLongJumpBuffer : jmp_buf;

procedure CatchCompilationErrors;
begin
  LongJmp(CatchErrorLongJumpBuffer,1);
end;

procedure Compile(const FileName, ConfigFile: string);
var
  cmd : string;
  ExitReason : integer;
  ExitAddr,StoreExitProc : pointer;
{$ifdef USE_EXTERNAL_COMPILER}
  CompilerOut : Text;
  CompilerOutputLine : longint;
  V,p,p1,p2,lineNb,ColumnNb : longint;
  error : word;
  ModuleName,Line : string;
  error_in_reading : boolean;
{$endif USE_EXTERNAL_COMPILER}
begin
{$ifndef USE_EXTERNAL_COMPILER}
  cmd:='-d'+SwitchesModeStr[SwitchesMode];
  if ConfigFile<>'' then
    cmd:='['+ConfigFile+'] '+cmd;
{$else USE_EXTERNAL_COMPILER}
  cmd:='-n -d'+SwitchesModeStr[SwitchesMode];
  if ConfigFile<>'' then
    cmd:='@'+ConfigFile+' '+cmd;
  if not UseExternalCompiler then
{$endif USE_EXTERNAL_COMPILER}
{ Add the switches from the primary file }
  if PrimaryFileSwitches<>'' then
    cmd:=cmd+' '+PrimaryFileSwitches;
  cmd:=cmd+' '+FileName;
{ call the compiler }
{$ifdef USE_EXTERNAL_COMPILER}
  if UseExternalCompiler then
    begin
      If not LocateExeFile(ExternalCompilerExe) then
        begin
          CompilerMessageWindow^.AddMessage(
            0,ExternalCompilerExe+' not found','',0,0);
          exit;
        end;
      CompilerMessageWindow^.AddMessage(
        0,'Running: '+ExternalCompilerExe+' '+cmd,'',0,0);
      if not ExecuteRedir(ExternalCompilerExe,cmd,'','ppc___.out','ppc___.err') then
        begin
          CompilerMessageWindow^.AddMessage(
            V_error,msg_errorinexternalcompilation,'',0,0);
          CompilerMessageWindow^.AddMessage(
            V_error,FormatStrInt(msg_iostatusis,IOStatus),'',0,0);
          CompilerMessageWindow^.AddMessage(
            V_error,FormatStrInt(msg_executeresultis,ExecuteResult),'',0,0);
          if IOStatus<>0 then
            exit;
        end;
      Assign(CompilerOut,'ppc___.out');
      Reset(CompilerOut);
      error_in_reading:=false;
      CompilerOutputLine:=0;
      While not eof(CompilerOut) do
        begin
          readln(CompilerOut,Line);
          Inc(CompilerOutputLine);
          p:=pos('(',line);
          if p>0 then
            begin
              ModuleName:=copy(Line,1,p-1);
              Line:=Copy(Line,p+1,255);
              p1:=pos(',',Line);
              val(copy(Line,1,p1-1),lineNb,error);
              Line:=Copy(Line,p1+1,255);
              p2:=pos(')',Line);
              if error=0 then
                val(copy(Line,1,p2-1),ColumnNb,error);
              Line:=Copy(Line,p2+1,255);
              V:=0;
              { using constants here isn't a good idea, because this won't
                work with localized versions of the compiler - Gabor }
              If Pos(' Error:',line)=1 then
                begin
                  V:=V_error;
                  Line:=Copy(Line,8,Length(Line));
                end
              else if Pos(' Fatal:',line)=1 then
                begin
                  V:=V_fatal;
                  Line:=Copy(Line,8,Length(Line));
                end
              else if Pos(' Hint:',line)=1 then
                begin
                  V:=V_hint;
                  Line:=Copy(Line,7,Length(Line));
                end
              else if Pos(' Note:',line)=1 then
                begin
                  V:=V_note;
                  Line:=Copy(Line,7,Length(Line));
                end;
              if error=0 then
                CompilerMessageWindow^.AddMessage(V,Line,ModuleName,LineNb,ColumnNb)
              else
                error_in_reading:=true;
            end
          else
            CompilerMessageWindow^.AddMessage(0,Line,'',0,0);
          ;
        end;
      Close(CompilerOut);
    end
  else
{$endif USE_EXTERNAL_COMPILER}
    begin
      try
          Compiler.Compile(cmd);
      except
          on e : exception do
            begin
              CompilationPhase:=cpFailed;
              CompilerMessageWindow^.AddMessage(V_Error,
                'Compiler exited','',0,0);
              CompilerMessageWindow^.AddMessage(V_Error,
                e.message,'',0,0);
            end;
      end;
    end;
end;

{$ifdef USE_EXTERNAL_COMPILER}
function version_string : string;
  begin
    if not ExecuteRedir(ExternalCompilerExe,'-iV','','ppc___.out','ppc___.err') then
      version_string:=version.version_string
    else
     begin
      Assign(CompilerOut,'ppc___.out');
      Reset(CompilerOut);
      Readln(CompilerOut,s);
      Close(CompilerOut);
      version_string:=s;
     end;
  end;

function full_version_string : string;
  begin
    if not ExecuteRedir(ExternalCompilerExe,'-iW','','ppc___.out','ppc___.err') then
      full_version_string:=version.full_version_string
    else
     begin
      Assign(CompilerOut,'ppc___.out');
      Reset(CompilerOut);
      Readln(CompilerOut,s);
      Close(CompilerOut);
      if Pos ('-iW', S) <> 0 then
(* Unknown option - full version not supported! *)
       S := Version_String;
      full_version_string:=s;
     end;
  end;
{$endif USE_EXTERNAL_COMPILER}

procedure SetPrimaryFile(const fn:string);
var
  t : text;
begin
  PrimaryFile:='';
  PrimaryFileMain:='';
  PrimaryFileSwitches:='';
  PrimaryFilePara:='';
  if UpcaseStr(ExtOf(fn))='.PRI' then
   begin
     assign(t,fn);
     {$I-}
     reset(t);
     if ioresult=0 then
      begin
        PrimaryFile:=fn;
        readln(t,PrimaryFileMain);
        readln(t,PrimaryFileSwitches);
        readln(t,PrimaryFilePara);
        close(t);
      end;
     {$I+}
     EatIO;
   end
  else
   begin
     PrimaryFile:=fn;
     PrimaryFileMain:=fn;
   end;
  if PrimaryFilePara<>'' then
   SetRunParameters(PrimaryFilePara);
end;



end.
