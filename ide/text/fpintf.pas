{
    $Id$
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
interface

{ Run }
function  GetRunParameters: string;
procedure SetRunParameters(const Params: string);

{ Compile }
procedure Compile(const FileName: string);
procedure SetPrimaryFile(const fn:string);


implementation

uses
  Compiler,CompHook,
{$ifndef NODEBUG}
  FPDebug,
{$endif NODEBUG}
  FPCompile,FPRedir,FPVars,
  FPUtils,FPSwitch;

{****************************************************************************
                                   Run
****************************************************************************}

var
  RunParameters : string;

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


{****************************************************************************
                                   Compile
****************************************************************************}

procedure Compile(const FileName: string);
var
  cmd : string;
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
  cmd:='[fp.cfg] -d'+SwitchesModeStr[SwitchesMode];
{$else USE_EXTERNAL_COMPILER}
  cmd:='-n @fp.cfg -d'+SwitchesModeStr[SwitchesMode];
  if not UseExternalCompiler then
{$endif USE_EXTERNAL_COMPILER}
    if LinkAfter then
      cmd:=cmd+' -s';
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
    Compiler.Compile(cmd);
end;


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
{
  $Log$
  Revision 1.10  2000-05-02 08:42:27  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.9  2000/03/01 22:37:25  pierre
   + USE_EXTERNAL_COMPILER

  Revision 1.8  2000/01/03 11:38:34  michael
  Changes from Gabor

  Revision 1.7  1999/09/16 14:34:59  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.6  1999/06/30 23:58:15  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.5  1999/06/21 23:38:37  pierre
   + support for LinkAfter var

  Revision 1.4  1999/03/12 01:12:22  peter
    * extended primaryfile to load a .pri file

  Revision 1.3  1999/02/05 13:51:41  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.2  1998/12/28 15:47:45  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.4  1998/12/22 10:39:43  peter
    + options are now written/read
    + find and replace routines

}