{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
{$modeswitch advancedrecords}
{$IF FPC_FULLVERSION>=30301}
{$modeswitch FUNCTIONREFERENCES}
{$define FPC_HAS_REFERENCE_PROCEDURE}
{$endif}
{ determine the type of the resource/form file }
{$define Win16Res}

{$IFNDEF FPC_DOTTEDUNITS}
unit Classes;
{$ENDIF FPC_DOTTEDUNITS}
{$INLINE ON}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils,
  System.Types,
  System.TypInfo,
{$ifdef FPC_TESTGENERICS}
  System.FGL,
{$endif}
  System.RtlConsts,
  System.SortBase;
{$ELSE FPC_DOTTEDUNITS}
uses
  sysutils,
  types,
  typinfo,
{$ifdef FPC_TESTGENERICS}
  fgl,
{$endif}
  rtlconsts,
  sortbase;
{$ENDIF FPC_DOTTEDUNITS}

{$i classesh.inc}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  UnixApi.Base,UnixApi.Unix
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  BaseUnix,unix
  ;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF LINUX}
{$DEFINE HAS_TTHREAD_GETSYSTEMTIMES}
class function TThread.GetSystemTimes(out aSystemTimes : TSystemTimes) : Boolean;

const
  StatFile = '/proc/stat';
  CPULine = 'cpu';

var
  Line: string;
  aFile : Text;
  Idle : Int64;

  Function GetNextWord(var l : String) : String;

  var
    P : Integer;

  begin
    P:=Pos(' ',L);
    if P=0 then 
      P:=Length(L)+1;
    Result:=Copy(L,1,P-1);
    Delete(L,1,P);
    L:=Trim(L);
  end;
  
  Function GetNextInt : Int64; inline;
  
  begin
    Result:=StrToint64(GetNextWord(Line));
  end;

begin
  Result := False;
  aSystemTimes:=Default(TThread.TSystemTimes);
  {$i-}
  AssignFile(aFile,StatFile);
  Reset(aFile);
  if IOResult<>0 then 
    exit;
  {$i+}
  While not EOF(aFile) do
    begin
    ReadLn(aFile,Line);
    if Pos(CPULine,Line)>0 then
      begin
      GetNextWord(Line); // Skip "cpu"
      // cpuN usertime nicetime kerneltime idletime
      With aSystemTimes do
        begin
        Inc(UserTime, GetNextInt);
        Inc(NiceTime, GetNextInt);
        Inc(KernelTime, GetNextInt);
        Idle:=GetNextInt;
        Inc(KernelTime,Idle); // windows seems to count idle as kernel
        Inc(IdleTime,Idle);
        end;
      Result:=True;
      end
    end;
 CloseFile(aFile);  
end;
{$ENDIF}


{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;
finalization
  CommonCleanup;

  if ThreadsInited then
     DoneThreads;
end.
