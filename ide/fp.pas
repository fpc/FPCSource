{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Berczi Gabor

    Main program of the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program FP;

{$I globdir.inc}
(**********************************************************************)
(* CONDITIONAL DEFINES                                                *)
(*  - NODEBUG    No Debugging support                                 *)
(*  - TP         Turbo Pascal mode                                    *)
(*  - i386       Target is an i386 IDE                                *)
(**********************************************************************)

uses
{$ifdef IDEHeapTrc}
  HeapTrc,
{$endif IDEHeapTrc}
{$ifdef go32v2}
  dpmiexcp,
{$endif go32v2}
{$ifdef fpc}
  keyboard,video,mouse,
{$endif fpc}
{$ifdef HasSignal}
  fpcatch,
{$endif HasSignal}
  Dos,Objects,
  BrowCol,
{$ifdef FVISION}
  FVConsts,
{$else}
  Commands,
{$endif}
  Drivers,Views,App,Dialogs,
  Menus,StdDlg,Validate,
  {$ifdef EDITORS}Editors{$else}WEditor,WCEdit{$endif},
{$ifndef FVISION}
  ColorSel,
  ASCIITab,
{$endif FVISION}
  WUtils,WViews,WHTMLScn,WHelp,
  FPIDE,FPCalc,FPCompil,FPString,
  FPIni,FPViews,FPConst,FPVars,FPUtils,FPHelp,FPSwitch,FPUsrScr,
  FPTools,{$ifndef NODEBUG}FPDebug,{$endif}FPTemplt,FPRedir,FPDesk,
  FPCodTmp,FPCodCmp;


{$ifdef fpc}
Const
  DummyMouseDriver : TMouseDriver = (
    useDefaultQueue : true;
    InitDriver      : nil;
    DoneDriver      : nil;
    DetectMouse     : nil;
    ShowMouse       : nil;
    HideMouse       : nil;
    GetMouseX       : nil;
    GetMouseY       : nil;
    GetMouseButtons : nil;
    SetMouseXY      : nil;
    GetMouseEvent   : nil;
    PollMouseEvent  : nil;
    PutMouseEvent   : nil;
  );
{$endif fpc}

procedure ProcessParams(BeforeINI: boolean);

  function IsSwitch(const Param: string): boolean;
  begin
    IsSwitch:=(Param<>'') and (Param[1]<>DirSep) { <- allow UNIX root-relative paths            }
          and (Param[1] in ['-','/']);           { <- but still accept dos switch char, eg. '/' }
  end;

var I: Sw_integer;
    Param: string;
begin
  for I:=1 to ParamCount do
  begin
    Param:=System.ParamStr(I);
    if IsSwitch(Param) then
      begin
        Param:=copy(Param,2,255);
        if Param<>'' then
        if UpcaseStr(copy(Param,1,2))='HM' then
          { HeapMonitor }
          begin
            if (copy(Param,3,1)='+') or (copy(Param,3,1)='') then
              StartupOptions:=StartupOptions or soHeapMonitor
            else
            if (copy(Param,3,1)='-') then
              StartupOptions:=StartupOptions and not soHeapMonitor;
          end else
{$ifdef go32v2}
        if UpcaseStr(Param)='NOLFN' then
          begin
            LFNSupport:=false;
          end else
{$endif go32v2}
        if UpcaseStr(Param)='README' then
          begin
            ShowReadme:=true;
          end else
        case Upcase(Param[1]) of
          'C' : { custom config file (BP compatiblity) }
           if BeforeINI then
            begin
              if (length(Param)>=1) and (Param[1] in['=',':']) then
                Delete(Param,1,1); { eat separator }
              IniFileName:=Param;
            end;
          'R' : { enter the directory last exited from (BP comp.) }
            begin
              Param:=copy(Param,2,255);
              if (Param='') or (Param='+') then
                StartupOptions:=StartupOptions or soReturnToLastDir
              else
              if (Param='-') then
                StartupOptions:=StartupOptions and (not soReturnToLastDir);
            end;
          'S' :
             if Length(Param)=1 then
               begin
                 UseMouse:=false;
{$ifdef fpc}
                 DoneMouse;
                 SetMouseDriver(DummyMouseDriver);
{$endif fpc}
                 ButtonCount:=0;
               end;
{$ifdef fpc}
          'F' :
             if Length(Param)=1 then
               NoExtendedFrame:=true;
{$ifdef Unix}
          'T' :  DebuggeeTTY:=Copy(Param,2,High(Param));
{$endif Unix}
         { 'M' : TryToMaximizeScreen:=true;}
{$endif fpc}
        end;
      end
    else
      if not BeforeINI then
        TryToOpenFile(nil,Param,0,0,{false}true);
  end;
end;

Procedure MyStreamError(Var S: TStream); {$ifndef FPC}far;{$endif}
var ErrS: string;
begin
  case S.Status of
    stGetError : ErrS:='Get of unregistered object type';
    stPutError : ErrS:='Put of unregistered object type';
  else ErrS:='';
  end;
  if ErrS<>'' then
  begin
    {$ifdef GABOR}{$ifdef TP}asm int 3;end;{$endif}{$endif}
    if Assigned(Application) then
      ErrorBox('Stream error: '+#13+ErrS,nil)
    else

      writeln('Error: ',ErrS);
  end;
end;

procedure DelTempFiles;
begin
  DeleteFile(FPOutFileName);
  DeleteFile(FPErrFileName);
  DeleteFile(GDBOutFileName);
  DeleteFile(GDBOutPutFileName);
  DeleteFile(GREPOutName);
  DeleteFile(GREPErrName);
end;

procedure RegisterIDEObjects;
begin
  RegisterApp;
  RegisterCodeComplete;
  RegisterCodeTemplates;
{$ifndef FVISION}
  RegisterColorSel;
  RegisterAsciiTab;
{$endif FVISION}
  RegisterDialogs;
{$ifdef EDITORS}
  RegisterEditors;
{$else}
  RegisterWEditor;
  RegisterWCEdit;
{$endif}
  RegisterFPCalc;
  RegisterFPCompile;
  RegisterFPTools;
  RegisterFPViews;
{$ifndef NODEBUG}
  RegisterFPDebugViews;
{$endif}
  RegisterMenus;
  RegisterStdDlg;
  RegisterSymbols;
  RegisterObjects;
  RegisterValidate;
  RegisterViews;

  RegisterWHTMLScan;
  RegisterWUtils;
  RegisterWViews;
end;

var CanExit : boolean;
    SetJmpRes : longint;
    StoreExitProc : pointer;
    ErrS : String;
    P : record
          l1 : longint;
          s : pstring;
        end;
{$ifdef win32}
  ShowMouseExe : string;
{$endif win32}
const
  ExitIntercepted : boolean = false;
  SeenExitCode : longint =0;
  SeenErrorAddr : pointer = nil;
  UserWantsToGoOn: boolean = false;


procedure InterceptExit;
begin
  if StopJmpValid then
    begin
      ExitIntercepted:=true;
      SeenExitCode:=ExitCode;
      SeenErrorAddr:=ErrorAddr;
      LongJmp(StopJmp,1);
    end;
end;

BEGIN
  {$ifdef DEV}HeapLimit:=4096;{$endif}
  writeln('þ Free Pascal IDE  Version '+VersionStr);
{$ifdef win32}
  Win32ShowMouse;
{$endif win32}

  ProcessParams(true);

  InitDirs;

  RegisterIDEObjects;
  StreamError:=@MyStreamError;

  ShowReadme:=ShowReadme or (LocateFile(INIFileName)='');

{$ifdef VESA}
  InitVESAScreenModes;
{$endif}
  InitRedir;
{$ifndef NODEBUG}
  InitBreakpoints;
  InitWatches;
{$endif}
  InitReservedWords;
  InitHelpFiles;
  InitSwitches;
  InitINIFile;
  InitUserScreen;
  InitTools;
  InitTemplates;
  InitCodeTemplates;
  InitCodeComplete;

  IDEApp.Init;
  CheckINIFile;
  ReadSwitches(SwitchesPath);
  { load all options after init because of open files }
  ReadINIFile;
  InitDesktopFile;
  LoadDesktop;
  ParseUserScreen;
  { why are the screen contents parsed at startup? Gabor }
  EnableCatchSignals;
  { Update IDE }
  IDEApp.Update;
  IDEApp.UpdateMode;
  IDEApp.UpdateTarget;

  ProcessParams(false);

  if ShowReadme then
  begin
    PutCommand(Application,evCommand,cmShowReadme,nil);
    ShowReadme:=false; { do not show next time }
  end;

  StoreExitProc:=ExitProc;
  ExitProc:=@InterceptExit;

  repeat
    SetJmpRes:=setjmp(StopJmp);
    StopJmpValid:=true;
    UserWantsToGoOn:=false;

    if SetJmpRes=0 then
      IDEApp.Run
    else
      begin
        if (SetJmpRes=1) and ExitIntercepted then
          begin
            { If ExitProc=@InterceptExit then
              ExitProc:=StoreExitProc;}
            Str(SeenExitCode,ErrS);
            if Assigned(Application) then
              begin
                P.l1:=SeenExitCode;
                ErrS:=hexstr(longint(SeenErrorAddr),8);
                P.s:=@ErrS;
                if OKCancelBox(error_programexitedwitherror,@P)=cmCancel then
                  UserWantsToGoOn:=true;
              end
            else
              writeln('Abnormal exit error: ',ErrS);
          end
        else
          begin
            Str(SetJmpRes,ErrS);
          { Longjmp was called by fpcatch }
            if Assigned(Application) then
              begin
                P.l1:=SetJmpRes;
                if OKCancelBox(error_programexitedwithsignal,@P)=cmCancel then
                  UserWantsToGoOn:=true;
              end
            else
              writeln('Signal error: ',ErrS);
          end;
      end;
    if (AutoSaveOptions and asEditorFiles)=0 then
      CanExit:=IDEApp.AskSaveAll
    else
      CanExit:=IDEApp.SaveAll;
    StopJmpValid:=false;
    if (SetJmpRes<>0) then
      begin
        if (not CanExit) or UserWantsToGoOn then
          begin
            if ConfirmBox(continue_despite_error,nil,false)=cmNo then
              CanExit:=true
            else
              CanExit:=false;
          end
        else
          begin
            ErrorBox(leaving_after_error,nil);
          end;
      end;
  until CanExit;

  If ExitProc=pointer(@InterceptExit) then
    ExitProc:=StoreExitProc;
  IDEApp.AutoSave;

  DoneDesktopFile;

  DelTempFiles;
  IDEApp.Done;
  WriteSwitches(SwitchesPath);

  DisableCatchSignals;

  DoneCodeComplete;
  DoneCodeTemplates;
  DoneTemplates;
  DoneTools;
  DoneUserScreen;
  DoneSwitches;
  DoneHelpFiles;
  DoneHelpFilesTypes;
  DoneReservedWords;
  DoneToolMessages;
  DoneBrowserCol;
{$ifndef NODEBUG}
  DoneDebugger;
  DoneBreakpoints;
  DoneWatches;
{$endif}
{$ifdef fpc}
{$ifdef unix}
  Video.ClearScreen;
{$endif unix}
  Video.DoneVideo;
  Keyboard.DoneKeyboard;
{$endif fpc}
{$ifdef unix}
  Keyboard.RestoreStartMode;
{$endif unix}
  StreamError:=nil;
END.
{
  $Log$
  Revision 1.7  2002-04-12 09:00:01  pierre
   * enhance internal error handling

  Revision 1.6  2002/03/28 16:32:48  pierre
   * clearscrenn at exit for unix

  Revision 1.5  2002/03/20 14:56:41  pierre
   * correct last commit

  Revision 1.4  2002/03/20 14:53:37  pierre
   + rescue handlers in main loop

  Revision 1.3  2002/01/09 09:46:10  pierre
   * fix problems with -S option

  Revision 1.2  2001/08/05 12:23:00  peter
    * Automatically support for fvision or old fv

  Revision 1.1  2001/08/04 11:30:22  peter
    * ide works now with both compiler versions

  Revision 1.1.2.10  2001/03/27 12:39:27  pierre
   * Use RestoreStartMode function for Unix

  Revision 1.1.2.9  2001/03/20 00:20:41  pierre
   * fix some memory leaks + several small enhancements

  Revision 1.1.2.8  2001/03/14 17:57:07  pierre
   * fix invisible mouse problem for win32 on win9X

  Revision 1.1.2.7  2000/12/20 14:27:48  pierre
   * fp.ini for unix

  Revision 1.1.2.6  2000/12/13 16:56:41  pierre
   + -t option to specify tty for program run input/output for unix

  Revision 1.1.2.5  2000/11/29 00:54:44  pierre
   + preserve window number and save special windows

  Revision 1.1.2.4  2000/11/09 08:53:35  pierre
   + -F option to force use of only one graphic set

  Revision 1.1.2.3  2000/09/27 22:32:26  pierre
   * suppress lineinfo explicit in _uses

  Revision 1.1.2.2  2000/08/16 18:46:14  peter
   [*] double clicking on a droplistbox caused GPF (due to invalid recurson)
   [*] Make, Build now possible even in Compiler Messages Window
   [+] when started in a new dir the IDE now ask whether to create a local
       config, or to use the one located in the IDE dir

  Revision 1.1.2.1  2000/07/18 05:50:22  michael
  + Merged Gabors fixes

  Revision 1.1  2000/07/13 09:48:34  michael
  + Initial import

  Revision 1.47  2000/06/16 08:50:40  pierre
   + new bunch of Gabor's changes

  Revision 1.46  2000/05/29 10:44:56  pierre
   + New bunch of Gabor's changes: see fixes.txt

  Revision 1.45  2000/05/02 08:42:26  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.44  2000/04/25 08:42:32  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.43  2000/04/18 11:42:36  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.42  2000/03/21 23:34:10  pierre
   adapted to wcedit addition by Gabor

  Revision 1.41  2000/03/13 20:41:34  pierre
    + option -S to disable the mouse
    * adapted to changes in fpusrscr for DOS

  Revision 1.40  2000/03/07 21:58:58  pierre
   + uses ParseUserScreen and UpdateMode

  Revision 1.39  2000/02/12 23:58:26  carl
    + Conditional define explanaations

  Revision 1.38  2000/02/07 11:54:17  pierre
   + RegisterWUtils by Gabor

  Revision 1.37  2000/01/25 00:26:35  pierre
   + Browser info saving

  Revision 1.36  2000/01/10 15:53:37  pierre
  * WViews objects were not registered

  Revision 1.35  2000/01/03 11:38:33  michael
  Changes from Gabor

  Revision 1.34  1999/12/20 14:23:16  pierre
    * MyApp renamed IDEApp
    * TDebugController.ResetDebuggerRows added to
      get resetting of debugger rows

  Revision 1.33  1999/12/20 09:36:49  pierre
   * get the mouse visible on win32 fp

  Revision 1.32  1999/12/10 13:02:05  pierre
  + VideoMode save/restore

  Revision 1.31  1999/09/13 11:43:59  peter
    * fixes from gabor, idle event, html fix

  Revision 1.30  1999/08/22 22:24:15  pierre
   * avoid objpas paramstr functions

  Revision 1.29  1999/08/03 20:22:25  peter
    + TTab acts now on Ctrl+Tab and Ctrl+Shift+Tab...
    + Desktop saving should work now
       - History saved
       - Clipboard content saved
       - Desktop saved
       - Symbol info saved
    * syntax-highlight bug fixed, which compared special keywords case sensitive
      (for ex. 'asm' caused asm-highlighting, while 'ASM' didn't)
    * with 'whole words only' set, the editor didn't found occourences of the
      searched text, if the text appeared previously in the same line, but didn't
      satisfied the 'whole-word' condition
    * ^QB jumped to (SelStart.X,SelEnd.X) instead of (SelStart.X,SelStart.Y)
      (ie. the beginning of the selection)
    * when started typing in a new line, but not at the start (X=0) of it,
      the editor inserted the text one character more to left as it should...
    * TCodeEditor.HideSelection (Ctrl-K+H) didn't update the screen
    * Shift shouldn't cause so much trouble in TCodeEditor now...
    * Syntax highlight had problems recognizing a special symbol if it was
      prefixed by another symbol character in the source text
    * Auto-save also occours at Dos shell, Tool execution, etc. now...

  Revision 1.28  1999/07/10 01:24:11  pierre
   + First implementation of watches window

  Revision 1.27  1999/06/29 22:43:12  peter
    * try to add extensions to params

  Revision 1.26  1999/06/28 23:31:14  pierre
   * typo inside go32v2 cond error removed

  Revision 1.25  1999/06/28 19:25:34  peter
    * fixes from gabor

  Revision 1.24  1999/06/28 12:40:56  pierre
   + clear tool messages at exit

  Revision 1.23  1999/06/25 00:48:05  pierre
   + adds current target in menu at startup

  Revision 1.22  1999/05/22 13:44:28  peter
    * fixed couple of bugs

  Revision 1.21  1999/04/07 21:55:40  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.20  1999/03/23 16:16:36  peter
    * linux fixes

  Revision 1.19  1999/03/23 15:11:26  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.18  1999/03/21 22:51:35  florian
    + functional screen mode switching added

  Revision 1.17  1999/03/16 12:38:06  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.16  1999/03/12 01:13:01  peter
    * use TryToOpen() with parameter files to overcome double opened files
      at startup

  Revision 1.15  1999/03/08 14:58:08  peter
    + prompt with dialogs for tools

  Revision 1.14  1999/03/05 17:53:00  pierre
   + saving and opening of open files on exit

  Revision 1.13  1999/03/01 15:41:48  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.12  1999/02/20 15:18:25  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.11  1999/02/18 13:44:30  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.10  1999/02/15 09:07:10  pierre
   * HEAPTRC conditionnal renamed IDEHEAPTRC

  Revision 1.9  1999/02/10 09:55:43  pierre
     + Memory tracing if compiled with -dHEAPTRC
     * Many memory leaks removed

  Revision 1.8  1999/02/08 09:30:59  florian
    + some split heap stuff, in $ifdef TEMPHEAP

  Revision 1.7  1999/02/05 13:51:38  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.6  1999/01/21 11:54:10  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.5  1999/01/12 14:29:31  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

}
