{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Main program of the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program FP;

uses
{$ifdef IDEHeapTrc}
  HeapTrc,
{$endif IDEHeapTrc}
  Dos,
  BrowCol,
  FPIni,FPViews,FPConst,FPVars,FPUtils,FPIde,FPHelp,FPSwitch,FPUsrScr,
  FPTools,FPDebug,FPTemplt,FPCatch,FPRedir
{$ifdef TEMPHEAP}
  ,dpmiexcp
{$endif TEMPHEAP}
  ;


procedure ProcessParams(BeforeINI: boolean);

  function IsSwitch(const Param: string): boolean;
  begin
    IsSwitch:=(Param<>'') and (Param[1]<>DirSep) { <- allow UNIX root-relative paths            }
          and (Param[1] in ['-','/']);           { <- but still accept dos switch char, eg. '/' }
  end;

var I: integer;
    Param: string;
begin
  for I:=1 to ParamCount do
  begin
    Param:=ParamStr(I);
    if IsSwitch(Param) then
      begin
        Param:=copy(Param,2,255);
        if Param<>'' then
        case Upcase(Param[1]) of
          'C' : { custom config file (BP compatiblity) }
           if BeforeINI then
            INIPath:=copy(Param,2,255);
          'R' : { enter the directory last exited from (BP comp.) }
            begin
              Param:=copy(Param,2,255);
              if (Param='') or (Param='+') then
                StartupOptions:=StartupOptions or soReturnToLastDir
              else
              if (Param='-') then
                StartupOptions:=StartupOptions and (not soReturnToLastDir);
            end;
        end;
      end
    else
      if not BeforeINI then
        TryToOpenFile(nil,Param,0,0,true);
  end;
end;

BEGIN
  {$ifdef DEV}HeapLimit:=4096;{$endif}
  writeln('þ Free Pascal IDE  Version '+VersionStr);
  StartupDir:=CompleteDir(FExpand('.'));
  IDEDir:=CompleteDir(DirOf(Paramstr(0)));

  ProcessParams(true);

  InitRedir;
  InitBreakpoints;
  InitReservedWords;
  InitHelpFiles;
  InitSwitches;
  InitINIFile;
  InitUserScreen;
  InitTools;
  InitTemplates;

  ReadSwitches(SwitchesPath);

  MyApp.Init;
  { load all options after init because of open files }
  ReadINIFile;
  { Update IDE }
  if PrimaryFile<>'' then
   MyApp.UpdatePrimaryFile;

  ProcessParams(false);

  MyApp.Run;

  { must be written before done for open files }
  WriteINIFile;

  MyApp.Done;

  WriteSwitches(SwitchesPath);

  DoneTemplates;
  DoneTools;
  DoneUserScreen;
  DoneSwitches;
  DoneHelpFiles;
  DoneReservedWords;
  DoneBrowserCol;
  DoneDebugger;
  DoneBreakpoints;
END.
{
  $Log$
  Revision 1.16  1999-03-12 01:13:01  peter
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
