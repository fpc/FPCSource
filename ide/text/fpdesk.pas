{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Desktop loading/saving routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPDesk;

interface

const
     DesktopVersion     = $0005; { <- if you change any Load&Store methods,
                                      default object properties (Options,State)
                                      then you should also change this }

     ResDesktopFlags    = 'FLAGS';
     ResVideo           = 'VIDEOMODE';
     ResHistory         = 'HISTORY';
     ResClipboard       = 'CLIPBOARD';
     ResWatches         = 'WATCHES';
     ResBreakpoints     = 'BREAKPOINTS';
     ResDesktop         = 'DESKTOP';
     ResSymbols         = 'SYMBOLS';

procedure InitDesktopFile;
function  LoadDesktop: boolean;
function  SaveDesktop: boolean;
procedure DoneDesktopFile;
function  WriteSymbolsFile(const filename : string): boolean;
function  ReadSymbolsFile(const filename : string): boolean;

implementation

uses Dos,
     Objects,Drivers,Video,
     Views,App,HistList,BrowCol,
     WResource,WViews,WEditor,
     WUtils,
{$ifndef NODEBUG}
     fpdebug,
{$endif ndef NODEBUG}
     FPConst,FPVars,FPUtils,FPViews,FPCompile,FPTools,FPHelp;

procedure InitDesktopFile;
begin
  if DesktopLocation=dlCurrentDir then
    DesktopPath:=FExpand(DesktopName)
  else
    DesktopPath:=FExpand(DirOf(INIPath)+DesktopName);
end;

procedure DoneDesktopFile;
begin
end;

function ReadHistory(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  PushStatus('Reading history...');
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resHistory,langDefault,S^);
  S^.Seek(0);
  if OK then
    LoadHistory(S^);
  Dispose(S, Done);
  if OK=false then
    ErrorBox('Error loading history',nil);
  PopStatus;
  ReadHistory:=OK;
end;

function WriteHistory(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  PushStatus('Storing history...');

  New(S, Init(10*1024,4096));
  StoreHistory(S^);
  S^.Seek(0);
  F^.CreateResource(resHistory,rcBinary,0);
  OK:=F^.AddResourceEntryFromStream(resHistory,langDefault,0,S^,S^.GetSize);
  Dispose(S, Done);
  if OK=false then
    ErrorBox('Error storing history',nil);
  PopStatus;
  WriteHistory:=OK;
end;

(*function ReadClipboard(F: PResourceFile): boolean;
begin
  ReadClipboard:=true;
end;

function WriteClipboard(F: PResourceFile): boolean;
var S: PMemoryStream;
begin
  if Assigned(Clipboard) then
  begin
    PushStatus('Storing clipboard content...');

    New(S, Init(10*1024,4096));
    Clipboard^.SaveToStream(S^);
    S^.Seek(0);
    F^.CreateResource(resClipboard,rcBinary,0);
    F^.AddResourceEntryFromStream(resClipboard,langDefault,0,S^,S^.GetSize);
    Dispose(S, Done);
    PopStatus;
  end;
  WriteClipboard:=true;
end;*)

function ReadWatches(F: PResourceFile): boolean;
{$ifndef NODEBUG}
var S: PMemoryStream;
    OK: boolean;
    OWC : PWatchesCollection;
{$endif}
begin
{$ifndef NODEBUG}
  PushStatus('Reading watches...');
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resWatches,langDefault,S^);
  S^.Seek(0);
  if OK then
    begin
      OWC:=WatchesCollection;
      WatchesCollection:=PWatchesCollection(S^.Get);
      OK:=(S^.Status=stOK);
      if OK and assigned(OWC) and assigned(WatchesCollection) then
        Dispose(OWC,Done)
      else if assigned(OWC) then
        WatchesCollection:=OWC;
    end;
  if OK=false then
    ErrorBox('Error loading watches',nil);
  ReadWatches:=OK;
  Dispose(S, Done);
  PopStatus;
{$else NODEBUG}
  ReadWatches:=true;
{$endif NODEBUG}
end;

function WriteWatches(F: PResourceFile): boolean;
var
  S : PMemoryStream;
  OK : boolean;
begin
{$ifndef NODEBUG}
  if not assigned(WatchesCollection) then
{$endif NODEBUG}
    WriteWatches:=true
{$ifndef NODEBUG}
  else
    begin
      PushStatus('Storing watches...');
      New(S, Init(30*1024,4096));
      S^.Put(WatchesCollection);
      S^.Seek(0);
      F^.CreateResource(resWatches,rcBinary,0);
      OK:=F^.AddResourceEntryFromStream(resWatches,langDefault,0,S^,S^.GetSize);
      Dispose(S, Done);
      if OK=false then
        ErrorBox('Error storing watches',nil);
      PopStatus;
      WriteWatches:=OK;
    end;
{$endif NODEBUG}
end;

function ReadBreakpoints(F: PResourceFile): boolean;
{$ifndef NODEBUG}
var S: PMemoryStream;
    OK: boolean;
    OBC : PBreakpointCollection;
{$endif}
begin
{$ifndef NODEBUG}
  PushStatus('Reading breakpoints...');
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resBreakpoints,langDefault,S^);
  S^.Seek(0);
  if OK then
    begin
      OBC:=BreakpointsCollection;
      BreakpointsCollection:=PBreakpointCollection(S^.get);
      OK:=(S^.Status=stOK);

      If OK and assigned(OBC) and assigned(BreakpointsCollection) then
        Dispose(OBC,Done)
      else if assigned(OBC) then
        BreakpointsCollection:=OBC;
    end;
  if OK=false then
    ErrorBox('Error loading breakpoints',nil);
  ReadBreakpoints:=OK;
  Dispose(S, Done);
  PopStatus;
{$else NODEBUG}
  ReadBreakpoints:=true;
{$endif NODEBUG}
end;

function WriteBreakpoints(F: PResourceFile): boolean;
var
  S : PMemoryStream;
  OK : boolean;
begin
{$ifndef NODEBUG}
  if not assigned(BreakpointsCollection) then
{$endif NODEBUG}
    WriteBreakPoints:=true
{$ifndef NODEBUG}
  else
    begin
      PushStatus('Storing breakpoints...');
      New(S, Init(30*1024,4096));
      BreakpointsCollection^.Store(S^);
      S^.Seek(0);
      F^.CreateResource(resBreakpoints,rcBinary,0);
      OK:=F^.AddResourceEntryFromStream(resBreakpoints,langDefault,0,S^,S^.GetSize);
      Dispose(S, Done);
      if OK=false then
        ErrorBox('Error storing breakpoints',nil);
      WriteBreakPoints:=OK;
      PopStatus;
    end;
{$endif NODEBUG}
end;

function ReadOpenWindows(F: PResourceFile): boolean;
var S: PMemoryStream;
    TempDesk: PFPDesktop;
    OK: boolean;
    R : TRect;
    W: word;
begin
  PushStatus('Reading desktop contents...');
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resDesktop,langDefault,S^);
  S^.Seek(0);
  if OK then
  begin
    S^.Read(W,SizeOf(W));
    OK:=(W=DesktopVersion);
    if OK=false then
      ErrorBox('Invalid desktop version. Desktop layout lost.',nil);
  end;
  if OK then
    begin
      TempDesk:=PFPDesktop(S^.Get);
      OK:=Assigned(TempDesk);
      if OK then
        begin
          Dispose(Desktop, Done);
          Desktop:=TempDesk;

          with Desktop^ do
          begin
            GetSubViewPtr(S^,CompilerMessageWindow);
            GetSubViewPtr(S^,CompilerStatusDialog);
            GetSubViewPtr(S^,ClipboardWindow);
            if Assigned(ClipboardWindow) then Clipboard:=ClipboardWindow^.Editor;
            GetSubViewPtr(S^,CalcWindow);
            GetSubViewPtr(S^,ProgramInfoWindow);
            GetSubViewPtr(S^,GDBWindow);
            GetSubViewPtr(S^,BreakpointsWindow);
            GetSubViewPtr(S^,WatchesWindow);
            GetSubViewPtr(S^,UserScreenWindow);
            GetSubViewPtr(S^,ASCIIChart);
            GetSubViewPtr(S^,MessagesWindow); LastToolMessageFocused:=nil;
          end;
          Application^.GetExtent(R);
          Inc(R.A.Y);Dec(R.B.Y);
          DeskTop^.Locate(R);
          Application^.Insert(Desktop);
          Desktop^.ReDraw;
          Message(Application,evBroadcast,cmUpdate,nil);
        end;
      if OK=false then
        ErrorBox('Error loading desktop',nil);
    end;
  Dispose(S, Done);
  PopStatus;
  ReadOpenWindows:=OK;
end;

function WriteOpenWindows(F: PResourceFile): boolean;
var S: PMemoryStream;
    W: word;
    OK: boolean;
begin
  PushStatus('Storing desktop contents...');

  New(S, Init(30*1024,4096));
  OK:=Assigned(S);
  if OK then
  begin
    W:=DesktopVersion;
    S^.Write(W,SizeOf(W));
    S^.Put(Desktop);
    with Desktop^ do
    begin
      PutSubViewPtr(S^,CompilerMessageWindow);
      PutSubViewPtr(S^,CompilerStatusDialog);
      PutSubViewPtr(S^,ClipboardWindow);
      PutSubViewPtr(S^,CalcWindow);
      PutSubViewPtr(S^,ProgramInfoWindow);
      PutSubViewPtr(S^,GDBWindow);
      PutSubViewPtr(S^,BreakpointsWindow);
      PutSubViewPtr(S^,WatchesWindow);
      PutSubViewPtr(S^,UserScreenWindow);
      PutSubViewPtr(S^,ASCIIChart);
      PutSubViewPtr(S^,MessagesWindow);
    end;
    OK:=(S^.Status=stOK);
    if OK then
    begin
      S^.Seek(0);
      OK:=F^.CreateResource(resDesktop,rcBinary,0);
      OK:=OK and F^.AddResourceEntryFromStream(resDesktop,langDefault,0,S^,S^.GetSize);
    end;
    Dispose(S, Done);
  end;
  if OK=false then
    ErrorBox('Error storing desktop',nil);
  PopStatus;
  WriteOpenWindows:=OK;
end;

function WriteFlags(F: PResourceFile): boolean;
var
    OK: boolean;
begin
  F^.CreateResource(resDesktopFlags,rcBinary,0);
  OK:=F^.AddResourceEntry(resDesktopFlags,langDefault,0,DesktopFileFlags,
    SizeOf(DesktopFileFlags));
  if OK=false then
    ErrorBox('Error writing flags',nil);
  WriteFlags:=OK;
end;

function ReadFlags(F: PResourceFile): boolean;
var
  size : sw_word;
    OK: boolean;
begin
  OK:=F^.ReadResourceEntry(resDesktopFlags,langDefault,DesktopFileFlags,
    size);
  if OK=false then
    ErrorBox('Error loading flags',nil);
  ReadFlags:=OK;
end;

function WriteVideoMode(F: PResourceFile): boolean;
var
    OK: boolean;
begin
  F^.CreateResource(resVideo,rcBinary,0);
  OK:=F^.AddResourceEntry(resVideo,langDefault,0,ScreenMode,
    SizeOf(TVideoMode));
  if OK=false then
    ErrorBox('Error storing video mode',nil);
  WriteVideoMode:=OK;
end;

function ReadVideoMode(F: PResourceFile;var NewScreenMode : TVideoMode): boolean;
var
  size : sw_word;
  OK,test : boolean;
begin
  size:=SizeOf(TVideoMode);
  test:=F^.ReadResourceEntry(resVideo,langDefault,NewScreenMode,
    size);
  if not test then
    NewScreenMode:=ScreenMode;
  OK:=test and (size = SizeOf(TVideoMode));
  if OK=false then
    ErrorBox('Error loading video mode',nil);
  ReadVideoMode:=OK;
end;

function ReadSymbols(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  { if no symbols stored ... no problems }
  if not Assigned(F^.FindResource(resSymbols)) then
    exit;
  PushStatus('Reading symbol information...');
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resSymbols,langDefault,S^);
  S^.Seek(0);
  if OK then
    OK:=LoadBrowserCol(S);
  Dispose(S, Done);
  if OK=false then
    ErrorBox('Error loading symbol information',nil);
  PopStatus;
  ReadSymbols:=OK;
end;

function WriteSymbols(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  OK:=Assigned(Modules);

  if OK then
  begin
    PushStatus('Storing symbol information...');

    New(S, Init(200*1024,4096));
    OK:=Assigned(S);
    if OK then
      OK:=StoreBrowserCol(S);
    if OK then
      begin
        S^.Seek(0);
        F^.CreateResource(resSymbols,rcBinary,0);
        OK:=F^.AddResourceEntryFromStream(resSymbols,langDefault,0,S^,S^.GetSize);
      end;
    Dispose(S, Done);
    if OK=false then
      ErrorBox('Error storing symbol information',nil);
    PopStatus;
  end;
  WriteSymbols:=OK;
end;

function LoadDesktop: boolean;
var OK,VOK: boolean;
    F: PResourceFile;
    VM : TVideoMode;
begin
  PushStatus('Reading desktop file...');
  New(F, LoadFile(GetShortName(DesktopPath)));

  OK:=false;

  if Assigned(F) then
  begin
    OK:=ReadFlags(F);
    VOK:=ReadVideoMode(F,VM);
    if VOK and ((VM.Col<>ScreenMode.Col) or
       (VM.Row<>ScreenMode.Row) or (VM.Color<>ScreenMode.Color)) then
      Application^.SetScreenVideoMode(VM);
    if ((DesktopFileFlags and dfHistoryLists)<>0) then
      OK:=OK and ReadHistory(F);
    if ((DesktopFileFlags and dfWatches)<>0) then
      OK:=OK and ReadWatches(F);
    if ((DesktopFileFlags and dfBreakpoints)<>0) then
      OK:=OK and ReadBreakpoints(F);
    if ((DesktopFileFlags and dfOpenWindows)<>0) then
      OK:=OK and ReadOpenWindows(F);
    { no errors if no browser info available PM }
    if ((DesktopFileFlags and dfSymbolInformation)<>0) then
      OK:=OK and ReadSymbols(F);
    Dispose(F, Done);
  end;

  PopStatus;
  LoadDesktop:=OK;
end;

function SaveDesktop: boolean;
var OK: boolean;
    F: PResourceFile;
    TempPath: string;
begin
  TempPath:=DirOf(DesktopPath)+DesktopTempName;
  PushStatus('Writing desktop file...');
  New(F, CreateFile(GetShortName(TempPath)));

  if Assigned(Clipboard) then
    if (DesktopFileFlags and dfClipboardContent)<>0 then
      Clipboard^.Flags:=Clipboard^.Flags or efStoreContent
    else
      Clipboard^.Flags:=Clipboard^.Flags and not efStoreContent;
  OK:=false;

  if Assigned(F) then
    begin
      OK:=WriteFlags(F);
      OK:=OK and WriteVideoMode(F);
      if ((DesktopFileFlags and dfHistoryLists)<>0) then
        OK:=OK and WriteHistory(F);
      if ((DesktopFileFlags and dfWatches)<>0) then
        OK:=OK and WriteWatches(F);
      if ((DesktopFileFlags and dfBreakpoints)<>0) then
        OK:=OK and WriteBreakpoints(F);
      if ((DesktopFileFlags and dfOpenWindows)<>0) then
        OK:=OK and WriteOpenWindows(F);
      { no errors if no browser info available PM }
      if ((DesktopFileFlags and dfSymbolInformation)<>0) then
        OK:=OK and (WriteSymbols(F) or not Assigned(Modules));
      Dispose(F, Done);
    end;
  if OK then
    begin
      if ExistsFile(DesktopPath) then
        OK:=EraseFile(DesktopPath);
      OK:=OK and RenameFile(TempPath,DesktopPath);
      if OK=false then
        ErrorBox('Failed to replace desktop file.',nil);
    end;
  PopStatus;
  SaveDesktop:=OK;
end;

function  WriteSymbolsFile(const filename : string): boolean;
var OK: boolean;
    F: PResourceFile;
begin
  WriteSymbolsFile:=false;
  If not assigned(Modules) then
    exit;
  New(F, CreateFile(GetShortName(FileName)));
  OK:=Assigned(F);
  if OK and ((DesktopFileFlags and dfSymbolInformation)<>0) then
    OK:=OK and WriteSymbols(F);
  if assigned(F) then
    Dispose(F,Done);
  WriteSymbolsFile:=OK;
end;

function  ReadSymbolsFile(const FileName : string): boolean;
var OK: boolean;
    F: PResourceFile;
begin
  ReadSymbolsFile:=false;
  { Don't read again !! }
  If assigned(Modules) then
    exit;
  New(F, LoadFile(GetShortName(FileName)));
  OK:=Assigned(F);
  if OK and ((DesktopFileFlags and dfSymbolInformation)<>0) then
      OK:=OK and ReadSymbols(F);
  if assigned(F) then
    Dispose(F,Done);
  ReadSymbolsFile:=OK;
end;

END.
{
  $Log$
  Revision 1.19  2000-01-25 00:26:36  pierre
   + Browser info saving

  Revision 1.18  2000/01/03 11:38:33  michael
  Changes from Gabor

  Revision 1.17  1999/12/20 00:30:56  pierre
   * problem with VideoMode storing solved

  Revision 1.16  1999/12/10 13:02:05  pierre
  + VideoMode save/restore

  Revision 1.15  1999/11/26 17:09:51  pierre
   * Force Desktop into Screen

  Revision 1.14  1999/11/25 00:25:43  pierre
   * add Status when loading/saving files

  Revision 1.13  1999/09/20 15:37:59  pierre
   * ReadOpenWindows and ReadSymobls was missing, still does not work correctly :(

  Revision 1.12  1999/09/17 16:41:10  pierre
   * other stream error for Watches/Breakpoints corrected

  Revision 1.11  1999/09/17 16:28:58  pierre
   * ResWatches in WriteBreakpoints typo !

  Revision 1.10  1999/09/16 14:34:58  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.9  1999/09/07 09:23:00  pierre
   * no errors if no browser info available

  Revision 1.8  1999/08/16 18:25:16  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.7  1999/08/03 20:22:30  peter
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

  Revision 1.5  1999/06/30 23:58:13  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.4  1999/04/15 08:58:05  peter
    * syntax highlight fixes
    * browser updates

  Revision 1.3  1999/04/07 21:55:45  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.2  1999/03/23 16:16:39  peter
    * linux fixes

  Revision 1.1  1999/03/23 15:11:28  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

}