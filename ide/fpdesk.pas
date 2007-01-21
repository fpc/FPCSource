{
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
     MinDesktopVersion  = $000A;
     DesktopVersion     = $000A; { <- if you change any Load&Store methods,
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
     ResCodeComplete    = 'CODECOMPLETE';
     ResCodeTemplates   = 'CODETEMPLATES';
     ResKeys            = 'KEYS';

procedure InitDesktopFile;
function  LoadDesktop: boolean;
function  SaveDesktop: boolean;
procedure DoneDesktopFile;
function  WriteSymbolsFile(const filename : string): boolean;
function  ReadSymbolsFile(const filename : string): boolean;

implementation

uses Dos,
     Objects,Drivers,
     Video,
     Views,App,HistList,BrowCol,
     WUtils,WResourc,WViews,WEditor,
     fpdebug,
{$ifdef Unix}
     FPKeys,
{$endif Unix}
     FPConst,FPVars,FPTools,FPUtils,FPViews,FPHelp,
     FPCompil,FPCodCmp,FPCodTmp;

type
     TWindowInfo =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
     packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
     record
       HelpCtx   : word;
       Bounds    : TRect;
       Visible   : boolean;
       WinNb     : byte;
       ExtraDataSize : word;
       TitleLen  : word;
       Title     : packed record end;
     end;

{$ifdef useresstrings}
resourcestring
{$else}
const
{$endif}
      { Desktop file messages }
      msg_readingdesktopfile = 'Reading desktop file...';
      msg_writingdesktopfile = 'Writing desktop file...';
      msg_readingdesktopcontents = 'Reading desktop contents...';
      msg_storingdesktopcontents = 'Storing desktop contents...';
      msg_readinghistory = 'Reading history...';
      msg_storinghistory = 'Storing history...';
      msg_readingwatches = 'Reading watches...';
      msg_storingwatches = 'Storing watches...';
      msg_readingbreakpoints = 'Reading breakpoints...';
      msg_storingbreakpoints = 'Storing breakpoints...';
      msg_readingcodecompletewordlist = 'Reading CodeComplete wordlist...';
      msg_storingcodecompletewordlist = 'Writing CodeComplete wordlist...';
      msg_readingcodetemplates = 'Reading CodeTemplates...';
      msg_storingcodetemplates = 'Writing CodeTemplates...';
      msg_readingsymbolinformation = 'Reading symbol information...';
      msg_storingsymbolinformation = 'Storing symbol information...';
      msg_failedtoreplacedesktopfile = 'Failed to replace desktop file.';
      msg_errorloadinghistory = 'Error loading history';
      msg_errorstoringhistory = 'Error storing history';
      msg_errorloadingkeys = 'Error loading custom keys';
      msg_errorstoringkeys = 'Error storing custom keys';
      msg_errorloadingwatches = 'Error loading watches';
      msg_errorstoringwatches = 'Error storing watches';
      msg_errorloadingbreakpoints = 'Error loading breakpoints';
      msg_errorstoringbreakpoints = 'Error storing breakpoints';
      msg_errorloadingdesktop = 'Error loading desktop';
      msg_errorstoringdesktop = 'Error storing desktop';
      msg_errorreadingflags = 'Error loading flags';
      msg_errorwritingflags = 'Error writing flags';
      msg_errorreadingvideomode = 'Error reading video mode';
      msg_errorstoringvideomode = 'Error storing video mode';
      msg_errorloadingcodetemplates = 'Error loading CodeTemplates';
      msg_errorstoringcodetemplates = 'Error writing CodeTemplates';
      msg_errorloadingsymbolinformation = 'Error loading symbol information';
      msg_errorstoringsymbolinformation = 'Error storing symbol information';
      msg_errorloadingcodecompletewordlist = 'Error loading CodeComplete wordlist';
      msg_errorstoringcodecompletewordlist = 'Error writing CodeComplete wordlist';
      msg_invaliddesktopversionlayoutlost = 'Invalid desktop version. Desktop layout lost.';
      msg_saveansifile = 'Save previous screen as Ansi File';
      msg_click_upper_left = 'Click to select upper left corner; Escape to cancel; Enter to select (0,0)';
      msg_click_lower_right = 'Click to select lower right corner; Escape to cancel; Enter to select (maxX,maxY)';

      msg_cantopenfile = 'Can''t open %s';
      msg_cantcreatefile = 'Can''t create %s';
      msg_cantfindfile = 'Can''t find %s';
      msg_errorreadingfile = 'Error reading file %s';
      msg_loadingfile = 'Loading %s';
      msg_storingfile = 'Storing %s';
      msg_closingfile = 'Closing %s';

      msg_openingsourcefile = 'Opening source file... (%s)';
      msg_readingfileineditor = 'Reading %s into editor...';

procedure InitDesktopFile;
begin
  if DesktopLocation=dlCurrentDir then
    DesktopPath:=FExpand(DesktopName)
  else
    DesktopPath:=FExpand(DirOf(IniFileName)+DesktopName);
end;

procedure DoneDesktopFile;
begin
end;

function ReadHistory(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  PushStatus(msg_readinghistory);
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resHistory,langDefault,S^);
  S^.Seek(0);
  if OK then
    LoadHistory(S^);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorloadinghistory,nil);
  PopStatus;
  ReadHistory:=OK;
end;

function WriteHistory(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  PushStatus(msg_storinghistory);

  New(S, Init(10*1024,4096));
  StoreHistory(S^);
  S^.Seek(0);
  F^.CreateResource(resHistory,rcBinary,0);
  OK:=F^.AddResourceEntryFromStream(resHistory,langDefault,0,S^,S^.GetSize);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorstoringhistory,nil);
  PopStatus;
  WriteHistory:=OK;
end;

{$ifdef Unix}
function ReadKeys(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resKeys,langDefault,S^);
  S^.Seek(0);
  if OK then
    LoadKeys(S^);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorloadingkeys,nil);
  ReadKeys:=OK;
end;

function WriteKeys(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  New(S, Init(10*1024,4096));
  StoreKeys(S^);
  S^.Seek(0);
  F^.CreateResource(resKeys,rcBinary,0);
  OK:=F^.AddResourceEntryFromStream(resKeys,langDefault,0,S^,S^.GetSize);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorstoringkeys,nil);
  WriteKeys:=OK;
end;
{$endif Unix}

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
  PushStatus(msg_readingwatches);
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
    ErrorBox(msg_errorloadingwatches,nil);
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
      PushStatus(msg_storingwatches);
      New(S, Init(30*1024,4096));
      S^.Put(WatchesCollection);
      S^.Seek(0);
      F^.CreateResource(resWatches,rcBinary,0);
      OK:=F^.AddResourceEntryFromStream(resWatches,langDefault,0,S^,S^.GetSize);
      Dispose(S, Done);
      if OK=false then
        ErrorBox(msg_errorstoringwatches,nil);
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
  PushStatus(msg_readingbreakpoints);
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resBreakpoints,langDefault,S^);
  S^.Seek(0);
  if OK then
    begin
      OBC:=BreakpointsCollection;
      BreakpointsCollection:=PBreakpointCollection(S^.get);
      OK:=(S^.Status=stOK);

      If OK and assigned(OBC) and assigned(BreakpointsCollection) then
        Begin
          Dispose(OBC,Done);
          BreakpointsCollection^.ShowAllBreakpoints;
        end
      else if assigned(OBC) then
        BreakpointsCollection:=OBC;
    end;
  if OK=false then
    ErrorBox(msg_errorloadingbreakpoints,nil);
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
      PushStatus(msg_storingbreakpoints);
      New(S, Init(30*1024,4096));
      S^.Put(BreakpointsCollection);
      S^.Seek(0);
      F^.CreateResource(resBreakpoints,rcBinary,0);
      OK:=F^.AddResourceEntryFromStream(resBreakpoints,langDefault,0,S^,S^.GetSize);
      Dispose(S, Done);
      if OK=false then
        ErrorBox(msg_errorstoringbreakpoints,nil);
      WriteBreakPoints:=OK;
      PopStatus;
    end;
{$endif NODEBUG}
end;

function ReadOpenWindows(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
    DV: word;
    WI: TWindowInfo;
    Title: string;
    XDataOfs: word;
    XData: array[0..1024] of byte;
procedure GetData(var B; Size: word);
begin
  Move(XData[XDataOfs],B,Size);
  Inc(XDataOfs,Size);
end;
procedure ProcessWindowInfo;
var W: PWindow;
    SW: PSourceWindow absolute W;
    St: string;
    Ch: char;
    TP,TP2: TPoint;
    L: longint;
    R: TRect;
begin
  XDataOfs:=0;
  Desktop^.Lock;
  W:=SearchWindow(Title);
  case WI.HelpCtx of
    hcSourceWindow :
      begin
        GetData(St[0],1);
        GetData(St[1],ord(St[0]));
        W:=ITryToOpenFile(@WI.Bounds,St,0,0,false,false,true);
        if Assigned(W)=false then
          begin
            ClearFormatParams;
            AddFormatParamStr(St);
            Desktop^.Unlock;
            ErrorBox(msg_cantopenfile,@FormatParams);
            Desktop^.Lock;
          end
        else
        begin
          GetData(L,sizeof(L)); SW^.Editor^.SetFlags(L);
          GetData(TP,sizeof(TP)); GetData(TP2,sizeof(TP2));
          SW^.Editor^.SetSelection(TP,TP2);
          GetData(TP,sizeof(TP)); SW^.Editor^.SetCurPtr(TP.X,TP.Y);
          GetData(TP,sizeof(TP)); SW^.Editor^.ScrollTo(TP.X,TP.Y);
        end;
      end;
     hcClipboardWindow:
       W:=ClipboardWindow;
     hcCalcWindow:
       W:=CalcWindow;
     hcMessagesWindow:
       begin
         if MessagesWindow=nil then
           Desktop^.Insert(New(PMessagesWindow, Init));
         W:=MessagesWindow;
       end;
     hcCompilerMessagesWindow:
       W:=CompilerMessageWindow;
{$ifndef NODEBUG}
     hcGDBWindow:
       begin
         InitGDBWindow;
         W:=GDBWindow;
       end;
     hcDisassemblyWindow:
       begin
         InitDisassemblyWindow;
         W:=DisassemblyWindow;
       end;
     hcWatchesWindow:
       begin
         if WatchesWindow=nil then
           begin
             New(WatchesWindow,Init);
             Desktop^.Insert(WatchesWindow);
           end;
         W:=WatchesWindow;
       end;
     hcStackWindow:
       begin
         if StackWindow=nil then
           begin
             New(StackWindow,Init);
             Desktop^.Insert(StackWindow);
           end;
         W:=StackWindow;
       end;
     hcFPURegisters:
       begin
         if FPUWindow=nil then
           begin
             New(FPUWindow,Init);
             Desktop^.Insert(FPUWindow);
           end;
         W:=FPUWindow;
       end;
     hcVectorRegisters:
       begin
         if VectorWindow=nil then
           begin
             New(VectorWindow,Init);
             Desktop^.Insert(VectorWindow);
           end;
         W:=VectorWindow;
       end;
     hcRegistersWindow:
       begin
         if RegistersWindow=nil then
           begin
             New(RegistersWindow,Init);
             Desktop^.Insert(RegistersWindow);
           end;
         W:=RegistersWindow;
       end;
     hcBreakpointListWindow:
       begin
         if BreakpointsWindow=nil then
           begin
             New(BreakpointsWindow,Init);
             Desktop^.Insert(BreakpointsWindow);
           end;
         W:=BreakpointsWindow;
       end;
{$endif NODEBUG}
     hcASCIITableWindow:
       begin
         if ASCIIChart=nil then
           begin
             New(ASCIIChart, Init);
             Desktop^.Insert(ASCIIChart);
           end;
         W:=ASCIIChart;
         if DV>=$A then
           begin
             GetData(ch,sizeof(char));
             AsciiChart^.Report^.AsciiChar:=ord(ch);
             AsciiChart^.Table^.SetCursor(
               ord(ch) mod AsciiChart^.Table^.Size.X,
               ord(ch) div AsciiChart^.Table^.Size.X);
           end;
      end;
  end;
  if W=nil then
    begin
      Desktop^.Unlock;
      Exit;
    end;
  W^.GetBounds(R);
  if (R.A.X<>WI.Bounds.A.X) or (R.A.Y<>WI.Bounds.A.Y) then
    R.Move(WI.Bounds.A.X-R.A.X,WI.Bounds.A.Y-R.A.Y);
  if (W^.Flags and wfGrow)<>0 then
    begin
      R.B.X:=R.A.X+(WI.Bounds.B.X-WI.Bounds.A.X);
      R.B.Y:=R.A.Y+(WI.Bounds.B.Y-WI.Bounds.A.Y);
    end;
  W^.Locate(R);
  if W^.GetState(sfVisible)<>WI.Visible then
    if WI.Visible then
      begin
        W^.Show;
        W^.MakeFirst;
      end
    else
      W^.Hide;
  W^.Number:=WI.WinNb;
  Desktop^.Unlock;
end;
begin
  PushStatus(msg_readingdesktopcontents);
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resDesktop,langDefault,S^);
  S^.Seek(0);
  if OK then
  begin
    S^.Read(DV,SizeOf(DV));
    OK:=(DV=DesktopVersion) or (DV>=MinDesktopVersion);
    if OK=false then
      ErrorBox(msg_invaliddesktopversionlayoutlost,nil);
  end;
  if OK then
    begin
      XDataOfs:=0;
      repeat
        S^.Read(WI,sizeof(WI));
        if S^.Status=stOK then
        begin
          Title[0]:=chr(WI.TitleLen);
          S^.Read(Title[1],WI.TitleLen);
          if WI.ExtraDataSize>0 then
          S^.Read(XData,WI.ExtraDataSize);
          ProcessWindowInfo;
        end;
      until (S^.Status<>stOK) or (S^.GetPos=S^.GetSize);
(*      TempDesk:=PFPDesktop(S^.Get);
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
        end;*)
      if OK=false then
        ErrorBox(msg_errorloadingdesktop,nil);
    end;
  Dispose(S, Done);
  PopStatus;
  ReadOpenWindows:=OK;
end;

function WriteOpenWindows(F: PResourceFile): boolean;
var S: PMemoryStream;
procedure CollectInfo(P: PView); {$ifndef FPC}far;{$endif}
var W: PWindow;
    SW: PSourceWindow absolute W;
    WI: TWindowInfo;
    Title: string;
    XDataOfs: word;
    XData: array[0..1024] of byte;
    St: string;
    Ch: char;
    TP: TPoint;
    L: longint;
procedure AddData(const B; Size: word);
begin
  Move(B,XData[XDataOfs],Size);
  Inc(XDataOfs,Size);
end;
begin
  XDataOfs:=0;
  W:=nil;
  if (P^.HelpCtx=hcSourceWindow) or
     (P^.HelpCtx=hcHelpWindow) or
     (P^.HelpCtx=hcClipboardWindow) or
     (P^.HelpCtx=hcCalcWindow) or
     (P^.HelpCtx=hcInfoWindow) or
     (P^.HelpCtx=hcBrowserWindow) or
     (P^.HelpCtx=hcMessagesWindow) or
     (P^.HelpCtx=hcCompilerMessagesWindow) or
     (P^.HelpCtx=hcGDBWindow) or
     (P^.HelpCtx=hcDisassemblyWindow) or
     (P^.HelpCtx=hcStackWindow) or
     (P^.HelpCtx=hcRegistersWindow) or
     (P^.HelpCtx=hcFPURegisters) or
     (P^.HelpCtx=hcVectorRegisters) or
     (P^.HelpCtx=hcWatchesWindow) or
     (P^.HelpCtx=hcBreakpointListWindow) or
     (P^.HelpCtx=hcASCIITableWindow)
   then
     W:=PWindow(P);

  if Assigned(W) and (P^.HelpCtx=hcSourceWindow) then
    if SW^.Editor^.FileName='' then
      W:=nil;

  if W=nil then Exit;
  FillChar(WI,sizeof(WI),0);
  Title:=W^.GetTitle(255);
  WI.HelpCtx:=W^.HelpCtx;
  W^.GetBounds(WI.Bounds);
  WI.Visible:=W^.GetState(sfVisible);
  WI.WinNb:=W^.Number;
  case WI.HelpCtx of
    hcSourceWindow :
      begin
        St:=SW^.Editor^.FileName; AddData(St,length(St)+1);
        L:=SW^.Editor^.GetFlags; AddData(L,sizeof(L));
        TP:=SW^.Editor^.SelStart; AddData(TP,sizeof(TP));
        TP:=SW^.Editor^.SelEnd; AddData(TP,sizeof(TP));
        TP:=SW^.Editor^.CurPos; AddData(TP,sizeof(TP));
        TP:=SW^.Editor^.Delta; AddData(TP,sizeof(TP));
      end;
    hcAsciiTableWindow :
      begin
        ch:=chr(PFPAsciiChart(P)^.Report^.AsciiChar);
        AddData(ch,sizeof(char));
      end;
  end;

  WI.TitleLen:=length(Title);
  WI.ExtraDataSize:=XDataOfs;
  S^.Write(WI,sizeof(WI));
  S^.Write(Title[1],WI.TitleLen);
  if WI.ExtraDataSize>0 then
    S^.Write(XData,WI.ExtraDataSize);
end;
var W: word;
    OK: boolean;
    PV: PView;
begin
  PushStatus(msg_storingdesktopcontents);

  New(S, Init(30*1024,4096));
  OK:=Assigned(S);
  if OK then
  begin
    W:=DesktopVersion;
    S^.Write(W,SizeOf(W));
{    S^.Put(Desktop);
    with Desktop^ do
    begin
      PutSubViewPtr(S^,CompilerMessageWindow);
      PutSubViewPtr(S^,CompilerStatusDialog);
      PutSubViewPtr(S^,ClipboardWindow);
      PutSubViewPtr(S^,CalcWindow);
      PutSubViewPtr(S^,GDBWindow);
      PutSubViewPtr(S^,BreakpointsWindow);
      PutSubViewPtr(S^,WatchesWindow);
      PutSubViewPtr(S^,UserScreenWindow);
      PutSubViewPtr(S^,ASCIIChart);
      PutSubViewPtr(S^,MessagesWindow);
    end;}
{    PV:=Application^.Last;
    while PV<>nil do
    begin
      CollectInfo(PV);
      PV:=PV^.PrevView;
    end;}
    PV:=Desktop^.Last;
    while PV<>nil do
    begin
      CollectInfo(PV);
      PV:=PV^.PrevView;
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
    ErrorBox(msg_errorstoringdesktop,nil);
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
    ErrorBox(msg_errorwritingflags,nil);
  WriteFlags:=OK;
end;

function ReadCodeComplete(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  PushStatus(msg_readingcodecompletewordlist);
  New(S, Init(1024,1024));
  OK:=F^.ReadResourceEntryToStream(resCodeComplete,langDefault,S^);
  S^.Seek(0);
  if OK then
    OK:=LoadCodeComplete(S^);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorloadingcodecompletewordlist,nil);
  PopStatus;
  ReadCodeComplete:=OK;
end;

function WriteCodeComplete(F: PResourceFile): boolean;
var OK: boolean;
    S: PMemoryStream;
begin
  PushStatus(msg_storingcodecompletewordlist);
  New(S, Init(1024,1024));
  OK:=StoreCodeComplete(S^);
  if OK then
  begin
    S^.Seek(0);
    F^.CreateResource(resCodeComplete,rcBinary,0);
    OK:=F^.AddResourceEntryFromStream(resCodeComplete,langDefault,0,S^,S^.GetSize);
  end;
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorstoringcodecompletewordlist,nil);
  PopStatus;
  WriteCodeComplete:=OK;
end;

function ReadCodeTemplates(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
begin
  PushStatus(msg_readingcodetemplates);
  New(S, Init(1024,4096));
  OK:=F^.ReadResourceEntryToStream(resCodeTemplates,langDefault,S^);
  S^.Seek(0);
  if OK then
    OK:=LoadCodeTemplates(S^);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorloadingcodetemplates,nil);
  PopStatus;
  ReadCodeTemplates:=OK;
end;

function WriteCodeTemplates(F: PResourceFile): boolean;
var OK: boolean;
    S: PMemoryStream;
begin
  PushStatus(msg_storingcodetemplates);
  New(S, Init(1024,4096));
  OK:=StoreCodeTemplates(S^);
  if OK then
  begin
    S^.Seek(0);
    F^.CreateResource(resCodeTemplates,rcBinary,0);
    OK:=F^.AddResourceEntryFromStream(resCodeTemplates,langDefault,0,S^,S^.GetSize);
  end;
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorstoringcodetemplates,nil);
  PopStatus;
  WriteCodeTemplates:=OK;
end;

function ReadFlags(F: PResourceFile): boolean;
var
  OK: boolean;
begin
  OK:=F^.ReadResourceEntry(resDesktopFlags,langDefault,DesktopFileFlags,
    sizeof(DesktopFileFlags));
  if OK=false then
    ErrorBox(msg_errorreadingflags,nil);
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
    ErrorBox(msg_errorstoringvideomode,nil);
  WriteVideoMode:=OK;
end;

function ReadVideoMode(F: PResourceFile;var NewScreenMode : TVideoMode): boolean;
var
  OK,test : boolean;
begin
  test:=F^.ReadResourceEntry(resVideo,langDefault,NewScreenMode,
    sizeof(NewScreenMode));
  if not test then
    NewScreenMode:=ScreenMode;
  OK:=test;
  if OK=false then
    ErrorBox(msg_errorreadingvideomode,nil);
  ReadVideoMode:=OK;
end;

function ReadSymbols(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
    R: PResource;
begin
  ReadSymbols:=false;  { if no symbols stored ... no problems }
  R:=F^.FindResource(resSymbols);
  if not Assigned(R) then
    exit;
  PushStatus(msg_readingsymbolinformation);
  New(S, Init(32*1024,4096));
  OK:=F^.ReadResourceEntryToStream(resSymbols,langDefault,S^);
  S^.Seek(0);
  if OK then
    OK:=LoadBrowserCol(S);
  Dispose(S, Done);
  if OK=false then
    ErrorBox(msg_errorloadingsymbolinformation,nil);
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
    PushStatus(msg_storingsymbolinformation);

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
      ErrorBox(msg_errorstoringsymbolinformation,nil);
    PopStatus;
  end;
  WriteSymbols:=OK;
end;

function LoadDesktop: boolean;
var OK,VOK: boolean;
    F: PResourceFile;
    VM : TVideoMode;
begin
  PushStatus(msg_readingdesktopfile);
  New(F, LoadFile(DesktopPath));

  OK:=false;

  if Assigned(F) then
  begin
    OK:=ReadFlags(F);
    VOK:=ReadVideoMode(F,VM);
    if VOK and ((VM.Col<>ScreenMode.Col) or
       (VM.Row<>ScreenMode.Row) or (VM.Color<>ScreenMode.Color)) then
      begin
        if Assigned(Application) then
          Application^.SetScreenVideoMode(VM);
      end;
    if ((DesktopFileFlags and dfHistoryLists)<>0) then
      OK:=ReadHistory(F) and OK;
    if ((DesktopFileFlags and dfWatches)<>0) then
      OK:=ReadWatches(F) and OK;
    if ((DesktopFileFlags and dfBreakpoints)<>0) then
      OK:=ReadBreakpoints(F) and OK;
    if ((DesktopFileFlags and dfOpenWindows)<>0) then
      OK:=ReadOpenWindows(F) and OK;
    { no errors if no browser info available PM }
    if ((DesktopFileFlags and dfSymbolInformation)<>0) then
      OK:=ReadSymbols(F) and OK;
    if ((DesktopFileFlags and dfCodeCompleteWords)<>0) then
      OK:=ReadCodeComplete(F) and OK;
    if ((DesktopFileFlags and dfCodeTemplates)<>0) then
      OK:=ReadCodeTemplates(F) and OK;
{$ifdef Unix}
    OK:=ReadKeys(F) and OK;
{$endif Unix}
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
  PushStatus(msg_writingdesktopfile);
  New(F, CreateFile(TempPath));

  if Assigned(Clipboard) then
    if (DesktopFileFlags and dfClipboardContent)<>0 then
      Clipboard^.SetFlags(Clipboard^.GetFlags or efStoreContent)
    else
      Clipboard^.SetFlags(Clipboard^.GetFlags and not efStoreContent);
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
      if ((DesktopFileFlags and dfCodeCompleteWords)<>0) then
        OK:=OK and WriteCodeComplete(F);
      if ((DesktopFileFlags and dfCodeTemplates)<>0) then
        OK:=OK and WriteCodeTemplates(F);
{$ifdef Unix}
      OK:=OK and WriteKeys(F);
{$endif Unix}
      Dispose(F, Done);
    end;
  if OK then
    begin
      if ExistsFile(DesktopPath) then
        OK:=EraseFile(DesktopPath);
      OK:=OK and RenameFile(TempPath,DesktopPath);
      if OK=false then
        ErrorBox(msg_failedtoreplacedesktopfile,nil);
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
  New(F, CreateFile(FileName));
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
  New(F, LoadFile(FileName));
  OK:=Assigned(F);
  if OK and ((DesktopFileFlags and dfSymbolInformation)<>0) then
      OK:=OK and ReadSymbols(F);
  if assigned(F) then
    Dispose(F,Done);
  ReadSymbolsFile:=OK;
end;

END.
