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
     MinDesktopVersion  = $0009;
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
{$ifndef FVISION}
     Video,
{$else FVISION}
{$ifndef GRAPH_API}
     Video,
{$endif GRAPH_API}
{$endif FVISION}
     Views,App,HistList,BrowCol,
     WUtils,WResourc,WViews,WEditor,
{$ifndef NODEBUG}
     fpdebug,
{$endif ndef NODEBUG}
{$ifdef Unix}
     FPKeys,
{$endif Unix}
     FPConst,FPVars,FPString,FPTools,FPUtils,FPViews,FPHelp,
     FPCompil,FPCodCmp,FPCodTmp;

type
     TWindowInfo = packed record
       HelpCtx   : word;
       Bounds    : TRect;
       Visible   : boolean;
       WinNb     : byte;
       ExtraDataSize : word;
       TitleLen  : word;
       Title     : packed record end;
     end;

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
      BreakpointsCollection^.Store(S^);
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
     hcInfoWindow:
       begin
         if ProgramInfoWindow=nil then
           begin
             New(ProgramInfoWindow, Init);
             Desktop^.Insert(ProgramInfoWindow);
           end;
         W:=ProgramInfoWindow;
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
      PutSubViewPtr(S^,ProgramInfoWindow);
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
  size : sw_word;
    OK: boolean;
begin
  OK:=F^.ReadResourceEntry(resDesktopFlags,langDefault,DesktopFileFlags,
    size);
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
    ErrorBox(msg_errorreadingvideomode,nil);
  ReadVideoMode:=OK;
end;

function ReadSymbols(F: PResourceFile): boolean;
var S: PMemoryStream;
    OK: boolean;
    R: PResource;
begin
  { if no symbols stored ... no problems }
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
    if ((DesktopFileFlags and dfCodeCompleteWords)<>0) then
      OK:=OK and ReadCodeComplete(F);
    if ((DesktopFileFlags and dfCodeTemplates)<>0) then
      OK:=OK and ReadCodeTemplates(F);
{$ifdef Unix}
    OK:=OK and ReadKeys(F);
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
{
  $Log$
  Revision 1.4  2002-05-31 12:37:09  pierre
   + register asciitable char

  Revision 1.3  2001/10/11 11:38:22  pierre
   * small fvision specific changes

  Revision 1.2  2001/08/05 12:23:00  peter
    * Automatically support for fvision or old fv

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.8  2001/03/22 17:28:03  pierre
   * small fix to OpenWindows

  Revision 1.1.2.7  2001/03/12 17:34:55  pierre
   + Disassembly window started

  Revision 1.1.2.6  2000/12/12 16:51:50  pierre
   + keys loading/storing begin

  Revision 1.1.2.5  2000/11/29 11:25:59  pierre
   + TFPDlgWindow that handles cmSearchWindow

  Revision 1.1.2.4  2000/11/29 00:54:44  pierre
   + preserve window number and save special windows

  Revision 1.1.2.3  2000/10/18 21:53:26  pierre
   * several Gabor fixes

  Revision 1.1.2.2  2000/09/18 13:20:54  pierre
   New bunch of Gabor changes

  Revision 1.1.2.1  2000/07/20 11:02:15  michael
  + Fixes from gabor. See fixes.txt

  Revision 1.1  2000/07/13 09:48:34  michael
  + Initial import

  Revision 1.29  2000/06/22 09:07:12  pierre
   * Gabor changes: see fixes.txt

  Revision 1.28  2000/05/02 08:42:27  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.27  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.26  2000/04/18 11:42:36  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.25  2000/03/21 23:32:05  pierre
   adapted to wcedit addition by Gabor

  Revision 1.24  2000/03/20 19:19:46  pierre
   * LFN support in streams

  Revision 1.23  2000/03/13 20:36:52  pierre
   * Breakpoints saved and loaded before sources

  Revision 1.22  2000/02/07 12:03:48  pierre
   Last commit is from Gabor's changes!

  Revision 1.21  2000/02/07 11:55:27  pierre
   + Code Complete and Template saving from Gabor

  Revision 1.20  2000/02/04 00:12:57  pierre
   * Breakpoint are marked in source at desktop loading

  Revision 1.19  2000/01/25 00:26:36  pierre
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
