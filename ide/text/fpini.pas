{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Write/Read Options to INI File

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPIni;
interface

uses
  FPUtils;

const
    ININame = 'fp.ini';

    ConfigDir  : string = '.'+DirSep;
    INIFileName: string = ININame;


procedure InitINIFile;
function  ReadINIFile: boolean;
function  WriteINIFile: boolean;


implementation

uses
  Dos,Objects,Drivers,
  WINI,{$ifndef EDITORS}WEditor{$else}Editors{$endif},
  FPDebug,FPConst,FPVars,
  FPIntf,FPTools,FPSwitch;

const
  { INI file sections }
  secFiles	   = 'Files';
  secRun	     = 'Run';
  secCompile	 = 'Compile';
  secColors	  = 'Colors';
  secHelp	    = 'Help';
  secEditor	  = 'Editor';
  secBreakpoint  = 'Breakpoints';
  secHighlight       = 'Highlight';
  secMouse	   = 'Mouse';
  secSearch	  = 'Search';
  secTools	   = 'Tools';

  { INI file tags }
  ieRecentFile       = 'RecentFile';
  ieRunParameters    = 'Parameters';
  iePrimaryFile      = 'PrimaryFile';
  ieCompileMode      = 'CompileMode';																									
  iePalette	     = 'Palette';
  ieHelpFiles	     = 'Files';
  ieDefaultTabSize   = 'DefaultTabSize';
  ieDefaultEditorFlags='DefaultFlags';
  ieHighlightExts    = 'Exts';
  ieTabsPattern      = 'NeedsTabs';
  ieDoubleClickDelay = 'DoubleDelay';
  ieReverseButtons   = 'ReverseButtons';
  ieAltClickAction   = 'AltClickAction';
  ieCtrlClickAction  = 'CtrlClickAction';
  ieFindFlags	= 'FindFlags';
  ieToolName	 = 'Title';
  ieToolProgram      = 'Program';
  ieToolParams       = 'Params';
  ieToolHotKey       = 'HotKey';
  ieBreakpointTyp    = 'Type';
  ieBreakpointCount  = 'Count';
  ieBreakpointState  = 'State';
  ieBreakpointFunc   = 'Function';
  ieBreakpointFile   = 'FileName';
  ieBreakpointLine   = 'LineNumber';

const
     BreakpointTypeStr : Array[BreakpointType] of String[9]
       = ( 'function','file-line','invalid' );
     BreakpointStateStr : Array[BreakpointState] of String[8]
       = ( 'enabled','disabled','invalid' );

procedure InitINIFile;
var S: string;
begin
  S:=LocateFile(ININame);
  if S<>'' then
    INIPath:=S;
  INIPath:=FExpand(INIPath);
end;

function PaletteToStr(S: string): string;
var C: string;
    I: integer;
begin
  C:='';
  for I:=1 to length(S) do
    begin
      C:=C+'#$'+IntToHexL(ord(S[I]),2);
    end;
  PaletteToStr:=C;
end;

function StrToPalette(S: string): string;
var I,P,X: integer;
    C: string;
    Hex: boolean;
    OK: boolean;
begin
  C:=''; I:=1;
  OK:=S<>'';
  while OK and (I<=length(S)) and (S[I]='#') do
  begin
    Inc(I); Hex:=false;
    if S[I]='$' then begin Inc(I); Hex:=true; end;
    P:=Pos('#',copy(S,I,255)); if P>0 then P:=I+P-1 else P:=length(S)+1;
    if Hex=false then
      begin
	X:=StrToInt(copy(S,I,P-I));
	OK:=(LastStrToIntResult=0) and (0<=X) and (X<=255);
      end
    else
      begin
	X:=HexToInt(copy(S,I,P-I));
	OK:=(LastHexToIntResult=0) and (0<=X) and (X<=255);
      end;
    if OK then C:=C+chr(X);
    Inc(I,P-I);
  end;
  StrToPalette:=C;
end;

procedure WriteOneBreakPointEntry(I : longint;INIFile : PINIFile);
var PB : PBreakpoint;
    S : String;
begin
  Str(I,S);
  PB:=BreakpointCollection^.At(I);
  If assigned(PB) then
   With PB^ do
    Begin
      INIFile^.SetEntry(secBreakpoint,ieBreakpointTyp+S,BreakpointTypeStr[typ]);
      INIFile^.SetEntry(secBreakpoint,ieBreakpointState+S,BreakpointStateStr[state]);
      case typ of
        bt_function :
          INIFile^.SetEntry(secBreakpoint,ieBreakpointFunc+S,Name^);
        bt_file_line :
          begin
            INIFile^.SetEntry(secBreakpoint,ieBreakpointFile+S,Name^);
            INIFile^.SetIntEntry(secBreakpoint,ieBreakpointLine+S,Line);
          end;
      end;
    end;
end;

procedure ReadOneBreakPointEntry(i : longint;INIFile : PINIFile);
var PB : PBreakpoint;
    S,S2 : string;
    Line : longint;
    typ : BreakpointType;
    state : BreakpointState;
    
begin
  Str(I,S2);
  typ:=bt_invalid;
  S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointTyp+S2,BreakpointTypeStr[typ]);
  for typ:=low(BreakpointType) to high(BreakpointType) do
    If pos(BreakpointTypeStr[typ],S)>0 then break;
  state:=bs_invalid;
  S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointState+S2,BreakpointStateStr[state]);
  for state:=low(BreakpointState) to high(BreakpointState) do
    If pos(BreakpointStateStr[state],S)>0 then break;
  case typ of
     bt_invalid :;
     bt_function :
       begin
         S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointFunc+S2,'');
       end;
     bt_file_line :
       begin
         S:=INIFile^.GetEntry(secBreakpoint,ieBreakpointFile+S2,'');
         Line:=INIFile^.GetIntEntry(secBreakpoint,ieBreakpointLine+S2,0);
       end;
     end;
   if (typ=bt_function) and (S<>'') then
     new(PB,init_function(S))
   else if (typ=bt_file_line) and (S<>'') then
     new(PB,init_file_line(S,Line));
   If assigned(PB) then
     PB^.state:=state;
   If assigned(PB) then
     BreakpointCollection^.Insert(PB);
end;

function ReadINIFile: boolean;
var INIFile: PINIFile;
    S,PS,S1,S2,S3: string;
    I,P: integer;
    BreakPointCount:longint;
    OK: boolean;
    ts : TSwitchMode;
    W: word;
begin
  OK:=ExistsFile(INIPath);
  if OK then
 begin
  New(INIFile, Init(INIPath));
  RecentFileCount:=High(RecentFiles);
  for I:=Low(RecentFiles) to High(RecentFiles) do
    begin
      S:=INIFile^.GetEntry(secFiles,ieRecentFile+IntToStr(I),'');
      if (S='') and (RecentFileCount>I-1) then RecentFileCount:=I-1;
      with RecentFiles[I] do
      begin
	P:=Pos(',',S); if P=0 then P:=length(S)+1;
	FileName:=copy(S,1,P-1); Delete(S,1,P);
	P:=Pos(',',S); if P=0 then P:=length(S)+1;
	LastPos.X:=Max(0,StrToInt(copy(S,1,P-1))); Delete(S,1,P);
	P:=Pos(',',S); if P=0 then P:=length(S)+1;
	LastPos.Y:=Max(0,StrToInt(copy(S,1,P-1))); Delete(S,1,P);
      end;
    end;
  SetRunParameters(INIFile^.GetEntry(secRun,ieRunParameters,GetRunParameters));
  PrimaryFile:=INIFile^.GetEntry(secCompile,iePrimaryFile,PrimaryFile);
 {   SwitchesModeStr : array[TSwitchMode] of string[8]=
      ('NORMAL','DEBUG','RELEASE');}
  S:=INIFile^.GetEntry(secCompile,ieCompileMode,'');
  for ts:=low(TSwitchMode) to high(TSwitchMode) do
    begin
      if SwitchesModeStr[ts]=S then
	begin
	  SwitchesMode:=ts;
	end;
    end;
  S:=INIFile^.GetEntry(secHelp,ieHelpFiles,'');
  repeat
    P:=Pos(';',S); if P=0 then P:=length(S)+1;
    PS:=copy(S,1,P-1);
    if PS<>'' then HelpFiles^.Insert(NewStr(PS));
    Delete(S,1,P);
  until S='';
{$ifndef EDITORS}
  DefaultTabSize:=INIFile^.GetIntEntry(secEditor,ieDefaultTabSize,DefaultTabSize);
  DefaultCodeEditorFlags:=INIFile^.GetIntEntry(secEditor,ieDefaultEditorFlags,DefaultCodeEditorFlags);
{$endif}
  HighlightExts:=INIFile^.GetEntry(secHighlight,ieHighlightExts,HighlightExts);
  TabsPattern:=INIFile^.GetEntry(secHighlight,ieTabsPattern,TabsPattern);
  DoubleDelay:=INIFile^.GetIntEntry(secMouse,ieDoubleClickDelay,DoubleDelay);
  MouseReverse:=boolean(INIFile^.GetIntEntry(secMouse,ieReverseButtons,byte(MouseReverse)));
  AltMouseAction:=INIFile^.GetIntEntry(secMouse,ieAltClickAction,AltMouseAction);
  CtrlMouseAction:=INIFile^.GetIntEntry(secMouse,ieCtrlClickAction,CtrlMouseAction);
  FindFlags:=INIFile^.GetIntEntry(secSearch,ieFindFlags,FindFlags);
  { Breakpoints }
  BreakpointCount:=INIFile^.GetIntEntry(secBreakpoint,ieBreakpointCount,0);
  for i:=1 to BreakpointCount do
    ReadOneBreakPointEntry(i-1,INIFile);
    
  for I:=1 to MaxToolCount do
    begin
      S:=IntToStr(I);
      S1:=INIFile^.GetEntry(secTools,ieToolName+S,'');
      if S1='' then Break; { !!! }
      S2:=INIFile^.GetEntry(secTools,ieToolProgram+S,'');
      S3:=INIFile^.GetEntry(secTools,ieToolParams+S,'');
      W:=Max(0,Min(65535,INIFile^.GetIntEntry(secTools,ieToolHotKey+S,0)));
      AddTool(S1,S2,S3,W);
    end;
  S:=AppPalette;
  PS:=StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_1_40',PaletteToStr(copy(S,1,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_41_80',PaletteToStr(copy(S,41,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_81_120',PaletteToStr(copy(S,81,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_121_160',PaletteToStr(copy(S,121,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_161_200',PaletteToStr(copy(S,161,40))));
  PS:=PS+StrToPalette(INIFile^.GetEntry(secColors,iePalette+'_201_240',PaletteToStr(copy(S,201,40))));
  AppPalette:=PS;
  Dispose(INIFile, Done);
 end;
  ReadINIFile:=OK;
end;

function WriteINIFile: boolean;
var INIFile: PINIFile;
    S: string;
    S1,S2,S3: string;
    W: word;
    BreakPointCount:longint;
    I: integer;
    OK: boolean;
procedure ConcatName(P: PString); {$ifndef FPC}far;{$endif}
begin
  if (S<>'') then S:=S+';';
  S:=S+P^;
end;
begin
  New(INIFile, Init(INIPath));
  for I:=1 to High(RecentFiles) do
    begin
      if I<=RecentFileCount then
	 with RecentFiles[I] do S:=FileName+','+IntToStr(LastPos.X)+','+IntToStr(LastPos.Y)
      else
	 S:='';
      INIFile^.SetEntry(secFiles,ieRecentFile+IntToStr(I),S);
    end;
  INIFile^.SetEntry(secRun,ieRunParameters,GetRunParameters);
  INIFile^.SetEntry(secCompile,iePrimaryFile,PrimaryFile);
  INIFile^.SetEntry(secCompile,ieCompileMode,SwitchesModeStr[SwitchesMode]);
  S:='';
  HelpFiles^.ForEach(@ConcatName);
  INIFile^.SetEntry(secHelp,ieHelpFiles,'"'+S+'"');
{$ifndef EDITORS}
  INIFile^.SetIntEntry(secEditor,ieDefaultTabSize,DefaultTabSize);
  INIFile^.SetIntEntry(secEditor,ieDefaultEditorFlags,DefaultCodeEditorFlags);
{$endif}
  INIFile^.SetEntry(secHighlight,ieHighlightExts,'"'+HighlightExts+'"');
  INIFile^.SetEntry(secHighlight,ieTabsPattern,'"'+TabsPattern+'"');
  INIFile^.SetIntEntry(secMouse,ieDoubleClickDelay,DoubleDelay);
  INIFile^.SetIntEntry(secMouse,ieReverseButtons,byte(MouseReverse));
  INIFile^.SetIntEntry(secMouse,ieAltClickAction,AltMouseAction);
  INIFile^.SetIntEntry(secMouse,ieCtrlClickAction,CtrlMouseAction);
  INIFile^.SetIntEntry(secSearch,ieFindFlags,FindFlags);
  { Breakpoints }
  BreakPointCount:=BreakpointCollection^.Count;
  INIFile^.SetIntEntry(secBreakpoint,ieBreakpointCount,BreakpointCount);
  for i:=1 to BreakpointCount do
    WriteOneBreakPointEntry(I-1,INIFile);
  INIFile^.DeleteSection(secTools);
  for I:=1 to GetToolCount do
    begin
      S:=IntToStr(I);
      GetToolParams(I-1,S1,S2,S3,W);
      if S1<>'' then S1:='"'+S1+'"';
      if S2<>'' then S2:='"'+S2+'"';
      if S3<>'' then S3:='"'+S3+'"';
      INIFile^.SetEntry(secTools,ieToolName+S,S1);
      INIFile^.SetEntry(secTools,ieToolProgram+S,S2);
      INIFile^.SetEntry(secTools,ieToolParams+S,S3);
      INIFile^.SetIntEntry(secTools,ieToolHotKey+S,W);
    end;
  if AppPalette<>CIDEAppColor then
  begin
    { this has a bug. if a different palette has been read on startup, and
      then changed back to match the default, this will not update it in the
      ini file, eg. the original (non-default) will be left unmodified... }
    S:=AppPalette;
    INIFile^.SetEntry(secColors,iePalette+'_1_40',PaletteToStr(copy(S,1,40)));
    INIFile^.SetEntry(secColors,iePalette+'_41_80',PaletteToStr(copy(S,41,40)));
    INIFile^.SetEntry(secColors,iePalette+'_81_120',PaletteToStr(copy(S,81,40)));
    INIFile^.SetEntry(secColors,iePalette+'_121_160',PaletteToStr(copy(S,121,40)));
    INIFile^.SetEntry(secColors,iePalette+'_161_200',PaletteToStr(copy(S,161,40)));
    INIFile^.SetEntry(secColors,iePalette+'_201_240',PaletteToStr(copy(S,201,40)));
  end;
  OK:=INIFile^.Update;
  Dispose(INIFile, Done);
  WriteINIFile:=OK;
end;

end.
{
  $Log$
  Revision 1.6  1999-02-04 13:32:04  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.5  1999/01/21 11:54:15  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.4  1999/01/04 11:49:45  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:45  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
