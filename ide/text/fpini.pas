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
  FPConst,FPVars,FPIntf;

const
  { INI file sections }
  secFiles           = 'Files';
  secRun             = 'Run';
  secCompile         = 'Compile';
  secColors          = 'Colors';
  secHelp            = 'Help';
  secEditor          = 'Editor';
  secHighlight       = 'Highlight';
  secMouse           = 'Mouse';

  { INI file tags }
  ieRecentFile       = 'RecentFile';
  ieRunParameters    = 'Parameters';
  iePrimaryFile      = 'PrimaryFile';
  iePalette          = 'Palette';
  ieHelpFiles        = 'Files';
  ieDefaultTabSize   = 'DefaultTabSize';
  ieDefaultEditorFlags='DefaultFlags';
  ieHighlightExts    = 'Exts';
  ieDoubleClickDelay = 'DoubleDelay';
  ieReverseButtons   = 'ReverseButtons';
  ieAltClickAction   = 'AltClickAction';
  ieCtrlClickAction  = 'CtrlClickAction';

procedure InitINIFile;
begin
  INIPath:=LocateFile(ININame);
  if INIPath='' then
    INIPath:=ININame;
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

function ReadINIFile: boolean;
var INIFile: PINIFile;
    S,PS: string;
    I,P: integer;
    OK: boolean;
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
  DoubleDelay:=INIFile^.GetIntEntry(secMouse,ieDoubleClickDelay,DoubleDelay);
  MouseReverse:=boolean(INIFile^.GetIntEntry(secMouse,ieReverseButtons,byte(MouseReverse)));
  AltMouseAction:=INIFile^.GetIntEntry(secMouse,ieAltClickAction,AltMouseAction);
  CtrlMouseAction:=INIFile^.GetIntEntry(secMouse,ieCtrlClickAction,CtrlMouseAction);
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
  S:='';
  HelpFiles^.ForEach(@ConcatName);
  INIFile^.SetEntry(secHelp,ieHelpFiles,'"'+S+'"');
{$ifndef EDITORS}
  INIFile^.SetIntEntry(secEditor,ieDefaultTabSize,DefaultTabSize);
  INIFile^.SetIntEntry(secEditor,ieDefaultEditorFlags,DefaultCodeEditorFlags);
{$endif}
  INIFile^.SetEntry(secHighlight,ieHighlightExts,HighlightExts);
  INIFile^.SetIntEntry(secMouse,ieDoubleClickDelay,DoubleDelay);
  INIFile^.SetIntEntry(secMouse,ieReverseButtons,byte(MouseReverse));
  INIFile^.SetIntEntry(secMouse,ieAltClickAction,AltMouseAction);
  INIFile^.SetIntEntry(secMouse,ieCtrlClickAction,CtrlMouseAction);
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
  Revision 1.2  1998-12-30 10:25:01  peter
    * fixed readinifile

  Revision 1.1  1998/12/28 15:47:45  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
