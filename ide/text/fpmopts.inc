{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Options menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.Compiler;
var R,R2,TabR,TabIR: TRect;
    D: PCenterDialog;
    CB1,CB2,CB3,CB4: PCheckBoxes;
    RB1,RB2: PRadioButtons;
    Items: PSItem;
    IL: PInputLine;
    Count : integer;
    I,L: longint;
    Tab: PTab;
    Label11,Label21,Label22,Label23,Label24,Label31: PLabel;
begin
  R.Assign(0,0,72,18);
  New(D, Init(R, 'Compiler Options'));
  with D^ do
  begin
    GetExtent(R);
    R.Grow(-2,-1);
    Dec(R.B.Y,2);
    TabR.Copy(R);
    TabIR.Copy(R);
    Inc(TabIR.A.Y,2);
    TabIR.Grow(0,-1);

    { --- Sheet 1 --- }
    Count:=SyntaxOptions^.ItemCount;
    R.Copy(TabIR);
    R2.Copy(R);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(SyntaxOptions^.ItemName(I), Items);
    New(CB1, Init(R2, Items));
    for I:=0 to Count-1 do
      if SyntaxOptions^.ItemIsSet(I) then
        CB1^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label11, Init(R2, 'Syntax options', CB1));

    { --- Sheet 2 --- }
    Count:=CodegenOptions^.ItemCount;
    R2.Copy(TabIR);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2-2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(CodegenOptions^.ItemName(I), Items);
    New(CB3, Init(R2, Items));
    for I:=0 to Count-1 do
      if CodegenOptions^.ItemIsSet(I) then
        CB3^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label21, Init(R2, 'Run-time checks', CB3));

    Count:=OptimizationOptions^.ItemCount;
    R2.Copy(TabIR);
    R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2;
    Dec(R2.B.X,4);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(OptimizationOptions^.ItemName(I), Items);
    New(CB2, Init(R2, Items));
    for I:=0 to Count-1 do
      if OptimizationOptions^.ItemIsSet(I) then
        CB2^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label22, Init(R2, 'Optimizations', CB2));

    Count:=ProcessorOptions^.ItemCount;
    R2.Copy(TabIR);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2-2;
    Inc(R2.A.Y,1+CodegenOptions^.ItemCount);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(ProcessorOptions^.ItemName(I), Items);
    New(RB1, Init(R2, Items));
    L:=ProcessorOptions^.GetCurrSel;
    RB1^.SetData(L);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label23, Init(R2, 'Target processor', RB1));

    Count:=AsmReaderOptions^.ItemCount;
    R2.Copy(TabIR);
    R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2;
    Dec(R2.B.X,4);
    Inc(R2.A.Y, 1+OptimizationOptions^.ItemCount+1+ProcessorOptions^.ItemCount);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(AsmReaderOptions^.ItemName(I), Items);
    New(RB2, Init(R2, Items));
    L:=AsmReaderOptions^.GetCurrSel;
    RB2^.SetData(L);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label24, Init(R2, 'Assembler format', RB2));

    { --- Sheet 3 --- }
    Count:=VerboseOptions^.ItemCount;
    R.Copy(TabIR);
    R2.Copy(R);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(VerboseOptions^.ItemName(I), Items);
    New(CB4, Init(R2, Items));
    for I:=0 to Count-1 do
      if VerboseOptions^.ItemIsSet(I) then
        CB4^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label31, Init(R2, 'Verbose options', CB4));

    { create tabs }
    New(Tab, Init(TabR,
      NewTabDef('S~y~ntax',CB1,
        NewTabItem(Label11,
        NewTabItem(CB1,
        nil)),
      NewTabDef('Code ~g~eneration',CB3,
        NewTabItem(Label21,
        NewTabItem(CB3,
        NewTabItem(Label22,
        NewTabItem(CB2,
        NewTabItem(Label23,
        NewTabItem(RB1,
        NewTabItem(Label24,
        NewTabItem(RB2,
        nil)))))))),
      NewTabDef('~V~erbose',CB4,
        NewTabItem(Label31,
        NewTabItem(CB4,
        nil)),
      nil)))));
    Tab^.GrowMode:=0;
    Insert(Tab);

    R2.Copy(TabR); R2.A.Y:=R2.B.Y+1; R2.B.Y:=R2.A.Y+1;
    New(IL, Init(R2, 128));
    IL^.Data^:=GetConditionalDefines;
    Insert(IL);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2, 'Conditio~n~al defines', IL)));
  end;
  InsertButtons(D);
  if Desktop^.ExecView(D)=cmOK then
  begin
    for I:=0 to SyntaxOptions^.ItemCount-1 do
      SyntaxOptions^.ItemSet(I,CB1^.Mark(I));
    for I:=0 to CodeGenOptions^.ItemCount-1 do
      CodegenOptions^.ItemSet(I,CB3^.Mark(I));
    for I:=0 to OptimizationOptions^.ItemCount-1 do
      OptimizationOptions^.ItemSet(I,CB2^.Mark(I));
    for I:=0 to VerboseOptions^.ItemCount-1 do
      VerboseOptions^.ItemSet(I,CB4^.Mark(I));
    ProcessorOptions^.SetCurrSel(RB1^.Value);
    AsmReaderOptions^.SetCurrSel(RB2^.Value);
    SetConditionalDefines(IL^.Data^);
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.MemorySizes;
var R,R2,R3: TRect;
    D: PCenterDialog;
    IL1,IL2: PIntegerLine;
begin
{$ifdef NOTYET}
  R.Assign(0,0,40,7);
  New(D, Init(R, 'Memory sizes'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+1;
    R3.Copy(R2); R3.A.X:=21; R3.B.X:=R3.A.X+10;
    New(IL1, Init(R3, MinStackSize, MaxStackSize));
    IL1^.Data^:=IntToStr(GetStackSize);
    Insert(IL1);
    R3.Copy(R2); R3.B.X:=21;
    Insert(New(PLabel, Init(R3, '~S~tack size', IL1)));

    R2.Move(0,2);
    R3.Copy(R2); R3.A.X:=21; R3.B.X:=R3.A.X+10;
    New(IL2, Init(R3, MinHeapSize, MaxHeapSize));
    IL2^.Data^:=IntToStr(GetHeapSize);
    Insert(IL2);
    R3.Copy(R2); R3.B.X:=21;
    Insert(New(PLabel, Init(R3, 'Local ~h~eap size', IL2)));
  end;
  InsertButtons(D);
  IL1^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    SetStackSize(StrToInt(IL1^.Data^));
    SetHeapSize(StrToInt(IL2^.Data^));
  end;
  Dispose(D, Done);
{$endif}
end;

procedure TIDEApp.Linker;
var R,R2: TRect;
    D: PCenterDialog;
    RB1,RB2: PRadioButtons;
    Mask,V,I: longint;
    Items: PSItem;
    L: longint;
begin
{$ifdef NOTYET}
  R.Assign(0,0,56,8);
  New(D, Init(R, 'Linker'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+GetEXEFormatCount; R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
    Items:=nil;
    for I:=GetEXEFormatCount downto 1 do
      Items:=NewSItem(GetEXEFormatName(TEXEFormat(I)),Items);
    New(RB1, Init(R2, Items));
    Mask:=0; V:=1;
    for I:=1 to GetEXEFormatCount do
     begin
       if IsEXEFormatAvailable(TEXEFormat(I)) then
          Mask:=Mask or V;
       V:=V shl 1;
     end;
    RB1^.SetButtonState($ffff,false);
    RB1^.SetButtonState(Mask,true);
    L:=ord(GetEXEFormat)-1;
    RB1^.SetData(L);
    Insert(RB1);
    R2.Copy(R); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Output format', RB1)));

    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+2; R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2+1;
    New(RB2, Init(R2,
      NewSItem('D~y~namic libraries',
      NewSItem('S~t~atic libraries',
      nil))));
    L:=ord(GetLinkLibraryType)-1;
    RB2^.SetData(L);
    Insert(RB2);
    R2.Copy(R); R2.B.Y:=R2.A.Y+1; R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2+1;
    Insert(New(PLabel, Init(R2, 'Preferred library type', RB2)));
  end;
  InsertButtons(D);
  if Desktop^.ExecView(D)=cmOK then
  begin
    SetEXEFormat(TEXEFormat(RB1^.Value+1));
    SetLinkLibraryType(TLibraryType(RB2^.Value+1));
  end;
  Dispose(D, Done);
{$endif}
end;

procedure TIDEApp.Debugger;
var R,R2: TRect;
    D: PCenterDialog;
    RB1,RB2: PRadioButtons;
    L: longint;
begin
{$ifdef NOTYET}
  R.Assign(0,0,46,11);
  New(D, Init(R, 'Debugging/Profiling'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
    R2.Copy(R); Inc(R2.A.Y,2); R2.B.Y:=R2.A.Y+3;
    New(RB1, Init(R2,
      NewSItem('~S~trip all symbols from executable',
      NewSItem('Generate g~s~ym symbol information',
      NewSItem('Generate ~d~bx symbol information',
      nil)))));
    L:=ord(GetSymbolFormat);
    RB1^.SetData(L);
    Insert(RB1);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Symbol information', RB1)));

    R2.Copy(R); Inc(R2.A.Y,7); R2.B.Y:=R2.A.Y+2;
    New(RB2, Init(R2,
      NewSItem('N~o~ profile information',
      NewSItem('Generate ~p~rofile code for gprof',
      nil))));
    L:=ord(GetProfileFormat);
    RB2^.SetData(L);
    Insert(RB2);
    R2.Copy(R); Inc(R2.A.Y,6); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Profiling options', RB2)));
  end;
  InsertButtons(D);
  RB1^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    SetSymbolFormat(TSymbolFormat(RB1^.Value));
    SetProfileFormat(TProfileFormat(RB2^.Value));
  end;
  Dispose(D, Done);
{$endif}
end;

procedure TIDEApp.Directories;
var R,R2: TRect;
    D: PCenterDialog;
    IL1,IL2,IL3,IL4: PInputLine;
{    Dirs: TDirectoryCfg; }
const LW = 25;
begin
{$ifdef NOTYET}
  GetDirectoryCfg(Dirs);
  R.Assign(0,0,64,10);
  New(D, Init(R, 'Directories'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2); Dec(R.B.X); R.B.Y:=R.A.Y+1;
    R2.Copy(R); R2.A.X:=LW;
    New(IL1, Init(R2, 128));
    IL1^.Data^:=Dirs.EXETPUDir;
    Insert(IL1);
    R2.Copy(R); R2.B.X:=LW;
    Insert(New(PLabel, Init(R2, '~E~XE & unit directory', IL1)));

    R.Move(0,2);
    R2.Copy(R); R2.A.X:=LW;
    New(IL2, Init(R2, 128));
    IL2^.Data^:=Dirs.IncludeDirs;
    Insert(IL2);
    R2.Copy(R); R2.B.X:=LW;
    Insert(New(PLabel, Init(R2, '~I~nclude directories', IL2)));

    R.Move(0,2);
    R2.Copy(R); R2.A.X:=LW;
    New(IL3, Init(R2, 128));
    IL3^.Data^:=Dirs.UnitDirs;
    Insert(IL3);
    R2.Copy(R); R2.B.X:=LW;
    Insert(New(PLabel, Init(R2, '~U~nit directories', IL3)));

    R.Move(0,2);
    R2.Copy(R); R2.A.X:=LW;
    New(IL4, Init(R2, 128));
    IL4^.Data^:=Dirs.ObjectDirs;
    Insert(IL4);
    R2.Copy(R); R2.B.X:=LW;
    Insert(New(PLabel, Init(R2, '~O~bject directories', IL4)));
  end;
  InsertButtons(D);
  IL1^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    Dirs.EXETPUDir:=IL1^.Data^;
    Dirs.IncludeDirs:=IL2^.Data^;
    Dirs.UnitDirs:=IL3^.Data^;
    Dirs.ObjectDirs:=IL4^.Data^;
    SetDirectoryCfg(Dirs);
  end;
  Dispose(D, Done);
{$endif NOTYET}
end;

procedure TIDEApp.Colors;
var D: PColorDialog;
begin
  New(D, Init(GetPalette^,
    ColorGroup('Desktop', DesktopColorItems(nil),
    ColorGroup('Dialogs', DialogColorItems(dpGrayDialog,nil),
    ColorGroup('Editor',
      ColorItem('Frame passive'   , 167,
      ColorItem('Frame active'    , 168,
      ColorItem('Frame icon'      , 169,
      ColorItem('Scroll bar page' , 170,
      ColorItem('Scroll bar icons', 171,
      ColorItem('Normal text'     , 199,
      ColorItem('Selected text'   , 208,
      ColorItem('Highlight column', 209,
      ColorItem('Highlight row'   , 210,
      ColorItem('Error messages'  , 214,
      nil)))))))))),
    ColorGroup('Help',
      ColorItem('Frame passive'   , 128,
      ColorItem('Frame active'    , 129,
      ColorItem('Frame icon'      , 130,
      ColorItem('Scroll bar page' , 131,
      ColorItem('Scroll bar icons', 132,
      ColorItem('Help text'       , 160,
      ColorItem('Help links'      , 161,
      ColorItem('Selected link'   , 162,
      ColorItem('Selected text'   , 163,
      nil))))))))),
    ColorGroup('Menus',   MenuColorItems(nil),
    ColorGroup('Syntax',
      ColorItem('Whitespace'      , 200,
      ColorItem('Comments'        , 201,
      ColorItem('Reserved words'  , 202,
      ColorItem('Identifiers'     , 203,
      ColorItem('Strings'         , 204,
      ColorItem('Numbers'         , 205,
      ColorItem('Assembler'       , 206,
      ColorItem('Symbols'         , 207,
      ColorItem('Directives'      , 211,
      nil))))))))),
    nil))))))));
  if ExecuteDialog(D, GetPalette)=cmOK then
    begin
      DoneMemory;
      Message(Application,evBroadcast,cmUpdate,nil);
      ReDraw;
    end;
end;

{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.3  1998/12/22 10:39:48  peter
    + options are now written/read
    + find and replace routines

}
