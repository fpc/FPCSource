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

procedure TIDEApp.SetSwitchesMode;
var R,R2: TRect;
    D: PCenterDialog;
    RB: PRadioButtons;
    i : TSwitchMode;
    SwitchesCount : integer;
    LastItem: PSItem;
    L: longint;
begin
  SwitchesCount:=ord(high(TSwitchMode))-ord(low(TSwitchMode))+1;
  R.Assign(0,0,36,4+SwitchesCount);
  New(D, Init(R, 'SwitchesMode'));
  with D^ do
  begin
    GetExtent(R);
    R.Grow(-3,-1);
    Inc(R.A.Y);
    R2.Copy(R);
    Inc(R2.A.Y);
    R2.B.Y:=R2.A.Y+SwitchesCount;
    LastItem:=nil;
    for I:=high(TSwitchMode) downto low(TSwitchMode) do
      LastItem:=NewSItem(SwitchesModeName[I], LastItem);
    New(RB, Init(R2, LastItem));
    L:=ord(SwitchesMode);
    { ^^^ this is necessary, since TRadioButtons.GetData() reads a full
      longint and by just specifying the SwitchesMode var (only 1 bytes),
      the three bytes located next to it in the memory will determine the
      three most significant bytes of the longint. And if they aren't all
      zero, then we will select some items outside the actual ones... }
    RB^.SetData(L);
    Insert(RB);
    R2.Copy(R);
    R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Switches Mode', RB)));
  end;
  InsertButtons(D);
  RB^.Select;
  if Desktop^.ExecView(D)=cmOK then
   SwitchesMode:=TSwitchMode(RB^.Value);
  Dispose(D, Done);
end;

procedure TIDEApp.DoCompilerSwitch;
var R,R2,TabR,TabIR: TRect;
    D: PCenterDialog;
    CB1,CB2,CB3,CB4: PCheckBoxes;
    RB1,RB2,RB3: PRadioButtons;
    Items: PSItem;
    IL: PInputLine;
    Count : integer;
    I,L: longint;
    Tab: PTab;
    Label11,Label21,Label22,Label23,Label24,Label31: PLabel;
begin
  R.Assign(0,0,72,18);
  New(D, Init(R, 'Compiler Switches'));
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
    Count:=SyntaxSwitches^.ItemCount;
    R.Copy(TabIR);
    R2.Copy(R);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(SyntaxSwitches^.ItemName(I), Items);
    New(CB1, Init(R2, Items));
    for I:=0 to Count-1 do
      if SyntaxSwitches^.GetBooleanItem(I) then
        CB1^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label11, Init(R2, 'Syntax Switches', CB1));

    { --- Sheet 2 --- }
    Count:=CodegenSwitches^.ItemCount;
    R2.Copy(TabIR);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2-2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(CodegenSwitches^.ItemName(I), Items);
    New(CB3, Init(R2, Items));
    for I:=0 to Count-1 do
      if CodegenSwitches^.GetBooleanItem(I) then
        CB3^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label21, Init(R2, 'Run-time checks', CB3));

    Count:=OptimizingGoalSwitches^.ItemCount;
    R2.Copy(TabIR);
    R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2;
    Dec(R2.B.X,4);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(OptimizingGoalSwitches^.ItemName(I), Items);
    New(RB3, Init(R2, Items));
    for I:=0 to Count-1 do
      if OptimizingGoalSwitches^.GetBooleanItem(I) then
        RB3^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label22, Init(R2, 'Optimizations', RB3));

    Count:=OptimizationSwitches^.ItemCount;
    R2.Copy(TabIR);
    R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2;
    Dec(R2.B.X,4);
    Inc(R2.A.Y, OptimizingGoalSwitches^.ItemCount);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(OptimizationSwitches^.ItemName(I), Items);
    New(CB2, Init(R2, Items));
    for I:=0 to Count-1 do
      if OptimizationSwitches^.GetBooleanItem(I) then
        CB2^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;

    Count:=ProcessorSwitches^.ItemCount;
    R2.Copy(TabIR);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2-2;
    Inc(R2.A.Y,1+CodegenSwitches^.ItemCount);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(ProcessorSwitches^.ItemName(I), Items);
    New(RB1, Init(R2, Items));
    L:=ProcessorSwitches^.GetCurrSel;
    RB1^.SetData(L);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label23, Init(R2, 'Target processor', RB1));

    Count:=AsmReaderSwitches^.ItemCount;
    R2.Copy(TabIR);
    R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2;
    Dec(R2.B.X,4);
    Inc(R2.A.Y, OptimizationSwitches^.ItemCount+OptimizingGoalSwitches^.ItemCount);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(AsmReaderSwitches^.ItemName(I), Items);
    New(RB2, Init(R2, Items));
    L:=AsmReaderSwitches^.GetCurrSel;
    RB2^.SetData(L);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label24, Init(R2, 'Assembler format', RB2));

    { --- Sheet 3 --- }
    Count:=VerboseSwitches^.ItemCount;
    R.Copy(TabIR);
    R2.Copy(R);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(VerboseSwitches^.ItemName(I), Items);
    New(CB4, Init(R2, Items));
    for I:=0 to Count-1 do
      if VerboseSwitches^.GetBooleanItem(I) then
        CB4^.Press(I);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label31, Init(R2, 'Verbose Switches', CB4));

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
        NewTabItem(RB3,
        NewTabItem(RB1,
        NewTabItem(Label24,
        NewTabItem(RB2,
        nil))))))))),
      NewTabDef('~V~erbose',CB4,
        NewTabItem(Label31,
        NewTabItem(CB4,
        nil)),
      nil)))));
    Tab^.GrowMode:=0;
    Insert(Tab);

    { conditionnals }
    R2.Copy(TabR); R2.A.Y:=R2.B.Y+1; R2.B.Y:=R2.A.Y+1;
    New(IL, Init(R2, 128));
    IL^.Data^:=ConditionalSwitches^.GetStringItem(0);
    Insert(IL);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2,ConditionalSwitches^.ItemName(0), IL)));
  end;
  InsertButtons(D);
  if Desktop^.ExecView(D)=cmOK then
  begin
    for I:=0 to SyntaxSwitches^.ItemCount-1 do
      SyntaxSwitches^.SetBooleanItem(I,CB1^.Mark(I));
    for I:=0 to CodeGenSwitches^.ItemCount-1 do
      CodegenSwitches^.SetBooleanItem(I,CB3^.Mark(I));
    for I:=0 to OptimizationSwitches^.ItemCount-1 do
      OptimizationSwitches^.SetBooleanItem(I,CB2^.Mark(I));
    for I:=0 to VerboseSwitches^.ItemCount-1 do
      VerboseSwitches^.SetBooleanItem(I,CB4^.Mark(I));
    OptimizingGoalSwitches^.SetCurrSel(RB3^.Value);
    ProcessorSwitches^.SetCurrSel(RB1^.Value);
    AsmReaderSwitches^.SetCurrSel(RB2^.Value);
    ConditionalSwitches^.SetStringItem(0,IL^.Data^);
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.MemorySizes;
var R,R2,R3: TRect;
    D: PCenterDialog;
    ILs: array[0..10] of PIntegerLine;
    I: integer;
begin
  R.Assign(0,0,40,2+MemorySwitches^.ItemCount*2);
  New(D, Init(R, 'Memory sizes'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+1;
    for I:=0 to MemorySwitches^.ItemCount-1 do
     begin
       R3.Copy(R2); R3.A.X:=21; R3.B.X:=R3.A.X+10;
       New(ILs[I], Init(R3, MinMemSize, MaxMemSize));
       ILs[I]^.Data^:=IntToStr(MemorySwitches^.GetLongintItem(I));
       Insert(ILs[I]);
       R3.Copy(R2); R3.B.X:=21;
       Insert(New(PLabel, Init(R3, MemorySwitches^.ItemName(I), ILs[I])));
       R2.Move(0,2);
     end;

{    R3.Copy(R2); R3.A.X:=21; R3.B.X:=R3.A.X+10;
    New(IL2, Init(R3, MinHeapSize, MaxHeapSize));
    IL2^.Data^:=IntToStr(GetHeapSize);
    Insert(IL2);
    R3.Copy(R2); R3.B.X:=21;
    Insert(New(PLabel, Init(R3, 'Local ~h~eap size', IL2)));}
  end;
  InsertButtons(D);
  ILs[0]^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    for I:=0 to MemorySwitches^.ItemCount-1 do
     begin
       MemorySwitches^.SetLongintItem(I,StrToInt(ILs[I]^.Data^));
     end;
{    SetStackSize(StrToInt(IL1^.Data^));
    SetHeapSize(StrToInt(IL2^.Data^));}
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.DoLinkerSwitch;
var R,R2: TRect;
    D: PCenterDialog;
    RB2: PRadioButtons;
    I: longint;
    Items: PSItem;
    L: longint;
begin
  R.Assign(0,0,56,8);
  New(D, Init(R, 'Linker'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
{    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+GetEXEFormatCount; R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
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
    Insert(New(PLabel, Init(R2, 'Output format', RB1)));}

    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+LibLinkerSwitches^.ItemCount; R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2+1;
    Items:=nil;
    for I:=LibLinkerSwitches^.ItemCount-1 downto 0 do
      Items:=NewSItem(LibLinkerSwitches^.ItemName(I), Items);
    New(RB2, Init(R2, Items));
    L:=LibLinkerSwitches^.GetCurrSel;
    RB2^.SetData(L);
    Insert(RB2);
    R2.Copy(R); R2.B.Y:=R2.A.Y+1; R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2+1;
    Insert(New(PLabel, Init(R2, 'Preferred library type', RB2)));
  end;
  InsertButtons(D);
  if Desktop^.ExecView(D)=cmOK then
  begin
{    SetEXEFormat(TEXEFormat(RB1^.Value+1));}
    LibLinkerSwitches^.SetCurrSel(RB2^.Value);
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.DoDebuggerSwitch;
var R,R2: TRect;
    D: PCenterDialog;
    RB1,RB2: PRadioButtons;
    IL2: PInputLine;
    L,I: longint;
    Items: PSItem;
begin
  R.Assign(0,0,60,2+DebugInfoSwitches^.ItemCount+2
      +ProfileInfoSwitches^.ItemCount+4);
  New(D, Init(R, 'Debugging/Profiling'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
    R2.Copy(R); Inc(R2.A.Y,2); R2.B.Y:=R2.A.Y+DebugInfoSwitches^.ItemCount;
    Items:=nil;
    for I:=DebugInfoSwitches^.ItemCount-1 downto 0 do
      Items:=NewSItem(DebugInfoSwitches^.ItemName(I), Items);
    New(RB1, Init(R2, Items));
    L:=DebugInfoSwitches^.GetCurrSel;
    RB1^.SetData(L);
    Insert(RB1);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Debugging information', RB1)));

    R2.Copy(R); Inc(R2.A.Y,2+DebugInfoSwitches^.ItemCount+2); R2.B.Y:=R2.A.Y+ProfileInfoSwitches^.ItemCount;
    Items:=nil;
    for I:=ProfileInfoSwitches^.ItemCount-1 downto 0 do
      Items:=NewSItem(ProfileInfoSwitches^.ItemName(I), Items);
    New(RB2, Init(R2, Items));
    L:=ProfileInfoSwitches^.GetCurrSel;
    RB2^.SetData(L);
    Insert(RB2);
    R2.Copy(R); Inc(R2.A.Y,6); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Profiling Switches', RB2)));

    {custom }
    R2.A.Y:=R2.B.Y+2;
    R2.B.Y:=R2.A.Y+1;
    New(IL2, Init(R2, 255));
    IL2^.Data^:=CustomArg[SwitchesMode];
    Insert(IL2);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2,'~A~dditional compiler args', IL2)));
  end;
  InsertButtons(D);
  RB1^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    DebugInfoSwitches^.SetCurrSel(RB1^.Value);
    ProfileInfoSwitches^.SetCurrSel(RB2^.Value);
    CustomArg[SwitchesMode]:=IL2^.Data^;
  end;
  Dispose(D, Done);
end;


procedure TIDEApp.Directories;
var R,R2: TRect;
    D: PCenterDialog;
    IL : array[0..11] of PInputLine;
    Count,I : integer;
const
  LW = 25;
begin
  Count:=DirectorySwitches^.ItemCount;
  R.Assign(0,0,64,2+Count*2);
  New(D, Init(R, 'Directories'));
  with D^ do
  begin
    GetExtent(R);
    R.Grow(-2,-2);
    Dec(R.B.X);
    R.B.Y:=R.A.Y+1;
    for i:=Count-1 downto 0 do
     begin
       R2.Copy(R);
       R2.A.X:=LW;
       New(IL[i], Init(R2, 255));
       IL[i]^.Data^:=DirectorySwitches^.GetStringItem(i);
       Insert(IL[i]);
       R2.Copy(R);
       R2.B.X:=LW;
       Insert(New(PLabel, Init(R2, DirectorySwitches^.ItemName(i), IL[i])));
       R.Move(0,2);
     end;
  end;
  InsertButtons(D);
  IL[Count-1]^.Select;
  if Desktop^.ExecView(D)=cmOK then
   begin
     for i:=Count-1 downto 0 do
      DirectorySwitches^.SetStringItem(i,IL[i]^.Data^);
   end;
  Dispose(D, Done);
end;

procedure TIDEApp.Tools;
begin
  ExecuteDialog(New(PToolsDialog, Init),nil);
end;

procedure TIDEApp.EditorOptions(Editor: PEditor);
var D: PCenterDialog;
    R,R2,R3: TRect;
    CB: PCheckBoxes;
    IL: PIntegerLine;
    ExtIL,TabExtIL: PInputLine;
    TabSize: Integer;
    EFlags: Longint;
begin
  if Editor=nil
     then begin TabSize:=DefaultTabSize; EFlags:=DefaultCodeEditorFlags; end
     else begin TabSize:=Editor^.TabSize; EFlags:=Editor^.Flags; end;
  R.Assign(0,0,56,18);
  New(D, Init(R, 'Editor Options'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2); R.B.Y:=R.A.Y+7;
    R2.Copy(R); Inc(R2.A.Y);
    New(CB, Init(R2,
      NewSItem('Create backup ~f~iles',
      NewSItem('~I~nsert mode',
      NewSItem('~A~uto indent mode',
      NewSItem('~U~se tab characters',
      NewSItem('~B~ackspace unindents',
      NewSItem('~P~ersistent blocks',
      NewSItem('~S~yntax highlight',
      NewSItem('~B~lock insert cursor',
      NewSItem('~V~ertical blocks',
      NewSItem('Highlight ~c~olumn',
      NewSItem('Highlight ~r~ow',
      NewSItem('Aut~o~-closing brackets',
      nil))))))))))))));
    CB^.Value:=EFlags;
    Insert(CB);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Editor options', CB)));

    R.Move(0,(R.B.Y-R.A.Y)+1); R.B.Y:=R.A.Y+1;
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y;
    R3.Copy(R); Inc(R3.A.X,10); R3.B.X:=R3.A.X+5;
    New(IL, Init(R3, 0,100));
    IL^.Data^:=IntToStr(TabSize);
    Insert(IL);
    R3.Copy(R); R3.B.X:=R3.A.X+10;
    Insert(New(PLabel, Init(R3, '~T~ab size', IL)));

    R.Move(0,(R.B.Y-R.A.Y)+1); R.B.Y:=R.A.Y+2;
    R2.Copy(R); Inc(R2.A.Y);
    New(ExtIL, Init(R2, 128));
    ExtIL^.SetData(HighlightExts);
    Insert(ExtIL);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2, '~H~ighlight extensions', ExtIL)));

    R.Move(0,(R.B.Y-R.A.Y)+1); R.B.Y:=R.A.Y+2;
    R2.Copy(R); Inc(R2.A.Y);
    New(TabExtIL, Init(R2, 128));
    TabExtIL^.SetData(TabsPattern);
    Insert(TabExtIL);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2, 'File ~p~atterns needing tabs', TabExtIL)));
  end;
  InsertButtons(D);
  CB^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    TabSize:=StrToInt(IL^.Data^); EFlags:=CB^.Value;
    if Editor=nil then
       begin
         DefaultTabSize:=TabSize;
         DefaultCodeEditorFlags:=EFlags;
       end
    else
       begin
         Editor^.TabSize:=TabSize;
         Editor^.SetFlags(EFlags);
       end;
    ExtIL^.GetData(HighlightExts);
    TabExtIL^.GetData(TabsPattern);
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.Mouse;
var R,R2: TRect;
    D: PCenterDialog;
    SB: PScrollBar;
    CB: PCheckBoxes;
    RB1,RB2: PRadioButtons;
begin
  R.Assign(0,0,62,15);
  New(D, Init(R, 'Mouse Options'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-2); R.B.Y:=R.A.Y+3;
    R2.Copy(R); Inc(R2.A.Y,2); R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2 -1;
    New(SB, Init(R2)); SB^.GrowMode:=0; SB^.Options:=SB^.Options or ofSelectable;
    SB^.SetParams(DoubleDelay,1,20,1,1);
    Insert(SB);
    R2.Move(0,-1);
    Insert(New(PStaticText, Init(R2, 'Fast       Medium      Slow')));
    R2.Move(-1,-1);
    Insert(New(PLabel, Init(R2, '~M~ouse double click', SB)));

    R2.Copy(R); Inc(R2.A.Y,2); R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2 +1;
    New(CB, Init(R2, NewSItem('~R~everse mouse buttons', nil) ));
    if MouseReverse then CB^.Press(0);
    Insert(CB);

    R.Move(0,(R.B.Y-R.A.Y)+1); R.B.Y:=R.A.Y+8;
    R2.Copy(R); Inc(R2.A.Y); R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2 -1;
    New(RB1, Init(R2,
      NewSItem('~N~othing',
      NewSItem('~T~opic search',
      NewSItem('~G~o to cursor',
      NewSItem('~B~reakpoint',
      NewSItem('~E~valuate',
      NewSItem('~A~dd watch',
      NewSItem('Browse ~s~ymbol',
      nil)))))))));
    RB1^.Press(CtrlMouseAction);
    Insert(RB1);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Ctrl+Right mouse button', RB1)));

    R2.Copy(R); Inc(R2.A.Y); R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2 +1;
    New(RB2, Init(R2,
      NewSItem('N~o~thing',
      NewSItem('To~p~ic search',
      NewSItem('Go to ~c~ursor',
      NewSItem('Brea~k~point',
      NewSItem('E~v~aluate',
      NewSItem('Add ~w~atch',
      NewSItem('Browse s~y~mbol',
      nil)))))))));
    RB2^.Press(AltMouseAction);
    Insert(RB2);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Alt+Right mouse button', RB2)));
  end;
  InsertButtons(D);
  RB1^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    MouseReverse:=CB^.Mark(0);
    DoubleDelay:=SB^.Value;
    CtrlMouseAction:=RB1^.Value;
    AltMouseAction:=RB2^.Value;
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.Colors;
var D: PColorDialog;
begin
  New(D, Init(AppPalette,
    ColorGroup('Browser',
      ColorItem('Frame passive'   , 215,
      ColorItem('Frame active'    , 216,
      ColorItem('Frame icon'      , 217,
      ColorItem('Scroll bar page' , 218,
      ColorItem('Scroll bar icons', 219,
      ColorItem('Normal text'     , 220,
      ColorItem('Selected text'   , 221,
      ColorItem('Active item'     , 222,
      ColorItem('Inactive item'   , 223,
      ColorItem('Focused item'    , 224,
      ColorItem('Selected item'   , 225,
      ColorItem('Divider'         , 226,
      nil)))))))))))),
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
      ColorItem('Hex numbers'     , 212,
      ColorItem('Assembler'       , 206,
      ColorItem('Symbols'         , 207,
      ColorItem('Directives'      , 211,
      ColorItem('Tabs'            , 213,
      nil))))))))))),
    nil)))))))));
  if ExecuteDialog(D, @AppPalette)=cmOK then
    begin
      DoneMemory;
      Message(Application,evBroadcast,cmUpdate,nil);
      ReDraw;
      UpdateScreen(true);
    end;
end;

procedure TIDEApp.OpenINI;
var D: PFileDialog;
    FileName: string;
begin
  New(D, Init(ININame,'Open Options',ININame,fdOpenButton,0));
  if Desktop^.ExecView(D)<>cmCancel then
    begin
      D^.GetFileName(FileName);
      if ExistsFile(FileName)=false then ErrorBox('Can''t open config file.',nil) else
        begin
          INIPath:=FileName;
          ReadINIFile;
          Message(Application,evBroadcast,cmUpdate,nil);
        end;
    end;
  Dispose(D, Done);
end;

procedure TIDEApp.SaveINI;
begin
  if WriteINIFile=false then
    ErrorBox('Error saving config file.',nil);
end;

procedure TIDEApp.SaveAsINI;
var D: PFileDialog;
    FileName: string;
    CanWrite: boolean;
begin
  New(D, Init(ININame,'Save Options',ININame,fdOpenButton,0));
  if Desktop^.ExecView(D)<>cmCancel then
    begin
      D^.GetFileName(FileName);
      CanWrite:=(ExistsFile(FileName)=false);
      if CanWrite=false then
        CanWrite:=ConfirmBox('File '+SmartPath(FileName)+' already exists. Overwrite?',nil,false)=cmYes;
      if CanWrite then
        begin
          INIPath:=FileName;
          if WriteINIFile=false then
            ErrorBox('Error saving config file.',nil);
          Message(Application,evBroadcast,cmUpdate,nil);
        end;
    end;
  Dispose(D, Done);
end;

{
  $Log$
  Revision 1.13  1999-02-10 09:52:43  pierre
   * MemorySizeSwitches -> MemorySwitches (were duplicates)

  Revision 1.12  1999/02/08 17:41:34  pierre
   + CustomArg added to debug dialog

  Revision 1.11  1999/02/08 10:37:45  peter
    + html helpviewer

  Revision 1.10  1999/02/06 00:07:47  florian
    * speed/size optimization is now a radio button

  Revision 1.9  1999/02/05 12:11:58  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.8  1999/02/04 13:32:06  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.7  1999/01/22 10:24:04  peter
    * first debugger things

  Revision 1.6  1999/01/21 11:54:19  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.5  1999/01/12 14:29:35  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.4  1999/01/04 11:49:47  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:49  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.3  1998/12/22 10:39:48  peter
    + Switches are now written/read
    + find and replace routines

}
