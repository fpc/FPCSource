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
    RB1,RB2,RB3,RB4: PRadioButtons;
    Items: PSItem;
    IL: PInputLine;
    Count : integer;
    I,L: longint;
    Tab: PTab;
    Label11,Label12,
    Label21,Label22,Label23,
    Label31,Label41: PLabel;
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
    R2.B.X:=(R2.A.X+(R2.B.X-R2.A.X) div 2)-2;
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

    Count:=AsmReaderSwitches^.ItemCount;
    R2.Copy(TabIR);
    R2.A.X:=R2.B.X-(R2.B.X-R2.A.X) div 2;
    Dec(R2.B.X,4);
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(AsmReaderSwitches^.ItemName(I), Items);
    New(RB2, Init(R2, Items));
    L:=AsmReaderSwitches^.GetCurrSel;
    RB2^.SetData(L);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label12, Init(R2, 'Assembler format', RB2));

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
    Inc(R2.A.Y,CodegenSwitches^.ItemCount+2);
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

    { --- Sheet 4 --- }
    Count:=BrowserSwitches^.ItemCount;
    R.Copy(TabIR);
    R2.Copy(R);
    R2.B.X:=R2.A.X+(R2.B.X-R2.A.X) div 2;
    R2.B.Y:=R2.A.Y+Count;
    Items:=nil;
    for I:=Count-1 downto 0 do
      Items:=NewSItem(BrowserSwitches^.ItemName(I), Items);
    New(RB4, Init(R2, Items));
    L:=BrowserSwitches^.GetCurrSel;
    RB4^.SetData(L);
    Dec(R2.A.Y);
    R2.B.Y:=R2.A.Y+1;
    New(Label41, Init(R2, 'Browser', RB4));

    { create tabs }
    New(Tab, Init(TabR,
      NewTabDef('S~y~ntax',CB1,
        NewTabItem(Label11,
        NewTabItem(CB1,
        NewTabItem(Label12,
        NewTabItem(RB2,
        nil)))),
      NewTabDef('Code ~g~eneration',CB3,
        NewTabItem(Label21,
        NewTabItem(CB3,
        NewTabItem(Label22,
        NewTabItem(CB2,
        NewTabItem(Label23,
        NewTabItem(RB3,
        NewTabItem(RB1,
        nil))))))),
      NewTabDef('~V~erbose',CB4,
        NewTabItem(Label31,
        NewTabItem(CB4,
        nil)),
      NewTabDef('~B~rowser',RB4,
        NewTabItem(Label41,
        NewTabItem(RB4,
        nil)),
      nil))))));
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
    BrowserSwitches^.SetCurrSel(RB4^.Value);
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
    RB : PRadioButtons;
    CB: PCheckBoxes;
{$ifdef win32}
    CB2: PCheckBoxes;
{$endif win32}
{$ifdef linux}
    IL: PInputLine;
{$endif linux}
    IL2: PInputLine;
    L,I: longint;
    Items: PSItem;
const
{$ifdef win32}
  OtherFieldLines = 3;
{$else not win32}
{$ifdef linux}
  OtherFieldLines = 3;
{$else not linux}
  OtherFieldLines = 0;
{$endif linux}
{$endif win32}
begin
  R.Assign(0,0,60,2+DebugInfoSwitches^.ItemCount+2
      +ProfileInfoSwitches^.ItemCount+2+2+1+OtherFieldLines);
  New(D, Init(R, 'Browsing/Debugging/Profiling'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1);
    R2.Copy(R); Inc(R2.A.Y,2); R2.B.Y:=R2.A.Y+DebugInfoSwitches^.ItemCount;
    Items:=nil;
    for I:=DebugInfoSwitches^.ItemCount-1 downto 0 do
      Items:=NewSItem(DebugInfoSwitches^.ItemName(I), Items);
    New(RB, Init(R2, Items));
    L:=DebugInfoSwitches^.GetCurrSel;
    RB^.SetData(L);
    Insert(RB);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Debugging information', RB)));

    R2.Copy(R); Inc(R2.A.Y,2+DebugInfoSwitches^.ItemCount+2); R2.B.Y:=R2.A.Y+ProfileInfoSwitches^.ItemCount;
    Items:=nil;
    for I:=ProfileInfoSwitches^.ItemCount-1 downto 0 do
      Items:=NewSItem(ProfileInfoSwitches^.ItemName(I), Items);
    New(CB, Init(R2, Items));
    L:=ProfileInfoSwitches^.GetCurrSel;
    If L = 1 then L:=3;
    CB^.SetData(L);
    Insert(CB);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Profiling Switches', CB)));

    {custom }
    Inc(R2.A.Y,1+DebugInfoSwitches^.ItemCount+2); R2.B.Y:=R2.A.Y+1;
    New(IL2, Init(R2, 255));
    IL2^.Data^:=CustomArg[SwitchesMode];
    Insert(IL2);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2,'~A~dditional compiler args', IL2)));
{$ifdef win32}
    R2.Move(0,4);
    New(CB2,Init(R2,NewSItem('Use ~A~nother console ', nil)));
    Insert(CB2);
    if DebuggeeTTY<>'' then
      L:=1
    else
      L:=0;
    CB2^.SetData(L);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2,'Debuggee ~R~edirection', CB2)));
{$endif win32}
{$ifdef linux}
    R2.Move(0,4);
    New(IL, Init(R2, 255));
    IL^.Data^:=DebuggeeTTY;
    Insert(IL);
    R2.Move(0,-1);
    Insert(New(PLabel, Init(R2,'Use ~A~nother tty for Debuggee', IL)));
{$endif win32}
  end;
  InsertButtons(D);
  RB^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    DebugInfoSwitches^.SetCurrSel(RB^.Value);
    {Writeln(':',CB^.Value,':');
    readln;}
    if CB^.Value = 3 then
      I := 1
    else
      I := 0;
    ProfileInfoSwitches^.SetCurrSel(I);
    CustomArg[SwitchesMode]:=IL2^.Data^;
{$ifdef win32}
    if CB2^.value<>0 then
      DebuggeeTTY:='on'
    else
      DebuggeeTTY:='';
{$endif win32}
{$ifdef linux}
    DebuggeeTTY:=IL^.Data^;
{$endif linux}
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

(*procedure TIDEApp.Preferences;
var R,R2: TRect;
    D: PCenterDialog;
    RB1 : PRadioButtons;
    CountModes : integer;
    hp : pvideomodelist;
    items : PSItem;
    videomode : tvideomode;
    i,modevalue : longint;


  function ToStr(l : longint) : string;

    var
       s : string;

    begin
       str(l,s);
       ToStr:=s;
    end;

  const
     color2str : array[false..true] of string = ('in b/w','in color');

begin
  GetVideoMode(videomode);
  CountModes:=0;
  i:=0;
  modevalue:=0;
  R.Assign(0,0,64,18);
  New(D, Init(R, 'Preferences'));
  with D^ do
  begin
     hp:=video.modes;
     items:=nil;
     r2.assign(2,3,24,17);
     while assigned(hp) do
       begin
          items:=NewSItem(ToStr(hp^.col)+'x'+ToStr(hp^.row)+' '+color2str[hp^.color],items);
          if (hp^.col=videomode.col) and (hp^.row=videomode.row) and
            (hp^.color=videomode.color) then
            modevalue:=i;
          inc(CountModes);
          { we can't display an infinite number of modes }
          if CountModes>=r.b.y-r.a.y+1 then
            break;
          inc(i);
          hp:=hp^.next;
       end;
     modevalue:=CountModes-modevalue-1;
     new(rb1,init(r2,items));
     insert(rb1);
     rb1^.value:=modevalue;

     r2.move(0,-1);r2.b.y:=r2.a.y+1;
     insert(new(plabel,init(r2,'~V~ideo mode',rb1)));
  end;
  InsertButtons(D);
  if Desktop^.ExecView(D)=cmOK then
   begin
      { change video mode ? }
      if rb1^.value<>modevalue then
        begin
           modevalue:=CountModes-rb1^.value-1;
           hp:=video.modes;
           for i:=1 to modevalue do
             hp:=hp^.next;
           videomode.col:=hp^.col;
           videomode.row:=hp^.row;
           videomode.color:=hp^.color;
           SetScreenVideoMode(videomode);
        end;
   end;
  Dispose(D, Done);
end;*)

type
     PVideoModeCollection = ^TVideoModeCollection;
     TVideoModeCollection = object(TSortedCollection)
       function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
       procedure FreeItem(Item: Pointer); virtual;
     end;

function TVideoModeCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var R: Sw_integer;
    K1: PVideoModeList absolute Key1;
    K2: PVideoModeList absolute Key2;
begin
  if K1^.Col<K2^.Col then R:=-1 else
  if K1^.Col>K2^.Col then R:= 1 else
  if K1^.Row<K2^.Row then R:=-1 else
  if K1^.Row>K2^.Row then R:= 1 else
  if (K1^.Color=false) and (K2^.Color=true ) then R:=-1 else
  if (K1^.Color=true ) and (K2^.Color=false) then R:= 1 else
  R:=0;
  Compare:=R;
end;

procedure TVideoModeCollection.FreeItem(Item: Pointer);
begin
  { don't do anything here }
end;

procedure TIDEApp.Preferences;
function SearchVideoMode(Col,Row: word; Color: boolean): PVideoModeList;
var I,P: PVideoModeList;
begin
  I:=nil; P:=Video.Modes;
  while (I=nil) and (P<>nil) do
    begin
      if (P^.Col=Col) and (P^.Row=Row) and (P^.Color=Color) then
        I:=P
      else
        P:=P^.Next;
    end;
  SearchVideoMode:=I;
end;
var R,R2: TRect;
    D: PCenterDialog;
    C: PVideoModeCollection;
    VMLB: PVideoModeListBox;
    VP: PVideoModeList;
    VM: TVideoMode;
    RB1: PPlainRadioButtons;
    CB1,CB2: PPlainCheckBoxes;
    CurM: PVideoModeList;
    CurIdx: integer;
begin
  New(C, Init(10,50));
  VP:=Video.Modes; CurM:=nil;
  while VP<>nil do
    begin
      C^.Insert(VP);
      if (VP^.Row=ScreenMode.Row) and (VP^.Col=ScreenMode.Col) and
         (VP^.Color=ScreenMode.Color) then
       CurM:=VP;
      VP:=VP^.Next;
    end;
  R.Assign(0,0,64,15);
  New(D, Init(R, 'Preferences'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2);
    R.B.X:=R.A.X+(R.B.X-R.A.X) div 2 - 1;

    R.B.Y:=R.A.Y+3;
    R2.Copy(R); R2.Grow(-1,-1);
    New(VMLB, Init(R2, Min(4,C^.Count), C));
    if CurM=nil then CurIdx:=-1 else
      CurIdx:=C^.IndexOf(CurM);
    if CurIdx<>-1 then
      VMLB^.FocusItem(CurIdx);
    Insert(New(PGroupView, Init(R, 'Video mode', VMLB)));
    Insert(VMLB);

    R.Move(0,R.B.Y-R.A.Y{+1}); R.B.Y:=R.A.Y+4;
    R2.Copy(R); R2.Grow(-1,-1);
    New(RB1, Init(R2,
      NewSItem('C~u~rrent directory',
      NewSItem('Conf~i~g file directory',
      nil))));
    RB1^.Press(DesktopLocation);
    Insert(New(PGroupView, Init(R, 'Desktop file', RB1)));
    Insert(RB1);

    R.Move(0,R.B.Y-R.A.Y{+1}); R.B.Y:=R.A.Y+5;
    R2.Copy(R); R2.Grow(-1,-1);
    New(CB1, Init(R2,
      NewSItem('Editor ~f~iles',
      NewSItem('~E~nvironment',
      NewSItem('~D~esktop',
      nil)))));
    CB1^.Value:=AutoSaveOptions;
    Insert(New(PGroupView, Init(R, 'Auto save', CB1)));
    Insert(CB1);

    GetExtent(R); R.Grow(-2,-2);
    R.A.X:=R.B.X-(R.B.X-R.A.X) div 2 + 1;
    R.B.Y:=R.A.Y+7;

    R.Move(0,R.B.Y-R.A.Y{+1}); R.B.Y:=R.A.Y+5;
    R2.Copy(R); R2.Grow(-1,-1);
    New(CB2, Init(R2,
      NewSItem('~A~uto track source',
      NewSItem('C~l~ose on go to source',
      NewSItem('C~h~ange dir on open',
      nil)))));
    CB2^.Value:=MiscOptions;
    Insert(New(PGroupView, Init(R, 'Options', CB2)));
    Insert(CB2);

  end;
  InsertButtons(D);
  if Desktop^.ExecView(D)=cmOK then
   begin
     with PVideoModeList(C^.At(VMLB^.Focused))^ do
      begin
        VM.Col:=Col;
        VM.Row:=Row;
        VM.Color:=Color;
      end;
     if (VM.Col<>ScreenMode.Col) or (VM.Row<>ScreenMode.Row) or (VM.Color<>ScreenMode.Color) then
       SetScreenVideoMode(VM);
     AutoSaveOptions:=CB1^.Value;
     MiscOptions:=CB2^.Value;
     DesktopLocation:=RB1^.Value;
   end;
  Dispose(D, Done);
  Dispose(C, Done);
end;

procedure TIDEApp.EditorOptions(Editor: PEditor);
var D: PCenterDialog;
    R,R2,R3: TRect;
    CB: PCheckBoxes;
    IL: PIntegerLine;
    ExtIL,TabExtIL: PInputLine;
    TabSize: Integer;
    EFlags,EFValue: Longint;
    Title: string;
begin
  if Editor=nil then
    begin
      TabSize:=DefaultTabSize; EFlags:=DefaultCodeEditorFlags;
      Title:='Default Editor Options';
    end
  else
    begin
      TabSize:=Editor^.TabSize; EFlags:=Editor^.Flags;
      Title:='Editor Options';
    end;

  EFValue:=0;
  if (EFlags and efBackupFiles       )<>0 then EFValue:=EFValue or (1 shl  0);
  if (EFlags and efInsertMode        )<>0 then EFValue:=EFValue or (1 shl  1);
  if (EFlags and efAutoIndent        )<>0 then EFValue:=EFValue or (1 shl  2);
  if (EFlags and efUseTabCharacters  )<>0 then EFValue:=EFValue or (1 shl  3);
  if (EFlags and efBackSpaceUnindents)<>0 then EFValue:=EFValue or (1 shl  4);
  if (EFlags and efPersistentBlocks  )<>0 then EFValue:=EFValue or (1 shl  5);
  if (EFlags and efSyntaxHighlight   )<>0 then EFValue:=EFValue or (1 shl  6);
  if (EFlags and efBlockInsCursor    )<>0 then EFValue:=EFValue or (1 shl  7);
  if (EFlags and efVerticalBlocks    )<>0 then EFValue:=EFValue or (1 shl  8);
  if (EFlags and efHighlightColumn   )<>0 then EFValue:=EFValue or (1 shl  9);
  if (EFlags and efHighlightRow      )<>0 then EFValue:=EFValue or (1 shl 10);
  if (EFlags and efAutoBrackets      )<>0 then EFValue:=EFValue or (1 shl 11);
  if (EFlags and efKeepTrailingSpaces)<>0 then EFValue:=EFValue or (1 shl 12);
  if (EFlags and efCodeComplete      )<>0 then EFValue:=EFValue or (1 shl 13);

  R.Assign(0,0,56,19);
  New(D, Init(R, Title));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2); R.B.Y:=R.A.Y+8;
    R2.Copy(R); Inc(R2.A.Y);
    New(CB, Init(R2,
      NewSItem('Create backup ~f~iles',
      NewSItem('~I~nsert mode',
      NewSItem('~A~uto indent mode',
      NewSItem('~U~se tab characters',
      NewSItem('~B~ackspace unindents',
      NewSItem('~P~ersistent blocks',
      NewSItem('~S~yntax highlight',
      NewSItem('B~l~ock insert cursor',
      NewSItem('~V~ertical blocks',
      NewSItem('Highlight ~c~olumn',
      NewSItem('Highlight ~r~ow',
      NewSItem('Aut~o~-closing brackets',
      NewSItem('~K~eep trailing spaces',
      NewSItem('Co~d~eComplete enabled',
      nil))))))))))))))));
    CB^.Value:=EFValue;
    Insert(CB);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, '~E~ditor options', CB)));

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
    EFlags:=0;
    if (CB^.Value and (1 shl  0))<>0 then EFlags:=EFlags or efBackupFiles;
    if (CB^.Value and (1 shl  1))<>0 then EFlags:=EFlags or efInsertMode;
    if (CB^.Value and (1 shl  2))<>0 then EFlags:=EFlags or efAutoIndent;
    if (CB^.Value and (1 shl  3))<>0 then EFlags:=EFlags or efUseTabCharacters;
    if (CB^.Value and (1 shl  4))<>0 then EFlags:=EFlags or efBackSpaceUnindents;
    if (CB^.Value and (1 shl  5))<>0 then EFlags:=EFlags or efPersistentBlocks;
    if (CB^.Value and (1 shl  6))<>0 then EFlags:=EFlags or efSyntaxHighlight;
    if (CB^.Value and (1 shl  7))<>0 then EFlags:=EFlags or efBlockInsCursor;
    if (CB^.Value and (1 shl  8))<>0 then EFlags:=EFlags or efVerticalBlocks;
    if (CB^.Value and (1 shl  9))<>0 then EFlags:=EFlags or efHighlightColumn;
    if (CB^.Value and (1 shl 10))<>0 then EFlags:=EFlags or efHighlightRow;
    if (CB^.Value and (1 shl 11))<>0 then EFlags:=EFlags or efAutoBrackets;
    if (CB^.Value and (1 shl 12))<>0 then EFlags:=EFlags or efKeepTrailingSpaces;
    if (CB^.Value and (1 shl 13))<>0 then EFlags:=EFlags or efCodeComplete;
    TabSize:=StrToInt(IL^.Data^);
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

procedure TIDEApp.CodeComplete;
begin
  ExecuteDialog(New(PCodeCompleteDialog, Init),nil);
end;

procedure TIDEApp.CodeTemplates;
begin
  ExecuteDialog(New(PCodeTemplatesDialog, Init),nil);
end;

procedure TIDEApp.BrowserOptions(Browser: PBrowserWindow);
var D: PCenterDialog;
    R,R2,R3 : TRect;
    PreS: string[15];
    CB1,CB2: PCheckBoxes;
    RB1,RB2: PRadioButtons;
begin
  if Browser=nil then
    begin
      PreS:='';
    end
  else
    begin
      PreS:='Local ';
    end;
  R.Assign(0,0,56,15);
  New(D, Init(R, PreS+'Browser Options'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2);
    R.B.Y:=R.A.Y+1+3; R2.Copy(R); Inc(R2.A.Y);
    New(CB1, Init(R2,
      NewSItem(RExpand('~L~abels',21+2),
      NewSItem('~C~onstants',
      NewSItem('~T~ypes',
      NewSItem('~V~ariables',
      NewSItem('~P~rocedures',
      NewSItem('~I~nherited',
      nil)))))))
    );
    Insert(CB1);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Symbols', CB1)));

    R.Move(0,R.B.Y-R.A.Y+1);
    R.B.Y:=R.A.Y+1+2; R2.Copy(R);
    R3.Copy(R2); R3.B.X:=R3.A.X+(R3.B.X-R3.A.X) div 2-1; Inc(R3.A.Y);
    New(RB1, Init(R3,
      NewSItem('~N~ew browser',
      NewSItem('~R~eplace current',
      nil)))
    );
    Insert(RB1);
    R3.Move(0,-1); R3.B.Y:=R3.A.Y+1;
    Insert(New(PLabel, Init(R3, 'Sub-browsing', RB1)));
    R3.Copy(R2); R3.A.X:=R3.B.X-(R3.B.X-R3.A.X) div 2+1; Inc(R3.A.Y);
    New(RB2, Init(R3,
      NewSItem('~S~cope',
      NewSItem('R~e~ference',
      nil)))
    );
    Insert(RB2);
    R3.Move(0,-1); R3.B.Y:=R3.A.Y+1;
    Insert(New(PLabel, Init(R3, 'Preferred pane', RB2)));

    R.Move(0,R.B.Y-R.A.Y+1);
    R.B.Y:=R.A.Y+1+1; R2.Copy(R); Inc(R2.A.Y);
    New(CB2, Init(R2,
      NewSItem(RExpand('~Q~ualified symbols',21+2),
      NewSItem('S~o~rt always',
      nil)))
    );
    Insert(CB2);
    R2.Move(0,-1); R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, 'Display', CB2)));
  end;
  InsertButtons(D);
  CB1^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
  end;
  Dispose(D, Done);
end;

procedure TIDEApp.Startup;
begin
  NotImplemented;
end;

procedure TIDEApp.DesktopOptions;
var R: TRect;
    D: PCenterDialog;
    CB: PCheckBoxes;
begin
  R.Assign(0,0,40,12);
  New(D, Init(R, 'Desktop Preferences'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.B.Y:=R.A.Y+8;
    New(CB, Init(R,
      NewSItem('~H~istory lists',
      NewSItem('~C~lipboard content',
      NewSItem('~W~atch expressions',
      NewSItem('~B~reakpoints',
      NewSItem('~O~pen windows',
      NewSItem('~S~ymbol information',
      NewSItem('Co~d~eComplete wordlist',
      NewSItem('Code~T~emplates',
      nil))))))))));
    CB^.Value:=DesktopFileFlags;
    Insert(CB);
    R.Move(0,-1); R.B.Y:=R.A.Y+1;
    Insert(New(PLabel, Init(R, '~P~reserved across sessions', CB)));
  end;
  InsertButtons(D);
  CB^.Select;
  if Desktop^.ExecView(D)=cmOK then
    begin
      DesktopFileFlags:=CB^.Value;
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
    ColorGroup('Clock',
      ColorItem('clock view'      , 227,
      nil),
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
    nil))))))))));
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
  Revision 1.28  2000-02-07 12:02:32  pierre
   Gabor's changes

  Revision 1.27  1999/11/10 17:18:17  pierre
   + new console for Win32 or other tty for linux

  Revision 1.26  1999/10/14 10:22:50  pierre
   + color selection for ClockView

  Revision 1.25  1999/08/16 18:25:21  peter
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

  Revision 1.24  1999/03/23 16:16:40  peter
    * linux fixes

  Revision 1.23  1999/03/23 15:11:33  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.22  1999/03/21 22:51:36  florian
    + functional screen mode switching added

  Revision 1.21  1999/03/16 12:38:12  peter
    * tools macro fixes
    + tph writer
    + first things for resource files

  Revision 1.20  1999/03/14 22:18:16  florian
    + options/preferences dialog added, without function yet

  Revision 1.19  1999/03/12 01:14:00  peter
    * flag if trytoopen should look for other extensions
    + browser tab in the tools-compiler

  Revision 1.18  1999/03/01 15:41:57  peter
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

  Revision 1.17  1999/02/22 11:51:37  peter
    * browser updates from gabor

  Revision 1.16  1999/02/18 13:44:32  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.15  1999/02/16 17:15:28  pierre
   * Peter's debug code commented

  Revision 1.14  1999/02/16 10:43:56  peter
    * use -dGDB for the compiler
    * only use gdb_file when -dDEBUG is used
    * profiler switch is now a toggle instead of radiobutton

  Revision 1.13  1999/02/10 09:52:43  pierre
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
