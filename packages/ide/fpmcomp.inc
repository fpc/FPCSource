{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Compiler menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.Target;
var R,R2,R3: TRect;
    D: PCenterDialog;
    RB: PScrollerRadioButtons;
    VScrollBar : PScrollBar;
    TargetCount,TargetHeight,I: Sw_Integer;
    LastItem: PSItem;
    L: longint;
begin
  TargetCount:=TargetSwitches^.ItemCount;
  TargetHeight:=TargetCount;
  GetExtent(R);
  if (R.B.Y-R.A.Y) < (TargetHeight+9) then TargetHeight:= (R.B.Y-R.A.Y)-9;
  if TargetHeight<3 then TargetHeight:=3;
  R.Assign(0,0,60,4+TargetHeight);
  New(D, Init(R, dialog_target));
  with D^ do
  begin
    HelpCtx:=hcTarget;
    GetExtent(R); R.Grow(-3,-1); Inc(R.A.Y);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+TargetHeight;
    {have scroll bar for Targets only if they does not fit in view}
    VScrollBar :=nil;
    if TargetHeight<>TargetCount then
    begin
      Dec(R2.B.X);
      R3.Copy(R2);R3.A.X:=R3.B.X; R3.B.X:=R3.B.X+1;
      R3.B.Y:=R3.B.Y-3; {-3 because of InsertButtons later}
      VScrollBar := New(PScrollBar, Init(R3));
      Insert(VScrollBar);
    end;
    LastItem:=nil;
    for I:=TargetCount-1 downto 0 do
      LastItem:=NewSItem(TargetSwitches^.ItemName(I), LastItem);
    New(RB, Init(R2, LastItem, VScrollBar));
    L:=ord(TargetSwitches^.GetCurrSel);
    RB^.SetData(L);
    RB^.CentreSelected;
    Insert(RB);
    R2.Copy(R);
    R2.B.Y:=R2.A.Y+1;
    Insert(New(PLabel, Init(R2, label_target_platform, RB)));
  end;
  InsertButtons(D);
  RB^.Select;
  if Desktop^.ExecView(D)=cmOK then
    begin
       TargetSwitches^.SetCurrSel(RB^.Value);
       { we allways need to recompile if we change
         the target OS PM }
       if L<>RB^.Value then
         PrevMainFile:='';
       UpdateTarget;
       UpdateAsmOutputSwitches;
    end;
  Dispose(D, Done);
end;

procedure TIDEApp.UpdateTarget;
var TargetMenu : PMenuItem;
begin
  TargetMenu:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmTarget);
  if assigned(TargetMenu) then
    begin
      If assigned(TargetMenu^.Param) then
        DisposeStr(TargetMenu^.Param);
      TargetMenu^.Param:=NewStr(KillTilde(TargetSwitches^.ItemName(TargetSwitches^.GetCurrSel)));
    end;
end;

procedure TIDEApp.UpdateMode;
var ModeMenu : PMenuItem;
begin
  ModeMenu:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cmSwitchesMode);
  if assigned(ModeMenu) then
    begin
      If assigned(ModeMenu^.Param) then
        DisposeStr(ModeMenu^.Param);
      ModeMenu^.Param:=NewStr(KillTilde(SwitchesModeName[SwitchesMode]));
    end;
end;

procedure TIDEApp.DoPrimaryFile;
var
  D : PFPFileDialog;
  FileName : string;
begin
  New(D, Init('*.pri;*.pas',label_primaryfile_primaryfile,'*.pri;*.pas',fdOpenButton,hidPrimaryFile));
  D^.HelpCtx:=hcPrimaryFile;
  if Desktop^.ExecView(D)<>cmCancel then
  begin
    D^.GetFileName(FileName);
    SetPrimaryFile(FileName);
    UpdatePrimaryFile;
  end;
end;


procedure TIDEApp.DoClearPrimary;
begin
  PrimaryFile:='';
  PrimaryFileMain:='';
  PrimaryFileSwitches:='';
  PrimaryFilePara:='';
  UpdatePrimaryFile;
end;


procedure TIDEApp.DoCompilerMessages;
begin
  if not CompilerMessageWindow^.GetState(sfVisible) then
   CompilerMessageWindow^.Show;
  CompilerMessageWindow^.MakeFirst;
end;

