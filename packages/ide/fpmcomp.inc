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
var R: TRect;
    D: PFPSwitchesDialog;
    TargetHeight: Sw_Integer;
    L: longint;
begin
  TargetHeight:=TargetSwitches^.ItemCount;
  GetExtent(R);
  if (R.B.Y-R.A.Y) < (TargetHeight+9) then TargetHeight:= (R.B.Y-R.A.Y)-9;
  if TargetHeight<3 then TargetHeight:=3;
  R.Assign(0,0,60,4+TargetHeight);
  New(D, Init(R, dialog_target, label_target_platform, TargetSwitches));
  D^.HelpCtx:=hcTarget;
  L:=D^.RB^.Value;
  if Desktop^.ExecView(D)=cmOK then
    begin
       TargetSwitches^.SetCurrSel(D^.RB^.Value);
       { we always need to recompile if we change
         the target OS PM }
       if L<>D^.RB^.Value then
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

