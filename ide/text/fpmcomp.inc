{
    $Id$
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
var R,R2: TRect;
    D: PCenterDialog;
    RB: PRadioButtons;
    TargetCount,I: integer;
    LastItem: PSItem;
    L: longint;
begin
  TargetCount:=TargetSwitches^.ItemCount;
  R.Assign(0,0,36,4+TargetCount);
  New(D, Init(R, dialog_target));
  with D^ do
  begin
    GetExtent(R); R.Grow(-3,-1); Inc(R.A.Y);
    R2.Copy(R); Inc(R2.A.Y); R2.B.Y:=R2.A.Y+TargetCount;
    LastItem:=nil;
    for I:=TargetCount-1 downto 0 do
      LastItem:=NewSItem(TargetSwitches^.ItemName(I), LastItem);
    New(RB, Init(R2, LastItem));
    L:=ord(TargetSwitches^.GetCurrSel);
    RB^.SetData(L);
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
  D : PFileDialog;
  FileName : string;
begin
  New(D, Init('*.pri;*.pas',label_primaryfile_primaryfile,'*.pri;*.pas',fdOpenButton,0));
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


procedure TIDEApp.DoInformation;
begin
  if ProgramInfoWindow=nil then
    begin
      New(ProgramInfoWindow, Init);
      Desktop^.Insert(ProgramInfoWindow);
    end;
  with ProgramInfoWindow^ do
   begin
     if not GetState(sfVisible) then
       Show;
     MakeFirst;
   end;
end;


procedure TIDEApp.DoCompilerMessages;
begin
  if not CompilerMessageWindow^.GetState(sfVisible) then
   CompilerMessageWindow^.Show;
  CompilerMessageWindow^.MakeFirst;
end;

{
  $Log$
  Revision 1.1  2000-07-13 09:48:35  michael
  + Initial import

  Revision 1.9  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.8  2000/03/07 21:45:33  pierre
   + TIDEApp.UpdateMode

  Revision 1.7  2000/01/10 14:59:08  pierre
   * force recompiling if we change target OS

  Revision 1.6  1999/06/25 00:37:56  pierre
   + UpdateTarget to see current target main in Menu

  Revision 1.5  1999/04/07 21:55:49  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.4  1999/03/19 16:04:30  peter
    * new compiler dialog

  Revision 1.3  1999/03/12 01:13:59  peter
    * flag if trytoopen should look for other extensions
    + browser tab in the tools-compiler

  Revision 1.2  1998/12/28 15:47:46  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.3  1998/12/22 10:39:44  peter
    + Switches are now written/read
    + find and replace routines

}