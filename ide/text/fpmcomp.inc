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
  New(D, Init(R, 'Target'));
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
    Insert(New(PLabel, Init(R2, 'Target platform', RB)));
  end;
  InsertButtons(D);
  RB^.Select;
  if Desktop^.ExecView(D)=cmOK then
    TargetSwitches^.SetCurrSel(RB^.Value);
  Dispose(D, Done);
end;

procedure TIDEApp.PrimaryFile_;
var
  D : PFileDialog;
  FileName : string;
begin
  New(D, Init('*.pas','Primary file','*.pas',fdOpenButton,0));
  if Desktop^.ExecView(D)<>cmCancel then
  begin
    D^.GetFileName(FileName);
    PrimaryFile:=FileName;
    UpdatePrimaryFile;
  end;
end;


procedure TIDEApp.ClearPrimary;
begin
  PrimaryFile:='';
  UpdatePrimaryFile;
end;


procedure TIDEApp.Information;
begin
  with ProgramInfoWindow^ do
   begin
     if not GetState(sfVisible) then
       Show;
     MakeFirst;
   end;
end;

{
  $Log$
  Revision 1.2  1998-12-28 15:47:46  peter
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
