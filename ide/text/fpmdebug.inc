{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Debug menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.ShowUserScreen;
begin
  if UserScreen=nil then
   begin
     ErrorBox('Sorry, user screen not available.',nil);
     Exit;
   end;
  DoneMouse;
  DoneVideo;

  UserScreen^.SwitchTo;
  Keyboard.GetKeyEvent;
  while (Keyboard.PollKeyEvent<>0) do
   Keyboard.GetKeyEvent;
  UserScreen^.SwitchBack;

  InitVideo;
  InitMouse;
  ReDraw;
  UpdateScreen(true);
end;

{
  $Log$
  Revision 1.1  1998-12-28 15:47:47  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
