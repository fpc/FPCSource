{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Compiler call routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPRun;
interface

procedure DoRun;


implementation

uses
  Dos,Mouse,Video,
  FPViews,FPVars,FPUtils,FPIntf,
  FPCompile,FPUsrScr;

procedure TIDEApp.DoRun;
var
  ExeFile : string;
begin
  if (MainFile='') or (CompilationPhase<>cpDone) then
   DoCompile(cRun);
  if (MainFile='') or (CompilationPhase<>cpDone) then
   begin
     ErrorBox('Oooops, nothing to run.',nil);
     Exit;
   end;
  ExeFile:=MakeExeName(MainFile);

  if UserScreen=nil then
   begin
     ErrorBox('Sorry, user screen not available.',nil);
     Exit;
   end;
  DoneMouse;
  DoneVideo;

  UserScreen^.SwitchTo;

//  Exec(ExeFile,GetRunParameters);

  UserScreen^.SwitchBack;

  InitVideo;
  InitMouse;
  ReDraw;
  UpdateScreen(true);
end;

end.
{
  $Log$
  Revision 1.2  1999-01-12 14:29:38  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.1  1998/12/28 15:47:52  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
