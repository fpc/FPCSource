{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Run menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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

  Exec(ExeFile,GetRunParameters);

  UserScreen^.SwitchBack;

  InitVideo;
  InitMouse;
  ReDraw;
  UpdateScreen(true);
end;

procedure TIDEApp.Parameters;
var R,R2: TRect;
    D: PCenterDialog;
    IL: PInputLine;
begin
  R.Assign(0,0,54,4);
  New(D, Init(R, 'Program parameters'));
  with D^ do
  begin
    GetExtent(R); R.Grow(-2,-1); Inc(R.A.Y); R.B.Y:=R.A.Y+1;
    R2.Copy(R); R2.A.X:=14;
    New(IL, Init(R2, 255));
    IL^.Data^:=GetRunParameters;
    Insert(IL);
    R2.Copy(R); R2.B.X:=14;
    Insert(New(PLabel, Init(R2, '~P~arameter', IL)));
  end;
  InsertButtons(D);
  IL^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
    SetRunParameters(IL^.Data^);
  end;
  Dispose(D, Done);
end;

{
  $Log$
  Revision 1.1  1998-12-28 15:47:50  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
