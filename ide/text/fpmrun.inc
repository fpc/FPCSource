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
begin
  if (not ExistsFile(ExeFile)) or (CompilationPhase<>cpDone) then
   DoCompile(cRun);
  if CompilationPhase<>cpDone then
     Exit;
  if (EXEFile='') then
   begin
     ErrorBox('Oooops, nothing to run.',nil);
     Exit;
   end;
  if not ExistsFile(ExeFile) then
    begin
      MsgParms[1].Ptr:=@EXEFile;
      ErrorBox('Invalid filename %s',@MsgParms);
      Exit;
    end;

  if UserScreen=nil then
   begin
     ErrorBox('Sorry, user screen not available.',nil);
     Exit;
   end;
  DoneMouse;
  DoneVideo;

  if Assigned(UserScreen) then
    UserScreen^.SwitchTo;

  Exec(ExeFile,GetRunParameters);
  LastExitCode:=DosExitCode;

  if Assigned(UserScreen) then
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
  Revision 1.4  1999-01-14 21:42:22  peter
    * source tracking from Gabor

  Revision 1.3  1999/01/12 14:29:36  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.2  1999/01/04 11:49:48  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:50  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
