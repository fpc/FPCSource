{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Tools menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.DoAsciiTable;
var
  P: PAsciiChart;
begin
  P := New(PAsciiChart, Init);
  P^.HelpCtx := hcAsciiTable;
  InsertWindow(P);
end;


procedure TIDEApp.Calculator;
begin
  with CalcWindow^ do
  begin
    if GetState(sfVisible)=false then Show;
    MakeFirst;
  end;
end;

procedure TIDEApp.ExecuteTool(Idx: integer);
var Title,ProgramPath,Params: string;
    W: word;
    Err: integer;
begin
  if (Idx<1) or (Idx>GetToolCount) then Exit;
  GetToolParams(Idx-1,Title,ProgramPath,Params,W);
  Err:=ParseToolParams(Params,false);
  if Err=-1 then Exit;
  if Err<>0 then
    begin ErrorBox(^C'Error parsing tool params.',nil); Exit; end;
  DoExecute(ProgramPath,Params,false);
  if (DosError<>0) then
    ErrorBox('Error executing tool '+KillTilde(GetToolName(Idx-1)),nil) else
  if DosExitCode<>0 then
    ErrorBox('Execution successful. Exit code '+IntToStr(DosExitCode),nil);
end;

procedure TIDEApp.Grep;

  var PGrepDialog : PCenterDialog;
      R,R2 : TRect;
      IL1,IL2 : PInputLine;
      p,lineNb : longint;
      error : word;
      searchword,
      GrepExe,GrepArgs,Line,ModuleName : String;
      GrepOut : text;
      Params : Array[0..4] of longint;

  Const GrepOutName = 'grep$$.out';
{$ifdef linux}
   GrepExeName = 'grep';
{$else linux}
   GrepExeName = 'grep.exe';
{$endif linux}
begin
{ Find grep.exe }
  GrepExe:=GrepExeName;
  If not LocateExeFile(GrepExe) then
    Begin
      ErrorBox('Grep program not found',nil);
      Exit;
    End;
{ Try to load the word from the editor }
  If not(DeskTop^.First=nil) and
     (DeskTop^.First^.HelpCtx=hcSourceWindow) then
    Searchword:=PSourceWindow(DeskTop^.First)^.Editor^.GetCurrentWord
  else
    Searchword:='';
  GrepArgs:='-n -i $TEXT *.pas *.pp *.inc';
{ Dialog }
  R.Assign(0,0,45,8);
  new(PGrepDialog,Init(R,'Grep arguments'));
  with PGrepDialog^ do
    begin
      R2.A.Y:=R.A.Y+3;
      R2.B.Y:=R2.A.Y+1;
      R2.A.X:=R.A.X+3;
      R2.B.X:=R.B.X-3;
      New(IL1, Init(R2, 128));
      IL1^.Data^:=SearchWord;
      Insert(IL1);
      R2.Move(0,-1);
      Insert(New(PLabel, Init(R2, '~T~ext to find', IL1)));

      R2.Move(0,4);
      New(IL2, Init(R2, 128));
      IL2^.Data^:=GrepArgs;
      Insert(IL2);
      R2.Move(0,-1);
      Insert(New(PLabel, Init(R2, '~G~rep arguments', IL2)));
    end;

    InsertButtons(PGrepDialog);
    IL1^.Select;

    if Desktop^.ExecView(PGrepDialog)=cmOK then
    begin
      SearchWord:=IL1^.Data^;
      if SearchWord<>'' then
       begin
         GrepArgs:=IL2^.Data^;
         ReplaceStr(GrepArgs,'$TEXT',SearchWord);
         { Linux ? }
         if not ExecuteRedir(GrepExe,GrepArgs,GrepOutName,'grep$$.err') then
           Begin
              { 2 as exit code just means that
                some file vwere not found ! }
              if (IOStatus<>0) or (ExecuteResult<>2) then
                begin
                  Params[0]:=IOStatus;
                  Params[1]:=ExecuteResult;
                  WarningBox(#3'Error running Grep'#13#3'DosError = %d'#13#3'Exit code = %d',@Params);
                end;
           End;
         {$I-}
         Assign(GrepOut,GrepOutName);
         Reset(GrepOut);
         While not eof(GrepOut) do
           begin
             readln(GrepOut,Line);
             p:=pos(':',line);
             if p>0 then
               begin
                 ModuleName:=copy(Line,1,p-1);
                 Line:=Copy(Line,p+1,255);
                 p:=pos(':',Line);
                 val(copy(Line,1,p-1),lineNb,error);
                 if error=0 then
                   ProgramInfoWindow^.AddMessage(V_Normal,Copy(Line,p+1,255),
                     ModuleName,LineNb);
               end;
             ProgramInfoWindow^.Show;
             ProgramInfoWindow^.MakeFirst;
           end;
         Close(GrepOut);
         Erase(GrepOut);
         {$I+}
         EatIO;
       end;
    end;
  Dispose(PGrepDialog, Done);
end;
{
  $Log$
  Revision 1.8  1999-02-22 02:15:17  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.7  1999/02/20 15:18:31  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.6  1999/02/05 13:51:42  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.5  1999/02/05 12:11:59  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.4  1999/02/04 15:59:08  pierre
   * grep$$$.out was not closed

  Revision 1.3  1999/02/04 13:32:09  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.2  1999/01/21 11:54:21  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/22 10:39:49  peter
    + options are now written/read
    + find and replace routines

}
