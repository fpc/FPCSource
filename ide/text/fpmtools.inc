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
      IL : PInputLine;
      p,lineNb : longint;
      error : word;
      GrepArgs,Line,ModuleName : String;
      GrepOut : text;
      Params : Array[0..4] of longint;
      
  Const GrepOutName = 'grep$$.out';
{$ifdef linux}
   GrepExeName = 'grep';
{$else linux}
   GrepExeName = 'c:\djgpp\bin\grep.exe';
{$endif linux}
begin
  R.Assign(0,0,45,6);
  new(PGrepDialog,Init(R,'Grep arguments'));
  with PGrepDialog^ do
    begin
      R2.A.Y:=R.A.Y+3;
      R2.B.Y:=R2.A.Y+1;
      R2.A.X:=R.A.X+3;
      R2.B.X:=R.B.X-3;
      New(IL, Init(R2, 128));
      If Assigned(DeskTop^.First) and
         (DeskTop^.First^.HelpCtx=hcSourceWindow) then
        GrepArgs:=PSourceWindow(DeskTop^.First)^.Editor^.GetCurrentWord
      else
        GrepArgs:='';
      GrepArgs:='-n -i '+GrepArgs+' *.pas *.pp *.inc';
      IL^.Data^:=GrepArgs;
      Insert(IL);
      R2.Move(0,-1);
      Insert(New(PLabel, Init(R2, '~G~rep arguments', IL)));
    end;

  InsertButtons(PGrepDialog);
    if Desktop^.ExecView(PGrepDialog)=cmOK then
    begin
      GrepArgs:=IL^.Data^;
      { Linux ? }
      if not ExecuteRedir(GrepExeName,GrepArgs,GrepOutName,'grep$$.err') then
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
         end;
      Close(GrepOut);
  Dispose(PGrepDialog, Done);
end;
{
  $Log$
  Revision 1.4  1999-02-04 15:59:08  pierre
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
