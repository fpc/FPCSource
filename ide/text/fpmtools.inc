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

{
  $Log$
  Revision 1.2  1999-01-21 11:54:21  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/22 10:39:49  peter
    + options are now written/read
    + find and replace routines

}
