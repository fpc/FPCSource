{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Linux specific part of TProcess.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses
{$ifdef ver1_0}
  Linux
{$else}
  Unix
{$endif}
  ;

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    FRunning:=GetExitStatus=-1;
  Result:=FRunning;
end;

Procedure TProcess.Execute;

begin
  FreeStreams;
  CreatePipeStreams (FChildInputSTream,FParentOutPutStream);
  CreatePipeStreams (FParentInputStream,FChildOutPutStream);
  If poUsePipes in FCreateOptions then
    begin
    if poStdErrToOutPut in FCreateOptions then
      CreatePipeStreams (FParentErrorStream,FChildErrorStream)
    else
      begin
      FChildErrorStream:=FChildOutPutStream;
      FParentErrorStream:=FParentInputStream;
      end;
    end
  else
    CreatePipeStreams (FParentErrorStream,FChildErrorStream);
  If FCurrentDirectory<>'' then
    Chdir(FCurrentDirectory);
  FHandle:=fork();
  if FHandle=0 then
   begin
   // Child
   fdClose(0);
   fdClose(1);
   fdclose(2);
   dup2(FChildInputStream.Handle,0);
   dup2(FCHildOutputStream.Handle,1);
   dup2(FChildErrorStream.Handle,2);
   execl(FCommandline);
   halt(127);
   end
  else
    begin
    // Parent
    FPID:=FHandle;
    FThreadHandle:=FHandle;
    fdclose(FChildOutputStream.Handle);
    fdclose(FChildInputStream.Handle);
    fdclose(FChildErrorStream.Handle);
    FRunning:=True;
    if (poWaitOnExit in FCreateOptions) and
        not (poRunSuspended in FCreateOptions) then
    WaitOnExit;
    end;
end;

Function TProcess.WaitOnExit : Dword;

begin
  waitpid(FPID, nil, 0);
{
  Result:=WaitForSingleObject (FprocessInformation.hProcess,Infinite);
  If Result<>Wait_Failed then
    GetExitStatus;
}  FRunning:=False;
  Result := 0;
end;

Function TProcess.Suspend : Longint;

begin
  Result:=Kill(Handle,SIGSTOP);
end;

Function TProcess.Resume : LongInt;

begin
  Result:=Kill(FHandle,SIGCONT);
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  If ExitStatus=-1 then
    Result:=Kill(FHandle,SIGTERM)=0;
end;

{
  $Log$
  Revision 1.1  2002-07-30 16:03:29  marco
   * Added for OpenBSD. Plain copy of NetBSD

  Revision 1.1  2001/09/05 14:30:04  marco
   * NetBSD fcl makefile fixes. Plain copy from FreeBSD

  Revision 1.4  2001/04/08 11:26:03  peter
    * update so it can be compiled by both 1.0.x and 1.1

  Revision 1.3  2001/01/21 20:45:09  marco
   * Rename fest II  FCL version.

  Revision 1.2  2000/10/26 22:30:21  peter
    * freebsd update
    * classes.rst

  Revision 1.1.2.1  2000/10/17 13:47:43  marco
   * Copy of fcl/linux dir with adapted makefiles to ease FreeBSD 1.0.2
  packaging

  Revision 1.1  2000/07/13 06:33:44  michael
  + Initial import

  Revision 1.5  2000/02/15 22:03:38  sg
  * Inserted wrong copyright notice ;)  Fixed.

  Revision 1.4  2000/02/15 21:57:51  sg
  * Added copyright notice and CVS log tags where necessary

}
