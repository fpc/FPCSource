uses linux;

Function TProcess.GetRunning : Boolean;

begin
  IF FRunning then
    FRunning:=GetExitStatus=-1;
  Result:=FRunning;
end;

Procedure TProcess.Execute;

begin
  If poUsePipes in FCreateOptions then
    begin
    FreeStreams;
    CreatePipeStreams (FChildInputSTream,FParentOutPutStream);
    CreatePipeStreams (FParentInputStream,FChildOutPutStream);
    if poStdErrToOutPut in FCreateOptions then
      CreatePipeStreams (FParentErrorStream,FChildErrorStream)
    else
      begin
      FChildErrorStream:=FChildOutPutStream;
      FParentErrorStream:=FParentInputStream;
      end;
    end;
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
{
  Result:=WaitForSingleObject (FprocessInformation.hProcess,Infinite);
  If Result<>Wait_Failed then
    GetExitStatus;
}  FRunning:=False;
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
