{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2008 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Const
  PriorityConstants : Array [TProcessPriority] of Integer =
                      (20,20,0,-20);

Const
  GeometryOption : String = '-geometry';
  TitleOption : String ='-title';

procedure TProcess.CloseProcessHandles;

begin
 // Do nothing. Win32 call.
end;

Function TProcess.PeekExitStatus : Boolean;
var
  res: cint;
begin
  repeat
    res:=fpWaitPid(Handle,pcint(@FExitCode),WNOHANG);
  until (res<>-1) or (fpgeterrno<>ESysEINTR);
  result:=res=Handle;
  If Result then
    FExitCode:=wexitstatus(FExitCode)
  else
    FexitCode:=0;
end;

Type
  TPCharArray = Array[Word] of pchar;
  PPCharArray = ^TPcharArray;

Function StringsToPCharList(List : TStrings) : PPChar;

Var
  I : Integer;
  S : String;

begin
  I:=(List.Count)+1;
  GetMem(Result,I*sizeOf(PChar));
  PPCharArray(Result)^[List.Count]:=Nil;
  For I:=0 to List.Count-1 do
    begin
    S:=List[i];
    Result[i]:=StrNew(PChar(S));
    end;
end;

Procedure FreePCharList(List : PPChar);

Var
  I : integer;

begin
  I:=0;
  While List[i]<>Nil do
    begin
    StrDispose(List[i]);
    Inc(I);
    end;
  FreeMem(List);
end;


Procedure CommandToList(S : String; List : TStrings);

  Function GetNextWord : String;

  Const
    WhiteSpace = [' ',#8,#10];
    Literals = ['"',''''];

  Var
    Wstart,wend : Integer;
    InLiteral : Boolean;
    LastLiteral : char;

  begin
    WStart:=1;
    While (WStart<=Length(S)) and (S[WStart] in WhiteSpace) do
      Inc(WStart);
    WEnd:=WStart;
    InLiteral:=False;
    LastLiteral:=#0;
    While (Wend<=Length(S)) and (Not (S[Wend] in WhiteSpace) or InLiteral) do
      begin
      if S[Wend] in Literals then
        If InLiteral then
          InLiteral:=Not (S[Wend]=LastLiteral)
        else
          begin
          InLiteral:=True;
          LastLiteral:=S[Wend];
          end;
       inc(wend);
       end;

     Result:=Copy(S,WStart,WEnd-WStart);

     if  (Length(Result) > 0)
     and (Result[1] = Result[Length(Result)]) // if 1st char = last char and..
     and (Result[1] in Literals) then // it's one of the literals, then
       Result:=Copy(Result, 2, Length(Result) - 2); //delete the 2 (but not others in it)

     While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
       inc(Wend);
     Delete(S,1,WEnd-1);

  end;

Var
  W : String;

begin
  While Length(S)>0 do
    begin
    W:=GetNextWord;
    If (W<>'') then
      List.Add(W);
    end;
end;


Function MakeCommand(P : TProcess) : PPchar;

Var
  Cmd : String;
  S  : TStringList;
  G : String;
  
begin
  if (P.ApplicationName='') then
    begin
    If (P.CommandLine='') then
      Raise EProcess.Create(SNoCommandline);
    Cmd:=P.CommandLine;
    end
  else
    begin
    If (P.CommandLine='') then
      Cmd:=P.ApplicationName
    else
      Cmd:=P.CommandLine;
    end;
  S:=TStringList.Create;
  try
    CommandToList(Cmd,S);
    if poNewConsole in P.Options then
      begin
      {$ifdef darwin}
      S.Insert(0,'open');
      {$else}
      S.Insert(0,'-e');
      If (P.ApplicationName<>'') then
        begin
        S.Insert(0,P.ApplicationName);
        S.Insert(0,'-title');
        end;
      if suoUseCountChars in P.StartupOptions then
        begin
        S.Insert(0,Format('%dx%d',[P.dwXCountChars,P.dwYCountChars]));
        S.Insert(0,'-geometry');
        end;
      S.Insert(0,'xterm');
      {$endif}
      end;
    if (P.ApplicationName<>'') then
      begin
      S.Add(TitleOption);
      S.Add(P.ApplicationName);
      end;
    G:='';
    if (suoUseSize in P.StartupOptions) then
      g:=format('%dx%d',[P.dwXSize,P.dwYsize]);
    if (suoUsePosition in P.StartupOptions) then
      g:=g+Format('+%d+%d',[P.dwX,P.dwY]);
    if G<>'' then
      begin
      S.Add(GeometryOption);
      S.Add(g);
      end;
    Result:=StringsToPcharList(S);
  Finally
    S.free;
  end;
end;

Function GetLastError : Integer;

begin
  Result:=-1;
end;

Type
  TPipeEnd = (peRead,peWrite);
  TPipePair = Array[TPipeEnd] of cint;

Procedure CreatePipes(Var HI,HO,HE : TPipePair; CE : Boolean);

  Procedure CreatePair(Var P : TPipePair);

   begin
    If not CreatePipeHandles(P[peRead],P[peWrite]) then
      Raise EProcess.Create('Failed to create pipes');
   end;

  Procedure ClosePair(Var P : TPipePair);

  begin
    if (P[peRead]<>-1) then
      FileClose(P[peRead]);
    if (P[peWrite]<>-1) then
      FileClose(P[peWrite]);
  end;

begin
  HO[peRead]:=-1;HO[peWrite]:=-1;
  HI[peRead]:=-1;HI[peWrite]:=-1;
  HE[peRead]:=-1;HE[peWrite]:=-1;
  Try
    CreatePair(HO);
    CreatePair(HI);
    If CE then
      CreatePair(HE);
  except
    ClosePair(HO);
    ClosePair(HI);
    If CE then
      ClosePair(HE);
    Raise;
  end;
end;

Function safefpdup2(fildes, fildes2 : cInt): cInt;
begin
  repeat
    safefpdup2:=fpdup2(fildes,fildes2);
  until (safefpdup2<>-1) or (fpgeterrno<>ESysEINTR);
end;

Procedure TProcess.Execute;

Var
  HI,HO,HE : TPipePair;
  PID      : Longint;
  FEnv     : PPChar;
  Argv     : PPChar;
  fd       : Integer;
  res      : cint;
  FoundName,
  PName    : String;

begin
  If (poUsePipes in FProcessOptions) then
    CreatePipes(HI,HO,HE,Not (poStdErrToOutPut in FProcessOptions));
  Try
    if FEnvironment.Count<>0 then
      FEnv:=StringsToPcharList(FEnvironment)
    else
      FEnv:=Nil;
    Try
      Argv:=MakeCommand(Self);
      Try
        If (Argv<>Nil) and (ArgV[0]<>Nil) then
          PName:=StrPas(Argv[0])
        else
          begin
          // This should never happen, actually.
          PName:=ApplicationName;
          If (PName='') then
            PName:=CommandLine;
          end;

        if not FileExists(PName) then begin
          FoundName := ExeSearch(Pname,fpgetenv('PATH'));
          if FoundName<>'' then
            PName:=FoundName
          else
            raise EProcess.CreateFmt(SErrNoSuchProgram,[PName]);
        end;

{$if (defined(DARWIN) or defined(SUNOS))}
        Pid:=fpvfork;
{$else}
        Pid:=fpfork;
{$endif}
        if Pid<0 then
          Raise EProcess.Create('Failed to Fork process');
        if (PID>0) then
          begin
            // Parent process. Copy process information.
            FProcessHandle:=PID;
            FThreadHandle:=PID;
            FProcessId:=PID;
            //FThreadId:=PID;
          end
        else
          begin
            { We're in the child }
            if (FCurrentDirectory<>'') then
               ChDir(FCurrentDirectory);
            if PoUsePipes in Options then
              begin
                FileClose(HI[peWrite]);
                safefpdup2(HI[peRead],0);
                FileClose(HO[peRead]);
                safefpdup2(HO[peWrite],1);
                if (poStdErrToOutPut in Options) then
                  safefpdup2(HO[peWrite],2)
                else
                  begin
                    FileClose(HE[peRead]);
                    safefpdup2(HE[peWrite],2);
                  end
              end
            else if poNoConsole in Options then
              begin
                fd:=FileOpen('/dev/null',fmOpenReadWrite or fmShareDenyNone);
                safefpdup2(fd,0);
                safefpdup2(fd,1);
                safefpdup2(fd,2);
              end;
            if (poRunSuspended in Options) then
              sigraise(SIGSTOP);
            if FEnv<>Nil then
              fpexecve(PName,Argv,Fenv)
            else
              fpexecv(PName,argv);
            fpExit(127);
          end
      Finally
        FreePcharList(Argv);
      end;
    Finally
      If (FEnv<>Nil) then
        FreePCharList(FEnv);
    end;
  Finally
    if POUsePipes in FProcessOptions then
      begin
        FileClose(HO[peWrite]);
        FileClose(HI[peRead]);
        if Not (poStdErrToOutPut in FProcessOptions) then
          FileClose(HE[peWrite]);
        CreateStreams(HI[peWrite],HO[peRead],HE[peRead]);
      end;
  end;
  FRunning:=True;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
     (poWaitOnExit in FProcessOptions) and
      not (poRunSuspended in FProcessOptions) then
    WaitOnExit;
end;

Function TProcess.WaitOnExit : Boolean;

Var
  R : Dword;

begin
  fexitcode:=waitprocess(handle);
  Result:=(fexitcode>=0);
  FRunning:=False;
end;

Function TProcess.Suspend : Longint;

begin
  If fpkill(Handle,SIGSTOP)<>0 then
    Result:=-1
  else
    Result:=1;
end;

Function TProcess.Resume : LongInt;

begin
  If fpKill(Handle,SIGCONT)<>0 then
    Result:=-1
  else
    Result:=0;
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;

begin
  Result:=False;
  Result:=fpkill(Handle,SIGTERM)=0;
  If Result then
    begin
    If Running then
      Result:=fpkill(Handle,SIGKILL)=0;
    end;
  GetExitStatus;
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);

begin
  FShowWindow:=Value;
end;

