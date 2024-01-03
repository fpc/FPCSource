{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Karoly Balogh and other members of the
    Free Pascal development team

    Amiga  Dummy process.inc - the simplest version based on SysUtils.ExecuteProcess

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$DEFINE OS_HASCREATEPIPE}

{$IFDEF FPC_DOTTEDUNITS}
uses
  Amiga.Core.Exec, Amiga.Core.Amigados, Amiga.Core.Utility;
{$ELSE FPC_DOTTEDUNITS}
uses
  Exec, AmigaDos, Utility;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  SNoCommandLine        = 'Cannot execute empty command-line';
  SErrCannotExecute     = 'Failed to execute %s : %d';
  SErrNoSuchProgram     = 'Executable not found: "%s"';


procedure TProcess.CloseProcessHandles;
begin
end;

Function TProcess.PeekExitStatus : Boolean;
begin
  Result := true; (* Dummy version assumes always synchronous execution *)
end;

function GetNextWordPos (const S: ansistring): integer;
const
  WhiteSpace = [' ', #9, #10, #13];
  Literals = ['"', ''''];
var
  WStart: integer;
  InLiteral: boolean;
  LastLiteral: ansiChar;
begin
  WStart := 1;
(* Skip whitespaces at the beginning *)
  while (WStart <= Length (S)) and (S [WStart] in WhiteSpace) do
   Inc (WStart);
  InLiteral := false;
  LastLiteral := #0;
  while (WStart <= Length (S)) and
                               (not (S [WStart] in WhiteSpace) or InLiteral) do
   begin
    if S [WStart] in Literals then
     if InLiteral then
      InLiteral := not (S [WStart] = LastLiteral)
     else
      begin
       InLiteral := true;
       LastLiteral := S [WStart];
      end;
     Inc (WStart);
    end;
(* Skip whitespaces at the end *)
  while (WStart <= Length (S)) and (S [WStart] in WhiteSpace) do
   Inc (WStart);
  Result := WStart;
end;


function MaybeQuote (const S: ansistring): ansistring;
begin
  if (Pos (' ', S) <> 0) then
   Result := '"' + S + '"'
  else
   Result := S;
end;

var
  UID: Integer = 0;

{$ifdef MorphOS}
const
  BUF_LINE = 0; // flush on \n, etc
  BUF_FULL = 1; // never flush except when needed
  BUF_NONE = 2; // no buffering
{$endif}

Procedure TProcess.SysExecute;
var
  I: integer;
  ExecName, FoundName: ansistring;
  E2: EProcess;
  OrigDir: ansistring;
  Cmd,Params: ansistring;
  TempName,OTempName: ansistring;
  cos: BPTR;
  {$ifdef MorphOS}
  inA, inB, OutA, OutB: BPTR;
  Res: Integer;
  {$endif}
begin
  if (ApplicationName = '') and (CommandLine = '') and (Executable = '') then
   raise EProcess.Create (SNoCommandline);
  if (FApplicationName <> '') then
   ExecName := FApplicationName;
  if (FCommandLine <> '') then
   begin
    Cmd:=FCommandLine;
    Params := Cmd;
    if ExecName = '' then
     begin
      I := GetNextWordPos (Params);
      ExecName := Copy (Params, 1, Pred (I));
      ExecName := Trim (ExecName);
      Delete (Params, 1, Pred (I));
     end
    else if Copy (Cmd, 1, Length (ExecName)) = ExecName then
     Delete (Params, 1, Succ (Length (ExecName)))
    else
     Delete (Params, 1, Pred (GetNextWordPos (Params)));
    Params := Trim (Params);
   end
  else
   for I := 0 to Pred (Parameters.Count) do
    Params := Params + ' ' + MaybeQuote (Parameters [I]);
  if (FExecutable <> '') and (ExecName = '') then
   ExecName := Executable;
  if not FileExists (ExecName) then
   begin
    FoundName := ExeSearch (ExecName, '');
    if FoundName <> '' then
     ExecName := FoundName
    else
     raise EProcess.CreateFmt (SErrNoSuchProgram, [ExecName]);
   end;
  if (FCurrentDirectory <> '') then
   begin
    GetDir (0, OrigDir);
    ChDir (FCurrentDirectory);
   end;
  try
    {$ifdef MorphOS}
    if poUsePipes in Options then
    begin
      FDescriptors[phtInput].PrepareHandles;
      FDescriptors[phtOutput].PrepareHandles;
      //FDescriptors[phtError].PrepareHandles;
    end;
    if (not (poWaitOnExit in Options)) then
    begin
      FProcessID := 0;
        // MVC : IMO this should go to CreatePipes.
     if (FDescriptors[phtInput].IOType=iotPipe) then
       begin
       // Read handle
       InB:=FDescriptors[phtInput].FTheirHandle;
       // Write handle
       OutA:=FDescriptors[phtInput].FOurHandle;
       end;
     if (FDescriptors[phtOutput].IOType=iotPipe) then
       begin
       // Write handle
       OutB:=FDescriptors[phtOutput].FTheirHandle;
       // Read handle
       InA:=FDescriptors[phtOutput].FOurHandle;
       end;
      // set buffer for all pipes
      SetVBuf(inA, nil, BUF_NONE, -1);
      SetVBuf(inB, nil, BUF_LINE, -1);
      SetVBuf(outA, nil, BUF_NONE, -1);
      SetVBuf(outB, nil, BUF_LINE, -1);
      // the actual Start of the command with given parameter and streams
      Res := SystemTags(PAnsiChar(ExecName + ' ' + Params),
                        [SYS_Input, AsTag(inB),
                         SYS_Output, AsTag(outB),
                         SYS_Asynch, AsTag(True),
                         TAG_END]);
      // the two streams will be destroyed by system, we do not need to care about
      // the other two we will destroy when the PipeStreams they are attached to are destroyed
      if Res <> -1 then
      begin
        FProcessID := 1;
        // Remove the Used Pipe end because they will be freed by the system
        FDescriptors[phtInput].FTheirHandle := INVALID_HANDLE_VALUE;
        FDescriptors[phtOutput].FTheirHandle := INVALID_HANDLE_VALUE;
        // No longer needed, done in TIOFileDescriptor
        // CreateStreams(THandle(outB), THandle(inA),0);
      end
      else
      begin
        // if the command did not start, we need to delete all Streams
        // not needed to remove the pipes anymore the descriptors will care about that
      end;
    end
    else
    {$endif}
    begin
      // if no streams needed we still use the old sychronous way
      FProcessID := 0;
      cos := BPTR(0);
      repeat
        Inc(UID);
        TempName := 'T:PrO_'+ HexStr(FindTask(nil)) + '_' + IntToHex(UID,8);
      until not FileExists(TempName);
      //sysdebugln('TProcess start: "' + ExecName + ' ' + Params+'"  >' + TempName);
      cos := AmigaDos.DosOpen(PAnsiChar(TempName), MODE_READWRITE);
      FExitCode := LongInt(amigados.Execute(PAnsiChar(ExecName + ' ' + Params), BPTR(0), cos));
      DosSeek(cos, 0, OFFSET_BEGINNING);
    end;
    //FExitCode := ExecuteProcess (ExecName, Params);
  except
(* Normalize the raised exception so that it is aligned to other platforms. *)
    On E: EOSError do
     begin
      raise EProcess.CreateFmt (SErrCannotExecute, [FCommandLine, E.ErrorCode]);
      if (FCurrentDirectory <> '') then
       ChDir (OrigDir);
      end;
  end;
  if (FCurrentDirectory <> '') then
   ChDir (OrigDir);
end;

Function TProcess.WaitOnExit : Boolean;
begin
  Result:=True;
end;

Function TProcess.WaitOnExit(Timeout : DWord) : Boolean;
begin
  Result:=True;
end;

Function TProcess.Suspend : Longint;
begin
  Result:=0;
end;

Function TProcess.Resume : LongInt;
begin
  Result:=0;
end;

Function TProcess.Terminate(AExitCode : Integer) : Boolean;
begin
  Result:=False;
end;

Procedure TProcess.SetShowWindow (Value : TShowWindowOptions);
begin
end;

function TIODescriptor.SysNullFileName: string;
begin
//  result:='/dev/null';
end;

function TIODescriptor.SysIsTypeSupported(AValue: TIOType): Boolean;
begin
  Result:=True;
end; 


function TIODescriptor.SysCreateFileNameHandle(const aFileName: string): THandle;

const
  DefaultRights = 438; // 438 = 666 octal which is rw rw rw
  ModeNames : Array[Boolean] of String = ('Reading','Writing');

begin
  if (aFileName='') then
    Raise EProcess.Create('No filename provided');
  case ProcessHandleType of
    phtInput:  Result:=FileOpen(aFileName,fmOpenRead);
    phtOutput,
    phtError: if FileExists(aFileName) then
                Result:=FileOpen(aFileName,fmOpenWrite or fmShareDenyNone)
              else
                Result:=FileCreate(aFileName,fmShareDenyNone,DefaultRights)
  end;
  if (Result=-1) then
    Raise EProcess.CreateFmt('Could not open file "%s" for %s',[aFileName,ModeNames[ProcessHandleType<>phtInput]]);
end;

function TIODescriptor.SysCreatePipeHandle: THandle;

var
  TempName : AnsiString;
  HIn,HOut : BPTR;
  
begin
  // MVC : This should be moved to CreatePipeHandles.
  TempName := 'PIPE:PrO_' + HexStr(Self) + HexStr(GetTickCount, 8);
  if ProcessHandleType=phtInput then
    TempName:=TempName+'i'
  else  
    TempName:=TempName+'o';
  // Read handle
  HIn := DOSOpen(PAnsiChar(TempName), MODE_OLDFILE);
  // Write handle
  HOut := DOSOpen(PAnsiChar(TempName), MODE_NEWFILE);
  // Check correctness ?
  //
  // Set needed handkers
  case ProcessHandleType of
    phtInput:
      begin
      Result:=THandle(HIn);
      FOurHandle:=THandle(hOut);
      end;
    phtOutput,
    phtError:
      begin
      Result:=THandle(hOut);
      FOurHandle:=THandle(HIn);
      end;
  end;
  
end;

function TIODescriptor.SysPrepareCreatedHandleForProcess(aHandle: THandle): THandle;
begin
  Result:=aHandle;
end;   