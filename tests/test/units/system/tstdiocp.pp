{ %TARGET=win32,win64 }
{ Redirected standard text files use DefaultSystemCodePage instead of the console code page, issue #31746 }
program tstdiocp;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils;

const
  cText = UnicodeString('e'#$00E9'E'#$20AC'.');


// Halts with aCode when the code page of aFile is not DefaultSystemCodePage
procedure CheckCodePage(aCode: Integer; var aFile: Text);

begin
  if TextRec(aFile).CodePage<>DefaultSystemCodePage then
    halt(aCode);
end;


// Checks the code pages of the redirected standard files and writes cText to Output
procedure RunChild;

begin
  CheckCodePage(11,Input);
  CheckCodePage(12,Output);
  CheckCodePage(13,ErrOutput);
  CheckCodePage(14,StdOut);
  CheckCodePage(15,StdErr);
  writeln(cText);
end;


// Opens aName for writing with an inheritable handle
function CreateInheritable(const aName: UnicodeString; aAccess, aDisposition: DWord): THandle;

var
  lSA: TSecurityAttributes;

begin
  FillChar(lSA,SizeOf(lSA),0);
  lSA.nLength:=SizeOf(lSA);
  lSA.bInheritHandle:=True;
  Result:=CreateFileW(PWideChar(aName),aAccess,FILE_SHARE_READ or FILE_SHARE_WRITE,@lSA,aDisposition,FILE_ATTRIBUTE_NORMAL,0);
end;


// Returns the content of the file aName and deletes it
function TakeFile(const aName: UnicodeString): RawByteString;

var
  lHandle: THandle;
  lCount: DWord;

begin
  Result:='';
  lHandle:=CreateFileW(PWideChar(aName),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,0,0);
  SetLength(Result,GetFileSize(lHandle,nil));
  if Length(Result)>0 then
    ReadFile(lHandle,Result[1],Length(Result),lCount,nil);
  CloseHandle(lHandle);
  DeleteFileW(PWideChar(aName));
end;


// Runs this program as child with stdout to a file or a pipe; returns the exit code and the output in aOutput
function RunRedirected(aPipe: Boolean; out aOutput: RawByteString): DWord;

var
  lSA: TSecurityAttributes;
  lSI: TStartupInfoW;
  lPI: TProcessInformation;
  lBase, lCmd: UnicodeString;
  lIn, lOut, lErr, lRead, lWrite: THandle;
  lBuf: array[0..1023] of AnsiChar;
  lCount: DWord;
  lSize: Integer;

begin
  aOutput:='';
  lBase:=UnicodeString(ChangeFileExt(ParamStr(0),''));
  lIn:=CreateInheritable(lBase+'.in',GENERIC_READ or GENERIC_WRITE,CREATE_ALWAYS);
  lErr:=CreateInheritable(lBase+'.err',GENERIC_WRITE,CREATE_ALWAYS);
  lOut:=INVALID_HANDLE_VALUE;
  lRead:=0;
  lWrite:=0;
  if aPipe then
    begin
    FillChar(lSA,SizeOf(lSA),0);
    lSA.nLength:=SizeOf(lSA);
    lSA.bInheritHandle:=True;
    CreatePipe(lRead,lWrite,@lSA,0);
    SetHandleInformation(lRead,HANDLE_FLAG_INHERIT,0);
    end
  else
    lOut:=CreateInheritable(lBase+'.out',GENERIC_WRITE,CREATE_ALWAYS);
  FillChar(lSI,SizeOf(lSI),0);
  lSI.cb:=SizeOf(lSI);
  lSI.dwFlags:=STARTF_USESTDHANDLES;
  lSI.hStdInput:=lIn;
  lSI.hStdError:=lErr;
  if aPipe then
    lSI.hStdOutput:=lWrite
  else
    lSI.hStdOutput:=lOut;
  lCmd:='"'+UnicodeString(ParamStr(0))+'" child';
  UniqueString(lCmd);
  if not CreateProcessW(nil,PWideChar(lCmd),nil,nil,True,0,nil,nil,lSI,lPI) then
    begin
    writeln('CreateProcess failed: ',GetLastError);
    halt(1);
    end;
  if aPipe then
    begin
    CloseHandle(lWrite);
    while ReadFile(lRead,lBuf,SizeOf(lBuf),lCount,nil) and (lCount>0) do
      begin
      lSize:=Length(aOutput);
      SetLength(aOutput,lSize+Integer(lCount));
      Move(lBuf,aOutput[lSize+1],lCount);
      end;
    CloseHandle(lRead);
    end;
  WaitForSingleObject(lPI.hProcess,INFINITE);
  GetExitCodeProcess(lPI.hProcess,Result);
  CloseHandle(lPI.hProcess);
  CloseHandle(lPI.hThread);
  CloseHandle(lIn);
  CloseHandle(lErr);
  DeleteFileW(PWideChar(lBase+'.in'));
  DeleteFileW(PWideChar(lBase+'.err'));
  if not aPipe then
    begin
    CloseHandle(lOut);
    aOutput:=TakeFile(lBase+'.out');
    end;
end;


// Returns aBytes as hexadecimal pairs
function ToHex(const aBytes: RawByteString): string;

var
  i: Integer;

begin
  Result:='';
  for i:=1 to Length(aBytes) do
    Result:=Result+IntToHex(Ord(aBytes[i]),2)+' ';
end;


// Halts with aCode when a redirected child fails or its output is not cText in DefaultSystemCodePage
procedure CheckRedirected(aCode: Integer; aPipe: Boolean);

var
  lOutput: RawByteString;
  lText: UnicodeString;
  lExpected: AnsiString;
  lExit: DWord;

begin
  lExit:=RunRedirected(aPipe,lOutput);
  if lExit<>0 then
    begin
    writeln('child exit code ',lExit,' (pipe=',aPipe,')');
    halt(aCode);
    end;
  lText:=cText;
  lExpected:=lText;
  lExpected:=lExpected+#13#10;
  if (Length(lOutput)<>Length(lExpected)) or not CompareMem(@lOutput[1],@lExpected[1],Length(lExpected)) then
    begin
    writeln('pipe=',aPipe,': got ',ToHex(lOutput),'expected ',ToHex(lExpected));
    halt(aCode+1);
    end;
end;


begin
  if ParamStr(1)='child' then
    RunChild
  else
    begin
    CheckRedirected(2,False);
    CheckRedirected(4,True);
    writeln('ok');
    end;
end.
