{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2008 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit part shared by win32/win64.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Error code definitions for the Win32 API functions


  Values are 32 bit values layed out as follows:
   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  +---+-+-+-----------------------+-------------------------------+
  |Sev|C|R|     Facility          |               Code            |
  +---+-+-+-----------------------+-------------------------------+

  where
      Sev - is the severity code
          00 - Success
          01 - Informational
          10 - Warning
          11 - Error

      C - is the Customer code flag
      R - is a reserved bit
      Facility - is the facility code
      Code - is the facility's status code
}

const
  SEVERITY_SUCCESS                        = $00000000;
  SEVERITY_INFORMATIONAL                  = $40000000;
  SEVERITY_WARNING                        = $80000000;
  SEVERITY_ERROR                          = $C0000000;

const
  STATUS_SEGMENT_NOTIFICATION             = $40000005;
  DBG_TERMINATE_THREAD                    = $40010003;
  DBG_TERMINATE_PROCESS                   = $40010004;
  DBG_CONTROL_C                           = $40010005;
  DBG_CONTROL_BREAK                       = $40010008;

  STATUS_GUARD_PAGE_VIOLATION             = $80000001;
  STATUS_DATATYPE_MISALIGNMENT            = $80000002;
  STATUS_BREAKPOINT                       = $80000003;
  STATUS_SINGLE_STEP                      = $80000004;
  DBG_EXCEPTION_NOT_HANDLED               = $80010001;

  STATUS_ACCESS_VIOLATION                 = $C0000005;
  STATUS_IN_PAGE_ERROR                    = $C0000006;
  STATUS_INVALID_HANDLE                   = $C0000008;
  STATUS_NO_MEMORY                        = $C0000017;
  STATUS_ILLEGAL_INSTRUCTION              = $C000001D;
  STATUS_NONCONTINUABLE_EXCEPTION         = $C0000025;
  STATUS_INVALID_DISPOSITION              = $C0000026;
  STATUS_ARRAY_BOUNDS_EXCEEDED            = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND           = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO             = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT             = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION          = $C0000090;
  STATUS_FLOAT_OVERFLOW                   = $C0000091;
  STATUS_FLOAT_STACK_CHECK                = $C0000092;
  STATUS_FLOAT_UNDERFLOW                  = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO           = $C0000094;
  STATUS_INTEGER_OVERFLOW                 = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION           = $C0000096;
  STATUS_STACK_OVERFLOW                   = $C00000FD;
  STATUS_CONTROL_C_EXIT                   = $C000013A;
  STATUS_FLOAT_MULTIPLE_FAULTS            = $C00002B4;
  STATUS_FLOAT_MULTIPLE_TRAPS             = $C00002B5;
  STATUS_REG_NAT_CONSUMPTION              = $C00002C9;

  { Exceptions raised by RTL use this code }
  FPC_EXCEPTION_CODE                      = $E0465043;

  EXCEPTION_EXECUTE_HANDLER               = 1;
  EXCEPTION_CONTINUE_EXECUTION            = -1;
  EXCEPTION_CONTINUE_SEARCH               = 0;

  { exception flags (not everything applies to Win32!) }
  EXCEPTION_NONCONTINUABLE  = $01;
  EXCEPTION_UNWINDING       = $02;
  EXCEPTION_EXIT_UNWIND     = $04;
  EXCEPTION_STACK_INVALID   = $08;
  EXCEPTION_NESTED_CALL     = $10;
  EXCEPTION_TARGET_UNWIND   = $20;
  EXCEPTION_COLLIDED_UNWIND = $40;


  CONTEXT_X86                             = $00010000;
  CONTEXT_CONTROL                         = CONTEXT_X86 or $00000001;
  CONTEXT_INTEGER                         = CONTEXT_X86 or $00000002;
  CONTEXT_SEGMENTS                        = CONTEXT_X86 or $00000004;
  CONTEXT_FLOATING_POINT                  = CONTEXT_X86 or $00000008;
  CONTEXT_DEBUG_REGISTERS                 = CONTEXT_X86 or $00000010;
  CONTEXT_EXTENDED_REGISTERS              = CONTEXT_X86 or $00000020;

  CONTEXT_FULL                            = CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS;

  MAXIMUM_SUPPORTED_EXTENSION             = 512;

type
  EXCEPTION_DISPOSITION=(
    ExceptionContinueExecution,
    ExceptionContinueSearch,
    ExceptionNestedException,
    ExceptionCollidedUnwind
  );

  TUnwindProc=procedure(frame: PtrUInt);

  PFilterRec=^TFilterRec;
  TFilterRec=record
    RvaClass: DWord;
    RvaHandler: DWord;
  end;

  TExceptObjProc=function(code: Longint; const rec: TExceptionRecord): Pointer; { Exception }
  TExceptClsProc=function(code: Longint): Pointer; { ExceptClass }


function RunErrorCode(const rec: TExceptionRecord): longint;
begin
  { negative result means 'FPU reset required' }
  case rec.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:      result := 200;    { reDivByZero }
    STATUS_FLOAT_DIVIDE_BY_ZERO:        result := -208;   { !!reZeroDivide }
    STATUS_ARRAY_BOUNDS_EXCEEDED:       result := 201;    { reRangeError }
    STATUS_STACK_OVERFLOW:              result := 202;    { reStackOverflow }
    STATUS_FLOAT_OVERFLOW:              result := -205;   { reOverflow }
    STATUS_FLOAT_DENORMAL_OPERAND,
    STATUS_FLOAT_UNDERFLOW:             result := -206;   { reUnderflow }
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:           result := -207;   { reInvalidOp }
    STATUS_INTEGER_OVERFLOW:            result := 215;    { reIntOverflow }
    STATUS_ILLEGAL_INSTRUCTION:         result := -216;
    STATUS_ACCESS_VIOLATION:            result := 216;    { reAccessViolation }
    STATUS_CONTROL_C_EXIT:              result := 217;    { reControlBreak }
    STATUS_PRIVILEGED_INSTRUCTION:      result := 218;    { rePrivilegedInstruction }
    STATUS_FLOAT_MULTIPLE_TRAPS,
    STATUS_FLOAT_MULTIPLE_FAULTS:       result := -255;   { indicate FPU reset }
  else
    result := 255;                                        { reExternalException }
  end;
end;

procedure TranslateMxcsr(mxcsr: longword; var code: longint);
begin
  { we can return only one value, further one's are lost }
  { InvalidOp }
  if (mxcsr and 1)<>0 then
    code:=-207
  { Denormal }
  else if (mxcsr and 2)<>0 then
    code:=-206
  { !!reZeroDivide }
  else if (mxcsr and 4)<>0 then
    code:=-208
  { reOverflow }
  else if (mxcsr and 8)<>0 then
    code:=-205
  { Underflow }
  else if (mxcsr and 16)<>0 then
    code:=-206
  { Precision }
  else if (mxcsr and 32)<>0 then
    code:=-207
  else { this should not happen }
    code:=-255
end;

function FilterException(var rec:TExceptionRecord; imagebase: PtrUInt; filterRva: DWord; errcode: Longint): Pointer;
var
  ExClass: TClass;
  i: Longint;
  Filter: Pointer;
  curFilt: PFilterRec;
begin
  result:=nil;
  if rec.ExceptionCode=FPC_EXCEPTION_CODE then
    ExClass:=TObject(rec.ExceptionInformation[1]).ClassType
  else if Assigned(ExceptClsProc) then
    ExClass:=TClass(TExceptClsProc(ExceptClsProc)(errcode))
  else
    Exit; { if we cannot determine type of exception, don't handle it }
  Filter:=Pointer(imagebase+filterRva);
  for i:=0 to PLongint(Filter)^-1 do
  begin
    CurFilt:=@PFilterRec(Filter+sizeof(Longint))[i];
    if (CurFilt^.RvaClass=$FFFFFFFF) or
      { TODO: exception might be coming from another module, need more advanced comparing }
      (ExClass.InheritsFrom({$if not defined(ver3_2)}PClass(imagebase+CurFilt^.RvaClass)^{$else}TClass(imagebase+CurFilt^.RvaClass){$endif})) then
    begin
      result:=Pointer(imagebase+CurFilt^.RvaHandler);
      exit;
    end;
  end;
end;

{*****************************************************************************
                              Parameter Handling
*****************************************************************************}

function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:PWideChar; cchWideChar:longint;
  lpMultiByteStr:LPSTR;cchMultiByte:longint; lpDefaultChar:PAnsiChar; lpUsedDefaultChar:PLongBool):longint; stdcall; external 'kernel32' name 'WideCharToMultiByte';
function GetCommandLineA : pansichar; stdcall;external KernelDLL name 'GetCommandLineA';

type
  { nargs — argument count (without first and without null terminator),
    nchars — total widechar count in arguments (with null terminators),
    nachars — total ansichar count in arguments (with null terminators), counted only if args = chars = nil. }
  ParseCommandLineResult = record
    nargs, nchars, nachars: SizeInt;
  end;

function ParseCommandLine(cmdLine: PWideChar; args: PPWideChar; chars: PWideChar): ParseCommandLineResult;
var
  argsStartInCmdLine: PWideChar;
  nCharsUpToPrevArg, nCharsPlusQuotes: SizeInt;
  c, quote: WideChar;
  skippingFirstArg: boolean;
begin
  argsStartInCmdLine:=cmdLine;
  nCharsUpToPrevArg:=0;
  FillChar(result,sizeof(result),0);
  skippingFirstArg:=true;
  quote:=' ';
  repeat
    c:=cmdLine^;
    inc(cmdLine);
    case c of
      #0..#32:
        if (quote=' ') or (c=#0) then
          begin
            if (result.nchars>nCharsUpToPrevArg) then
              begin
                // End of an argument found
                if Assigned(chars) then
                  chars[result.nchars]:=#0;
                inc(result.nchars); { Null terminator. }
                nCharsUpToPrevArg:=result.nchars;
              end;
            skippingFirstArg:=false;
            if c = #0 then
              break;
            continue; // Skip whitespace
          end;
      '"', '''':
        if (c='"') and (quote<>'''') or (c='''') and (quote<>'"') then
          if cmdLine^<>c then
            begin
              if quote=c then
                 quote:=' '
               else
                 quote:=c;
               continue;
            end
          else
            inc(cmdLine);
    end;
    if skippingFirstArg then
      continue;
    if result.nchars=nCharsUpToPrevArg then
      begin
        if Assigned(args) then
          args[result.nargs]:=chars+result.nchars;
        inc(result.nargs);
        if result.nchars=0 then
          argsStartInCmdLine:=cmdLine-1;
      end;
    if Assigned(chars) then
      chars[result.nchars]:=c;
    inc(result.nchars);
  until false;

  if Assigned(chars) then
    exit;
  { Number of widechars in command line starting from argsStartInCmdLine, including markdown: cmdLine - 1 - argsStartInCmdLine. Avoid implicit signed div. }
  nCharsPlusQuotes:=SizeUint(pointer(cmdLine-1)-pointer(argsStartInCmdLine)) div sizeof(widechar);
  result.nachars:=
    { Count of ANSI characters, including markdown. }
    WideCharToMultiByte(DefaultSystemCodePage, 0, argsStartInCmdLine, nCharsPlusQuotes, nil, 0, nil, nil)
    { Assume each markdown character (quote, space) is ANSI. Subtract markdown, add null terminators; result.nchars already includes null terminators. }
    -(nCharsPlusQuotes-result.nchars);
end;

var
  argvw: PPWideChar; { Start of the memory region. Should very preferably be private as argv can (and WILL, by LazUTF8) be changed from outside. }

procedure setup_arguments;
var
  CmdLineW, wchars: PWideChar;
  buf: array[0..MaxPathLen] of WideChar;
  iarg, nArg0W, nArg0A: SizeInt;
  pc: ParseCommandLineResult;
  achars, acharse: PAnsiChar;
begin
  CmdLine:=GetCommandLineA;
  CmdLineW:=GetCommandLineW;
  nArg0W:=GetModuleFileNameW(0, PWideChar(buf), Length(buf));
  nArg0A:=WideCharToMultiByte(DefaultSystemCodePage, 0, PWideChar(buf), nArg0W, nil, 0, nil, nil);
  pc:=ParseCommandLine(CmdLineW, nil, nil);
  argc:=pc.nargs+1;

  { Memory region layout:
    argc × PWideChar: argvw (internal, not terminated with nil).
    (argc + 1) × PAnsiChar: argv (terminated with nil).
    Nw × widechar: chars for argvw.
    Na × ansichar: chars for argv. }
  argvw:=nil;
  repeat { First iteration calculates region size (by adding to nil). Second iteration calculates pointers to region parts (by adding to region start). }
    argv:=PPAnsiChar(argvw+argc);
    wchars:=PWideChar(argv+argc+1);
    achars:=PAnsiChar(wchars+nArg0W+1+pc.nchars);
    acharse:=achars+nArg0A+1+pc.nachars;
    if Assigned(argvw) then
      break;
    argvw:=SysGetMem(PtrUint(acharse));
  until not Assigned(argvw); { If ReturnNilIfGrowHeapFails was customized to true, let it crash on allocation failure instead of looping endlessly. }

  Move(PWideChar(buf)^, wchars^, nArg0W*sizeof(widechar));
  wchars[nArg0W]:=#0;
  argvw[0]:=wchars;
  ParseCommandLine(CmdLineW, argvw+1, wchars+nArg0W+1);

  { Convert argvw to argv. }
  for iarg:=0 to pc.nargs do
    begin
      argv[iarg]:=achars;
      inc(achars, WideCharToMultiByte(DefaultSystemCodePage, 0, argvw[iarg], -1, achars, acharse-achars, nil, nil));
    end;
  argv[argc]:=nil;
end;

procedure finalize_arguments; inline;
begin
  SysFreeMem(argvw);
end;

function paramcount : longint;
begin
  paramcount := argc - 1;
end;

Function ParamStrU(l:Longint): UnicodeString; [public,alias:'_FPC_ParamStrU'];
begin
  if (l >= 0) and (l < argc) then
    Result:=argvw[l]
  else
    Result:='';
end;

Function ParamStrA(l:Longint): AnsiString; [public,alias:'_FPC_ParamStrA'];
begin
  Result:=AnsiString(ParamStrU(l));
end;

Function ParamStr(l:Longint): shortstring;
begin
  if (l >= 0) and (l < argc) then
    Result:=argv[l]
  else
    Result:='';
end;

{*****************************************************************************}

procedure randomize;
begin
  randseed:=GetTickCount;
end;

Var
  DLLInitState : Longint = -1;
  DLLBuf : Jmp_buf;

{$if defined(FPC_USE_WIN32_SEH) or defined(FPC_USE_WIN64_SEH)}
{$define FPC_USE_SEH}
{$endif}

function Dll_entry{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}(constref info : TEntryInformation){$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION} : longbool; [public,alias:'_FPC_DLL_Entry'];
  begin
{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
     SetupEntryInformation(info);
{$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION}
     IsLibrary:=true;
     DllInitState:=DLLreason;
     Dll_entry:=false;  { return value is ignored, except when DLLreason=DLL_PROCESS_ATTACH }
     case DLLreason of
       DLL_PROCESS_ATTACH :
         begin
           MainThreadIdWin32 := Win32GetCurrentThreadId;

           If SetJmp(DLLBuf) = 0 then
             begin
{$ifdef FPC_USE_SEH}
               try
{$endif}
{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
               EntryInformation.PascalMain();
{$else FPC_HAS_INDIRECT_ENTRY_INFORMATION}
               PascalMain;
{$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION}
               Dll_entry:=true;
{$ifdef FPC_USE_SEH}
               except
                 DoUnHandledException;
                 Dll_entry:=false;
               end;
{$endif}
             end
           else
             Dll_entry:=(ExitCode=0);
         end;
       DLL_THREAD_ATTACH :
         begin
           { SysInitMultithreading must not be called here,
             see comments in exec_tls_callback below }
           { Allocate Threadvars  }
           SysAllocateThreadVars;

           { NS : no idea what is correct to pass here - pass dummy value for now }
           { passing a dummy is ok, the correct value is read from the coff header of SysInstance (FK) }
           InitThread($1000000); { Assume everything is idempotent there, as the thread could have been created with BeginThread... }

           if assigned(Dll_Thread_Attach_Hook) then
             Dll_Thread_Attach_Hook(DllParam);
        end;
       DLL_THREAD_DETACH :
         begin
           if assigned(Dll_Thread_Detach_Hook) then
             Dll_Thread_Detach_Hook(DllParam);
           { Release Threadvars }
           if TlsGetValue(TLSKey^)<>nil then
             DoneThread; { Assume everything is idempotent there }
         end;
       DLL_PROCESS_DETACH :
         begin
           if MainThreadIDWin32=0 then // already been here.
             exit;
           If SetJmp(DLLBuf) = 0 then
             begin
               if assigned(Dll_Process_Detach_Hook) then
                 Dll_Process_Detach_Hook(DllParam);
               InternalExit;
             end;

           SysReleaseThreadVars;
           { Free TLS resources used by ThreadVars }
           SysFiniMultiThreading;
           MainThreadIDWin32:=0;
         end;
     end;
     DllInitState:=-1;
  end;


{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

function MessageBox(hWnd:THandle;lpText,lpCaption:PAnsiChar;uType:uint32):longint;
   stdcall;external 'user32' name 'MessageBoxA';

const
  ErrorBufferLength = 1024;
var
  ErrorBuf : array[0..ErrorBufferLength] of AnsiChar;
  ErrorLen : SizeInt;

procedure ShowError(final: boolean);
const
  IDCANCEL = 2;
var
  showStart, showEnd, tailStart, errLen: SizeInt;
begin
  errLen:=ErrorLen; { Local copy of ErrorLen, to soften (when multithreading) or avoid (with single thread) reenterancy issues. }
  { See e.g. comment in ErrorOpen about why not set ErrorLen := 0 there. }
  tailStart:=errLen;
  if tailStart=0 then
    exit;

  { Search for last line ending to show prettier message.
    line1 #13 #10 line2 #13 #10 line3
                        ^       ^
                   showEnd     tailStart
    #0 is then written at showEnd (possibly overwriting EOL character). In the worst case of race, there always will be #0 at ErrorBufferLength. }
  if not final then
    begin
      while (tailStart>ErrorBufferLength div 2) and not (ErrorBuf[tailStart-1] in [#13,#10]) do
        dec(tailStart);
      if tailStart=ErrorBufferLength div 2 then
        tailStart:=errLen;
    end;

  if not NoErrMsg then
    begin
      { Strip trailing EOLs even if final. Required when not final (to have a spare character for #0), but even if final, they aren’t pretty and don’t add to anything. }
      showEnd:=tailStart;
      while (showEnd>0) and (ErrorBuf[showEnd-1] in [#13,#10]) do
        dec(showEnd);

      { Also strip starting EOLs. }
      showStart:=0;
      while (showStart<showEnd) and (ErrorBuf[showStart] in [#13,#10]) do
        inc(showStart);

      ErrorBuf[showEnd]:=#0;
      NoErrMsg:=NoErrMsg or (MessageBox(0,@ErrorBuf[showStart],nil,ord(not final) {MB_OK is 0 and MB_OKCANCEL is 1})=IDCANCEL);
    end;

  dec(errLen,tailStart);
  Move(ErrorBuf[tailStart],ErrorBuf[0],errLen*sizeof(ErrorBuf[0]));
  ErrorLen:=errLen;
end;

procedure ErrorWrite(Var F: TextRec);
{
  An error message should always end with #13#10#13#10
}
var
  i,errLen : SizeInt;
Begin
  while F.BufPos>0 do
    begin
      errLen:=ErrorLen; { Not required for single thread unlike in ShowError, but still prevents crashes on races. }
      i:=ErrorBufferLength-errLen;
      if i>F.BufPos then
        i:=F.BufPos;
      Move(F.BufPtr^,ErrorBuf[errLen],i);
      inc(errLen,i);
      ErrorLen:=errLen;
      if errLen=ErrorBufferLength then
        ShowError(false);
      Dec(F.BufPos,i);
      Move(PAnsiChar(F.BufPtr^)[i],F.BufPtr^[0],F.BufPos);
    end;
End;


procedure ErrorClose(Var F: TextRec);
begin
  ShowError(true);
end;


procedure ErrorOpen(Var F: TextRec);
Begin
  TextRec(F).InOutFunc:=@ErrorWrite;
  TextRec(F).FlushFunc:=@ErrorWrite;
  TextRec(F).CloseFunc:=@ErrorClose;
  { Better not to set ErrorLen := 0 here: MessageBox performed by ShowError might/will lead to TLS callbacks that might/will open their own stderrs... }
End;


procedure AssignError(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@ErrorOpen;
  Rewrite(T);
end;


procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in a messagebox }
  { WARNING: this should be done only once at startup,
    not for DLL entry code, as the standard handles might
    have been redirected }
  if StdInputHandle=0 then
    StdInputHandle:=THandle(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  if StdOutputHandle=0 then
    StdOutputHandle:=THandle(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  if StdErrorHandle=0 then
    StdErrorHandle:=THandle(GetStdHandle(cardinal(STD_ERROR_HANDLE)));
  if not IsConsole then
   begin
     AssignError(stderr);
     AssignError(StdOut);
     Assign(Output,'');
     Assign(Input,'');
     Assign(ErrOutput,'');
   end
  else
   begin
     OpenStdIO(Input,fmInput,StdInputHandle);
     OpenStdIO(Output,fmOutput,StdOutputHandle);
     OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
     OpenStdIO(StdOut,fmOutput,StdOutputHandle);
     OpenStdIO(StdErr,fmOutput,StdErrorHandle);
   end;
end;

function GetProcessID: SizeUInt;
  begin
    GetProcessID := GetCurrentProcessID;
  end;


{******************************************************************************
                              Unicode
 ******************************************************************************}
const
  { MultiByteToWideChar  }
  MB_PRECOMPOSED = 1;
  WC_NO_BEST_FIT_CHARS = $400;

function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:PAnsiChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
    stdcall; external 'kernel32' name 'MultiByteToWideChar';
function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:PWideChar; cchWideChar:longint; lpMultiByteStr:PAnsiChar;cchMultiByte:longint; lpDefaultChar:PAnsiChar; lpUsedDefaultChar:pointer):longint;
    stdcall; external 'kernel32' name 'WideCharToMultiByte';
function CharUpperBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD;
    stdcall; external 'user32' name 'CharUpperBuffW';
function CharLowerBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD;
    stdcall; external 'user32' name 'CharLowerBuffW';

procedure Win32Unicode2AnsiMove(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
  var
    destlen: SizeInt;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    destlen:=WideCharToMultiByte(cp, 0, source, len, nil, 0, nil, nil);
    // this will null-terminate
    setlength(dest, destlen);
    if destlen>0 then
      begin
        WideCharToMultiByte(cp, 0, source, len, @dest[1], destlen, nil, nil);
        PAnsiRec(pointer(dest)-AnsiFirstOff)^.CodePage:=cp;
      end;
  end;

procedure Win32Ansi2UnicodeMove(source:PAnsiChar;cp : TSystemCodePage;var dest:UnicodeString;len:SizeInt);
  var
    destlen: SizeInt;
    dwflags: DWORD;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    case cp of
    // Under https://docs.microsoft.com/en-us/windows/desktop/api/stringapiset/nf-stringapiset-multibytetowidechar
      CP_UTF8, CP_UTF7, 50220, 50221, 50222, 50225, 50227, 50229, 57002..57011, 42:
                           dwFlags:=0
      else
        dwFlags:=MB_PRECOMPOSED;
      end;
    destlen:=MultiByteToWideChar(cp, dwFlags, source, len, nil, 0);
    { destlen=0 means that Windows cannot convert, so call the default
      handler. This is similiar to what unix does and is a good fallback
      if rawbyte strings are passed }
    if destlen=0 then
      begin
        DefaultAnsi2UnicodeMove(source,DefaultSystemCodePage,dest,len);
        exit;
      end;
    // this will null-terminate
    setlength(dest, destlen);
    if destlen>0 then
      begin
        MultiByteToWideChar(cp, dwFlags, source, len, @dest[1], destlen);
        PUnicodeRec(pointer(dest)-UnicodeFirstOff)^.CodePage:=CP_UTF16;
      end;
  end;


function Win32UnicodeUpper(const s : UnicodeString) : UnicodeString;
  begin
    result:=s;
    UniqueString(result);
    if length(result)>0 then
      CharUpperBuff(LPWSTR(result),length(result));
  end;


function Win32UnicodeLower(const s : UnicodeString) : UnicodeString;
  begin
    result:=s;
    UniqueString(result);
    if length(result)>0 then
      CharLowerBuff(LPWSTR(result),length(result));
  end;

{******************************************************************************
                              Widestring
 ******************************************************************************}

procedure Win32Ansi2WideMove(source:PAnsiChar;cp : TSystemCodePage;var dest:widestring;len:SizeInt);
  var
    destlen: SizeInt;
    dwFlags: DWORD;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    if cp=CP_UTF8 then
      dwFlags:=0
    else
      dwFlags:=MB_PRECOMPOSED;
    destlen:=MultiByteToWideChar(cp, dwFlags, source, len, nil, 0);
    // this will null-terminate
    setlength(dest, destlen);
    if destlen>0 then
      MultiByteToWideChar(cp, dwFlags, source, len, @dest[1], destlen);
  end;


function Win32WideUpper(const s : WideString) : WideString;
  begin
    result:=s;
    if length(result)>0 then
      CharUpperBuff(LPWSTR(result),length(result));
  end;


function Win32WideLower(const s : WideString) : WideString;
  begin
    result:=s;
    if length(result)>0 then
      CharLowerBuff(LPWSTR(result),length(result));
  end;

type
  PWStrInitEntry = ^TWStrInitEntry;
  TWStrInitEntry = record
    addr: PPointer;
    data: Pointer;
  end;

  PWStrInitTablesTable = ^TWStrInitTablesTable;
  TWStrInitTablesTable = packed record
    count  : sizeint;
    tables : packed array [1..32767] of PWStrInitEntry;
  end;

var
{$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
  WStrInitTablesTable: PWStrInitTablesTable;
{$else FPC_HAS_INDIRECT_ENTRY_INFORMATION}
  WStrInitTablesTableVar: TWStrInitTablesTable; external name 'FPC_WIDEINITTABLES';
  WStrInitTablesTable: PWStrInitTablesTable = @WStrInitTablesTableVar;
{$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION}

function GetACP:UINT; stdcall; external 'kernel32' name 'GetACP';
function GetConsoleCP:UINT; stdcall; external 'kernel32' name 'GetConsoleCP';

function Win32GetStandardCodePage(const stdcp: TStandardCodePageEnum): TSystemCodePage;
  begin
    case stdcp of
      scpAnsi,
      scpFileSystemSingleByte: Result := GetACP;
      scpConsoleInput: Result := GetConsoleCP;
      scpConsoleOutput: Result := GetConsoleOutputCP;
    end;
  end;

{ there is a similiar procedure in sysutils which inits the fields which
  are only relevant for the sysutils units }
procedure InitWin32Widestrings;
  var
    i: longint;
    ptable: PWStrInitEntry;
  begin
    { assign initial values to global Widestring typed consts }
    for i:=1 to WStrInitTablesTable^.count do
      begin
        ptable:=WStrInitTablesTable^.tables[i];
        while Assigned(ptable^.addr) do
          begin
            fpc_widestr_assign(ptable^.addr^, ptable^.data);
            Inc(ptable);
          end;
      end;

    { Note: since WideChar=UnicodeChar and PWideChar=PUnicodeChar,
      Wide2AnsiMoveProc is identical to Unicode2AnsiStrMoveProc. }

    { Widestring }
    widestringmanager.Wide2AnsiMoveProc:=@Win32Unicode2AnsiMove;
    widestringmanager.Ansi2WideMoveProc:=@Win32Ansi2WideMove;
    widestringmanager.UpperWideStringProc:=@Win32WideUpper;
    widestringmanager.LowerWideStringProc:=@Win32WideLower;
    { Unicode }
    widestringmanager.Unicode2AnsiMoveProc:=@Win32Unicode2AnsiMove;
    widestringmanager.Ansi2UnicodeMoveProc:=@Win32Ansi2UnicodeMove;
    widestringmanager.UpperUnicodeStringProc:=@Win32UnicodeUpper;
    widestringmanager.LowerUnicodeStringProc:=@Win32UnicodeLower;
    { Codepage }
    widestringmanager.GetStandardCodePageProc:=@Win32GetStandardCodePage;

    DefaultSystemCodePage:=GetACP;
    DefaultUnicodeCodePage:=CP_UTF16;
    DefaultFileSystemCodePage:=CP_UTF8;
    DefaultRTLFileSystemCodePage:=DefaultSystemCodePage;
  end;

type
  WINBOOL = longbool;
  PHANDLER_ROUTINE = function (dwCtrlType:DWORD):WINBOOL; stdcall;

function SetConsoleCtrlHandler(HandlerRoutine:PHANDLER_ROUTINE; Add:WINBOOL):WINBOOL; stdcall; 
  external 'kernel32' name 'SetConsoleCtrlHandler';

function WinCtrlBreakHandler(dwCtrlType:DWORD): WINBOOL;stdcall;
const
  CTRL_BREAK_EVENT = 1;
begin
  if Assigned(CtrlBreakHandler) then 
    Result:=CtrlBreakHandler((dwCtrlType and CTRL_BREAK_EVENT > 0))
  else
    Result:=false;
end;

function SysSetCtrlBreakHandler (Handler: TCtrlBreakHandler): TCtrlBreakHandler;
begin
  (* Return either nil or previous handler *)
  if (Assigned(CtrlBreakHandler)) and (not Assigned(Handler)) then   
    SetConsoleCtrlHandler(@WinCtrlBreakHandler, false)
  else if (not Assigned(CtrlBreakHandler)) and (Assigned(Handler)) then
    SetConsoleCtrlHandler(@WinCtrlBreakHandler, true);

  SysSetCtrlBreakHandler := CtrlBreakHandler;
  CtrlBreakHandler := Handler;
end;

procedure WinFinalizeSystem; inline;
begin
  finalize_arguments;
end;

