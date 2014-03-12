{
 ****************************************************************************

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Free Pascal development team

    Free Pascal - OS/2 runtime library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

****************************************************************************}

unit system;

interface

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
  {.$define IODEBUG}
  {.$define DEBUGENVIRONMENT}
  {.$define DEBUGARGUMENTS}
{$endif SYSTEMDEBUG}

{$DEFINE OS2EXCEPTIONS}
{$define DISABLE_NO_THREAD_MANAGER}
{$DEFINE HAS_GETCPUCOUNT}

{$I systemh.inc}


const
  LineEnding = #13#10;
{ LFNSupport is defined separately below!!! }
  DirectorySeparator = '\';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ';';
  AllowDirectorySeparators : set of char = ['\','/'];
  AllowDriveSeparators : set of char = [':'];
{ FileNameCaseSensitive and FileNameCasePreserving are defined separately below!!! }
  MaxExitCode = 65535;
  MaxPathLen = 260;
(* MaxPathLen is referenced as constant from unit SysUtils   *)
(* - changing to variable or typed constant is not possible. *)
  AllFilesMask = '*';
  RealMaxPathLen: word = MaxPathLen;
(* Default value only - real value queried from the system on startup. *)

type    Tos=(osDOS,osOS2,osDPMI);

const   OS_Mode: Tos = osOS2;
        First_Meg: pointer = nil;

const   UnusedHandle=-1;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        LFNSupport: boolean = true;
        FileNameCaseSensitive: boolean = false;
        FileNameCasePreserving: boolean = true;
        CtrlZMarksEOF: boolean = true; (* #26 is considered as end of file *)

        sLineBreak = LineEnding;
        DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

var
{ C-compatible arguments and environment }
  argc  : longint;
  argv  : ppchar;
  envp  : ppchar;
  EnvC: cardinal;

(* Pointer to the block of environment variables - used e.g. in unit Dos. *)
  Environment: PChar;


var
(* Type / run mode of the current process: *)
(* 0 .. full screen OS/2 session           *)
(* 1 .. DOS session                        *)
(* 2 .. VIO windowable OS/2 session        *)
(* 3 .. Presentation Manager OS/2 session  *)
(* 4 .. detached (background) OS/2 process *)
  ApplicationType: cardinal;

const
 HeapAllocFlags: cardinal = $53; (* Compatible to VP/2 *)
 (* mfPag_Commit or mfObj_Tile or mfPag_Write or mfPag_Read *)

function ReadUseHighMem: boolean;

procedure WriteUseHighMem (B: boolean);

(* Is allocation of memory above 512 MB address limit allowed? Initialized *)
(* during initialization of system unit according to capabilities of the   *)
(* underlying OS/2 version, can be overridden by user - heap is allocated  *)
(* for all threads, so the setting isn't declared as a threadvar and       *)
(* should be only changed at the beginning of the main thread if needed.   *)
property
  UseHighMem: boolean read ReadUseHighMem write WriteUseHighMem;
(* UseHighMem is provided for compatibility with 2.0.x. *)

const
(* Are file sizes > 2 GB (64-bit) supported on the current system? *)
  FSApi64: boolean = false;


procedure SetDefaultOS2FileType (FType: ShortString);

procedure SetDefaultOS2Creator (Creator: ShortString);

type
  TDosOpenL = function (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                                 EA: pointer): cardinal; cdecl;

  TDosSetFilePtrL = function (Handle: THandle; Pos: int64; Method: cardinal;
                                        var PosActual: int64): cardinal; cdecl;

  TDosSetFileSizeL = function (Handle: THandle; Size: int64): cardinal; cdecl;


function DummyDosOpenL (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                                 EA: pointer): cardinal; cdecl;

function DummyDosSetFilePtrL (Handle: THandle; Pos: int64; Method: cardinal;
                                        var PosActual: int64): cardinal; cdecl;

function DummyDosSetFileSizeL (Handle: THandle; Size: int64): cardinal; cdecl;


const
  Sys_DosOpenL: TDosOpenL = @DummyDosOpenL;
  Sys_DosSetFilePtrL: TDosSetFilePtrL = @DummyDosSetFilePtrL;
  Sys_DosSetFileSizeL: TDosSetFileSizeL = @DummyDosSetFileSizeL;


implementation


{*****************************************************************************

                        System unit initialization.

****************************************************************************}

{$I system.inc}

{*****************************************************************************

                           Exception handling.

****************************************************************************}

{$IFDEF OS2EXCEPTIONS}
var
  { value of the stack segment
    to check if the call stack can be written on exceptions }
  _SS : Cardinal;

function Is_Prefetch (P: pointer): boolean;
  var
    A: array [0..15] of byte;
    DoAgain: boolean;
    InstrLo, InstrHi, OpCode: byte;
    I: longint;
    MemSize, MemAttrs: cardinal;
  begin
    Is_Prefetch := false;

    MemSize := SizeOf (A);
    if (DosQueryMem (P, MemSize, MemAttrs) = 0) and
            (MemAttrs and (mfPag_Free or mfPag_Commit) <> 0)
                                               and (MemSize >= SizeOf (A)) then
     Move (P^, A [0], SizeOf (A))
    else
     Exit;
    I := 0;
    DoAgain := true;
    while DoAgain and (I < 15) do
      begin
        OpCode := A [I];
        InstrLo := OpCode and $f;
        InstrHi := OpCode and $f0;
        case InstrHi of
          { prefix? }
          $20, $30:
            DoAgain := (InstrLo and 7) = 6;
          $60:
            DoAgain := (InstrLo and $c) = 4;
          $f0:
            DoAgain := InstrLo in [0, 2, 3];
          $0:
            begin
              Is_Prefetch := (InstrLo = $f) and (A [I + 1] in [$D, $18]);
              Exit;
            end;
          else
            DoAgain := false;
        end;
        Inc (I);
      end;
  end;

const
  MaxExceptionLevel = 16;
  ExceptLevel: byte = 0;

var
  ExceptEIP: array [0..MaxExceptionLevel - 1] of longint;
  ExceptError: array [0..MaxExceptionLevel - 1] of byte;
  ResetFPU: array [0..MaxExceptionLevel - 1] of boolean;

{$ifdef SYSTEMEXCEPTIONDEBUG}
procedure DebugHandleErrorAddrFrame (Error: longint; Addr, Frame: pointer);
begin
 if IsConsole then
  begin
   Write (StdErr, ' HandleErrorAddrFrame (error = ', Error);
   Write (StdErr, ', addr = ', hexstr (PtrUInt (Addr), 8));
   WriteLn (StdErr, ', frame = ', hexstr (PtrUInt (Frame), 8), ')');
  end;
 HandleErrorAddrFrame (Error, Addr, Frame);
end;
{$endif SYSTEMEXCEPTIONDEBUG}

procedure JumpToHandleErrorFrame;
var
 EIP, EBP, Error: longint;
begin
 (* save ebp *)
 asm
  movl (%ebp),%eax
  movl %eax,ebp
 end;
 if (ExceptLevel > 0) then
  Dec (ExceptLevel);
 EIP := ExceptEIP [ExceptLevel];
 Error := ExceptError [ExceptLevel];
{$ifdef SYSTEMEXCEPTIONDEBUG}
 if IsConsole then
  WriteLn (StdErr, 'In JumpToHandleErrorFrame error = ', Error);
{$endif SYSTEMEXCEPTIONDEBUG}
 if ResetFPU [ExceptLevel] then
  SysResetFPU;
 { build a fake stack }
 asm
{$ifdef REGCALL}
  movl   ebp,%ecx
  movl   eip,%edx
  movl   error,%eax
  pushl  eip
  movl   ebp,%ebp // Change frame pointer
{$else}
  movl   ebp,%eax
  pushl  %eax
  movl   eip,%eax
  pushl  %eax
  movl   error,%eax
  pushl  %eax
  movl   eip,%eax
  pushl  %eax
  movl   ebp,%ebp // Change frame pointer
{$endif}

{$ifdef SYSTEMEXCEPTIONDEBUG}
  jmpl   DebugHandleErrorAddrFrame
{$else not SYSTEMEXCEPTIONDEBUG}
  jmpl   HandleErrorAddrFrame
{$endif SYSTEMEXCEPTIONDEBUG}
 end;
end;


function System_Exception_Handler (Report: PExceptionReportRecord;
                                   RegRec: PExceptionRegistrationRecord;
                                   Context: PContextRecord;
                                   DispContext: pointer): cardinal; cdecl;
var
 Res: cardinal;
 Err: byte;
 Must_Reset_FPU: boolean;
{$IFDEF SYSTEMEXCEPTIONDEBUG}
 CurSS: cardinal;
 B: byte;
{$ENDIF SYSTEMEXCEPTIONDEBUG}
begin
{$ifdef SYSTEMEXCEPTIONDEBUG}
 if IsConsole then
  begin
    asm
      xorl %eax,%eax
      movw %ss,%ax
      movl %eax,CurSS
    end;
    WriteLn (StdErr, 'In System_Exception_Handler, error = ',
                                            HexStr (Report^.Exception_Num, 8));
    WriteLn (StdErr, 'Context SS = ', HexStr (Context^.Reg_SS, 8),
                                         ', current SS = ', HexStr (CurSS, 8));
  end;
{$endif SYSTEMEXCEPTIONDEBUG}
 Res := Xcpt_Continue_Search;
 if Context^.Reg_SS = _SS then
  begin
   Err := 0;
   Must_Reset_FPU := true;
{$ifdef SYSTEMEXCEPTIONDEBUG}
   if IsConsole then
    Writeln (StdErr, 'Exception  ', HexStr (Report^.Exception_Num, 8));
{$endif SYSTEMEXCEPTIONDEBUG}
   case Report^.Exception_Num of
    Xcpt_Integer_Divide_By_Zero,
    Xcpt_Float_Divide_By_Zero:
      Err := 200;
    Xcpt_Array_Bounds_Exceeded:
     begin
      Err := 201;
      Must_Reset_FPU := false;
     end;
    Xcpt_Unable_To_Grow_Stack:
     begin
      Err := 202;
      Must_Reset_FPU := false;
     end;
    Xcpt_Float_Overflow:
     Err := 205;
    Xcpt_Float_Denormal_Operand,
    Xcpt_Float_Underflow:
     Err := 206;
    {Context^.FloatSave.StatusWord := Context^.FloatSave.StatusWord and $ffffff00;}
    Xcpt_Float_Inexact_Result,
    Xcpt_Float_Invalid_Operation,
    Xcpt_Float_Stack_Check:
     Err := 207;
    Xcpt_Integer_Overflow:
     begin
      Err := 215;
      Must_Reset_FPU := false;
     end;
    Xcpt_Illegal_Instruction:
          { if we're testing sse support, simply set the flag and continue }
     if SSE_Check then
      begin
       OS_Supports_SSE := false;
          { skip the offending movaps %xmm7, %xmm6 instruction }
       Inc (Context^.Reg_EIP, 3);
       Report^.Exception_Num := 0;
       Res := Xcpt_Continue_Execution;
      end
     else
      Err := 216;
    Xcpt_Access_Violation:
     { Athlon prefetch bug? }
     if Is_Prefetch (pointer (Context^.Reg_EIP)) then
      begin
       { if yes, then retry }
       Report^.Exception_Num := 0;
       Res := Xcpt_Continue_Execution;
      end
     else
      Err := 216;
    Xcpt_Signal:
     case Report^.Parameters [0] of
      Xcpt_Signal_KillProc:
       Err := 217;
      Xcpt_Signal_Break,
      Xcpt_Signal_Intr:
       if Assigned (CtrlBreakHandler) then
        if CtrlBreakHandler (Report^.Parameters [0] = Xcpt_Signal_Break) then
         begin
{$IFDEF SYSTEMEXCEPTIONDEBUG}
          WriteLn (StdErr, 'CtrlBreakHandler returned true');
{$ENDIF SYSTEMEXCEPTIONDEBUG}
          Report^.Exception_Num := 0;
          Res := Xcpt_Continue_Execution;
          DosAcknowledgeSignalException (Report^.Parameters [0]);
         end
        else
         Err := 217;
     end;
    Xcpt_Privileged_Instruction:
     begin
      Err := 218;
      Must_Reset_FPU := false;
     end;
    else
     begin
      if ((Report^.Exception_Num and Xcpt_Severity_Code)
                                                   = Xcpt_Fatal_Exception) then
       Err := 217
      else
       Err := 255;
     end;
   end;

   if (Err <> 0) and (ExceptLevel < MaxExceptionLevel) 
(* TH: The following line is necessary to avoid an endless loop *)
                 and (Report^.Exception_Num < Xcpt_Process_Terminate)
                                                                    then
    begin
     ExceptEIP [ExceptLevel] := Context^.Reg_EIP;
     ExceptError [ExceptLevel] := Err;
     ResetFPU [ExceptLevel] := Must_Reset_FPU;
     Inc (ExceptLevel);

     Context^.Reg_EIP := cardinal (@JumpToHandleErrorFrame);
     Report^.Exception_Num := 0;

     Res := Xcpt_Continue_Execution;
{$ifdef SYSTEMEXCEPTIONDEBUG}
     if IsConsole then
      begin
       WriteLn (StdErr, 'Exception Continue Exception set at ',
                                          HexStr (ExceptEIP [ExceptLevel], 8));
       WriteLn (StdErr, 'EIP changed to ',
             HexStr (longint (@JumpToHandleErrorFrame), 8), ', error = ', Err);
      end;
{$endif SYSTEMEXCEPTIONDEBUG}
    end;
  end
 else
  if (Report^.Exception_Num = Xcpt_Signal) and
    (Report^.Parameters [0] and (Xcpt_Signal_Intr or Xcpt_Signal_Break) <> 0)
                                           and Assigned (CtrlBreakHandler) then
{$IFDEF SYSTEMEXCEPTIONDEBUG}
   begin
    WriteLn (StdErr, 'XCPT_SIGNAL caught, CtrlBreakHandler assigned, Param = ',
                                                       Report^.Parameters [0]);
{$ENDIF SYSTEMEXCEPTIONDEBUG}
   if CtrlBreakHandler (Report^.Parameters [0] = Xcpt_Signal_Break) then
    begin
{$IFDEF SYSTEMEXCEPTIONDEBUG}
     WriteLn (StdErr, 'CtrlBreakHandler returned true');
{$ENDIF SYSTEMEXCEPTIONDEBUG}
     Report^.Exception_Num := 0;
     Res := Xcpt_Continue_Execution;
     DosAcknowledgeSignalException (Report^.Parameters [0]);
    end
   else
    Err := 217;
{$IFDEF SYSTEMEXCEPTIONDEBUG}
   end
  else
   if IsConsole then
    begin
     WriteLn (StdErr, 'Ctx flags = ', HexStr (Context^.ContextFlags, 8));
     if Context^.ContextFlags and Context_Floating_Point <> 0 then
      begin
       for B := 1 to 6 do
        Write (StdErr, 'Ctx Env [', B, '] = ', HexStr (Context^.Env [B], 8),
                                                                         ', ');
        WriteLn (StdErr, 'Ctx Env [7] = ', HexStr (Context^.Env [7], 8));
       for B := 0 to 6 do
        Write (StdErr, 'FPU stack [', B, '] = ', Context^.FPUStack [B], ', ');
       WriteLn (StdErr, 'FPU stack [7] = ', Context^.FPUStack [7]);
      end;
     if Context^.ContextFlags and Context_Segments <> 0 then
      WriteLn (StdErr, 'GS = ', HexStr (Context^.Reg_GS, 8),
                     ', FS = ', HexStr (Context^.Reg_FS, 8),
                     ', ES = ', HexStr (Context^.Reg_ES, 8),
                     ', DS = ', HexStr (Context^.Reg_DS, 8));
     if Context^.ContextFlags and Context_Integer <> 0 then
      begin
       WriteLn (StdErr, 'EDI = ', HexStr (Context^.Reg_EDI, 8),
                      ', ESI = ', HexStr (Context^.Reg_ESI, 8));
       WriteLn (StdErr, 'EAX = ', HexStr (Context^.Reg_EAX, 8),
                      ', EBX = ', HexStr (Context^.Reg_EBX, 8),
                      ', ECX = ', HexStr (Context^.Reg_ECX, 8),
                      ', EDX = ', HexStr (Context^.Reg_EDX, 8));
      end;
     if Context^.ContextFlags and Context_Control <> 0 then
      begin
       WriteLn (StdErr, 'EBP = ', HexStr (Context^.Reg_EBP, 8),
                      ', SS = ', HexStr (Context^.Reg_SS, 8),
                      ', ESP = ', HexStr (Context^.Reg_ESP, 8));
       WriteLn (StdErr, 'CS = ', HexStr (Context^.Reg_CS, 8),
                      ', EIP = ', HexStr (Context^.Reg_EIP, 8),
                      ', EFlags = ', HexStr (Context^.Flags, 8));
      end;
    end;
{$endif SYSTEMEXCEPTIONDEBUG}
 System_Exception_Handler := Res;
end;


var
  ExcptReg: PExceptionRegistrationRecord; public name '_excptregptr';

{$ifdef SYSTEMEXCEPTIONDEBUG}
var
 OldExceptAddr,
 NewExceptAddr: PtrUInt;
{$endif SYSTEMEXCEPTIONDEBUG}

procedure Install_Exception_Handler;
var
 T: cardinal;
begin
{$ifdef SYSTEMEXCEPTIONDEBUG}
(* ThreadInfoBlock is located at FS:[0], the first      *)
(* entry is pointer to head of exception handler chain. *)
 asm
  movl $0,%eax
  movl %fs:(%eax),%eax
  movl %eax, OldExceptAddr
 end;
{$endif SYSTEMEXCEPTIONDEBUG}
 with ExcptReg^ do
  begin
   Prev_Structure := nil;
   ExceptionHandler := TExceptionHandler (@System_Exception_Handler);
  end;
 (* Disable pop-up windows for errors and exceptions *)
 DosError (deDisableExceptions);
 DosSetExceptionHandler (ExcptReg^);
 if IsConsole then
  begin
   DosSetSignalExceptionFocus (1, T);
   DosAcknowledgeSignalException (Xcpt_Signal_Intr);
   DosAcknowledgeSignalException (Xcpt_Signal_Break);
  end;
{$ifdef SYSTEMEXCEPTIONDEBUG}
 asm
  movl $0,%eax
  movl %fs:(%eax),%eax
  movl %eax, NewExceptAddr
 end;
{$endif SYSTEMEXCEPTIONDEBUG}
end;

procedure Remove_Exception_Handlers;
begin
  DosUnsetExceptionHandler (ExcptReg^);
end;
{$ENDIF OS2EXCEPTIONS}

procedure system_exit;
begin
(*  if IsLibrary then
    ExitDLL(ExitCode);
*)
(*
  if not IsConsole then
   begin
     Close(stderr);
     Close(stdout);
     Close(erroutput);
     Close(Input);
     Close(Output);
   end;
*)
{$IFDEF OS2EXCEPTIONS}
  Remove_Exception_Handlers;
{$ENDIF OS2EXCEPTIONS}
  DosExit (1{process}, exitcode);
end;

{$ASMMODE ATT}



{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

function paramcount:longint;assembler;
asm
    movl argc,%eax
    decl %eax
end {['EAX']};

function paramstr(l:longint):string;

var p:^Pchar;

begin
  if (l>=0) and (l<=paramcount) then
  begin
    p:=argv;
    paramstr:=strpas(p[l]);
  end
    else paramstr:='';
end;

procedure randomize;
var
  dt: TSysDateTime;
begin
  // Hmm... Lets use timer
  DosGetDateTime(dt);
  randseed:=dt.hour+(dt.minute shl 8)+(dt.second shl 16)+(dt.sec100 shl 32);
end;



{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

const
  WinInitialize: TWinInitialize = nil;
  WinCreateMsgQueue: TWinCreateMsgQueue = nil;
  WinMessageBox: TWinMessageBox = nil;
  EnvSize: cardinal = 0;

var
  ErrorBuf: array [0..ErrorBufferLength] of char;
  ErrorLen: longint;
  PMWinHandle: cardinal;

function ErrorWrite (var F: TextRec): integer;
{
  An error message should always end with #13#10#13#10
}
var
  P: PChar;
  I: longint;
begin
  if F.BufPos > 0 then
   begin
     if F.BufPos + ErrorLen > ErrorBufferLength then
       I := ErrorBufferLength - ErrorLen
     else
       I := F.BufPos;
     Move (F.BufPtr^, ErrorBuf [ErrorLen], I);
     Inc (ErrorLen, I);
     ErrorBuf [ErrorLen] := #0;
   end;
  if ErrorLen > 3 then
   begin
     P := @ErrorBuf [ErrorLen];
     for I := 1 to 4 do
      begin
        Dec (P);
        if not (P^ in [#10, #13]) then
          break;
      end;
   end;
   if ErrorLen = ErrorBufferLength then
     I := 4;
   if (I = 4) then
    begin
      WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
      ErrorLen := 0;
    end;
  F.BufPos := 0;
  ErrorWrite := 0;
end;

function ErrorClose (var F: TextRec): integer;
begin
  if ErrorLen > 0 then
   begin
     WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
     ErrorLen := 0;
   end;
  ErrorLen := 0;
  ErrorClose := 0;
end;

function ErrorOpen (var F: TextRec): integer;
begin
  TextRec(F).InOutFunc := @ErrorWrite;
  TextRec(F).FlushFunc := @ErrorWrite;
  TextRec(F).CloseFunc := @ErrorClose;
  ErrorOpen := 0;
end;


procedure AssignError (var T: Text);
begin
  Assign (T, '');
  TextRec (T).OpenFunc := @ErrorOpen;
  Rewrite (T);
end;

procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in a messagebox }
(*
  StdInputHandle := longint(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  StdOutputHandle := longint(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  StdErrorHandle := longint(GetStdHandle(cardinal(STD_ERROR_HANDLE)));

  if not IsConsole then
    begin
      if (DosLoadModule (nil, 0, 'PMWIN', PMWinHandle) = 0) and
       (DosQueryProcAddr (PMWinHandle, 789, nil, pointer (WinMessageBox)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 763, nil, pointer (WinInitialize)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 716, nil, pointer (WinCreateMsgQueue))
                                                                           = 0)
        then
          begin
            WinInitialize (0);
            WinCreateMsgQueue (0, 0);
          end
        else
          HandleError (2);
     AssignError (StdErr);
     AssignError (StdOut);
     Assign (Output, '');
     Assign (Input, '');
   end
  else
   begin
*)
     OpenStdIO (Input, fmInput, StdInputHandle);
     OpenStdIO (Output, fmOutput, StdOutputHandle);
     OpenStdIO (ErrOutput, fmOutput, StdErrorHandle);
     OpenStdIO (StdOut, fmOutput, StdOutputHandle);
     OpenStdIO (StdErr, fmOutput, StdErrorHandle);
(*
   end;
*)
end;


function strcopy(dest,source : pchar) : pchar;assembler;
var
  saveeax,saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
{$ifdef REGCALL}
        movl    %eax,saveeax
        movl    %edx,%edi
{$else}
        movl    source,%edi
{$endif}
        testl   %edi,%edi
        jz      .LStrCopyDone
        leal    3(%edi),%ecx
        andl    $-4,%ecx
        movl    %edi,%esi
        subl    %edi,%ecx
{$ifdef REGCALL}
        movl    %eax,%edi
{$else}
        movl    dest,%edi
{$endif}
        jz      .LStrCopyAligned
.LStrCopyAlignLoop:
        movb    (%esi),%al
        incl    %edi
        incl    %esi
        testb   %al,%al
        movb    %al,-1(%edi)
        jz      .LStrCopyDone
        decl    %ecx
        jnz     .LStrCopyAlignLoop
        .balign  16
.LStrCopyAligned:
        movl    (%esi),%eax
        movl    %eax,%edx
        leal    0x0fefefeff(%eax),%ecx
        notl    %edx
        addl    $4,%esi
        andl    %edx,%ecx
        andl    $0x080808080,%ecx
        jnz     .LStrCopyEndFound
        movl    %eax,(%edi)
        addl    $4,%edi
        jmp     .LStrCopyAligned
.LStrCopyEndFound:
        testl   $0x0ff,%eax
        jz      .LStrCopyByte
        testl   $0x0ff00,%eax
        jz      .LStrCopyWord
        testl   $0x0ff0000,%eax
        jz      .LStrCopy3Bytes
        movl    %eax,(%edi)
        jmp     .LStrCopyDone
.LStrCopy3Bytes:
        xorb     %dl,%dl
        movw     %ax,(%edi)
        movb     %dl,2(%edi)
        jmp     .LStrCopyDone
.LStrCopyWord:
        movw    %ax,(%edi)
        jmp     .LStrCopyDone
.LStrCopyByte:
        movb    %al,(%edi)
.LStrCopyDone:
{$ifdef REGCALL}
        movl    saveeax,%eax
{$else}
        movl    dest,%eax
{$endif}
        movl    saveedi,%edi
        movl    saveesi,%esi
end;


threadvar
  DefaultCreator: ShortString;
  DefaultFileType: ShortString;


procedure SetDefaultOS2FileType (FType: ShortString);
begin
{$WARNING Not implemented yet!}
  DefaultFileType := FType;
end;


procedure SetDefaultOS2Creator (Creator: ShortString);
begin
{$WARNING Not implemented yet!}
  DefaultCreator := Creator;
end;


procedure InitEnvironment;
var env_count : longint;
    dos_env,cp : pchar;
begin
  env_count:=0;
  cp:=environment;
  while cp ^ <> #0 do
    begin
    inc(env_count);
    while (cp^ <> #0) do inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
    end;
  envp := sysgetmem((env_count+1) * sizeof(pchar));
  envc := env_count;
  if (envp = nil) then exit;
  cp:=environment;
  env_count:=0;
  while cp^ <> #0 do
  begin
    envp[env_count] := sysgetmem(strlen(cp)+1);
    strcopy(envp[env_count], cp);
{$IfDef DEBUGENVIRONMENT}
    Writeln(stderr,'env ',env_count,' = "',envp[env_count],'"');
{$EndIf}
    inc(env_count);
    while (cp^ <> #0) do
      inc(longint(cp)); { skip to NUL }
    inc(longint(cp)); { skip to next character }
  end;
  envp[env_count]:=nil;
end;


var
(* Initialized by system unit initialization *)
  PIB: PProcessInfoBlock;


procedure InitArguments;
var
  arglen,
  count   : PtrInt;
  argstart,
  pc,arg  : pchar;
  quote   : char;
  argvlen : PtrInt;

  procedure allocarg(idx,len: PtrInt);
    var
      oldargvlen : PtrInt;
    begin
      if idx>=argvlen then
       begin
         oldargvlen:=argvlen;
         argvlen:=(idx+8) and (not 7);
         sysreallocmem(argv,argvlen*sizeof(pointer));
{         fillchar(argv[oldargvlen],(argvlen-oldargvlen)*sizeof(pointer),0);}
       end;
      { use realloc to reuse already existing memory }
      { always allocate, even if length is zero, since }
      { the arg. is still present!                     }
      ArgV [Idx] := SysAllocMem (Succ (Len));
    end;

begin
  CmdLine := SysAllocMem (MaxPathLen);

  ArgV := SysAllocMem (8 * SizeOf (pointer));

  ArgLen := StrLen (PChar (PIB^.Cmd));
  Inc (ArgLen);

  if DosQueryModuleName (PIB^.Handle, MaxPathLen, CmdLine) = 0 then
   ArgVLen := Succ (StrLen (CmdLine))
  else
(* Error occurred - use program name from command line as fallback. *)
   begin
    Move (PIB^.Cmd^, CmdLine, ArgLen);
    ArgVLen := ArgLen;
   end;

{ Get ArgV [0] }
  ArgV [0] := SysAllocMem (ArgVLen);
  Move (CmdLine^, ArgV [0]^, ArgVLen);
  Count := 1;

(* PC points to leading space after program name on command line *)
  PC := PChar (PIB^.Cmd) + ArgLen;

(* ArgLen contains size of command line arguments including leading space. *)
  ArgLen := Succ (StrLen (PC));

  SysReallocMem (CmdLine, ArgVLen + ArgLen);

  Move (PC^, CmdLine [ArgVLen], Succ (ArgLen));

(* ArgV has space for 8 parameters from the first allocation. *)
  ArgVLen := 8;

  { process arguments }
  while pc^<>#0 do
   begin
     { skip leading spaces }
     while pc^ in [#1..#32] do
      inc(pc);
     if pc^=#0 then
      break;
     { calc argument length }
     quote:=' ';
     argstart:=pc;
     arglen:=0;
     while (pc^<>#0) do
      begin
        case pc^ of
          #1..#32 :
            begin
              if quote<>' ' then
               inc(arglen)
              else
               break;
            end;
          '"' :
            begin
              if quote<>'''' then
               begin
                 if pchar(pc+1)^<>'"' then
                  begin
                    if quote='"' then
                     quote:=' '
                    else
                     quote:='"';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          '''' :
            begin
              if quote<>'"' then
               begin
                 if pchar(pc+1)^<>'''' then
                  begin
                    if quote=''''  then
                     quote:=' '
                    else
                     quote:='''';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          else
            inc(arglen);
        end;
        inc(pc);
      end;
     { copy argument }
     { Don't copy the first one, it is already there.}
     If Count<>0 then
      begin
        allocarg(count,arglen);
        quote:=' ';
        pc:=argstart;
        arg:=argv[count];
        while (pc^<>#0) do
         begin
           case pc^ of
             #1..#32 :
               begin
                 if quote<>' ' then
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end
                 else
                  break;
               end;
             '"' :
               begin
                 if quote<>'''' then
                  begin
                    if pchar(pc+1)^<>'"' then
                     begin
                       if quote='"' then
                        quote:=' '
                       else
                        quote:='"';
                     end
                    else
                     inc(pc);
                  end
                 else
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end;
               end;
             '''' :
               begin
                 if quote<>'"' then
                  begin
                    if pchar(pc+1)^<>'''' then
                     begin
                       if quote=''''  then
                        quote:=' '
                       else
                        quote:='''';
                     end
                    else
                     inc(pc);
                  end
                 else
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end;
               end;
             else
               begin
                 arg^:=pc^;
                 inc(arg);
               end;
           end;
           inc(pc);
         end;
        arg^:=#0;
      end;
 {$IfDef DEBUGARGUMENTS}
     Writeln(stderr,'dos arg ',count,' #',arglen,'#',argv[count],'#');
 {$EndIf}
     inc(count);
   end;
  { get argc and create an nil entry }
  argc:=count;
  allocarg(argc,0);
  { free unused memory }
  sysreallocmem(argv,(argc+1)*sizeof(pointer));
end;

function GetFileHandleCount: longint;
var L1: longint;
    L2: cardinal;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

function CheckInitialStkLen (StkLen: SizeUInt): SizeUInt;
begin
  CheckInitialStkLen := StkLen;
end;

var TIB: PThreadInfoBlock;
    RC: cardinal;
    ErrStr: string;
    P: pointer;
    DosCallsHandle: THandle;
    DW: cardinal;

const
    DosCallsName: array [0..8] of char = 'DOSCALLS'#0;

{$IFDEF OS2UNICODE}
 {$I sysucode.inc}
{$ENDIF OS2UNICODE}

{*var}
{* ST: pointer;}
{*}
begin
{$IFDEF OS2EXCEPTIONS}
(*    asm
      { allocate space for exception registration record }
     pushl $0
     pushl $0}
{*     pushl %fs:(0)}
        { movl  %esp,%fs:(0)
          but don't insert it as it doesn't
          point to anything yet
          this will be used in signals unit }
     movl %esp,%eax
     movl %eax,ExcptReg
     pushl %ebp
     movl %esp,%eax
{*     movl %eax,st*}
     movl %eax,StackTop
    end;
{*    StackTop:=st;}
*)    asm
     xorl %eax,%eax
     movw %ss,%ax
     movl %eax,_SS
     call SysResetFPU
    end;
{$ENDIF OS2EXCEPTIONS}
    DosGetInfoBlocks (@TIB, @PIB);
    StackLength := CheckInitialStkLen (InitialStkLen);
    { TODO: verify if TIB^.StackLimit is correct,
      from MSWindows point of view TIB^.Stack should be used instead }
    StackBottom := TIB^.StackLimit - StackLength;

    {Set type of application}
    ApplicationType := PIB^.ProcType;
    ProcessID := PIB^.PID;
    ThreadID := TIB^.TIB2^.TID;
    IsConsole := ApplicationType <> 3;

    {Query maximum path length (QSV_MAX_PATH_LEN = 1)}
    if DosQuerySysInfo (1, 1, DW, SizeOf (DW)) = 0 then
     RealMaxPathLen := DW;

    ExitProc := nil;

{$IFDEF OS2EXCEPTIONS}
    Install_Exception_Handler;
{$ENDIF OS2EXCEPTIONS}

    (* Initialize the amount of file handles *)
    FileHandleCount := GetFileHandleCount;

    {Initialize the heap.}
    (* Logic is following:
       The heap is initially restricted to low address space (< 512 MB).
       If underlying OS/2 version allows using more than 512 MB per process
       (OS/2 WarpServer for e-Business, eComStation, possibly OS/2 Warp 4.0
       with FP13 and above as well), use of this high memory is allowed for
       future memory allocations at the end of System unit initialization.
       The consequences are that the compiled application can allocate more
       memory, but it must make sure to use direct DosAllocMem calls if it
       needs a memory block for some system API not supporting high memory.
       This is probably no problem for direct calls to these APIs, but
       there might be situations when a memory block needs to be passed
       to a 3rd party DLL which in turn calls such an API call. In case
       of problems usage of high memory can be turned off by setting
       UseHighMem to false - the program should change the setting at its
       very beginning (e.g. in initialization section of the first unit
       listed in the "uses" section) to avoid having preallocated memory
       from the high memory region before changing value of this variable. *)
    InitHeap;

    if DosQueryModuleHandle (@DosCallsName [0], DosCallsHandle) = 0 then
      begin
        if DosQueryProcAddr (DosCallsHandle, OrdDosOpenL, nil, P) = 0 then
          begin
            Sys_DosOpenL := TDosOpenL (P);
            if DosQueryProcAddr (DosCallsHandle, OrdDosSetFilePtrL, nil, P) = 0
                                                                           then
              begin
                Sys_DosSetFilePtrL := TDosSetFilePtrL (P);
                if DosQueryProcAddr (DosCallsHandle, OrdDosSetFileSizeL, nil,
                                                                    P) = 0 then
                  begin
                    Sys_DosSetFileSizeL := TDosSetFileSizeL (P);
                    FSApi64 := true;
                  end;
              end;
          end;
      end;

    { ... and exceptions }
    SysInitExceptions;
    fpc_cpucodeinit;

    InitUnicodeStringManager;
{$ifdef OS2UCODE}
    InitOS2WideStrings;
{$endif OS2UCODE}

    { ... and I/O }
    SysInitStdIO;

    { no I/O-Error }
    inoutres:=0;

    {Initialize environment (must be after InitHeap because allocates memory)}
    Environment := pointer (PIB^.Env);
    InitEnvironment;

    InitArguments;

    DefaultCreator := '';
    DefaultFileType := '';

    InitSystemThreads;
    InitVariantManager;

{$IFDEF EXTDUMPGROW}
{    Int_HeapSize := high (cardinal);}
{$ENDIF EXTDUMPGROW}
{$ifdef SYSTEMEXCEPTIONDEBUG}
 if IsConsole then
  WriteLn (StdErr, 'Old exception ', HexStr (OldExceptAddr, 8),
   ', new exception ', HexStr (NewExceptAddr, 8), ', _SS = ', HexStr (_SS, 8));
{$endif SYSTEMEXCEPTIONDEBUG}
end.
