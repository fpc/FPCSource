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


Const
   DLL_PROCESS_ATTACH = 1;
   DLL_THREAD_ATTACH = 2;
   DLL_PROCESS_DETACH = 0;
   DLL_THREAD_DETACH = 3;
   DLLExitOK : boolean = true;
Var
  DLLBuf : Jmp_buf;
  MainThreadIdWin32 : DWORD;

function Dll_entry{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}(const info : TEntryInformation){$endif FPC_HAS_INDIRECT_MAIN_INFORMATION} : longbool; [public,alias:'_FPC_DLL_Entry'];
  var
    res : longbool;
  begin
{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}
     EntryInformation:=info;
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
     IsLibrary:=true;
     Dll_entry:=false;
     case DLLreason of
       DLL_PROCESS_ATTACH :
         begin
           MainThreadIdWin32 := Win32GetCurrentThreadId;
           If SetJmp(DLLBuf) = 0 then
             begin
               if assigned(Dll_Process_Attach_Hook) then
                 begin
                   res:=Dll_Process_Attach_Hook(DllParam);
                   if not res then
                     exit(false);
                 end;
{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}
               EntryInformation.PascalMain();
{$else FPC_HAS_INDIRECT_MAIN_INFORMATION}
               PascalMain;
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
               Dll_entry:=true;
             end
           else
             Dll_entry:=DLLExitOK;
         end;
       DLL_THREAD_ATTACH :
         begin
           inclocked(Thread_count);

           if (Win32GetCurrentThreadId <> MainThreadIdWin32) then
           begin
             { Set up TLS slot for the DLL }
             SysInitMultiThreading;
             { Allocate Threadvars  }
             { NS : no idea what is correct to pass here - pass dummy value for now }
			 { passing a dummy is ok, the correct value is read from the coff header of SysInstance (FK) }
             InitThread($1000000); { Assume everything is idempotent there, as the thread could have been created with BeginThread... }
           end;

           if assigned(Dll_Thread_Attach_Hook) then
             Dll_Thread_Attach_Hook(DllParam);
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_THREAD_DETACH :
         begin
           declocked(Thread_count);
           if assigned(Dll_Thread_Detach_Hook) then
             Dll_Thread_Detach_Hook(DllParam);
           { Release Threadvars }
           if (Win32GetCurrentThreadId <> MainThreadIdWin32) then
             DoneThread; { Assume everything is idempotent there }
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_PROCESS_DETACH :
         begin
           Dll_entry:=true; { return value is ignored }
           If SetJmp(DLLBuf) = 0 then
             FPC_Do_Exit;
           if assigned(Dll_Process_Detach_Hook) then
             Dll_Process_Detach_Hook(DllParam);

           { Free TLS resources used by ThreadVars }
           SysFiniMultiThreading;
         end;
     end;
  end;


Procedure ExitDLL(Exitcode : longint);
  begin
    DLLExitOK:=ExitCode=0;
    LongJmp(DLLBuf,1);
  end;

{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

function MessageBox(w1:longint;l1,l2:pointer;w2:longint):longint;
   stdcall;external 'user32' name 'MessageBoxA';

const
  ErrorBufferLength = 1024;
var
  ErrorBuf : array[0..ErrorBufferLength] of char;
  ErrorLen : SizeInt;

Function ErrorWrite(Var F: TextRec): Integer;
{
  An error message should always end with #13#10#13#10
}
var
  i : SizeInt;
Begin
  while F.BufPos>0 do
    begin
      begin
        if F.BufPos+ErrorLen>ErrorBufferLength then
          i:=ErrorBufferLength-ErrorLen
        else
          i:=F.BufPos;
        Move(F.BufPtr^,ErrorBuf[ErrorLen],i);
        inc(ErrorLen,i);
        ErrorBuf[ErrorLen]:=#0;
      end;
      if ErrorLen=ErrorBufferLength then
        begin
          MessageBox(0,@ErrorBuf,pchar('Error'),0);
          ErrorLen:=0;
        end;
      Dec(F.BufPos,i);
    end;
  ErrorWrite:=0;
End;


Function ErrorClose(Var F: TextRec): Integer;
begin
  if ErrorLen>0 then
   begin
     MessageBox(0,@ErrorBuf,pchar('Error'),0);
     ErrorLen:=0;
   end;
  ErrorLen:=0;
  ErrorClose:=0;
end;


Function ErrorOpen(Var F: TextRec): Integer;
Begin
  TextRec(F).InOutFunc:=@ErrorWrite;
  TextRec(F).FlushFunc:=@ErrorWrite;
  TextRec(F).CloseFunc:=@ErrorClose;
  ErrorLen:=0;
  ErrorOpen:=0;
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
  StdInputHandle:=longint(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  StdOutputHandle:=longint(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  StdErrorHandle:=longint(GetStdHandle(cardinal(STD_ERROR_HANDLE)));
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

{ ProcessID cached to avoid repeated calls to GetCurrentProcess. }

var
  ProcessID: SizeUInt;

function GetProcessID: SizeUInt;
  begin
    GetProcessID := ProcessID;
  end;


{******************************************************************************
                              Unicode
 ******************************************************************************}

procedure Win32Unicode2AnsiMove(source:punicodechar;var dest:ansistring;len:SizeInt);
  var
    destlen: SizeInt;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    destlen:=WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS, source, len, nil, 0, nil, nil);
    // this will null-terminate
    setlength(dest, destlen);
    WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS, source, len, @dest[1], destlen, nil, nil);
  end;

procedure Win32Ansi2UnicodeMove(source:pchar;var dest:UnicodeString;len:SizeInt);
  var
    destlen: SizeInt;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    destlen:=MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, source, len, nil, 0);
    // this will null-terminate
    setlength(dest, destlen);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, source, len, @dest[1], destlen);
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


{ there is a similiar procedure in sysutils which inits the fields which
  are only relevant for the sysutils units }
procedure InitWin32Widestrings;
  begin
    { Widestring }
    widestringmanager.Wide2AnsiMoveProc:=@Win32Wide2AnsiMove;
    widestringmanager.Ansi2WideMoveProc:=@Win32Ansi2WideMove;
    widestringmanager.UpperWideStringProc:=@Win32WideUpper;
    widestringmanager.LowerWideStringProc:=@Win32WideLower;
{$ifndef VER2_2}
    { Unicode }
    widestringmanager.Unicode2AnsiMoveProc:=@Win32Unicode2AnsiMove;
    widestringmanager.Ansi2UnicodeMoveProc:=@Win32Ansi2UnicodeMove;
    widestringmanager.UpperUnicodeStringProc:=@Win32UnicodeUpper;
    widestringmanager.LowerUnicodeStringProc:=@Win32UnicodeLower;
{$endif VER2_2}
  end;
