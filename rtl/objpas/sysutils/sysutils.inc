{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  { Read filename handling functions implementation }
  {$i fina.inc}

  { variant error codes }
  {$i varerror.inc}

    Function FileSearch (Const Name, DirList : String) : String;
    Var
      I : longint;
      Temp : String;

    begin
      // Start with checking the file in the current directory
      Result:=Name;
      temp:=SetDirSeparators(DirList);
      while True do begin
        If (Result <> '') and FileExists(Result) Then
          exit;
        If Temp = '' then
          Break; // No more directories to search - fail
        I:=pos(PathSeparator,Temp);
        If I<>0 then
          begin
            Result:=Copy (Temp,1,i-1);
            system.Delete(Temp,1,I);
          end
        else
          begin
            Result:=Temp;
            Temp:='';
          end;
        If Result<>'' then
          Result:=IncludeTrailingPathDelimiter(Result)+name;
      end;
      result:='';
    end;

  {$ifndef OS_FILEISREADONLY}
  Function FileIsReadOnly(const FileName: String): Boolean;

  begin
    Result := (FileGetAttr(FileName) and faReadOnly) <> 0;
  end;
  {$endif OS_FILEISREADONLY}


  {$ifndef OS_FILESETDATEBYNAME}
  Function FileSetDate (Const FileName : String;Age : Longint) : Longint;
  Var
    fd : THandle;
  begin
    { at least windows requires fmOpenWrite here }
    fd:=FileOpen(FileName,fmOpenWrite);
    If (Fd<>feInvalidHandle) then
      try
        Result:=FileSetDate(fd,Age);
      finally
        FileClose(fd);
      end
    else
      Result:=fd;
  end;
  {$endif}

  { Read String Handling functions implementation }
  {$i sysstr.inc}

  { Read date & Time function implementations }
{$ifndef FPUNONE}
  {$i dati.inc}
{$endif}

  { Read pchar handling functions implementation }
  {$i syspch.inc}

  { generic internationalisation code }
  {$i sysint.inc}

  { MCBS functions }
  {$i sysansi.inc}

  { wide string functions }
  {$i syswide.inc}

  { threading stuff }
  {$i sysuthrd.inc}

  { OS utility code }
  {$i osutil.inc}

    procedure FreeAndNil(var obj);
      var
        temp: tobject;
      begin
        temp:=tobject(obj);
        pointer(obj):=nil;
        temp.free;
      end;

  { Interfaces support }
  {$i sysuintf.inc}

    constructor Exception.Create(const msg : string);

      begin
         inherited create;
         fmessage:=msg;
      end;


    constructor Exception.CreateFmt(const msg : string; const args : array of const);

      begin
         inherited create;
         fmessage:=Format(msg,args);
      end;


    constructor Exception.CreateRes(ResString: PString);

      begin
         inherited create;
         fmessage:=ResString^;
      end;


    constructor Exception.CreateResFmt(ResString: PString; const Args: array of const);

      begin
         inherited create;
         fmessage:=Format(ResString^,args);
      end;


    constructor Exception.CreateHelp(const Msg: string; AHelpContext: Integer);

      begin
         inherited create;
         fmessage:=Msg;
         fhelpcontext:=AHelpContext;
      end;


    constructor Exception.CreateFmtHelp(const Msg: string; const Args: array of const;
      AHelpContext: Integer);

    begin
       inherited create;
       fmessage:=Format(Msg,args);
       fhelpcontext:=AHelpContext;
    end;


    constructor Exception.CreateResHelp(ResString: PString; AHelpContext: Integer);

    begin
       inherited create;
       fmessage:=ResString^;
       fhelpcontext:=AHelpContext;
    end;


    constructor Exception.CreateResFmtHelp(ResString: PString; const Args: array of const;
      AHelpContext: Integer);

    begin
       inherited create;
       fmessage:=Format(ResString^,args);
       fhelpcontext:=AHelpContext;
    end;


    procedure EHeapMemoryError.FreeInstance;
    begin
       if AllowFree then
        inherited FreeInstance;
    end;


    Constructor EVariantError.CreateCode (Code : longint);
    begin
       case Code of
         VAR_OK:
           Create(SNoError);
         VAR_PARAMNOTFOUND:
           Create(SVarParamNotFound);
         VAR_TYPEMISMATCH:
           Create(SInvalidVarCast);
         VAR_BADVARTYPE:
           Create(SVarBadType);
         VAR_OVERFLOW:
           Create(SVarOverflow);
         VAR_BADINDEX:
           Create(SVarArrayBounds);
         VAR_ARRAYISLOCKED:
           Create(SVarArrayLocked);
         VAR_NOTIMPL:
           Create(SVarNotImplemented);
         VAR_OUTOFMEMORY:
           Create(SVarOutOfMemory);
         VAR_INVALIDARG:
           Create(SVarInvalid);
         VAR_UNEXPECTED,
         VAR_EXCEPTION:
           Create(SVarUnexpected);
         else
           CreateFmt(SUnknownErrorCode,[Code]);
       end;
       ErrCode:=Code;
    end;


{$ifopt S+}
{$define STACKCHECK_WAS_ON}
{$S-}
{$endif OPT S }
Procedure CatchUnhandledException (Obj : TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);[public,alias:'FPC_BREAK_UNHANDLED_EXCEPTION'];
Var
  Message : String;
  i : longint;
  hstdout : ^text;
begin
  hstdout:=@stdout;
  Writeln(hstdout^,'An unhandled exception occurred at $',HexStr(PtrUInt(Addr),sizeof(PtrUInt)*2),' :');
  if Obj is exception then
   begin
     Message:=Exception(Obj).ClassName+' : '+Exception(Obj).Message;
     Writeln(hstdout^,Message);
   end
  else
   Writeln(hstdout^,'Exception object ',Obj.ClassName,' is not of class Exception.');
  Writeln(hstdout^,BackTraceStrFunc(Addr));
  if (FrameCount>0) then
    begin
      for i:=0 to FrameCount-1 do
        Writeln(hstdout^,BackTraceStrFunc(Frames[i]));
    end;
  Writeln(hstdout^,'');
end;


Var OutOfMemory : EOutOfMemory;
    InValidPointer : EInvalidPointer;


Procedure RunErrorToExcept (ErrNo : Longint; Address,Frame : Pointer);

Var E : Exception;
    HS : PString;

begin
  Case Errno of
   1,203 : E:=OutOfMemory;
   204 : E:=InvalidPointer;
   2,3,4,5,6,100,101,102,103,105,106 : { I/O errors }
     begin
     Case Errno of
       2 : HS:=@SFileNotFound;
       3 : HS:=@SInvalidFileName;
       4 : HS:=@STooManyOpenFiles;
       5 : HS:=@SAccessDenied;
       6 : HS:=@SInvalidFileHandle;
       15 : HS:=@SInvalidDrive;
       100 : HS:=@SEndOfFile;
       101 : HS:=@SDiskFull;
       102 : HS:=@SFileNotAssigned;
       103 : HS:=@SFileNotOpen;
       104 : HS:=@SFileNotOpenForInput;
       105 : HS:=@SFileNotOpenForOutput;
       106 : HS:=@SInvalidInput;
     end;
     E:=EinOutError.Create (HS^);
     // this routine can be called from FPC_IOCHECK,
     // which clears inoutres and then passes its
     // original value to HandleErrorFrame() (which calls
     // us). So use errno rather than IOResult, and clear
     // InOutRes explicitly in case we can also be called
     // from a place that does not clear InOutRes explicitly
     EInoutError(E).ErrorCode:=errno;
     inoutres:=0;
     end;
  // We don't set abstracterrorhandler, but we do it here.
  // Unless the use sets another handler we'll get here anyway...
  200 : E:=EDivByZero.Create(SDivByZero);
  201 : E:=ERangeError.Create(SRangeError);
  205 : E:=EOverflow.Create(SOverflow);
  206 : E:=EOverflow.Create(SUnderflow);
  207 : E:=EInvalidOp.Create(SInvalidOp);
  211 : E:=EAbstractError.Create(SAbstractError);
  212 : E:=EExternalException.Create(SExternalException);
  214 : E:=EBusError.Create(SBusError);
  215 : E:=EIntOverflow.Create(SIntOverflow);
  216 : E:=EAccessViolation.Create(SAccessViolation);
  217 : E:=EControlC.Create(SControlC);
  218 : E:=EPrivilege.Create(SPrivilege);
  219 : E:=EInvalidCast.Create(SInvalidCast);
  220 : E:=EVariantError.Create(SInvalidVarCast);
  221 : E:=EVariantError.Create(SInvalidVarOp);
  222 : E:=EVariantError.Create(SDispatchError);
  223 : E:=EVariantError.Create(SVarArrayCreate);
  224 : E:=EVariantError.Create(SVarNotArray);
  225 : E:=EVariantError.Create(SVarArrayBounds);
  227 : E:=EAssertionFailed.Create(SAssertionFailed);
  228 : E:=EIntfCastError.Create(SIntfCastError);
  229 : E:=ESafecallException.Create(SSafecallException);
  231 : E:=EConvertError.Create(SiconvError);
  232 : E:=ENoThreadSupport.Create(SNoThreadSupport);
  233 : E:=ENoWideStringSupport.Create(SMissingWStringManager);
  else
   E:=Exception.CreateFmt (SUnKnownRunTimeError,[Errno]);
  end;
  Raise E at Address,Frame;
end;

{$IFDEF HAS_OSERROR}
Procedure RaiseLastOSError;

var
  ECode: Cardinal;
  E : EOSError;

begin
  ECode := GetLastOSError;
  If (ECode<>0) then
    E:=EOSError.CreateFmt(SOSError, [ECode, SysErrorMessage(ECode)])
  else
    E:=EOSError.Create(SUnkOSError);
  E.ErrorCode:=ECode;
  Raise E;
end;
{$else}
Procedure RaiseLastOSError;

begin
  Raise Exception.Create('RaiseLastOSError not implemented on this platform.');
end;
{$endif}
Procedure AssertErrorHandler (Const Msg,FN : ShortString;LineNo:longint; TheAddr : pointer);
Var
  S : String;
begin
  If Msg='' then
    S:=SAssertionFailed
  else
    S:=Msg;
  Raise EAssertionFailed.Createfmt(SAssertError,[S,Fn,LineNo]) at Pointer(theAddr);
end;

{$ifdef STACKCHECK_WAS_ON}
{$S+}
{$endif}

Procedure InitExceptions;
{
  Must install uncaught exception handler (ExceptProc)
  and install exceptions for system exceptions or signals.
  (e.g: SIGSEGV -> ESegFault or so.)
}
begin
  ExceptionClass := Exception;
  ExceptProc:=@CatchUnhandledException;
  // Create objects that may have problems when there is no memory.
  OutOfMemory:=EOutOfMemory.Create(SOutOfMemory);
  OutOfMemory.AllowFree:=false;
  InvalidPointer:=EInvalidPointer.Create(SInvalidPointer);
  InvalidPointer.AllowFree:=false;
  AssertErrorProc:=@AssertErrorHandler;
  ErrorProc:=@RunErrorToExcept;
  OnShowException:=Nil;
end;


Procedure DoneExceptions;
begin
  OutOfMemory.AllowFree:=true;
  OutOfMemory.Free;
  InValidPointer.AllowFree:=true;
  InValidPointer.Free;
end;


{ Exception handling routines }

function ExceptObject: TObject;

begin
  If RaiseList=Nil then
    Result:=Nil
  else
    Result:=RaiseList^.FObject;
end;

function ExceptAddr: Pointer;

begin
  If RaiseList=Nil then
    Result:=Nil
  else
    Result:=RaiseList^.Addr;
end;

function ExceptFrameCount: Longint;

begin
  If RaiseList=Nil then
    Result:=0
  else
    Result:=RaiseList^.Framecount;
end;

function ExceptFrames: PPointer;

begin
  If RaiseList=Nil then
    Result:=Nil
  else
    Result:=RaiseList^.Frames;
end;

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
                               Buffer: PChar; Size: Integer): Integer;

Var
  S : AnsiString;
  Len : Integer;

begin
  S:=Format(SExceptionErrorMessage,[ExceptAddr,ExceptObject.ClassName]);
  If ExceptObject is Exception then
    S:=Format('%s:'#10'%s',[S,Exception(ExceptObject).Message]);
  Len:=Length(S);
  If S[Len]<>'.' then
    begin
    S:=S+'.';
    Inc(len);
    end;
  If Len>Size then
    Len:=Size;
  if Len > 0 then
    Move(S[1],Buffer^,Len);
  Result:=Len;
end;

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);

// use shortstring. On exception, the heap may be corrupt.

Var
  Buf : ShortString;

begin
  SetLength(Buf,ExceptionErrorMessage(ExceptObject,ExceptAddr,@Buf[1],255));
  If IsConsole Then
    writeln(Buf)
  else
    If Assigned(OnShowException) Then
      OnShowException (Buf);
end;

procedure Abort;

begin
  Raise EAbort.Create(SAbortError) at Pointer(Get_Caller_addr(Get_Frame));
end;

procedure OutOfMemoryError;

begin
  Raise OutOfMemory;
end;

{ ---------------------------------------------------------------------
    Initialization/Finalization/exit code
  ---------------------------------------------------------------------}

Type
  PPRecord = ^TPRecord;
  TPRecord = Record
    Func : TTerminateProc;
    NextFunc : PPRecord;
  end;

Const
  TPList : PPRecord = Nil;

procedure AddTerminateProc(TermProc: TTerminateProc);

Var
  TPR : PPRecord;

begin
  New(TPR);
  With TPR^ do
    begin
    NextFunc:=TPList;
    Func:=TermProc;
    end;
  TPList:=TPR;
end;

function CallTerminateProcs: Boolean;

Var
  TPR : PPRecord;

begin
  Result:=True;
  TPR:=TPList;
  While Result and (TPR<>Nil) do
    begin
    Result:=TPR^.Func();
    TPR:=TPR^.NextFunc;
    end;
end;

{ ---------------------------------------------------------------------
    Diskh functions, OS independent.
  ---------------------------------------------------------------------}

function ForceDirectories(Const Dir: RtlString): Boolean;

var
  E: EInOutError;
  ADrv : RtlString;

function DoForceDirectories(Const Dir: RtlString): Boolean;
var
  ADir : RtlString;
  APath: RtlString;
begin
  Result:=True;
  ADir:=ExcludeTrailingPathDelimiter(Dir);
  if (ADir='') then Exit;
  if Not DirectoryExists(ADir) then
    begin
      APath := ExtractFilePath(ADir);
      //this can happen on Windows if user specifies Dir like \user\name/test/
      //and would, if not checked for, cause an infinite recusrsion and a stack overflow
      if (APath = ADir) then Result := False
        else Result:=DoForceDirectories(APath);
    If Result then
      Result := CreateDir(ADir);
    end;
end;

function IsUncDrive(const Drv: RtlString): Boolean;
begin
  Result := (Length(Drv) > 2) and (Drv[1] = PathDelim) and (Drv[2] = PathDelim);
end;

begin
  Result := False;
  ADrv := ExtractFileDrive(Dir);
  if (ADrv<>'') and (not DirectoryExists(ADrv))
  {$IFNDEF FORCEDIR_NO_UNC_SUPPORT} and (not IsUncDrive(ADrv)){$ENDIF} then Exit;
  if Dir='' then
    begin
      E:=EInOutError.Create(SCannotCreateEmptyDir);
      E.ErrorCode:=3;
      Raise E;
    end;
  Result := DoForceDirectories(SetDirSeparators(Dir));
end;

Var
  GUIDCalledRandomize : Boolean = False;

Procedure GetRandomBytes(Var Buf; NBytes : Integer);

Var
  I : Integer;
  P : PByte;

begin
  P:=@Buf;
  If Not GUIDCalledRandomize then
    begin
    Randomize;
    GUIDCalledRandomize:=True;
    end;
  For I:=0 to NBytes-1 do
    P[i]:=Random(256);
end;

{$IFDEF HASCREATEGUID}
Function SysCreateGUID(out GUID : TGUID) : Integer; forward;
{$ENDIF}

Function CreateGUID(out GUID : TGUID) : Integer;
begin
  If Assigned(OnCreateGUID) then
    Result:=OnCreateGUID(GUID)
  else
    begin
    {$IFDEF HASCREATEGUID}
    Result:=SysCreateGUID(GUID);
    {$ELSE}
    GetRandomBytes(GUID,SizeOf(Guid));
    Result:=0;
    {$ENDIF}
    end;
end;


function SafeLoadLibrary(const FileName: AnsiString;
  ErrorMode: DWord = {$ifdef windows}SEM_NOOPENFILEERRORBOX{$else windows}0{$endif windows}): HMODULE;
{$if defined(cpui386) or defined(cpux86_64)}
  var
    mode : DWord;
    fpucw : Word;
    ssecw : DWord;
{$endif}
  begin
{$if defined(win64) or defined(win32)}
    mode:=SetErrorMode(ErrorMode);
{$endif}
    try
{$if defined(cpui386) or defined(cpux86_64)}
      fpucw:=Get8087CW;
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        ssecw:=GetSSECSR;
{$endif}
{$if defined(windows) or defined(win32)}
      Result:=LoadLibraryA(PChar(Filename));
{$else}
      Result:=0;
{$endif}
      finally
{$if defined(cpui386) or defined(cpux86_64)}
      Set8087CW(fpucw);
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        SetSSECSR(ssecw);
{$endif}
{$if defined(win64) or defined(win32)}
      SetErrorMode(mode);
{$endif}
    end;
  end;

function GetModuleName(Module: HMODULE): string;

begin
{$ifdef MSWINDOWS}
  SetLength(Result,MAX_PATH);
  SetLength(Result,GetModuleFileName(Module, Pchar(Result),Length(Result)));
{$ELSE}
  Result:='';
{$ENDIF}
end;
