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
      Result:='';
      temp:=Dirlist;
      repeat
        While (Length(Temp)>0) and (Temp[1]=PathSeparator) do
          Delete(Temp,1,1);
        I:=pos(PathSep,Temp);
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
        If (Length(Result)>0) and (result[length(result)]<>DirectorySeparator) then
          Result:=Result+DirectorySeparator;
        Result:=Result+name;
        If not FileExists(Result) Then
         Result:='';
      until (length(temp)=0) or (length(result)<>0);
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
    fd : longint;

  begin
    fd:=FileOpen(FileName,fmOpenRead);
    If (Fd>=0) then
      try
        Result:=FileSetDate(fd,Age);
      finally
        FileClose(fd);
      end
    else
      Result:=Fd;
  end;
  {$endif}

  { Read String Handling functions implementation }
  {$i sysstr.inc}

  { Read date & Time function implementations }
  {$i dati.inc}

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

  { CPU Specific code }
  {$i sysutilp.inc}

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
begin
  Writeln(stdout,'An unhandled exception occurred at $',HexStr(Ptrint(Addr),sizeof(PtrInt)*2),' :');
  if Obj is exception then
   begin
     Message:=Exception(Obj).ClassName+' : '+Exception(Obj).Message;
     Writeln(stdout,Message);
   end
  else
   Writeln(stdout,'Exception object ',Obj.ClassName,' is not of class Exception.');
  Writeln(stdout,BackTraceStrFunc(Addr));
  if (FrameCount>0) then
    begin
      for i:=0 to FrameCount-1 do
        Writeln(stdout,BackTraceStrFunc(Frames[i]));
    end;
  Halt(217);
end;


Var OutOfMemory : EOutOfMemory;
    InValidPointer : EInvalidPointer;


Procedure RunErrorToExcept (ErrNo : Longint; Address,Frame : Pointer);

Var E : Exception;
    S : String;

begin
  Case Errno of
   1,203 : E:=OutOfMemory;
   204 : E:=InvalidPointer;
   2,3,4,5,6,100,101,102,103,105,106 : { I/O errors }
     begin
     Case Errno of
       2 : S:=SFileNotFound;
       3 : S:=SInvalidFileName;
       4 : S:=STooManyOpenFiles;
       5 : S:=SAccessDenied;
       6 : S:=SInvalidFileHandle;
       15 : S:=SInvalidDrive;
       100 : S:=SEndOfFile;
       101 : S:=SDiskFull;
       102 : S:=SFileNotAssigned;
       103 : S:=SFileNotOpen;
       104 : S:=SFileNotOpenForInput;
       105 : S:=SFileNotOpenForOutput;
       106 : S:=SInvalidInput;
     end;
     E:=EinOutError.Create (S);
     EInoutError(E).ErrorCode:=IOresult; // Clears InOutRes !!
     end;
  // We don't set abstracterrorhandler, but we do it here.
  // Unless the use sets another handler we'll get here anyway...
  200 : E:=EDivByZero.Create(SDivByZero);
  201 : E:=ERangeError.Create(SRangeError);
  205 : E:=EOverflow.Create(SOverflow);
  206 : E:=EOverflow.Create(SUnderflow);
  207 : E:=EInvalidOp.Create(SInvalidOp);
  211 : E:=EAbstractError.Create(SAbstractError);
  214 : E:=EBusError.Create(SBusError);
  215 : E:=EIntOverflow.Create(SIntOverflow);
  216 : E:=EAccessViolation.Create(SAccessViolation);
  217 : E:=EPrivilege.Create(SPrivilege);
  218 : E:=EControlC.Create(SControlC);
  219 : E:=EInvalidCast.Create(SInvalidCast);
  220 : E:=EVariantError.Create(SInvalidVarCast);
  221 : E:=EVariantError.Create(SInvalidVarOp);
  222 : E:=EVariantError.Create(SDispatchError);
  223 : E:=EVariantError.Create(SVarArrayCreate);
  224 : E:=EVariantError.Create(SVarNotArray);
  225 : E:=EVariantError.Create(SVarArrayBounds);
  227 : E:=EAssertionFailed.Create(SAssertionFailed);
  228 : E:=EExternalException.Create(SExternalException);
  229 : E:=EIntfCastError.Create(SIntfCastError);
  230 : E:=ESafecallException.Create(SSafecallException);
  232 : E:=ENoThreadSupport.Create(SNoThreadSupport);
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
  Raise EAssertionFailed.Createfmt(SAssertError,[S,Fn,LineNo]); // at Pointer(theAddr);
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


function ForceDirectories(Const Dir: string): Boolean;

var
  E: EInOutError;
  ADir : String;

begin
  Result:=True;
  ADir:=ExcludeTrailingPathDelimiter(Dir);
  if (ADir='') then
    begin
    E:=EInOutError.Create(SCannotCreateEmptyDir);
    E.ErrorCode:=3;
    Raise E;
    end;
  if Not DirectoryExists(ADir) then
    begin
    Result:=ForceDirectories(ExtractFilePath(ADir));
    If Result then
      CreateDir(ADir);
    end;
end;

Procedure GetRandomBytes(Var Buf; NBytes : Integer);

Var
  I : Integer;
  P : PByte;

begin
  P:=@Buf;
  Randomize;
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
  var
    mode : DWord;
{$if defined(cpui386) or defined(cpux86_64)}
    fpucw : Word;
    ssecw : DWord;
{$endif}
  begin
    mode:=SetErrorMode(ErrorMode);
    try
{$if defined(cpui386) or defined(cpux86_64)}
      fpucw:=Get8087CW;
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        ssecw:=GetSSECSR;
{$endif}
      Result:=LoadLibrary(PChar(Filename));
    finally
{$if defined(cpui386) or defined(cpux86_64)}
      Set8087CW(fpucw);
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        SetSSECSR(ssecw);
{$endif}
      SetErrorMode(mode);
    end;
  end;
