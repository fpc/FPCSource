{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  { Read message string definitions }
  {
   Add a language with IFDEF LANG_NAME
   just befor the final ELSE. This way English will always be the default.
  }

  {$IFDEF LANG_GERMAN}
  {$i strg.inc} // Does not exist yet !!
  {$ELSE}
  {$i stre.inc}
  {$ENDIF}

  { Read filename handling functions implementation }
  {$i fina.inc}

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

      { Read String Handling functions implementation }
  {$i sysstr.inc}

  { Read date & Time function implementations }
  {$i dati.inc}

  { Read pchar handling functions implementation }
  {$i syspch.inc}

  { MCBS functions }
  {$i sysansi.inc}

  { CPU Specific code }
  {$i sysutilp.inc}

    procedure FreeAndNil(var obj);
      var
        temp: tobject;
      begin
        temp:=tobject(obj);
        pointer(obj):=nil;
        temp.free;
      end;

{$ifdef HASINTF}
  { Interfaces support }
  {$i intf.inc}
{$endif HASINTF}

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


{$ifopt S+}
{$define STACKCHECK_WAS_ON}
{$S-}
{$endif OPT S }
Procedure CatchUnhandledException (Obj : TObject; Addr,Frame: Pointer);
Var
  Message : String;
begin
  Writeln(stdout,'An unhandled exception occurred at 0x',HexStr(Longint(Addr),8),' :');
  if Obj is exception then
   begin
     Message:=Exception(Obj).Message;
     Writeln(stdout,Message);
   end
  else
   Writeln(stdout,'Exception object ',Obj.ClassName,' is not of class Exception.');
  { to get a nice symify }
  Writeln(stdout,BackTraceStrFunc(Longint(Addr)));
  Dump_Stack(stdout,longint(frame));
  Writeln(stdout,'');
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
  else
   E:=Exception.CreateFmt (SUnKnownRunTimeError,[Errno]);
  end;
{$ifdef VER1_0}
  Raise E at longint(Address){$ifdef ENHANCEDRAISE},longint(Frame){$endif};
{$else VER1_0}
  Raise E at Address{$ifdef ENHANCEDRAISE},Frame){$endif};
{$endif VER1_0}
end;


Procedure AssertErrorHandler (Const Msg,FN : ShortString;LineNo,TheAddr : Longint);
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

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
                               Buffer: PChar; Size: Integer): Integer;

Var
  S : AnsiString;
  Len : Integer;

begin
  S:=Format(SExceptionErrorMessage,[ExceptObject.ClassName,ExceptAddr]);
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
{$ifdef VER1_0}
  Raise EAbort.Create(SAbortError) at Get_Caller_addr(Get_Frame);
{$else VER1_0}
  Raise EAbort.Create(SAbortError) at Pointer(Get_Caller_addr(Get_Frame));
{$endif VER1_0}
end;

procedure OutOfMemoryError;

begin
  Raise OutOfMemory;
end;

{
  $Log$
  Revision 1.13  2003-01-01 20:58:07  florian
    + added invalid instruction exception

  Revision 1.12  2002/10/07 19:43:24  florian
    + empty prototypes for the AnsiStr* multi byte functions added

  Revision 1.11  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

  Revision 1.10  2002/07/16 13:57:39  florian
    * raise takes now a void pointer as at and frame address
      instead of a longint, fixed

  Revision 1.9  2002/01/25 17:42:03  peter
    * interface helpers

  Revision 1.8  2002/01/25 16:23:03  peter
    * merged filesearch() fix

}
