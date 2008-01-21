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
{ Using inlining for small system functions/wrappers }
{$inline on}
{$define SYSUTILSINLINE}

  { Read internationalization settings }
  {$i sysinth.inc}

  { Read date & Time function declarations }
  {$i osutilsh.inc}

  {$i datih.inc}

  { Read String Handling functions declaration }
  {$i sysstrh.inc}

type
   { some helpful data types }

   THandle = System.THandle;

   TProcedure = procedure;

   TFilename = String;

   TIntegerSet = Set of 0..SizeOf(Integer)*8-1;

   LongRec = packed record
      case Integer of
        0 : (Lo,Hi : Word);
        1 : (Bytes : Array[0..3] of Byte);
   end;

   WordRec = packed record
     Lo,Hi : Byte;
   end;

   Int64Rec = packed record
      case integer of
        0 : (Lo,Hi : Cardinal);
        1 : (Words : Array[0..3] of Word);
        2 : (Bytes : Array[0..7] of Byte);
   end;

   PByteArray = ^TByteArray;
   TByteArray = Array[0..32767] of Byte;

   PWordarray = ^TWordArray;
   TWordArray = array[0..16383] of Word;

   { exceptions }
   Exception = class(TObject)
    private
      fmessage : string;
      fhelpcontext : longint;
    public
      constructor Create(const msg : string);
      constructor CreateFmt(const msg : string; const args : array of const);
      constructor CreateRes(ResString: PString);
      constructor CreateResFmt(ResString: PString; const Args: array of const);
      constructor CreateHelp(const Msg: string; AHelpContext: Integer);
      constructor CreateFmtHelp(const Msg: string; const Args: array of const;
        AHelpContext: Integer);
      constructor CreateResHelp(ResString: PString; AHelpContext: Integer);
      constructor CreateResFmtHelp(ResString: PString; const Args: array of const;
        AHelpContext: Integer);
      { !!!! }
      property HelpContext : longint read fhelpcontext write fhelpcontext;
      property Message : string read fmessage write fmessage;
   end;

   ExceptClass = class of Exception;

   EExternal = class(Exception)
   public
{$ifdef win32}
     ExceptionRecord : PExceptionRecord;
{$endif win32}
   end;

   { integer math exceptions }
   EInterror    = Class(EExternal);
   EDivByZero   = Class(EIntError);
   ERangeError  = Class(EIntError);
   EIntOverflow = Class(EIntError);

   { General math errors }
   EMathError  = Class(EExternal);
   EInvalidOp  = Class(EMathError);
   EZeroDivide = Class(EMathError);
   EOverflow   = Class(EMathError);
   EUnderflow  = Class(EMathError);

   { Run-time and I/O Errors }
   EInOutError = class(Exception)
     public
       ErrorCode : Longint;
   end;

   EHeapMemoryError = class(Exception)
     protected
       AllowFree : boolean;
       procedure FreeInstance;override;
   end;

   EHeapException = EHeapMemoryError;

   EExternalException = class(EExternal);
   EInvalidPointer  = Class(EHeapMemoryError);
   EOutOfMemory     = Class(EHeapMemoryError);
   EInvalidCast = Class(Exception);
   EVariantError = Class(Exception)
     ErrCode : longint;
     Constructor CreateCode(Code : Longint);
   end;

   EAccessViolation = Class(EExternal);
   EBusError = Class(EAccessViolation);
   EPrivilege = class(EExternal);
   EStackOverflow = class(EExternal);
   EControlC = class(EExternal);

   { String conversion errors }
   EConvertError = class(Exception);
   EFormatError = class(Exception);

   { Other errors }
   EAbort           = Class(Exception);
   EAbstractError   = Class(Exception);
   EAssertionFailed = Class(Exception);

   EPropReadOnly = class(Exception);
   EPropWriteOnly = class(Exception);

   EIntfCastError = class(Exception);
   EInvalidContainer = class(Exception);
   EInvalidInsert = class(Exception);

   EPackageError = class(Exception);

   EOSError = class(Exception)
   public
     ErrorCode: Longint;
   end;

   ESafecallException = class(Exception);
   ENoThreadSupport = Class(Exception);


   { Exception handling routines }
   function ExceptObject: TObject;
   function ExceptAddr: Pointer;
   function ExceptFrameCount: Longint;
   function ExceptFrames: PPointer;
   function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
                                  Buffer: PChar; Size: Integer): Integer;
   procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
   procedure Abort;
   procedure OutOfMemoryError;
   procedure Beep;
   function SysErrorMessage(ErrorCode: Integer): String;

Type
   TCreateGUIDFunc = Function(Out GUID : TGUID) : Integer;

Var
   OnCreateGUID : TCreateGUIDFunc = Nil;

   Function CreateGUID(out GUID : TGUID) : Integer;

type
  TTerminateProc = Function: Boolean;

  procedure AddTerminateProc(TermProc: TTerminateProc);
  function CallTerminateProcs: Boolean;


Var
   OnShowException : Procedure (Msg : ShortString);

  { FileRec/TextRec }
  {$i filerec.inc}
  {$i textrec.inc}

Const
   HexDisplayPrefix : string = '$';

const
// commenting is VP fix. These idents are in a different unit there.
  PathDelim={System.}DirectorySeparator;
  DriveDelim={System.}DriveSeparator;
  PathSep={System.}PathSeparator;
  MAX_PATH={System.}MaxPathLen;

Type
   TFileRec=FileRec;
   TTextRec=TextRec;


  { Read pchar handling functions declaration }
  {$i syspchh.inc}

  { MCBS functions }
  {$i sysansih.inc}

  { wide string functions }
  {$i syswideh.inc}

  { Read filename handling functions declaration }
  {$i finah.inc}

  { Read other file handling function declarations }
  {$i filutilh.inc}

  { Read disk function declarations }
  {$i diskh.inc}

  { read thread handling }
  {$i systhrdh.inc}

  procedure FreeAndNil(var obj);

  { interface handling }
  {$i intfh.inc}

  function SafeLoadLibrary(const FileName: AnsiString;
    ErrorMode: DWord = {$ifdef windows}SEM_NOOPENFILEERRORBOX{$else windows}0{$endif windows}): HMODULE;
    
  function GetModuleName(Module: HMODULE): string;
  
{ some packages and unit related constants for compatibility }

const
  pfExeModule = $00000000;
  pfNeverBuild = $00000001;
  pfDesignOnly = $00000002;
  pfRunOnly = $00000004;
  pfIgnoreDupUnits = $00000008;
  pfPackageModule = $40000000;
  pfModuleTypeMask = $C0000000;
  pfV3Produced =  $00000000;
  pfProducerUndefined = $04000000;
  pfBCB4Produced = $08000000;
  pfDelphi4Produced = $0C000000;
  pfLibraryModule = $80000000;
  pfProducerMask = $0C000000;

const
  ufMainUnit     = $01;
  ufPackageUnit  = $02;
  ufWeakUnit     = $04;
  ufOrgWeakUnit  = $08;
  ufImplicitUnit = $10;

  ufWeakPackageUnit = ufPackageUnit or ufWeakUnit;
