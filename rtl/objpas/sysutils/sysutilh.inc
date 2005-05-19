{
    $Id: sysutilh.inc,v 1.15 2005/05/12 20:30:51 michael Exp $
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
{$ifdef HASINLINE}
  {$inline on}
  {$define SYSUTILSINLINE}
{$endif}

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
   EVariantError = Class(Exception);

   EAccessViolation = Class(EExternal);
   EPrivilege = class(EExternal);
   EStackOverflow = class(EExternal);
   EControlC = class(EExternal);

   { String conversion errors }
   EConvertError = class(Exception);

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
   function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
                                  Buffer: PChar; Size: Integer): Integer;
   procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
   procedure Abort;
   procedure OutOfMemoryError;
   procedure Beep;
   function SysErrorMessage(ErrorCode: Integer): String;

   Function  CreateGUID(out GUID : TGUID) : Integer;

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

  { Read internationalization settings }
  {$i sysinth.inc}

  { Read pchar handling functions declaration }
  {$IFNDEF VIRTUALPASCAL}
  {$i syspchh.inc}
  {$ENDIF}

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

{$ifdef HASINTF}
  { interface handling }
  {$i intfh.inc}
{$endif HASINTF}

{
  $Log: sysutilh.inc,v $
  Revision 1.15  2005/05/12 20:30:51  michael
  + Added MAX_PATH for Delphi/Kylix compatibility.

  Revision 1.14  2005/03/12 14:56:22  florian
    + added Ansi* routines to widestring manager
    * made them using OS calls on windows

  Revision 1.13  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.12  2005/02/03 18:40:02  florian
    * compilation with 1.0.x fixed
    + infrastructure for WideCompareText implemented

  Revision 1.11  2005/02/03 16:21:59  peter
    * 1.0.x fix

  Revision 1.10  2005/02/01 20:22:50  florian
    * improved widestring infrastructure manager

}
