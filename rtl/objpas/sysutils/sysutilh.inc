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

  { Read date & Time function declarations }
  {$i osutilsh.inc}

  {$i datih.inc}

  { Read String Handling functions declaration }
  {$i sysstrh.inc}

type
   { some helpful data types }

{$IFDEF VER1_0}
(* System type alias cannot be used under version *)
(* 1.0 because of different names of System unit. *)
   THandle = longint;
{$ELSE VER1_0}
   THandle = System.THandle;
{$ENDIF VER1_0}

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

{$ifdef HASWIDESTRING}
  { wide string functions }
  {$i syswideh.inc}
{$endif}

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
  $Log$
  Revision 1.11  2005-02-03 16:21:59  peter
    * 1.0.x fix

  Revision 1.10  2005/02/01 20:22:50  florian
    * improved widestring infrastructure manager

  Revision 1.9  2004/08/31 11:03:35  michael
  + Some capitalization added for documentation purposes

  Revision 1.8  2004/08/31 10:34:11  michael
  + Some capitalization added for documentation purposes

  Revision 1.7  2004/01/28 22:09:52  peter
    * EOSError.errorcode change to Longint which is more compatible
      with unix platforms

  Revision 1.6  2004/01/05 22:37:24  florian
    * changed sysutils.exec to ExecuteProcess

  Revision 1.5  2003/11/26 22:17:42  michael
  + Merged fixbranch fixes, missing in main branch

  Revision 1.4  2003/11/26 20:12:08  michael
  + New runerror 231 (exception stack error) and 232 (nothread support)

  Revision 1.3  2003/11/26 20:00:19  florian
    * error handling for Variants improved

  Revision 1.2  2003/10/25 23:43:59  hajny
    * THandle in sysutils common using System.THandle

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.20  2003/09/06 20:49:54  marco
   * Two minimal VP fixes

  Revision 1.19  2003/01/01 20:58:07  florian
    + added invalid instruction exception

  Revision 1.18  2002/10/07 19:43:24  florian
    + empty prototypes for the AnsiStr* multi byte functions added

  Revision 1.17  2002/09/07 16:01:22  peter
    * old logs removed and tabs fixed

  Revision 1.16  2002/01/25 17:42:03  peter
    * interface helpers

}