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
(* This must be placed before other functions, because TDateTime from DosCalls
   would be used under OS/2 instead of that one from datih.inc otherwise. *)
  {$i osutilsh.inc}

  {$i datih.inc}

  { Read String Handling functions declaration }
  {$i sysstrh.inc}

type
   { some helpful data types }

   tprocedure = procedure;

   tfilename = string;

   tsyscharset = set of char;
   tintegerset = set of 0..sizeof(integer)*8-1;

   longrec = packed record
      case integer of
        0 : (lo,hi : word);
        1 : (bytes : array[0..3] of byte);
   end;

   wordrec = packed record
      lo,hi : byte;
   end;

   int64rec = packed record
      case integer of
        0 : (lo,hi : cardinal);
        1 : (words : array[0..3] of word);
        2 : (bytes : array[0..7] of byte);
   end;

   pbytearray = ^tbytearray;
   tbytearray = array[0..32767] of byte;

   pwordarray = ^twordarray;
   twordarray = array[0..16383] of word;

   TMethod = packed record
     Code, Data: Pointer;
   end;

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
      property helpcontext : longint read fhelpcontext write fhelpcontext;
      property message : string read fmessage write fmessage;
   end;

   ExceptClass = class of Exception;

   { integer math exceptions }
   EInterror    = Class(Exception);
   EDivByZero   = Class(EIntError);
   ERangeError  = Class(EIntError);
   EIntOverflow = Class(EIntError);

   { General math errors }
   EMathError  = Class(Exception);
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

   EInvalidPointer  = Class(EHeapMemoryError);
   EOutOfMemory     = Class(EHeapMemoryError);
   EAccessViolation = Class(Exception);
   EInvalidCast = Class(Exception);
   EVariantError = Class(Exception);


   { String conversion errors }
   EConvertError = class(Exception);

   { Other errors }
   EAbort           = Class(Exception);
   EAbstractError   = Class(Exception);
   EAssertionFailed = Class(Exception);

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

Var
   OnShowException : Procedure (Msg : ShortString);

  { FileRec/TextRec }
  {$i filerec.inc}
  {$i textrec.inc}

Const
   HexDisplayPrefix : string = '$';

const
  PathDelim=System.DirectorySeparator;
  DriveDelim=System.DriveSeparator;
  PathSep=System.PathSeparator;


Type
   TFileRec=FileRec;
   TTextRec=TextRec;

  { Read internationalization settings }
  {$i sysinth.inc}

  { Read pchar handling functions declration }
  {$i syspchh.inc}

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
  Revision 1.17  2002-09-07 16:01:22  peter
    * old logs removed and tabs fixed

  Revision 1.16  2002/01/25 17:42:03  peter
    * interface helpers

}
