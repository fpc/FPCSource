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
      lo,hi : word;
   end;

   wordrec = packed record
      lo,hi : byte;
   end;

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
   EInvalidPointer  = Class(Exception);
   EOutOfMemory     = Class(Exception);
   EAccessViolation = Class(Exception);
   EInvalidCast = Class(Exception);


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

{
  $Log$
  Revision 1.7  2001-01-18 22:09:09  michael
  + Merged fixes from fixbranch - file modes

  Revision 1.6  2000/12/07 09:15:18  florian
    + tintegerset added

  Revision 1.5  2000/09/24 21:55:07  peter
    * ttextrec,tfilerec added (merged)

  Revision 1.4  2000/08/30 18:29:34  hajny
    * OS/2 fix - datih.inc moved to the beginning

  Revision 1.3  2000/08/29 17:56:32  michael
  Merged syserrormsg fix

  Revision 1.2  2000/08/20 15:46:46  peter
    * sysutils.pp moved to target and merged with disk.inc, filutil.inc

  Revision 1.1.2.2  2000/08/22 19:21:48  michael
  + Implemented syserrormessage. Made dummies for go32v2 and OS/2
  * Changed linux/errors.pp so it uses pchars for storage.

  Revision 1.1.2.1  2000/08/20 15:07:36  peter
    * sysutils.pp moved into target specific directory and merged
      disk.inc and filutil.inc in it
}
