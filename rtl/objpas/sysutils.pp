{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysutils;
interface

{$MODE objfpc}

    uses
    {$ifdef linux}
       linux
    {$else}
       dos
      {$ifdef go32v2}
         ,go32
      {$endif go32v2}
    {$endif linux}
    {$ifndef AUTOOBJPAS}
       ,objpas
    {$endif}   
       ;


    type
       { some helpful data types }

       tprocedure = procedure;

       tfilename = string;

       longrec = packed record
          lo,hi : word;
       end;

       wordrec = packed record
          lo,hi : byte;
       end;

       { exceptions }
       exception = class(TObject)
        private
          fmessage : string;
          fhelpcontext : longint;
        public
          constructor create(const msg : string);
//!! No array of const yet.
//          constructor createfmt(const msg; const args : array of const);
          constructor createres(indent : longint);
          { !!!! }
          property helpcontext : longint read fhelpcontext write fhelpcontext;
          property message : string read fmessage write fmessage;
       end;

       exceptclass = class of exception;

       { math. exceptions }
       einterror = class(exception);
       edivbyzero = class(einterror);
       erangeerror = class(einterror);
       eintoverflow = class(einterror);
       ematherror = class(exception);


  { Read date & Time function declarations }
  {$i datih.inc}


  { Read String Handling functions declaration }
  {$i sysstrh.inc}


  { Read pchar handling functions declration }
  {$i syspchh.inc}

  { Read filename handling functions declaration }

  {$i finah.inc}


  implementation

  { Read filename handling functions implementation }

  {$i fina.inc}

  { Read date & Time function implementations }
  {$i dati.inc}


  { Read String Handling functions implementation }
  {$i sysstr.inc}


  { Read pchar handling functions implementation }
  {$i syspch.inc}

    constructor exception.create(const msg : string);

      begin
         inherited create;
         fmessage:=msg;
         {!!!!!}
      end;

{
    constructor exception.createfmt(const msg; const args : array of const);

      begin
         inherited create;
      end;
}

    constructor exception.createres(indent : longint);

      begin
         inherited create;
         {!!!!!}
      end;

       
Procedure CatchUnhandledException (Obj : TObject; Addr: Pointer);    
Var
  Message : String;
begin
{$ifndef USE_WINDOWS}
  Writeln ('An unhandled exception occurred at ',HexStr(Longint(Addr),8),' : ');
  if Obj is exception then
   begin
     Message:=Exception(Obj).Message;
     Writeln (Message);
   end
  else
   Writeln ('Exception object ',Obj.ClassName,' is not of class Exception.');
  Halt(217);
{$else}
{$endif}  
end;


Procedure InitExceptions;
{
  Must install uncaught exception handler (ExceptProc)
  and install exceptions for system exceptions or signals.
  (e.g: SIGSEGV -> ESegFault or so.)
}
begin
  ExceptProc:=@CatchUnhandledException;   
end;


{Initialization code.}
begin
  InitExceptions;
end.
{
    $Log$
    Revision 1.10  1998-09-24 23:45:27  peter
      * updated for auto objpas loading

    Revision 1.9  1998/09/24 16:13:49  michael
    Changes in exception and open array handling

    Revision 1.8  1998/09/18 23:57:26  michael
    * Changed use_excepions to useexceptions

    Revision 1.7  1998/09/16 14:34:38  pierre
      * go32v2 did not compile
      * wrong code in systr.inc corrected

    Revision 1.6  1998/09/16 08:28:44  michael
    Update from gertjan Schouten, plus small fix for linux

    Revision 1.5  1998/09/04 08:49:07  peter
      * 0.99.5 doesn't compile a whole objpas anymore to overcome crashes

    Revision 1.4  1998/08/10 15:52:27  peter
      * fixed so 0.99.5 compiles it, but no exception class

    Revision 1.3  1998/07/29 15:44:32  michael
     included sysutils and math.pp as target. They compile now.
}
