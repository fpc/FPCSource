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

  uses dos; { should become platform independent }

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

       exceptclass = class of exception;

       exception = class(tobject)
       private
          fmessage : string;
          fhelpcontext : longint;
        public
          constructor create(const msg : string);
          constructor createfmt(const msg; const args : array of const);
          constructor createres(indent : longint);
          { !!!! }
          property helpcontext : longint read fhelpcontext write fhelpcontext;
          property message : string read fmessage write fmessage;
       end;

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

    constructor texception.create(const msg : string);

      begin
         inherited create;
         message:=msg;
         {!!!!!}
      end;

    constructor texception.createfmt(const msg; const args : array of const);

      begin
         inherited create;
         {!!!!!}
      end;

    constructor texception.createres(indent : longint);

      begin
         inherited create;
         {!!!!!}
      end;

end.

{
    $Log$
    Revision 1.2  1998-04-10 15:18:21  michael
    Added a lot of functions donated by GertJan Schouten

    Revision 1.1.1.1  1998/03/25 11:18:49  root
    * Restored version

    Revision 1.1  1998/02/05 11:11:32  michael
    + moved to objpas directory

    Revision 1.2  1998/02/03 15:27:25  florian
    *** empty log message ***

    Revision 1.1  1998/02/01 23:32:01  florian
      + initial revision

}

