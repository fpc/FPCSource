{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Florian Klaempfl
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
unit comobj;

  interface

{$ifndef VER1_0}
    {$i comobjh.inc}

   function CreateComObject(const ClassID: TGUID) : IUnknown;
   function CreateRemoteComObject(const MachineName : WideString;const ClassID : TGUID) : IUnknown;
   function CreateOleObject(const ClassName : string) : IDispatch;
   function GetActiveOleObject(const ClassName: string) : IDispatch;
{$endif VER1_0}

  implementation

{$ifndef VER1_0}
    uses
       windows,activex;

    {$define FPC_COMOBJ_HAS_CREATE_CLASS_ID}
    function CreateClassID : ansistring;
      var
         ClassID : TCLSID;
         p : PWideChar;
      begin
         CoCreateGuid(ClassID);
         StringFromCLSID(ClassID,p);
         result:=p;
         CoTaskMemFree(p);
      end;

    {$i comobj.inc}

   function CreateComObject(const ClassID : TGUID) : IUnknown;
     begin
       {!!!!!!!}
       runerror(211);
     end;


   function CreateRemoteComObject(const MachineName : WideString;const ClassID : TGUID) : IUnknown;
     begin
       {!!!!!!!}
       runerror(211);
     end;


   function CreateOleObject(const ClassName : string) : IDispatch;
     begin
       {!!!!!!!}
       runerror(211);
     end;


   function GetActiveOleObject(const ClassName : string) : IDispatch;
     begin
       {!!!!!!!}
       runerror(211);
     end;


{$endif VER1_0}

end.
{
  $Log$
  Revision 1.2  2003-10-05 20:30:09  florian
    + necessary declarations for dx8 headers added

  Revision 1.1  2002/10/10 16:09:39  florian
    + initial revision
}
