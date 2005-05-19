{
    $Id: comobj.pp,v 1.1 2005/03/28 15:09:35 peter Exp $
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

   function CreateClassID : ansistring;

   function CreateComObject(const ClassID: TGUID) : IUnknown;
   function CreateRemoteComObject(const MachineName : WideString;const ClassID : TGUID) : IUnknown;
   function CreateOleObject(const ClassName : string) : IDispatch;
   function GetActiveOleObject(const ClassName: string) : IDispatch;

  implementation

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



end.
{
  $Log: comobj.pp,v $
  Revision 1.1  2005/03/28 15:09:35  peter
  new winunits packages

  Revision 1.3  2005/02/14 17:13:32  peter
    * truncate log

}
