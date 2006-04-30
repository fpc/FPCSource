{
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
{$inline on}
unit comobj;

  interface

    uses
      sysutils;

    type
      EOleError = class(Exception);

      EOleSysError = class(EOleError)
      private
        FErrorCode: HRESULT;
      public
        constructor Create(const Msg: string; aErrorCode: HRESULT;aHelpContext: Integer);
        property ErrorCode: HRESULT read FErrorCode write FErrorCode;
      end;

      EOleException = class(EOleSysError)
      private
        FHelpFile: string;
        FSource: string;
      public
        constructor Create(const Msg: string; aErrorCode: HRESULT;const aSource,aHelpFile: string;aHelpContext: Integer);
        property HelpFile: string read FHelpFile write FHelpFile;
        property Source: string read FSource write FSource;
      end;

  EOleRegistrationError = class(EOleError);


    function CreateClassID : ansistring;

    function CreateComObject(const ClassID: TGUID) : IUnknown;
    function CreateRemoteComObject(const MachineName : WideString;const ClassID : TGUID) : IUnknown;
    function CreateOleObject(const ClassName : string) : IDispatch;
    function GetActiveOleObject(const ClassName: string) : IDispatch;

    procedure OleCheck(Value : HResult);inline;
    procedure OleError(Code: HResult);

    function ProgIDToClassID(const id : string) : TGUID;

  implementation

    uses
       windows,activex;

    constructor EOleSysError.Create(const Msg: string; aErrorCode: HRESULT; aHelpContext: Integer);
      var
        m : string;
      begin
        if Msg='' then
          m:=SysErrorMessage(aErrorCode)
        else
          m:=Msg;
        inherited CreateHelp(m,HelpContext);
        FErrorCode:=aErrorCode;
      end;


    constructor EOleException.Create(const Msg: string; aErrorCode: HRESULT;const aSource,aHelpFile: string; aHelpContext: Integer);
      begin
        inherited Create(Msg,aErrorCode,aHelpContext);
        FHelpFile:=aHelpFile;
        FSource:=aSource;
      end;

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
     var
       id : TCLSID;
     begin
        id:=ProgIDToClassID(ClassName);
        OleCheck(CoCreateInstance(id,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IDispatch,result));
     end;


   function GetActiveOleObject(const ClassName : string) : IDispatch;
     begin
       {!!!!!!!}
       runerror(211);
     end;


   procedure OleError(Code: HResult);
     begin
       raise EOleSysError.Create('',Code,0);
     end;


   procedure OleCheck(Value : HResult);inline;
     begin
       if not(Succeeded(Value)) then
         OleError(Value);
     end;


   function ProgIDToClassID(const id : string) : TGUID;
     begin
       OleCheck(CLSIDFromProgID(PWideChar(WideString(id)),result));
     end;

const
  Initialized : boolean = false;

initialization
  if not(IsLibrary) then
    Initialized:=Succeeded(CoInitialize(nil));
finalization
  if Initialized then
    CoUninitialize;
end.
