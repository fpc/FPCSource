{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl
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
unit comserv;

interface

uses
  Classes, SysUtils, comobj, ActiveX;

{define DEBUG_COM}

//according to doc
// * ComServer Variable
// * DllCanUnloadNow Routine
// * DllGetClassObject Routine
// * DllRegisterServer Routine
// * DllUnregisterServer Routine
// * TComServer Class

//TODO Fix this
const
  CLASS_E_CLASSNOTAVAILABLE = -1;
  SELFREG_E_CLASS = -2;

type

  { TComServer }

  TComServer = class(TComServerObject)
  private
    fCountObject: Integer;
    fCountFactory: Integer;
  protected
    function CountObject(Created: Boolean): Integer; override;
    function CountFactory(Created: Boolean): Integer; override;
    function GetHelpFileName: string; override;
    function GetServerFileName: string; override;
    function GetServerKey: string; override;
    function GetServerName: string; override;
    function GetStartSuspended: Boolean; override;
    function GetTypeLib: ITypeLib; override;
    procedure SetHelpFileName(const Value: string); override;


    procedure RegisterServerFactory(Factory: TComObjectFactory);
    procedure UnregisterServerFactory(Factory: TComObjectFactory);
  public
    constructor Create;
    function CanUnloadNow: Boolean;
    procedure RegisterServer;
    procedure UnRegisterServer;
  end;

var
  ComServer: TComServer = nil;

//http://msdn.microsoft.com/en-us/library/ms690368%28VS.85%29.aspx
//If the function succeeds, the return value is S_OK. Otherwise, it is S_FALSE.
function DllCanUnloadNow: HResult; stdcall;


//S_OK - The object was retrieved successfully.
//CLASS_E_CLASSNOTAVAILABLE - The DLL does not support the class (object definition).
function DllGetClassObject(const rclsid: REFIID {should be REFCLSID}; const riid: REFIID; out ppv: Pointer): HResult; stdcall;

//S_OK - The registry entries were created successfully.
//SELFREG_E_TYPELIB - The server was unable to complete the registration of all the type libraries used by its classes.
//SELFREG_E_CLASS - The server was unable to complete the registration of all the object classes.
function DllRegisterServer: HResult; stdcall;

//S_OK - The registry entries were deleted successfully.
//S_FALSE - Unregistration of this server's known entries was successful, but other entries still exist for this server's classes.
//SELFREG_E_TYPELIB - The server was unable to remove the entries of all the type libraries used by its classes.
//SELFREG_E_CLASS - The server was unable to remove the entries of all the object classes.
function DllUnregisterServer: HResult; stdcall;

implementation

function DllCanUnloadNow: HResult; stdcall;
begin
{$ifdef DEBUG_COM}
  WriteLn('DllCanUnloadNow called');
{$endif}
  if ComServer.CanUnloadNow then
    Result := S_OK
  else
    Result := S_FALSE;
{$ifdef DEBUG_COM}
    WriteLn('DllCanUnloadNow return: ', Result);
{$endif}
end;

{
    //FROM MSDN (Error messages are different in MSDN.DllGetClassObject)

    HRESULT hres = E_OUTOFMEMORY;
    *ppvObj = NULL;

    CClassFactory *pClassFactory = new CClassFactory(rclsid);
    if (pClassFactory != NULL)   {
        hRes = pClassFactory->QueryInterface(riid, ppvObj);
        pClassFactory->Release();
    }
    return hRes;
}

function DllGetClassObject(const rclsid: REFIID {should be REFCLSID}; const riid: REFIID; out ppv: Pointer): HResult; stdcall;
var
  factory: TComObjectFactory;
begin
{$ifdef DEBUG_COM}
  WriteLn('DllGetClassObject called: ', GUIDToString(rclsid), ' ', GUIDToString(riid));
{$endif}

  factory := ComClassManager.GetFactoryFromClassID(rclsid);
  if factory = nil then
    Result := CLASS_E_CLASSNOTAVAILABLE
  else
  begin
    if factory.GetInterface(riid,ppv) then
      Result := S_OK
    else
      Result := CLASS_E_CLASSNOTAVAILABLE;
{$ifdef DEBUG_COM}
    WriteLn('DllGetClassObject return: ', Result);
{$endif}
  end;
end;

function DllRegisterServer: HResult; stdcall;
begin
{$ifdef DEBUG_COM}
  WriteLn('DllRegisterServer called');
{$endif}
  try
    ComServer.RegisterServer;
    Result := S_OK;
  except
    Result := SELFREG_E_CLASS;
  end;

end;

function DllUnregisterServer: HResult; stdcall;
begin
{$ifdef DEBUG_COM}
  WriteLn('DllUnregisterServer called');
{$endif}
  try
    ComServer.UnregisterServer;
    Result := S_OK;
  except
    Result := SELFREG_E_CLASS;
  end;
end;

{ TComServer }

function TComServer.CountObject(Created: Boolean): Integer;
begin
  if Created then
    InterLockedIncrement(fCountObject)
  else
    InterLockedDecrement(fCountObject);
end;

function TComServer.CountFactory(Created: Boolean): Integer;
begin
  if Created then
    InterLockedIncrement(fCountFactory)
  else
    InterLockedDecrement(fCountFactory);
end;

function TComServer.GetHelpFileName: string;
begin
  RunError(217);
end;

function TComServer.GetServerFileName: string;
begin
  RunError(217);
end;

function TComServer.GetServerKey: string;
begin
  RunError(217);
end;

function TComServer.GetServerName: string;
begin
  RunError(217);
end;

function TComServer.GetStartSuspended: Boolean;
begin
  RunError(217);
end;

function TComServer.GetTypeLib: ITypeLib;
begin
  RunError(217);
end;

procedure TComServer.SetHelpFileName(const Value: string);
begin
  RunError(217);
end;

procedure TComServer.RegisterServerFactory(Factory: TComObjectFactory);
begin
  Factory.UpdateRegistry(True);
end;

procedure TComServer.UnregisterServerFactory(Factory: TComObjectFactory);
begin
  Factory.UpdateRegistry(false);
end;

constructor TComServer.Create;
begin
  fCountFactory := 0;
  fCountObject := 0;
end;

function TComServer.CanUnloadNow: Boolean;
begin
  Result := False;
end;

procedure TComServer.RegisterServer;
begin
  ComClassManager.ForEachFactory(self, @RegisterServerFactory);
end;

procedure TComServer.UnRegisterServer;
begin
  ComClassManager.ForEachFactory(self, @UnregisterServerFactory);
end;

initialization
{$ifdef DEBUG_COM}
  WriteLn('comserv initialization begin');
{$endif}
  ComServer := TComServer.Create;
{$ifdef DEBUG_COM}
  WriteLn('comserv initialization end');
{$endif}
finalization
  ComServer.Free;
end.

