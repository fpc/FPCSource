program vclcomobject;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

type
  TDummyVCLComObject = class(TInterfacedObject, IVCLComObject)
  public
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; override;
    procedure FreeOnRelease;
  end;
var
  c: TComponent;
  v: IVCLComObject;

procedure DoCreateVCLComObject(Component: TComponent);
begin
  Component.VCLComObject := Pointer(V);
end;

{ TDummyVCLComObject }

procedure TDummyVCLComObject.FreeOnRelease;
begin

end;

function TDummyVCLComObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := E_UNEXPECTED;
end;

begin
  v := TDummyVCLComObject.Create;
  CreateVCLComObjectProc := @DoCreateVCLComObject;
  c := TComponent.Create(nil);
  if c.ComObject = nil then
    halt(1);
  c.Free;
  v := nil;
end.
