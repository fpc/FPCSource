unit EventSink;

{$mode objfpc}{$H+}
{ COM EventSink.

  Copyright (C) 2011 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}

interface

uses
 Windows, SysUtils, Classes, ActiveX;

type
 TInvokeEvent = procedure(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word;
  Params: TDispParams; VarResult, ExcepInfo, ArgErr: Pointer) of object;

 { TAbstractEventSink }

 TAbstractEventSink = class(TObject, IDispatch,IUnknown) // see mantis #22156
 private
  FDispatch: IDispatch;
  FDispIntfIID: TGUID;
  FConnection: DWORD;
  FOwner: TComponent;
 protected
  { IUnknown }
  frefcount : longint;
  function QueryInterface(constref IID: TGUID; out Obj): HRESULT; stdcall;
  function _AddRef : longint;stdcall;
  function _Release : longint;stdcall;
  { IDispatch }
  function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
  function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT; stdcall;
  function GetIDsOfNames(const IID: TGUID; Names: Pointer;
   NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
  function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
   Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT; stdcall;
 public
  constructor Create(AOwner: TComponent);
  destructor Destroy; override;
  procedure Connect(AnAppDispatch: IDispatch; const AnAppDispIntfIID: TGUID);
  procedure Disconnect;
 end;

 TEventSink = class(TComponent)
 private
  FSink: TAbstractEventSink;
  FOnInvoke: TInvokeEvent;
 protected
  procedure DoInvoke(DispID: Integer; const IID: TGUID;
   LocaleID: Integer; Flags: Word; var Params;
   VarResult, ExcepInfo, ArgErr: Pointer); virtual;
 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure Connect(AnAppDispatch: IDispatch; const AnAppDispIntfIID: TGUID);
  procedure Disconnect;
 published
  property OnInvoke: TInvokeEvent read FOnInvoke write FOnInvoke;
 end;


implementation

uses
 ComObj;

{ TAbstractEventSink }

constructor TAbstractEventSink.Create(AOwner: TComponent);
begin
 inherited Create;
 FOwner := AOwner;
end;

destructor TAbstractEventSink.Destroy;
var p:pointer;
begin
 inherited Destroy;
end;

function TAbstractEventSink.GetIDsOfNames(const IID: TGUID; Names: Pointer;
 NameCount, LocaleID: Integer; DispIDs: Pointer): HRESULT; stdcall;
begin
 Result := E_NOTIMPL;
end;

function TAbstractEventSink.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT; stdcall;
begin
 Result := E_NOTIMPL;
end;

function TAbstractEventSink.GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
begin
 Count := 0;
 Result := S_OK;
end;

function TAbstractEventSink.Invoke(DispID: Integer; const IID: TGUID;
 LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
 ArgErr: Pointer): HRESULT; stdcall;
begin
 (FOwner as TEventSink).DoInvoke(DispID, IID, LocaleID, Flags,
  Params, VarResult, ExcepInfo, ArgErr);
 Result := S_OK;
end;

function TAbstractEventSink.QueryInterface(constref IID: TGUID; out Obj): HRESULT; stdcall;
begin
 // We need to return the event interface when it's asked for
 Result := E_NOINTERFACE;
 if GetInterface(IID,Obj) then
  Result := S_OK;
 if IsEqualGUID(IID, FDispIntfIID) and GetInterface(IDispatch,Obj) then
  Result := S_OK;
end;

function TAbstractEventSink._AddRef: longint; stdcall;
begin
 frefcount:=frefcount+1;
  _addref:=frefcount;
end;

function TAbstractEventSink._Release: longint; stdcall;
begin
 frefcount:=frefcount-1;
 _Release:=frefcount;
 if frefcount=0 then
   self.destroy;
end;

procedure TAbstractEventSink.Connect(AnAppDispatch: IDispatch;
 const AnAppDispIntfIID: TGUID);
begin
 FDispIntfIID := AnAppDispIntfIID;
 FDispatch := AnAppDispatch;
 // Hook the sink up to the automation server
 InterfaceConnect(FDispatch, FDispIntfIID, Self, FConnection);
end;

procedure TAbstractEventSink.Disconnect;
begin
 if Assigned(FDispatch) then begin
  // Unhook the sink from the automation server
  InterfaceDisconnect(FDispatch, FDispIntfIID, FConnection);
  FDispatch := nil;
  FConnection := 0;
 end;
end;

{ TEventSink }

procedure TEventSink.Connect(AnAppDispatch: IDispatch;
 const AnAppDispIntfIID: TGUID);
begin
 FSink.Connect(AnAppDispatch, AnAppDispIntfIID);
end;

procedure TEventSink.Disconnect;
begin
  FSink.Disconnect;
end;

constructor TEventSink.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 FSink := TAbstractEventSink.Create(self);
end;

destructor TEventSink.Destroy;
begin
 FSink.Disconnect;   // reference count will destroy FSink
// calling FSink.Destroy will raise error 204 since refcount=1 (not disconnected yet)
// FSink.Destroy;

 inherited Destroy;
end;

procedure TEventSink.DoInvoke(DispID: Integer; const IID: TGUID;
 LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
 ArgErr: Pointer);
begin
 if Assigned(FOnInvoke) then
  FOnInvoke(self, DispID, IID, LocaleID, Flags, TDispParams(Params),
   VarResult, ExcepInfo, ArgErr);
end;

end.
