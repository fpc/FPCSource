{
    This file is part of the Free Component Library

    Server-side JSON-RPC functionality using Invoke.
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fprpcrtti;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpJson.Data, FpWeb.JsonRpc.Base, System.TypInfo, System.Rtti;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpjson, fpjsonrpc, typinfo, rtti;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TRTTIInstanceCreator = Function(const aClassName : string) : IInterface;

  IRPCCallContext = Interface ['{F026AE43-E0E5-4F3D-9878-9B70201E34B0}']
    Procedure SetRPCCallContext(aCallContext : TJSONRPCCallContext);
    Function GetRPCCallContext : TJSONRPCCallContext;
    Property RPCCallContext : TJSONRPCCallContext Read GetRPCCallContext Write SetRPCCallContext;
  end;

  { TRTTIJSONRPCHandler }

  TRTTIJSONRPCHandler = class(TCustomJSONRPCHandler)
  Private
    FIntfType : TRttiInterfaceType;
    FMethod : TRttiMethod;
    FCreator : TRTTIInstanceCreator;
    FRPCClassName: String;
  protected
    class function JSONToValue(aData: TJSONData; aType: TRttiType): TValue;
    class function ValueToJSON(const aValue: TValue; aType: TRttiType): TJSONData;
    Function CreateInstance : IInterface; virtual;
    Function DoExecute(Const Params : TJSONData; AContext : TJSONRPCCallContext): TJSONData; override;
    Property Method : TRttiMethod Read FMethod;
    Property IntfType : TRttiInterfaceType read FIntfType;
  Public
    Procedure SetRequestClassAndMethod(const aClassName,aMethodName : String); override;
    Property RPCClassName : String Read FRPCClassName;
  end;


  { TRTTIJSONRPCRegistry }

  TRTTIJSONRPCRegistry = class
  Private
    type
      TIntfEntry = record
        Name : String;
        GetInstance: TRTTIInstanceCreator;
        IntfType: TRttiInterfaceType;
      end;
    Class var
      _Instance : TRTTIJSONRPCRegistry;
  private
     fIntfs: array of TIntfEntry;
     fIntfCount: Integer;
     fContext: TRttiContext;
  Public
     class var
       SizeDelta : Integer;
  Public
     class constructor Init;
     class destructor done;
     Constructor Create; virtual;
     Destructor Destroy; override;
     Procedure Add(P : PTypeInfo; aCreator : TRTTIInstanceCreator; const aName : string = '');
     generic Procedure Add<T : IInterface>(aCreator : TRTTIInstanceCreator; const aName : string = '');
     Function Find(const aName : string; out IntfType: TRttiInterfaceType; out aCreator : TRTTIInstanceCreator) : Boolean;
     Function Get(const aName : string; out IntfType: TRttiInterfaceType; out aCreator : TRTTIInstanceCreator) : Boolean;
     class property Instance : TRTTIJSONRPCRegistry Read _Instance;
  end;

function RTTIJSONRPCRegistry : TRTTIJSONRPCRegistry;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpWeb.JSONRPC.Strings, FpJson.Value;
{$ELSE FPC_DOTTEDUNITS}
uses fprpcstrings, fpjsonvalue;
{$ENDIF FPC_DOTTEDUNITS}

function RTTIJSONRPCRegistry : TRTTIJSONRPCRegistry;

begin
  Result:=TRTTIJSONRPCRegistry.Instance;
end;

{ TRTTIJSONRPCRegistry }

class constructor TRTTIJSONRPCRegistry.Init;
begin
  SizeDelta:=32;
  _Instance:=TRTTIJSONRPCRegistry.Create;
end;

class destructor TRTTIJSONRPCRegistry.done;
begin
  _Instance.Free;
end;

constructor TRTTIJSONRPCRegistry.Create;
begin
  SetLength(FIntfs,SizeDelta);
  FContext:=TRTTIContext.Create;
  FIntfCount:=0;
end;

destructor TRTTIJSONRPCRegistry.Destroy;
begin
  SetLength(FIntfs,0);
  inherited Destroy;
end;

procedure TRTTIJSONRPCRegistry.Add(P: PTypeInfo; aCreator: TRTTIInstanceCreator;const aName : string = '');

var
  entry: TIntfEntry;
  aMethod : TRTTIMethod;
  aParamCount : Integer;

begin
  if aName='' then
    entry.Name:=P^.Name
  else
    entry.Name:=aName;
  entry.GetInstance := aCreator;
  entry.IntfType := fContext.GetType(P) as TRttiInterfaceType;
  if fIntfCount>=Length(fIntfs) then
    SetLength(fIntfs,Length(fIntfs)+SizeDelta);
  fIntfs[fIntfCount]:=entry;
  Inc(fIntfCount);
  for aMethod in entry.IntfType.GetDeclaredMethods do
    begin
    aParamCount:=Length(aMethod.GetParameters);
    JSONRPCHandlerManager.RegisterHandler(Entry.Name,aMethod.Name,TRTTIJSONRPCHandler,aParamCount);
    end;
end;

generic procedure TRTTIJSONRPCRegistry.Add<T>(aCreator : TRTTIInstanceCreator;const aName : string = '');

begin
  Add(PTypeInfo(TypeInfo(T)), aCreator, aName);
end;

function TRTTIJSONRPCRegistry.Find(Const aName: string; out IntfType: TRttiInterfaceType; out aCreator: TRTTIInstanceCreator): Boolean;

Var
  Idx : integer;
  Entry : TIntfEntry;

begin
  Result:=False;
  Idx:=fIntfCount-1;
  While (Idx>=0) and not Result do
    begin
    Result:=SameText(fIntfs[Idx].Name,aName);
    if Result then
      begin
      Entry:=fIntfs[Idx];
      IntfType:=Entry.IntfType;
      aCreator:=Entry.GetInstance;
      end;
    Dec(Idx);
    end;
end;

function TRTTIJSONRPCRegistry.Get(Const aName: string; out IntfType: TRttiInterfaceType; out aCreator: TRTTIInstanceCreator): Boolean;
begin
  Result:=Find(aName,IntfType,aCreator);
end;

{ TRTTIJSONRPCHandler }

function TRTTIJSONRPCHandler.CreateInstance: IInterface;
begin
  Result:=FCreator(FRPCClassName);
end;

procedure TRTTIJSONRPCHandler.SetRequestClassAndMethod(const aClassName, aMethodName: String);


begin
  FRPCClassName:=aClassName;
  RPCMethodName:=aMethodName;
  TRTTIJSONRPCRegistry.Instance.Get(FRPCClassName,FIntfType,FCreator);
  FMethod:=FIntfType.GetMethod(aMethodName);
  if FMethod=Nil then
    raise EJSONRPC.CreateFmt(SErrUnknownMethodForClass, [aClassName, aMethodName]);
end;

class function TRTTIJSONRPCHandler.ValueToJSON(const aValue: TValue; aType: TRttiType): TJSONData;
begin
  result:={$IFDEF FPC_DOTTEDUNITS}FpJson.Value{$ELSE}fpjsonvalue{$ENDIF}.ValueToJSON(aValue,aType);
end;

class function TRTTIJSONRPCHandler.JSONToValue(aData: TJSONData; aType: TRttiType): TValue;

begin
  result:={$IFDEF FPC_DOTTEDUNITS}FpJson.Value{$ELSE}fpjsonvalue{$ENDIF}.JSONToValue(aData,aType);
end;



function TRTTIJSONRPCHandler.DoExecute(const Params: TJSONData; AContext: TJSONRPCCallContext): TJSONData;

var
  margs: specialize TArray<TRttiParameter>;
  arg: TRttiParameter;
  args: array of TValue;
  argidx: SizeInt;
  resparams,i: LongInt;
  res, instance: TValue;
  intf,APIIntf : IUnknown;
  aVal : TJSONData;
  oRes : TJSONObject;
  CC : IRPCCallContext;


begin
  Result:=Nil;
  ResParams:=0;
  args:=[];
  if (Params.JSONType in StructuredJSONTypes) then
    SetLength(args, Params.Count)
  else
    args := Nil;
  argidx := 0;
  margs := method.GetParameters;
  for arg in margs do
    begin
    if pfHidden in arg.Flags then
      Continue
    else
      if ([pfVar,pfOut] * arg.Flags)<>[] then
        Inc(ResParams);
    if Params.JSONType = jtArray then
      aVal:=TJSONArray(Params).Items[argIdx]
    else
      aVal:=TJSONObject(Params).Elements[arg.Name];
    args[argidx] := JSONToValue(aVal, arg.ParamType);
    Inc(argidx);
    end;
  intf:=CreateInstance;
  if (Intf.QueryInterface(IRPCCallContext,CC)=S_OK) then
    CC.RPCCallContext:=aContext;
  if Intf.QueryInterface(FIntfType.GUID,APIIntf)<>S_OK then
    raise EJSONRPC.CreateFmt(SErrCreatorDoesNotSupportInterface, [FIntfType.Name]);
  TValue.Make(@APIIntf, PTypeInfo(FIntfType.Handle), instance);

  res := method.Invoke(instance, args);

  if ResParams=0 then
    begin
    if Assigned(method.ReturnType) then
      Result:=ValueToJSON(res, method.ReturnType)
    else
      Result:=TJSONNull.Create;
    end
  else
    begin
    oRes := TJSONObject.Create;
    Result:=oRes;
    try
      if Assigned(method.ReturnType) then
        oRes.Add('$result', ValueToJSON(res, method.ReturnType));
      argidx := 0;
      for i := 0 to High(margs) do
        begin
        arg := margs[i];
        if pfHidden in arg.Flags then
          Continue;
        if arg.Flags * [pfVar, pfOut] = [] then
          begin
          Inc(argidx);
          Continue;
          end;
        oRes.Add(arg.Name, ValueToJSON(args[argidx], arg.ParamType));
        Inc(argidx);
      end;
    except
      Result.Free;
    end;
    end;
  Intf:=nil;
end;


end.

