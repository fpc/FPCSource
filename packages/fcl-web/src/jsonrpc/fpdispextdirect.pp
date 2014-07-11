{
    This file is part of the Free Component Library

    Ext.Direct support - http independent part
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpdispextdirect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fpjsonrpc ;

Const
  DefaultExtDirectOptions = DefaultDispatchOptions + [jdoRequireClass];

Type
  { TCustomExtDirectDispatcher }

  TCustomExtDirectDispatcher = Class(TCustomJSONRPCDispatcher)
  private
    FAPIType: String;
    FNameSpace: String;
    FURL: String;
    function GetNameSpace: String;
    function isNameSpaceStored: boolean;
  Protected
    // Use this to initialize the container when the handler was created.
    Procedure InitContainer(H: TCustomJSONRPCHandler;  AContext: TJSONRPCCallContext; AContainer: TComponent); virtual;
    // Format the result
    function FormatResult(const AClassName, AMethodName: TJSONStringType; const Params, ID, Return: TJSONData): TJSONData; override;
    // Called during API creation. Can be used to restrict list of reported handlers.
    Function PublishHandler(H: TCustomJSONRPCHandler): Boolean; virtual;
    // Called during API creation. Can be used to restrict list of reported handlers.
    Function PublishHandlerDef(HD: TJSONRPCHandlerDef): Boolean; virtual;
    // 'tid'
    Class Function TransactionProperty : String; override;
    // 'method'
    Class Function MethodProperty : String; override;
    // 'action'
    Class Function ClassNameProperty : String; override;
    // 'data'
    Class Function ParamsProperty : String; override;
    // Add session support
    Function FindHandler(Const AClassName,AMethodName : TJSONStringType;AContext : TJSONRPCCallContext; Out FreeObject : TComponent) : TCustomJSONRPCHandler; override;
    // Add type field
    function CreateJSON2Error(Const AMessage : String; Const ACode : Integer; ID : TJSONData = Nil; idname : TJSONStringType = 'id' ) : TJSONObject; override;
    // Create API method description
    Function HandlerToAPIMethod (H: TCustomJSONRPCHandler): TJSONObject; virtual;
    Function HandlerDefToAPIMethod (H: TJSONRPCHandlerDef): TJSONObject; virtual;
    // Create API
    Function DoAPI : TJSONData; virtual;
    // Namespace for API description. Must be set. Default 'FPWeb'
    Property NameSpace : String Read GetNameSpace Write FNameSpace Stored isNameSpaceStored;
    // URL property for router. Must be set
    Property URL : String Read FURL Write FURL;
    // "type". By default: 'remoting'
    Property APIType : String Read FAPIType Write FAPIType;
  Public
    // Override to set additional opions.
    Constructor Create(AOwner : TComponent); override;
    // Return API description object
    Function API: TJSONData;
    // Return API Description including namespace, as a string
    Function APIAsString(Formatted : Boolean = False) : String; virtual;
  end;

  { TExtDirectDispatcher }

  TExtDirectDispatcher = Class(TCustomExtDirectDispatcher)
  Published
    Property NameSpace;
    Property URL;
    Property APIType;
    Property OnStartBatch;
    Property OnDispatchRequest;
    Property OnFindHandler;
    Property OnEndBatch;
    Property Options;
  end;


implementation
{ TCustomExtDirectDispatcher }
Const
  DefaultNameSpace = 'FPWeb';

function TCustomExtDirectDispatcher.GetNameSpace: String;
begin
  Result:=FNameSpace;
  If (Result='') then
    Result:=DefaultNameSpace
end;

function TCustomExtDirectDispatcher.isNameSpaceStored: boolean;
begin
  Result:=NameSpace<>DefaultNameSpace;
end;

function TCustomExtDirectDispatcher.FormatResult(const AClassName,
  AMethodName: TJSONStringType; const Params, ID, Return: TJSONData): TJSONData;

begin
  Result:=Inherited FormatResult(AClassName,AMethodName,Params,ID,Return);
  TJSONObject(Result).Add('type','rpc');
  TJSONObject(Result).Add('action',AClassName);
  TJSONObject(Result).Add('method',AMethodName);
end;

Class Function TCustomExtDirectDispatcher.TransactionProperty: String;
begin
  Result:='tid';
end;

Class Function TCustomExtDirectDispatcher.MethodProperty: String;
begin
  Result:='method';
end;

Class Function TCustomExtDirectDispatcher.ClassNameProperty: String;
begin
  Result:='action';
end;

Class Function TCustomExtDirectDispatcher.ParamsProperty: String;
begin
  Result:='data';
end;

Procedure TCustomExtDirectDispatcher.InitContainer(H : TCustomJSONRPCHandler; AContext : TJSONRPCCallContext; AContainer : TComponent);

begin
  // Do nothing, must be overridden in descendents
end;

Function TCustomExtDirectDispatcher.FindHandler(Const AClassName,
  AMethodName: TJSONStringType; AContext: TJSONRPCCallContext; Out
  FreeObject: TComponent): TCustomJSONRPCHandler;
begin
  {$ifdef extdebug}SendDebugFmt('Searching for %s %s',[AClassName,AMethodName]);{$endif}
  Result:=inherited FindHandler(AClassName, AMethodName, AContext, FreeObject);
  InitContainer(Result,AContext,FreeObject);
  {$ifdef extdebug}SendDebugFmt('Done with searching for %s %s : %d',[AClassName,AMethodName,Ord(Assigned(Result))]);{$endif}
end;

function TCustomExtDirectDispatcher.CreateJSON2Error(Const AMessage: String;
  Const ACode: Integer; ID: TJSONData; idname: TJSONStringType): TJSONObject;
begin
  Result:=inherited CreateJSON2Error(AMessage,ACode,ID,idname);
  TJSONObject(Result).Add('type','rpc');
end;

Function TCustomExtDirectDispatcher.HandlerToAPIMethod(H: TCustomJSONRPCHandler
  ): TJSONObject;
begin
  Result:=TJSONObject.Create(['name',H.Name,'len',H.ParamDefs.Count])
end;

Function TCustomExtDirectDispatcher.HandlerDefToAPIMethod(H: TJSONRPCHandlerDef
  ): TJSONObject;
begin
  Result:=TJSONObject.Create(['name',H.HandlerMethodName,'len',H.ArgumentCount])
end;

Function TCustomExtDirectDispatcher.PublishHandler(H : TCustomJSONRPCHandler) : Boolean;

begin
  Result:=(H<>Nil); // Avoid warning
end;

Function TCustomExtDirectDispatcher.PublishHandlerDef(HD : TJSONRPCHandlerDef) : Boolean;

begin
  Result:=(HD<>Nil); // Avoid warning
end;

Function TCustomExtDirectDispatcher.DoAPI: TJSONData;

Var
  A,D : TJSONObject;
  R : TJSONArray;
  N : TJSONStringType;
  H : TCustomJSONRPCHandler;
  I,J : Integer;
  M : TCustomJSONRPCHandlerManager;
  HD : TJSONRPCHandlerDef;

begin
  {$ifdef extdebug}SendDebugFmt('Creating API entries',[]);{$endif}
  D:=TJSONObject.Create;
  try
    D.Add('url',URL);
    D.Add('type',APIType);
    A:=TJSONObject.Create;
    D.Add('actions',A);
    R:=Nil;
    N:='';
    If (jdoSearchOwner in Options) and Assigned(Owner) then
      begin
      for I:=Owner.ComponentCount-1 downto 0 do
        If Owner.Components[i] is TCustomJSONRPCHandler then
          begin
          H:=Owner.Components[i] as TCustomJSONRPCHandler;
          if PublishHandler(H) then
            begin
            If (R=Nil) then
              begin
              N:=Owner.Name;
              R:=TJSONArray.Create;
              A.Add(N,R);
              end;
            R.Add(HandlerToAPIMethod(H));
            end;
          end;
      end;
    If (jdoSearchRegistry in Options) then
      begin
      M:=JSONRPCHandlerManager;
      For I:=M.HandlerCount-1 downto 0 do
        begin
        HD:=M.HandlerDefs[i];
        if PublishHandlerDef(HD) then
          begin
          If (R=Nil) or (CompareText(N,HD.HandlerClassName)<>0) then
            begin
            N:=HD.HandlerClassName;
            J:=A.IndexOfName(N);
            If (J=-1) then
              begin
              R:=TJSONArray.Create;
              A.Add(N,R);
              end
            else
              R:=A.Items[J] as TJSONArray;
            end;
          R.Add(HandlerDefToAPIMethod(HD));
          end;
        end;
      end;
    Result:=D;
  except
    FreeAndNil(D);
    Raise;
  end;
end;

Constructor TCustomExtDirectDispatcher.Create(AOwner: TComponent);

Var
  O : TJSONRPCDispatchOptions;

begin
  inherited Create(AOwner);
  Options:=DefaultExtDirectOptions;
  APIType:='remoting';
end;

Function TCustomExtDirectDispatcher.API: TJSONData;
begin
  Result:=DoAPI;
end;

Function TCustomExtDirectDispatcher.APIAsString(Formatted: Boolean = False): String;

Var
  A : TJSONData;

begin
  A:=API;
  try
    if Formatted then
      Result:=NameSpace + ' = ' + A.FormatJSON + ';'
    else
      Result:=NameSpace + ' = ' + A.AsJSON + ';';
  finally
    A.Free;
  end;
end;


{$ifdef extdebug}
uses dbugintf;
{$endif}

end.

