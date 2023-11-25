{
    This file is part of the Free Component Library

    Demonstrate bare-bones client-side JSON-RPC functionality using Invoke.
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program jsonrpcclient;


{$mode objfpc}{$H+}

{$if not defined(CPU386) and not defined(WIN64)}
{$define useffi}
{$endif}

uses
  SysUtils, Classes, fpjson, jsonparser, jsonscanner, fphttpclient, 
  rtti, typinfo {$ifdef useffi}, ffi.manager{$endif}, myapi, fpjsonvalue;

type
  TJsonRpcClient = class(TVirtualInterface)
  private
    class var
    aID : Integer;
  private
    fName: String;
    fBaseUrl: String;
    procedure HandleInvoke(aMethod: TRttiMethod; const aArgs: TValueArray; out
      aResult: TValue);

    function DoRequest(aRequest: TJSONData): TJSONData;
  public
    constructor Create(aTypeInfo: PTypeInfo; const aBaseUrl: String);

    generic class function GetClientIntf<T: IInterface>(const aBaseUrl: String): T;
  end;


procedure TJsonRpcClient.HandleInvoke(aMethod: TRttiMethod;
  const aArgs: TValueArray; out aResult: TValue);
var
  request, response: TJSONObject;
  args: specialize TArray<TRttiParameter>;
  arg: TRttiParameter;
  varParamCount, argidx, i: LongInt;
  resobj,argobj: TJSONObject;
  value: TValue;
  
  
begin
  VarParamCount:=0;
  request := TJSONObject.Create;
  try
    request.Add('method', aMethod.Name);
    request.Add('class', fName);
    request.Add('jsonrpc','2.0');
    inc(aID);
    request.Add('id',aID);
    { skip Self argument }
    argidx := 1;
    argobj := TJSONObject.Create;
    args := aMethod.GetParameters;
    for i := 0 to High(args) do begin
      arg := args[i];
      if [pfHidden,pfSelf] * arg.Flags <> [] then
        Continue
      else if ([pfVar,pfOut] * arg.Flags)<>[] then
        Inc(VarParamCount);
      argobj.Add(arg.Name, ValueToJSON(aArgs[argidx], arg.ParamType));
      Inc(argidx);
    end;
    request.Add('params', argobj);
    aResult:=Default(TValue);
    response := DoRequest(request) as TJSONObject;
    try
      if (VarParamCount=0) then
        begin    
        if Assigned(aMethod.ReturnType) then
          aResult := JSONToValue(response.Elements['result'], aMethod.ReturnType);
        end
      else
        begin  
        resObj:=response.Objects['result'];
        if Assigned(aMethod.ReturnType) then
          aResult := JSONToValue(resObj.Elements['$result'], aMethod.ReturnType);
        argidx := 1;
        for i := 0 to High(args) do 
          begin
          arg := args[i];
          if pfHidden in arg.Flags then
            Continue;
          if arg.Flags * [pfOut, pfVar] = [] then 
            begin
            Inc(argidx);
            Continue;
            end;
          value := JSONToValue(resObj.Elements[arg.Name], arg.ParamType);
          value.ExtractRawData(aArgs[argidx].GetReferenceToRawData);
          Inc(argidx);
          end; 
      end;
    finally
      response.Free;
    end;
  finally
    request.Free;
  end;
end;

function TJsonRpcClient.DoRequest(aRequest: TJSONData): TJSONData;
var
  client: TFPHTTPClient;
  ss: TStringStream;
  parser: TJSONParser;
  resp: String;
begin
  ss := TStringStream.Create(aRequest.AsJSON);
  try
    client := TFPHTTPClient.Create(Nil);
    try
      client.RequestBody := ss;

      resp := client.Post(fBaseUrl + fName);
      Writeln('Got response:');
      Writeln(resp);
      //parser := TJSONParser.Create(client.Post(fBaseUrl + fName), [joUTF8]);
      parser := TJSONParser.Create(resp, [joUTF8]);
      try
        Result := parser.Parse;
      finally
        parser.Free;
      end;
    finally
      client.Free;
    end;
  finally
    ss.Free;
  end;
end;

constructor TJsonRpcClient.Create(aTypeInfo: PTypeInfo; const aBaseUrl: String);
begin
  inherited Create(aTypeInfo, @HandleInvoke);
  fBaseUrl := aBaseUrl;
  if fBaseUrl[Length(fBaseUrl)] <> '/' then
    fBaseUrl := fBaseUrl + '/';
  fName := aTypeInfo^.Name;
end;

generic class function TJsonRpcClient.GetClientIntf<T>(const aBaseUrl: String): T;
var
  client: TJsonRpcClient;
  td: PTypeData;
begin
  client := TJsonRpcClient.Create(PTypeInfo(TypeInfo(T)), aBaseUrl);
  td := GetTypeData(PTypeInfo(TypeInfo(T)));
  client.QueryInterface(td^.GUID, Result);
end;

var
  client: IMyInterface;
  arr: TStringArray;
  s: String;
  res: Boolean;
begin
    client := TJsonRpcClient.specialize GetClientIntf<IMyInterface>('http://127.0.0.1:8080/RPC/');
    try
      Writeln('===== Testing SayHello');
      client.SayHello;
      Writeln('===== Testing DoSum');
      Writeln(client.DoSum(2, 6));
      Writeln('===== Testing Split');
      arr := client.Split('Hello FPC World', ' ');
      Writeln('Split data:');
      for s in arr do
        Writeln(#9, s);
      Writeln('===== Testing DoVarTest');
      s := 'Foobar';
      res := client.DoVarTest(s);
      Writeln(res, ' ', s);
      s := 'Test';
      res := client.DoVarTest(s);
      Writeln(res, ' ', s);
//      Writeln('===== Testing Echo');
//      writeln(Client.Echo(['This','is','Sparta']));
    finally
      client := Nil;
    end;
  {$ifndef unix}
  Readln;
  {$endif}
end.

