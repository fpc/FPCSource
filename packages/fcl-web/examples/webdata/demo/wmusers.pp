unit wmusers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, fpWeb,
  db, dbf, fpwebdata, fpextjs,extjsjson,extjsxml;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    Dbf1: TDbf;
    procedure TFPWebActions0Request(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure TFPWebActions1Request(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure TFPWebActions2Request(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure TFPWebActions3Request(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation
{$define wmdebug}

{$ifdef wmdebug}
uses dbugintf;
{$endif}

{ TFPWebModule1 }

procedure TFPWebModule1.TFPWebActions0Request(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);

Var
  PN : String;
  P : TFPWebDataProvider;
  F : TExtJSDataFormatter;
  DS : TDatasource;
  M : TMemoryStream;
  L : Text;

begin
  // Providername;
  PN:=ARequest.GetNextPathInfo;
//  P:=GetWebDataProvider(PN);
  P:=TFPWebDataProvider.Create(Self);
  try
    P.Adaptor:=TWebDataInputAdaptor.Create(Self);
    P.Adaptor.Request:=ARequest;
    If Request.QueryFields.values['format']='xml' then
      begin
      F:=TExtJSXMLDataFormatter.Create(Self);
      TExtJSXMLDataFormatter(F).TotalProperty:='total';
      AResponse.ContentType:='text/xml';
      end
    else
      F:=TExtJSJSONDataFormatter.Create(Self);
    {$ifdef wmdebug} SendDebug(className+' '+F.ClassName);{$endif}
    try
      DS:=TDatasource.Create(Self);
      try
        DBF1.TableName:=ExtractFilePath(ParamStr(0))+'users.dbf';
        DS.Dataset:=DBf1;
        DBF1.Open;
        try
          F.ADaptor:=P.Adaptor;
          P.Datasource:=DS;
          P.Adaptor.Action:=wdaRead;
          P.ApplyParams;
          F.Provider:=P;
          M:=TMemoryStream.Create;
          try
            F.GetContent(ARequest,M,Handled);
            M.Position:=0;
            Response.ContentStream:=M;
            Response.SendResponse;
            Response.ContentStream:=Nil;
            With TFileStream.Create('/tmp/data.xml',fmCreate) do
              try
                CopyFrom(M,0);
              finally
                Free;
              end;
          finally
            M.Free;
          end;
        finally
          DBF1.Close;
        end;
      finally
        DS.Free;
      end;
    finally
      F.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TFPWebModule1.TFPWebActions1Request(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
Var
  PN : String;
  P : TFPWebDataProvider;
  F : TExtJSDataFormatter;
  DS : TDatasource;
  M : TMemoryStream;
  L : Text;

begin
  // Providername;
  PN:=ARequest.GetNextPathInfo;
//  P:=GetWebDataProvider(PN);
  P:=TFPWebDataProvider.Create(Self);
  try
    P.IDFieldName:='ID';
    If Request.QueryFields.values['format']='xml' then
      begin
      F:=TExtJSXMLDataFormatter.Create(Self);
      AResponse.ContentType:='text/xml';
      P.Adaptor:=TWebDataInputAdaptor.Create(Self);
      end
    else
      begin
      F:=TExtJSJSONDataFormatter.Create(Self);
      P.Adaptor:=TExtJSJSonWebdataInputAdaptor.Create(Self);
      end;
    P.Adaptor.Request:=ARequest;
    {$ifdef wmdebug} SendDebug(className+' '+F.ClassName);{$endif}
    try
      DS:=TDatasource.Create(Self);
      try
        DBF1.TableName:=ExtractFilePath(ParamStr(0))+'users.dbf';
        DS.Dataset:=DBf1;
        DBF1.Open;
        try
          F.ADaptor:=P.Adaptor;
          P.Datasource:=DS;
          P.Adaptor.Action:=wdaInsert;
          P.ApplyParams;
          F.Provider:=P;
          M:=TMemoryStream.Create;
          try
            F.GetContent(ARequest,M,Handled);
            M.Position:=0;
            Response.ContentStream:=M;
            Response.SendResponse;
            Response.ContentStream:=Nil;
            With TFileStream.Create('/tmp/data.xml',fmCreate) do
              try
                CopyFrom(M,0);
              finally
                Free;
              end;
          finally
            M.Free;
          end;
        finally
          DBF1.Close;
        end;
      finally
        DS.Free;
      end;
    finally
      F.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TFPWebModule1.TFPWebActions2Request(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
Var
  PN : String;
  P : TFPWebDataProvider;
  F : TExtJSDataFormatter;
  DS : TDatasource;
  M : TMemoryStream;
  L : Text;

begin
  // Providername;
  {$ifdef wmdebug} SendDebug('Update request received');{$endif}
  PN:=ARequest.GetNextPathInfo;
//  P:=GetWebDataProvider(PN);
  P:=TFPWebDataProvider.Create(Self);
  try
    P.IDFieldName:='ID';
    If Request.QueryFields.values['format']='xml' then
      begin
      {$ifdef wmdebug} SendDebug('Update request received in XML');{$endif}
      F:=TExtJSXMLDataFormatter.Create(Self);
      AResponse.ContentType:='text/xml';
      P.Adaptor:=TWebDataInputAdaptor.Create(Self);
      end
    else
      begin
      F:=TExtJSJSONDataFormatter.Create(Self);
      P.Adaptor:=TExtJSJSonWebdataInputAdaptor.Create(Self);
      end;
    P.Adaptor.Request:=ARequest;
    {$ifdef wmdebug} SendDebug(className+' '+F.ClassName);{$endif}
    try
      DS:=TDatasource.Create(Self);
      try
        DBF1.TableName:=ExtractFilePath(ParamStr(0))+'users.dbf';
        DS.Dataset:=DBf1;
        DBF1.Open;
        try
          F.ADaptor:=P.Adaptor;
          P.Datasource:=DS;
          P.Adaptor.Action:=wdaUpdate;
          P.ApplyParams;
          F.Provider:=P;
          M:=TMemoryStream.Create;
          try
            F.GetContent(ARequest,M,Handled);
            M.Position:=0;
            Response.ContentStream:=M;
            Response.SendResponse;
            Response.ContentStream:=Nil;
            With TFileStream.Create('/tmp/data.xml',fmCreate) do
              try
                CopyFrom(M,0);
              finally
                Free;
              end;
          finally
            M.Free;
          end;
        finally
          DBF1.Close;
        end;
      finally
        DS.Free;
      end;
    finally
      F.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure TFPWebModule1.TFPWebActions3Request(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);

Var
  PN : String;
  P : TFPWebDataProvider;
  F : TExtJSDataFormatter;
  DS : TDatasource;
  M : TMemoryStream;
  L : Text;

begin
  // Providername;
  PN:=ARequest.GetNextPathInfo;
//  P:=GetWebDataProvider(PN);
  P:=TFPWebDataProvider.Create(Self);
  try
    P.IDFieldName:='ID';
    If Request.QueryFields.values['format']='xml' then
      begin
      F:=TExtJSXMLDataFormatter.Create(Self);
      AResponse.ContentType:='text/xml';
      P.Adaptor:=TWebDataInputAdaptor.Create(Self);
      end
    else
      begin
      F:=TExtJSJSONDataFormatter.Create(Self);
      P.Adaptor:=TExtJSJSonWebdataInputAdaptor.Create(Self);
      end;
    P.Adaptor.Request:=ARequest;
    {$ifdef wmdebug} SendDebug('className '+F.ClassName);{$endif}
    try
      DS:=TDatasource.Create(Self);
      try
        DBF1.TableName:=ExtractFilePath(ParamStr(0))+'users.dbf';
        DS.Dataset:=DBf1;
        DBF1.Open;
        try
          F.ADaptor:=P.Adaptor;
          P.Datasource:=DS;
          P.Adaptor.Action:=wdaDelete;
          P.ApplyParams;
          F.Provider:=P;
          M:=TMemoryStream.Create;
          try
            F.GetContent(ARequest,M,Handled);
            M.Position:=0;
            Response.ContentStream:=M;
            Response.SendResponse;
            Response.ContentStream:=Nil;
            With TFileStream.Create('/tmp/data.xml',fmCreate) do
              try
                CopyFrom(M,0);
              finally
                Free;
              end;
          finally
            M.Free;
          end;
        finally
          DBF1.Close;
        end;
      finally
        DS.Free;
      end;
    finally
      F.Free;
    end;
  finally
    P.Free;
  end;
end;

initialization
  {$I wmusers.lrs}

  RegisterHTTPModule('Provider', TFPWebModule1);
end.

