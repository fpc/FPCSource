unit webmodule; 

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpTemplate;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure func1callRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    procedure func1callReplaceTag(Sender: TObject; const TagString:String; 
      TagParams: TStringList; Out ReplaceText: String);
    function UseTemplate(TemplateFile:String; TagHandler:TReplaceTagEvent):String;
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.func1callRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  //func1callReplaceTag is recursively processing the template tags for all
  //embedded templates (INCLUDETEMPLATE tag defines a new template to embed)
  AResponse.Content := UseTemplate('maintemplate', @func1callReplaceTag);
  Handled := true;
end;

function TFPWebModule1.UseTemplate(TemplateFile:String; TagHandler:TReplaceTagEvent):String;
var TMPTemplate:TFPTemplate;
begin
  TMPTemplate := TFPTemplate.Create;
  TMPTemplate.FileName := '/path/to/templates/' + TemplateFile + '.html';
  TMPTemplate.AllowTagParams := true;
  TMPTemplate.StartDelimiter := '{+';
  TMPTemplate.EndDelimiter := '+}';
  TMPTemplate.OnReplaceTag := TagHandler;
  Result  := TMPTemplate.GetContent;
  TMPTemplate.Free;
end;

procedure TFPWebModule1.func1callReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin//HTML template tag handling for an html template file
  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else

  if AnsiCompareText(TagString, 'INCLUDETEMPLATE') = 0 then
  begin
    ReplaceText := UseTemplate(TagParams.Values['TEMPLATEFILE'], @func1callReplaceTag);
  end else begin

//Not found value for tag -> TagString
    ReplaceText := 'Template tag {+' + TagString + '+} is not implemented yet.';
  end;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
