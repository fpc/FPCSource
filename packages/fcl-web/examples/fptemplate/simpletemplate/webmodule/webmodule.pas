unit webmodule; 

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure func1callRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    procedure func1callReplaceTag(Sender: TObject; const TagString:String; 
      TagParams: TStringList; Out ReplaceText: String);
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
  //ModuleTemplate is a web module global property
  //To use the Template propery of the current web action (which is visible in
  //the object inspector for every Action), use
  //(Sender as TFPWebAction).Template.FileName := 'mytemplate1.html'; and so on.
  ModuleTemplate.FileName := 'mytemplate1.html';//best to use full path here
  ModuleTemplate.AllowTagParams := true;
  ModuleTemplate.OnReplaceTag := @func1callReplaceTag;

  AResponse.Content := ModuleTemplate.GetContent;

  Handled := true;
end;

procedure TFPWebModule1.func1callReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin
  if AnsiCompareText(TagString, 'TagName1') = 0 then
  begin
    ReplaceText := 'Here I am from the web module!';
  end else begin

//Not found value for tag -> TagString
    ReplaceText := 'Template tag {' + TagString + '} is not implemented yet.';
  end;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.
