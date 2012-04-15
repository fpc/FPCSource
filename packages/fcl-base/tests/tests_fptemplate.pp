unit tests_fptemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestTemplateParser }

  TTestTemplateParser= class(TTestCase)
  private
    Procedure TestAllowTagParamsBasics_replacetag(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
    Procedure TestAllowTagParamsFunctionLike_replacetag(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
    Procedure TestAllowTagParamsDelphiStyle_replacetag(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
  published
    procedure TestBasics;
    procedure TestBasicDelimiters;
    procedure TestAllowTagParamsBasics;
    procedure TestAllowTagParamsFunctionLike;
    procedure TestAllowTagParamsDelphiStyle;
  end;

implementation

uses
  fpTemplate;

procedure TTestTemplateParser.TestBasics;
var
  templ: TTemplateParser;
begin
  templ := TTemplateParser.Create;
  try
    templ.Values['dream'] := 'think';
    templ.Values['test'] := 'template';
    CheckEquals('This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test} I could {dream} of.'));

    templ.recursive := true;
    templ.Values['val2'] := 'template';
    templ.Values['test'] := '{val2} test';
    CheckEquals('This is the simplest template test I could think of.',
               templ.ParseString('This is the simplest {test} I could {dream} of.'));

  finally
    templ.free;
  end;
end;

procedure TTestTemplateParser.TestBasicDelimiters;
var
  templ: TTemplateParser;
begin
  templ := TTemplateParser.Create;
  try
    templ.StartDelimiter:='[-';
    templ.EndDelimiter:=')';
    templ.Values['dream'] := 'think';
    templ.Values['test'] := 'template';
    CheckEquals('This is [the] simplest template I could think (of).',
                 templ.ParseString('This is [the] simplest [-test) I could [-dream) (of).'));


    templ.StartDelimiter:='(';
    templ.EndDelimiter:='-)';
    templ.Values['dream'] := 'think';
    templ.Values['test'] := 'template';
    CheckEquals('This is [the] simplest template I could think of:-).',
                 templ.ParseString('This is [the] simplest (test-) I could (dream-) of:-).'));


  finally
    templ.free;
  end;
end;

procedure TTestTemplateParser.TestAllowTagParamsBasics;
var
  templ: TTemplateParser;
begin
  templ := TTemplateParser.Create;
  try
    templ.AllowTagParams := true;
    templ.OnReplaceTag := @TestAllowTagParamsBasics_replacetag;
    CheckEquals('This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test [- param1=test -]} I could {dream} of.'));

    CheckEquals('This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test[- param1=test -]} I could {dream} of.'));

    templ.ParamValueSeparator:=':';
    CheckEquals('This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test [- param1:test -]} I could {dream} of.'));

    CheckEquals('This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test [-param1:test -]} I could {dream} of.'));

    CheckEquals('This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test  [-param1:test -]} I could {dream} of.'));

  finally
    templ.free;
  end;
end;

procedure TTestTemplateParser.TestAllowTagParamsFunctionLike;
var
  templ: TTemplateParser;
begin
  templ := TTemplateParser.Create;
  try
    templ.AllowTagParams := true;
    templ.ParamStartDelimiter:='(';
    templ.ParamEndDelimiter:=')';
    templ.OnReplaceTag := @TestAllowTagParamsFunctionLike_replacetag;

    CheckEquals('THIS should be uppercased.',
                 templ.ParseString('{uppercase(This)} should be uppercased.'));
  finally
    templ.free;
  end;
end;

procedure TTestTemplateParser.TestAllowTagParamsDelphiStyle;
var
  templ: TTemplateParser;
begin
  templ := TTemplateParser.Create;
  try
    templ.AllowTagParams := true;
    templ.StartDelimiter:='<#';
    templ.EndDelimiter:='>';
    templ.ParamStartDelimiter:=' ';
    templ.ParamEndDelimiter:='"';
    templ.ParamValueSeparator:='="';
    templ.OnReplaceTag := @TestAllowTagParamsDelphiStyle_replacetag;

    CheckEquals('Test for a Delphi parameter.',
                 templ.ParseString('Test for a <#DelphiTag param1="first param" param2="second param">.'));
  finally
    templ.free;
  end;
end;

procedure TTestTemplateParser.TestAllowTagParamsBasics_replacetag(
  Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  if TagString='test' then
    begin
    CheckEquals(1,TagParams.Count);
    CheckEquals('param1',TagParams.Names[0]);
    CheckEquals('test ',TagParams.ValueFromIndex[0]);
    ReplaceText := 'template'

    end
  else if TagString='dream' then ReplaceText := 'think';
end;

procedure TTestTemplateParser.TestAllowTagParamsFunctionLike_replacetag(
  Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  if TagString='uppercase' then
    begin
    CheckEquals(1,TagParams.Count);
    ReplaceText:=UpperCase(TagParams[0]);
    end;
end;

procedure TTestTemplateParser.TestAllowTagParamsDelphiStyle_replacetag(
  Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  CheckEquals(2,TagParams.Count);
  CheckEquals('param1',TagParams.Names[0]);
  CheckEquals('first param',TagParams.ValueFromIndex[0]);
  CheckEquals('param2',TagParams.Names[1]);
  CheckEquals('second param',TagParams.ValueFromIndex[1]);
  ReplaceText := 'Delphi parameter'

end;

initialization

  RegisterTest(TTestTemplateParser);
end.

