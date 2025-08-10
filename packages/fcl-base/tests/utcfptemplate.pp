unit utcFPTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, fpTemplate;

procedure RegisterTests;

implementation

type
  TTestCallbacks = class(TObject)
  public
    procedure TestAllowTagParamsBasics_replacetag(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
    procedure TestAllowTagParamsFunctionLike_replacetag(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
    procedure TestAllowTagParamsDelphiStyle_replacetag(Sender : TObject; Const TagString : String; TagParams:TStringList; Out ReplaceText : String);
  end;

var
  Callbacks: TTestCallbacks;

function SuiteSetup: TTestString;
begin
  Result := '';
  Callbacks := TTestCallbacks.Create;
end;

function SuiteTearDown: TTestString;
begin
  Result := '';
  Callbacks.Free;
end;

procedure TTestCallbacks.TestAllowTagParamsBasics_replacetag(
  Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  if TagString='test' then
    begin
    AssertEquals('Callback basics: Param count', 1, TagParams.Count);
    AssertEquals('Callback basics: Param name', 'param1', TagParams.Names[0]);
    AssertEquals('Callback basics: Param value', 'test ', TagParams.ValueFromIndex[0]);
    ReplaceText := 'template'
    end
  else if TagString='dream' then ReplaceText := 'think';
end;

procedure TTestCallbacks.TestAllowTagParamsFunctionLike_replacetag(
  Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  if TagString='uppercase' then
    begin
    AssertEquals('Callback function-like: Param count', 1, TagParams.Count);
    ReplaceText:=UpperCase(TagParams[0]);
    end;
end;

procedure TTestCallbacks.TestAllowTagParamsDelphiStyle_replacetag(
  Sender: TObject; const TagString: String; TagParams: TStringList; out
  ReplaceText: String);
begin
  AssertEquals('Callback delphi-style: Param count', 2, TagParams.Count);
  AssertEquals('Callback delphi-style: Param 1 name', 'param1', TagParams.Names[0]);
  AssertEquals('Callback delphi-style: Param 1 value', 'first param', TagParams.ValueFromIndex[0]);
  AssertEquals('Callback delphi-style: Param 2 name', 'param2', TagParams.Names[1]);
  AssertEquals('Callback delphi-style: Param 2 value', 'second param', TagParams.ValueFromIndex[1]);
  ReplaceText := 'Delphi parameter'
end;

Function TFPtemplate_TestBasics : TTestString;
var
  templ: TTemplateParser;
begin
  Result:='';
  templ := TTemplateParser.Create;
  try
    templ.Values['dream'] := 'think';
    templ.Values['test'] := 'template';
    AssertEquals('TestBasics simple replace', 'This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test} I could {dream} of.'));

    templ.recursive := true;
    templ.Values['val2'] := 'template';
    templ.Values['test'] := '{val2} test';
    AssertEquals('TestBasics recursive replace', 'This is the simplest template test I could think of.',
               templ.ParseString('This is the simplest {test} I could {dream} of.'));

  finally
    templ.free;
  end;
end;

Function TFPtemplate_TestBasicDelimiters : TTestString;
var
  templ: TTemplateParser;
begin
  Result:='';
  templ := TTemplateParser.Create;
  try
    templ.StartDelimiter:='[-';
    templ.EndDelimiter:=')';
    templ.Values['dream'] := 'think';
    templ.Values['test'] := 'template';
    AssertEquals('TestBasicDelimiters custom 1', 'This is [the] simplest template I could think (of).',
                 templ.ParseString('This is [the] simplest [-test) I could [-dream) (of).'));


    templ.StartDelimiter:='(';
    templ.EndDelimiter:='-)';
    templ.Values['dream'] := 'think';
    templ.Values['test'] := 'template';
    AssertEquals('TestBasicDelimiters custom 2', 'This is [the] simplest template I could think of:-).',
                 templ.ParseString('This is [the] simplest (test-) I could (dream-) of:-).'));


  finally
    templ.free;
  end;
end;

Function TFPtemplate_TestAllowTagParamsBasics : TTestString;
var
  templ: TTemplateParser;
begin
  Result:='';
  templ := TTemplateParser.Create;
  try
    templ.AllowTagParams := true;
    templ.OnReplaceTag := @Callbacks.TestAllowTagParamsBasics_replacetag;
    AssertEquals('TestAllowTagParamsBasics 1', 'This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test [- param1=test -]} I could {dream} of.'));

    AssertEquals('TestAllowTagParamsBasics 2', 'This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test[- param1=test -]} I could {dream} of.'));

    templ.ParamValueSeparator:=':';
    AssertEquals('TestAllowTagParamsBasics 3', 'This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test [- param1:test -]} I could {dream} of.'));

    AssertEquals('TestAllowTagParamsBasics 4', 'This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test [-param1:test -]} I could {dream} of.'));

    AssertEquals('TestAllowTagParamsBasics 5', 'This is the simplest template I could think of.',
                 templ.ParseString('This is the simplest {test  [-param1:test -]} I could {dream} of.'));

  finally
    templ.free;
  end;
end;

Function TFPtemplate_TestAllowTagParamsFunctionLike : TTestString;
var
  templ: TTemplateParser;
begin
  Result:='';
  templ := TTemplateParser.Create;
  try
    templ.AllowTagParams := true;
    templ.ParamStartDelimiter:='(';
    templ.ParamEndDelimiter:=')';
    templ.OnReplaceTag := @Callbacks.TestAllowTagParamsFunctionLike_replacetag;

    AssertEquals('TestAllowTagParamsFunctionLike', 'THIS should be uppercased.',
                 templ.ParseString('{uppercase(This)} should be uppercased.'));
  finally
    templ.free;
  end;
end;

Function TFPtemplate_TestAllowTagParamsDelphiStyle : TTestString;
var
  templ: TTemplateParser;
begin
  Result:='';
  templ := TTemplateParser.Create;
  try
    templ.AllowTagParams := true;
    templ.StartDelimiter:='<#';
    templ.EndDelimiter:='>';
    templ.ParamStartDelimiter:=' ';
    templ.ParamEndDelimiter:='"';
    templ.ParamValueSeparator:='="';
    templ.OnReplaceTag := @Callbacks.TestAllowTagParamsDelphiStyle_replacetag;

    AssertEquals('TestAllowTagParamsDelphiStyle', 'Test for a Delphi parameter.',
                 templ.ParseString('Test for a <#DelphiTag param1="first param" param2="second param">.'));
  finally
    templ.free;
  end;
end;

procedure RegisterTests;
begin
  AddSuite('TFPtemplateTests', @SuiteSetup, @SuiteTearDown);
  AddTest('TestBasics', @TFPtemplate_TestBasics, 'TFPtemplateTests');
  AddTest('TestBasicDelimiters', @TFPtemplate_TestBasicDelimiters, 'TFPtemplateTests');
  AddTest('TestAllowTagParamsBasics', @TFPtemplate_TestAllowTagParamsBasics, 'TFPtemplateTests');
  AddTest('TestAllowTagParamsFunctionLike', @TFPtemplate_TestAllowTagParamsFunctionLike, 'TFPtemplateTests');
  AddTest('TestAllowTagParamsDelphiStyle', @TFPtemplate_TestAllowTagParamsDelphiStyle, 'TFPtemplateTests');
end;

end.
