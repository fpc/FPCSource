unit utcyamlparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpyaml.types, fpyaml.parser, fpyaml.data, utyamldata;

type

  { TCYamlParser }

  TCYamlParser= class(TTestYAMLData)
  private
    FParser: TYAMLParser;
    FYAMLDocument: TYAMLDocument;
    FYAMLStream: TYAMLStream;
    function GetDocument: TYAMLDocument;
    function GetStream: TYAMLStream;
    function GetValue: TYAMLData;
  protected
    procedure Parse(aContent : Array of string);
    procedure SetUp; override;
    procedure TearDown; override;
    property Parser : TYAMLParser Read FParser;
    property YAML : TYAMLStream Read GetStream;
    property Document: TYAMLDocument Read GetDocument;
    property Value: TYAMLData Read GetValue;
  published
    procedure TestHookUp;
  end;

implementation

procedure TCYamlParser.TestHookUp;
begin
//  Ignore('Write your own test');
end;

function TCYamlParser.GetDocument: TYAMLDocument;
begin
  Stream.Count
end;

function TCYamlParser.GetStream: TYAMLStream;
begin

end;

function TCYamlParser.GetValue: TYAMLData;
begin

end;

procedure TCYamlParser.Parse(aContent: array of string);
begin
  FParser:=TYAMLParser.Create(aContent);
  SetData(FParser.
end;

procedure TCYamlParser.SetUp;
begin
end;

procedure TCYamlParser.TearDown;
begin

end;

initialization

  RegisterTest(TCYamlParser);
end.

