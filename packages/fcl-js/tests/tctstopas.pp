unit tctstopas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tstopas;

Type

  { TTestTSToPas }
  TMyTypescriptToPas = class(TTypescriptToPas)
  end;

  TTestTSToPas = Class(TTestCase)
  private
    FConverter: TTypescriptToPas;
  Public
    Procedure Setup; override;
    procedure TearDown; override;
    procedure Convert(aSource : string); overload;
    procedure Convert(aSource : TStrings); overload;
    procedure CheckDeclaration(const aSection, aDeclaration : String);
    procedure CheckDeclaration(const aSection, aDeclaration, aDeclaration2 : String);
    procedure CheckDeclarations(const aSection : String; Const Declarations : Array of string);
    Property Converter : TTypescriptToPas Read FConverter;
  Published
    Procedure TestEmpty;
    Procedure TestVarDeclaration;
    Procedure Test2VarDeclarations;
    Procedure Test3VarDeclarations;
    Procedure TestKeywordVarDeclaration;
    Procedure TestSimpleType;
  end;

implementation

{ TTestTSToPas }

procedure TTestTSToPas.Setup;

begin
  inherited Setup;
  FConverter:=TMyTypescriptToPas.Create(Nil);
  FConverter.Options:=FConverter.Options+[coRaw];
end;

procedure TTestTSToPas.TearDown;

begin
  FreeAndNil(FConverter);
  inherited TearDown;
end;

procedure TTestTSToPas.Convert(aSource: string);

Var
  aSrc : TStrings;

begin
  aSrc:=TStringList.Create;
  try
    TStringList(aSrc).SkipLastLineBreak:=True;
    aSrc.Text:=aSource;
    Writeln('--');
    Writeln(aSrc.Text);
    Writeln('--');
    Convert(aSrc);
  finally
    aSrc.Free;
  end;

end;

procedure TTestTSToPas.Convert(aSource: TStrings);
Var
  S : TStream;
begin
  S:=TStringStream.Create(aSource.text);
  try
    FConverter.InputStream:=S;
    FConverter.Execute;
  finally
    S.Free;
  end;
end;

procedure TTestTSToPas.CheckDeclaration(const aSection, aDeclaration: String);

begin
  CheckDeclarations(aSection,[aDeclaration]);
end;

procedure TTestTSToPas.CheckDeclaration(const aSection, aDeclaration, aDeclaration2: String);

begin
  CheckDeclarations(aSection,[aDeclaration,aDeclaration2]);
end;

procedure TTestTSToPas.CheckDeclarations(const aSection: String; const Declarations: array of string);
Var
  Src : TStrings;
  I,J : Integer;
  D,S,actSrc : String;

begin
  Src:=FConverter.Source;
  Writeln(Src.Text);
  I:=0;
  While (I<Src.Count) and (Trim(Src[i])='') do
    Inc(I);
  AssertTrue('Section: Not at end',I<Src.Count);
  AssertEquals('Section correct',LowerCase(aSection),LowerCase(Trim(Src[i])));
  For J:=0 to Length(Declarations)-1 do
    begin
    D:=Format('Declaration %d: ',[J]);
    S:=Declarations[J];
    Inc(I);
    While (I<Src.Count) and (Trim(Src[i])='') do
      Inc(I);
    AssertTrue(D+'Not at end',I<Src.Count);
    actSrc:=Src[i];
    AssertEquals(D+'Declaration correct',LowerCase(S),LowerCase(Trim(actSrc)));
    end;
end;

procedure TTestTSToPas.TestEmpty;
begin
  AssertNotNull(Converter);
end;

procedure TTestTSToPas.TestVarDeclaration;
begin
  Convert('declare var x : number;');
  CheckDeclaration('var','x : double;');
end;

procedure TTestTSToPas.Test2VarDeclarations;
begin
  Convert('declare var x,y : number;');
  CheckDeclaration('var','x : double;','y : double;');
end;

procedure TTestTSToPas.Test3VarDeclarations;
begin
  Convert('declare var x,y,z : number;');
  CheckDeclarations('var',['x : double;','y : double;','z : double;']);
end;

procedure TTestTSToPas.TestKeywordVarDeclaration;
begin
  Convert('declare var on : string;');
  CheckDeclarations('var',['&on : string; external name ''on'';']);
end;

procedure TTestTSToPas.TestSimpleType;
begin
  Convert('declare type MyType = string;');
  CheckDeclarations('type',['TMyType = string;']);
end;

Initialization
  Registertest(TTestTSToPas);
end.

