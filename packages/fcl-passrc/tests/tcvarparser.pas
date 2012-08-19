unit tcvarparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, pscanner,
  tcbaseparser, testregistry;

Type
  { TTestVarParser }

  TTestVarParser = Class(TTestParser)
  private
    FHint: string;
    FVar: TPasVariable;
  Protected
    Function ParseVar(ASource : String; Const AHint : String = '') : TPasVariable; virtual; overload;
    Procedure AssertVariableType(Const ATypeName : String);
    Procedure AssertVariableType(Const AClass : TClass);
    Procedure AssertParseVarError(ASource : String);
    Property TheVar : TPasVariable Read FVar;
    Property Hint : string Read FHint Write FHint;
    procedure SetUp; override;
    Procedure TearDown; override;
  Published
    Procedure TestSimpleVar;
    Procedure TestSimpleVarDeprecated;
    Procedure TestSimpleVarPlatform;
    Procedure TestSimpleVarInitialized;
    procedure TestSimpleVarInitializedDeprecated;
    procedure TestSimpleVarInitializedPlatform;
    Procedure TestVarProcedure;
    Procedure TestVarProcedureDeprecated;
    Procedure TestVarRecord;
    Procedure TestVarRecordDeprecated;
    Procedure TestVarRecordPlatform;
    Procedure TestVarArray;
    Procedure TestVarArrayDeprecated;
    Procedure TestVarDynArray;
    Procedure TestVarExternal;
    Procedure TestVarExternalLib;
    Procedure TestVarExternalLibName;
    Procedure TestVarCVar;
    Procedure TestVarCVarExternal;
    Procedure TestVarPublic;
    Procedure TestVarPublicName;
    Procedure TestVarDeprecatedExternalName;
  end;

implementation

uses typinfo;

{ TTestVarParser }

function TTestVarParser.ParseVar(ASource: String; const AHint: String
  ): TPasVariable;
Var
  D : String;
begin
  Hint:=AHint;
  Add('Var');
  D:='A : '+ASource;
  If Hint<>'' then
    D:=D+' '+Hint;
  Add('  '+D+';');
//  Writeln(source.text);
  ParseDeclarations;
  AssertEquals('One variable definition',1,Declarations.Variables.Count);
  AssertEquals('First declaration is type definition.',TPasVariable,TObject(Declarations.Variables[0]).ClassType);
  Result:=TPasVariable(Declarations.Variables[0]);
  AssertEquals('First declaration has correct name.','A',Result.Name);
  FVar:=Result;
  Definition:=Result;
  if (Hint<>'') then
    CheckHint(TPasMemberHint(Getenumvalue(typeinfo(TPasMemberHint),'h'+Hint)));
end;

procedure TTestVarParser.AssertVariableType(const ATypeName: String);
begin
  AssertVariableType(TPasUnresolvedTypeRef);
  AssertEquals('Correct unresolved type name',ATypeName,theVar.VarType.Name);
end;

procedure TTestVarParser.AssertVariableType(const AClass: TClass);
begin
  AssertNotNull('Have variable type',theVar.VarType);
  AssertEquals('Correct type class',AClass,theVar.VarType.ClassType);
end;

procedure TTestVarParser.AssertParseVarError(ASource: String);
begin
  try
    ParseVar(ASource,'');
    Fail('Expected parser error');
  except
    // all OK.
  end;
end;

procedure TTestVarParser.SetUp;
begin
  inherited SetUp;
  FHint:='';
  FVar:=Nil;
end;

procedure TTestVarParser.TearDown;
begin
  FVar:=Nil;
  inherited TearDown;
end;

procedure TTestVarParser.TestSimpleVar;
begin
  ParseVar('b','');
  AssertVariableType('b');
end;

procedure TTestVarParser.TestSimpleVarDeprecated;
begin
  ParseVar('b','deprecated');
  AssertVariableType('b');
end;

procedure TTestVarParser.TestSimpleVarPlatform;
begin
  ParseVar('b','platform');
  AssertVariableType('b');
end;

procedure TTestVarParser.TestSimpleVarInitialized;
begin
  ParseVar('b = 123','');
  AssertVariableType('b');
  AssertNotNull(TheVar.expr);
  AssertExpression('Variable value',TheVar.expr,pekNumber,'123');
end;

procedure TTestVarParser.TestSimpleVarInitializedDeprecated;
begin
  ParseVar('b = 123','deprecated');
  AssertVariableType('b');
  AssertNotNull(TheVar.expr);
  AssertExpression('Variable value',TheVar.expr,pekNumber,'123');
end;

procedure TTestVarParser.TestSimpleVarInitializedPlatform;
begin
  ParseVar('b = 123','platform');
  AssertVariableType('b');
  AssertNotNull(TheVar.expr);
  AssertExpression('Variable value',TheVar.expr,pekNumber,'123');
end;

procedure TTestVarParser.TestVarProcedure;
begin
  ParseVar('procedure','');
  AssertVariableType(TPasProcedureType);
end;

procedure TTestVarParser.TestVarProcedureDeprecated;
begin
  ParseVar('procedure','deprecated');
  AssertVariableType(TPasProcedureType);
end;

procedure TTestVarParser.TestVarRecord;

Var
  R : TPasRecordtype;
begin
  ParseVar('record x,y : intger; end','');
  AssertVariableType(TPasRecordType);
  R:=TheVar.VarType as TPasRecordType;
  AssertEquals('Correct number of fields',2,R.Members.Count);
end;

procedure TTestVarParser.TestVarRecordDeprecated;
Var
  R : TPasRecordtype;
begin
  ParseVar('record x,y : integer; end','deprecated');
  AssertVariableType(TPasRecordType);
  R:=TheVar.VarType as TPasRecordType;
  AssertEquals('Correct number of fields',2,R.Members.Count);
end;

procedure TTestVarParser.TestVarRecordPlatform;
Var
  R : TPasRecordtype;
begin
  ParseVar('record x,y : integer; end','platform');
  AssertVariableType(TPasRecordType);
  R:=TheVar.VarType as TPasRecordType;
  AssertEquals('Correct number of fields',2,R.Members.Count);
end;

procedure TTestVarParser.TestVarArray;

Var
  R : TPasArrayType;

begin
  ParseVar('Array[1..20] of integer','');
  AssertVariableType(TPasArrayType);
  R:=TheVar.VarType as TPasArrayType;
  AssertNotNull('Correct array type name',R.ElType);
  AssertEquals('Correct array type name',TPasunresolvedTypeRef,R.ElType.ClassType);
end;

procedure TTestVarParser.TestVarArrayDeprecated;

Var
  R : TPasArrayType;

begin
  ParseVar('Array[1..20] of integer','Deprecated');
  AssertVariableType(TPasArrayType);
  R:=TheVar.VarType as TPasArrayType;
  AssertNotNull('Correct array type name',R.ElType);
  AssertEquals('Correct array type name',TPasunresolvedTypeRef,R.ElType.ClassType);
end;

procedure TTestVarParser.TestVarDynArray;

Var
  R : TPasArrayType;

begin
  ParseVar('Array of integer','');
  AssertVariableType(TPasArrayType);
  R:=TheVar.VarType as TPasArrayType;
  AssertEquals('No index','',R.IndexRange);
  AssertNotNull('Correct array type name',R.ElType);
  AssertEquals('Correct array type name',TPasunresolvedTypeRef,R.ElType.ClassType);
end;

procedure TTestVarParser.TestVarExternal;
begin
  ParseVar('integer; external','');
  AssertEquals('Variable modifiers',[vmexternal],TheVar.VarModifiers);
end;

procedure TTestVarParser.TestVarExternalLib;
begin
  ParseVar('integer; external name ''mylib''','');
  AssertEquals('Variable modifiers',[vmexternal],TheVar.VarModifiers);
  AssertEquals('Library name','',TheVar.LibraryName);
  AssertEquals('Library name','''mylib''',TheVar.ExportName);
end;

procedure TTestVarParser.TestVarExternalLibName;
begin
  ParseVar('integer; external ''mylib'' name ''d''','');
  AssertEquals('Variable modifiers',[vmexternal],TheVar.VarModifiers);
  AssertEquals('Library name','''mylib''',TheVar.LibraryName);
  AssertEquals('Library name','''d''',TheVar.ExportName);
end;

procedure TTestVarParser.TestVarCVar;
begin
  ParseVar('integer; cvar','');
  AssertEquals('Variable modifiers',[vmcvar],TheVar.VarModifiers);
end;

procedure TTestVarParser.TestVarCVarExternal;
begin
  ParseVar('integer; cvar;external','');
  AssertEquals('Variable modifiers',[vmcvar,vmexternal],TheVar.VarModifiers);
end;

procedure TTestVarParser.TestVarPublic;
begin
  ParseVar('integer; public','');
  AssertEquals('Variable modifiers',[vmpublic],TheVar.VarModifiers);
end;

procedure TTestVarParser.TestVarPublicName;
begin
  ParseVar('integer; public name ''c''','');
  AssertEquals('Variable modifiers',[vmpublic],TheVar.VarModifiers);
  AssertEquals('Public export name','''c''',TheVar.ExportName);
end;

procedure TTestVarParser.TestVarDeprecatedExternalName;
begin
  ParseVar('integer deprecated; external name ''me''','');
  CheckHint(TPasMemberHint(Getenumvalue(typeinfo(TPasMemberHint),'hdeprecated')));
  AssertEquals('Variable modifiers',[vmexternal],TheVar.VarModifiers);
  AssertEquals('Library name','''me''',TheVar.ExportName);
end;

initialization

  RegisterTests([TTestVarParser]);
end.

