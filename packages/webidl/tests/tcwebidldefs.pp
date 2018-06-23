unit tcwebidldefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, webidldefs;

Type

  { TTestParser }

  { TTestDefinition }

  TTestDefinition = Class(TTestCase)
  private
    FDef: TIDLDefinition;
    function CreateUnionTypeDef(Types : Array of UTF8String; withAttrs: Boolean=False): TIDLUnionTypeDefDefinition;
    function CreateArgument(isOptional : Boolean; DefaultValue : string = ''; withAttrs: Boolean=False): TIDLArgumentDefinition;
    function CreateFunction(Options: TFunctionOptions; Args: Array of UTF8String; withAttrs: Boolean=False): TIDLFunctionDefinition;
    function CreateAttribute(Options: TAttributeOptions; withAttrs: Boolean=False): TIDLAttributeDefinition;
    function CreateConst(withAttrs: Boolean=False): TIDLConstDefinition;
    function CreateImplements(Const ATypeName : String; withAttrs: Boolean=False): TIDLImplementsDefinition;
    function CreateIncludes(Const ATypeName : String; withAttrs: Boolean=False): TIDLIncludesDefinition;
    function CreateTypeDef(Const ATypeName : String; withAttrs: Boolean=False): TIDLTypeDefDefinition;
    function CreateInterface(Const AParentName : String; aMembers : Array of TIDLDefinition; withAttrs: Boolean=False): TIDLInterfaceDefinition;
    function CreateDictionaryMember(Const AName,aTypeName,aDefault : String; aRequired : Boolean; withAttrs: Boolean=False): TIDLDictionaryMemberDefinition;
    function CreateDictionary(Const AParentName : String; aMembers : Array of TIDLDictionaryMemberDefinition; withAttrs: Boolean=False): TIDLDictionaryDefinition;
    function CreateSequence(Const AElementName : String; withAttrs: Boolean=False): TIDLSequenceTypeDefDefinition;
    function CreatePromise(Const AReturnTypeName: String; withAttrs: Boolean=False): TIDLPromiseTypeDefDefinition;
    function CreateSetLike(Const AElementName: String; withAttrs: Boolean=False): TIDLSetLikeDefinition;
    function CreateMapLike(Const AKeyTypeName,AValueTypeName: String; withAttrs: Boolean=False): TIDLMapLikeDefinition;
    function CreateRecord(Const AKeyTypeName,AValueTypeName: String; withAttrs: Boolean=False): TIDLRecordDefinition;
  Public
    Procedure TearDown; override;
    function CreateDef(aClass: TIDLDefinitionClass; WithAttrs: Boolean=False): TIDLDefinition;
    Procedure TestDef(Const aDef : String; AFull : Boolean);
    Property Def : TIDLDefinition Read FDef Write FDef;
  published
    Procedure TestConst;
    procedure TestAttribute;
    procedure TestStringifierAttribute;
    procedure TestStringifierFunction;
    procedure TestFunction;
    procedure TestCallBackFunction;
    procedure TestArgument;
    procedure TestImplements;
    procedure TestIncludes;
    procedure TestTypeDef;
    procedure TestUnionTypeDef;
    procedure TestInterface;
    procedure TestDictionaryMember;
    procedure TestDictionary;
    procedure TestCallbackInterface;
    procedure TestSequence;
    procedure TestPromise;
    procedure TestMapLike;
    procedure TestSetLike;
    procedure TestRecord;
  end;

implementation

{ TTestDefinition }

procedure TTestDefinition.TearDown;
begin
  FreeAndNil(FDef);
  inherited TearDown;
end;

function TTestDefinition.CreateDef(aClass: TIDLDefinitionClass; WithAttrs : Boolean = False): TIDLDefinition;
begin
  FreeAndNil(FDef);
  FDef:=aClass.Create(Nil,'A');
  if WithAttrs then
    FDef.Attributes.Add('Me');
  Result:=FDef;
end;

procedure TTestDefinition.TestDef(const aDef: String; AFull: Boolean);

begin
  AssertEquals('Def '+FDef.ClassName +'.AsString('+BoolToStr(aFull,'True','False')+')',ADef,FDef.AsString(afull));
end;

function TTestDefinition.CreateConst(withAttrs: Boolean): TIDLConstDefinition;

begin
  Result:=CreateDef(TIDLConstDefinition,WithAttrs) as TIDLConstDefinition;
  Result.TypeName:='short';
  Result.Value:='0x8080';
end;

function TTestDefinition.CreateImplements(const ATypeName: String;
  withAttrs: Boolean): TIDLImplementsDefinition;
begin
  Result:=CreateDef(TIDLImplementsDefinition,WithAttrs) as TIDLImplementsDefinition;
  Result.ImplementedInterface:=ATypeName;
end;

function TTestDefinition.CreateIncludes(const ATypeName: String;
  withAttrs: Boolean): TIDLIncludesDefinition;
begin
  Result:=CreateDef(TIDLIncludesDefinition,WithAttrs) as TIDLIncludesDefinition;
  Result.IncludedInterface:=ATypeName;
end;

function TTestDefinition.CreateTypeDef(const ATypeName: String;
  withAttrs: Boolean): TIDLTypeDefDefinition;
begin
  Result:=CreateDef(TIDLTypeDefDefinition,WithAttrs) as TIDLTypeDefDefinition;
  Result.TypeName:=ATypeName;
end;

function TTestDefinition.CreateInterface(const AParentName: String;
  aMembers: array of TIDLDefinition; withAttrs: Boolean
  ): TIDLInterfaceDefinition;

Var
  M : TIDLDefinition;

begin
  Result:=CreateDef(TIDLInterfaceDefinition,WithAttrs) as TIDLInterfaceDefinition;
  Result.ParentName:=AParentName;
  For M in aMembers do
    Result.members.Add(M);
end;

function TTestDefinition.CreateDictionaryMember(const AName, aTypeName,
  aDefault: String; aRequired: Boolean; withAttrs: Boolean
  ): TIDLDictionaryMemberDefinition;
begin
  Result:=CreateDef(TIDLDictionaryMemberDefinition,WithAttrs) as TIDLDictionaryMemberDefinition;
  Result.Name:=aName;
  Result.MemberType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.MemberType.TypeName:=aTypeName;
  Result.IsRequired:=aRequired;
  if (aDefault<>'') then
    begin
    Result.DefaultValue:=TIDLConstDefinition.Create(Result,'');
    Result.DefaultValue.Value:=aDefault;
    end;
end;

function TTestDefinition.CreateDictionary(const AParentName: String;
  aMembers: array of TIDLDictionaryMemberDefinition; withAttrs: Boolean
  ): TIDLDictionaryDefinition;

Var
  M : TIDLDictionaryMemberDefinition;

begin
  Result:=CreateDef(TIDLDictionaryDefinition,WithAttrs) as TIDLDictionaryDefinition;
  Result.ParentName:=aParentName;
  for M in aMembers do
    Result.Members.Add(M);
end;

function TTestDefinition.CreateSequence(const AElementName: String;
  withAttrs: Boolean): TIDLSequenceTypeDefDefinition;

begin
  Result:=CreateDef(TIDLSequenceTypeDefDefinition,WithAttrs) as TIDLSequenceTypeDefDefinition;
  Result.ElementType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.ElementType.TypeName:=AElementName;
end;

function TTestDefinition.CreatePromise(const AReturnTypeName: String;
  withAttrs: Boolean): TIDLPromiseTypeDefDefinition;
begin
  Result:=CreateDef(TIDLPromiseTypeDefDefinition,withAttrs) as TIDLPromiseTypeDefDefinition;
  Result.ReturnType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.ReturnType.TypeName:=AReturnTypeName;
end;

function TTestDefinition.CreateSetLike(const AElementName: String;
  withAttrs: Boolean): TIDLSetLikeDefinition;
begin
  Result:=CreateDef(TIDLSetLikeDefinition,WithAttrs) as TIDLSetLikeDefinition;
  Result.ElementType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.ElementType.TypeName:=AElementName;

end;

function TTestDefinition.CreateMapLike(const AKeyTypeName,
  AValueTypeName: String; withAttrs: Boolean): TIDLMapLikeDefinition;
begin
  Result:=CreateDef(TIDLMapLikeDefinition,WithAttrs) as TIDLMapLikeDefinition;
  Result.KeyType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.KeyType.TypeName:=AKeyTypeName;
  Result.ValueType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.ValueType.TypeName:=AValueTypeName;
end;

function TTestDefinition.CreateRecord(const AKeyTypeName,
  AValueTypeName: String; withAttrs: Boolean): TIDLRecordDefinition;
begin
  Result:=CreateDef(TIDLRecordDefinition,WithAttrs) as TIDLRecordDefinition;
  Result.KeyType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.KeyType.TypeName:=AKeyTypeName;
  Result.ValueType:=TIDLTypeDefDefinition.Create(Result,'');
  Result.ValueType.TypeName:=AValueTypeName;
end;

procedure TTestDefinition.TestConst;

begin
  CreateConst(False);
  TestDef('const short A 0x8080',true);
  TestDef('short A 0x8080',False);
  CreateConst(True);
  TestDef('[Me] const short A 0x8080',true);
  TestDef('short A 0x8080',False);
end;

function TTestDefinition.CreateUnionTypeDef(Types: array of UTF8String;
  withAttrs: Boolean): TIDLUnionTypeDefDefinition;

Var
  S : UTF8String;
  T : TIDLTypeDefDefinition;

begin
  Result:=CreateDef(TIDLUnionTypeDefDefinition,WithAttrs) as TIDLUnionTypeDefDefinition;
  for S in Types do
    begin
    T:=TIDLTypeDefDefinition.Create(Result,'');
    T.TypeName:=S;
    Result.Union.Add(T);
    end;
end;

function TTestDefinition.CreateArgument(isOptional: Boolean; DefaultValue: string; withAttrs: Boolean): TIDLArgumentDefinition;
begin
  Result:=CreateDef(TIDLArgumentDefinition,WithAttrs) as TIDLArgumentDefinition;
  Result.ArgumentType:=TIDLTypeDefDefinition.Create(Result,'AN');
  Result.ArgumentType.TypeName:='short';
  Result.HasDefaultValue:=(DefaultValue<>'');
  Result.DefaultValue:=DefaultValue;
  Result.IsOptional:=IsOptional;
end;

function TTestDefinition.CreateFunction(Options: TFunctionOptions; Args: array of UTF8String; withAttrs: Boolean): TIDLFunctionDefinition;

Var
  I : integer;

begin
  Result:=CreateDef(TIDLFunctionDefinition,WithAttrs) as TIDLFunctionDefinition;
  Result.ReturnType:=TIDLTypeDefDefinition.Create(Result,'AN');
  Result.ReturnType.TypeName:='short';
  Result.Options:=Options;
  I:=0;
  While I<Length(Args)-1 do
    begin
    Result.Arguments.Add(TIDLArgumentDefinition,args[I+1]);
    Result.Argument[I div 2].ArgumentType:=TIDLTypeDefDefinition.Create(Result,'AN'+IntToStr(i));
    Result.Argument[I div 2].ArgumentType.TypeName:=args[i];
//    With Result.Argument[I div 2] do
//      Writeln(I,': ',Name+'->',ArgumentType.TypeName);
    Inc(I,2);
    end;
end;

function TTestDefinition.CreateAttribute(Options: TAttributeOptions;
  withAttrs: Boolean): TIDLAttributeDefinition;

begin
  Result:=CreateDef(TIDLAttributeDefinition,WithAttrs) as TIDLAttributeDefinition;
  Result.AttributeType:=TIDLTypeDefDefinition.Create(Result,'AN');
  Result.AttributeType.TypeName:='short';
  Result.Options:=Options;
end;

procedure TTestDefinition.TestAttribute;

begin
  CreateAttribute([],False);
  TestDef('attribute short A',true);
  TestDef('short A',False);
  CreateAttribute([],True);
  TestDef('[Me] attribute short A',true);
  TestDef('short A',False);
  CreateAttribute([aoReadonly],false);
  TestDef('readonly attribute short A',true);
  TestDef('short A',False);
  CreateAttribute([aoStatic],false);
  TestDef('static attribute short A',true);
  TestDef('short A',False);
end;

procedure TTestDefinition.TestStringifierAttribute;
begin
  CreateAttribute([aoStringifier],false);
  TestDef('stringifier attribute short A',true);
  TestDef('short A',False);
  CreateAttribute([aoStringifier,aoReadOnly],false);
  TestDef('stringifier readonly attribute short A',true);
  TestDef('short A',False);
end;

procedure TTestDefinition.TestStringifierFunction;
begin
  CreateFunction([foStringifier],[],False);
  TestDef('stringifier short A ()',True);
end;

procedure TTestDefinition.TestFunction;

Var
  F : TIDLFunctionDefinition;

begin
  CreateFunction([],[],False);
  TestDef('short A ()',True);
  CreateFunction([],['short','B'],False);
  TestDef('short A (short B)',True);
  TestDef('short A (short B)',False);
  CreateFunction([],['short','B'],True);
  TestDef('[Me] short A (short B)',True);
  F:=CreateFunction([],['short','B','long','C'],False);
  F.Argument[1].IsOptional:=True;
  TestDef('short A (short B, optional long C)',True);
  F.Argument[1].HasDefaultValue:=True;
  F.Argument[1].DefaultValue:='123';
  TestDef('short A (short B, optional long C = 123)',True);
  CreateFunction([foStatic],[],False);
  TestDef('static short A ()',True);
  CreateFunction([foGetter],[],False);
  TestDef('getter short A ()',True);
  CreateFunction([foSetter],[],False);
  TestDef('setter short A ()',True);
end;

procedure TTestDefinition.TestCallBackFunction;
begin
  CreateFunction([foCallback],[],False);
  TestDef('callback A = short ()',True);
end;

procedure TTestDefinition.TestArgument;

begin
  CreateArgument(False,'',False);
  TestDef('short A',true);
  CreateArgument(False,'',False).ArgumentType.AllowNull:=True;
  TestDef('short? A',true);
  CreateArgument(true,'',False);
  TestDef('optional short A',true);
  CreateArgument(true,'',true);
  TestDef('[Me] optional short A',true);
  CreateArgument(true,'1',true);
  TestDef('[Me] optional short A = 1',true);
end;

procedure TTestDefinition.TestImplements;

begin
  CreateImplements('IME',False);
  TestDef('A implements IME',False);
  TestDef('A implements IME',True);
  CreateImplements('IME',True);
  TestDef('A implements IME',False);
  TestDef('[Me] A implements IME',True);
end;

procedure TTestDefinition.TestIncludes;
begin
  CreateIncludes('IME',False);
  TestDef('A includes IME',False);
  TestDef('A includes IME',True);
  CreateIncludes('IME',True);
  TestDef('A includes IME',False);
  TestDef('[Me] A includes IME',True);
end;

procedure TTestDefinition.TestTypeDef;

begin
  CreateTypeDef('IME',False);
  TestDef('IME',False);
  TestDef('typedef IME A',true);
  CreateTypeDef('IME',True);
  TestDef('IME',False);
  TestDef('[Me] typedef IME A',true);
  CreateTypeDef('IME',True).AllowNull:=True;
  TestDef('IME?',False);
  TestDef('[Me] typedef IME? A',true);
end;

procedure TTestDefinition.TestUnionTypeDef;
begin
  CreateUnionTypeDef(['string','short'],False);
  TestDef('(string or short)',False);
  TestDef('typedef (string or short) A',true);
  CreateUnionTypeDef(['string','short','unsigned long long'],true);
  TestDef('(string or short or unsigned long long)',False);
  TestDef('[Me] typedef (string or short or unsigned long long) A',true);
end;

procedure TTestDefinition.TestInterface;

Var
  C : TIDLConstDefinition;
  D : TIDLFunctionDefinition;

begin
  CreateInterface('',[],False);
  TestDef('interface A {'+sLinebreak+'}',True);
  CreateInterface('B',[]);
  TestDef('interface A : B {'+sLinebreak+'}',True);
  C:=CreateConst(False);
  Def:=Nil;
  CreateInterface('B',[C]);
  TestDef('interface A : B {'+sLinebreak+'  const short A 0x8080;'+sLineBreak+'}',True);
  C:=CreateConst(False);
  C.Name:='D';
  Def:=Nil;
  D:=CreateFunction([],[],True);
  D.Name:='C';
  Def:=Nil;
  CreateInterface('B',[C,D]);
  TestDef('interface A : B {'+sLinebreak
  +'  const short D 0x8080;'+sLineBreak
  +'  [Me] short C ();'+sLineBreak
  +'}',True);
  CreateInterface('',[],False).IsPartial:=True;
  TestDef('partial interface A {'+sLinebreak+'}',True);
end;

procedure TTestDefinition.TestDictionaryMember;

begin
  CreateDictionaryMember('A','short','',False,False);
  TestDef('short A',False);
  TestDef('short A',True);
  CreateDictionaryMember('A','short','""',False,False);
  TestDef('short A = ""',False);
  TestDef('short A = ""',True);
  CreateDictionaryMember('A','short','',True,False);
  TestDef('required short A',False);
  TestDef('required short A',True);
  CreateDictionaryMember('A','short','',False,True);
  TestDef('short A',False);
  TestDef('[Me] short A',True);
  CreateDictionaryMember('A','short','',true,True);
  TestDef('required short A',False);
  TestDef('[Me] required short A',True);
end;

procedure TTestDefinition.TestDictionary;

Var
  m1,m2 : TIDLDictionaryMemberDefinition;

begin
  CreateDictionary('',[],False);
  TestDef('dictionary A {'+sLinebreak+'}',True);
  CreateDictionary('B',[],False);
  TestDef('dictionary A : B {'+sLinebreak+'}',True);
  m1:=CreateDictionaryMember('B','short','',False,False);
  Def:=Nil;
  CreateDictionary('',[m1],False);
  TestDef('dictionary A {'+sLinebreak+
  '  short B;'+sLinebreak+
  '}',True);
  m1:=CreateDictionaryMember('C','short','',False,False);
  Def:=Nil;
  m2:=CreateDictionaryMember('D','short','',true,True);
  Def:=Nil;
  CreateDictionary('B',[m1,m2],False);
  TestDef('dictionary A : B {'+sLinebreak+
  '  short C;'+sLinebreak+
  '  [Me] required short D;'+sLinebreak+
  '}',True);

end;

procedure TTestDefinition.TestCallbackInterface;
begin
  CreateInterface('',[],False).IsCallBack:=True;
  TestDef('callback interface A {'+sLinebreak+'}',True);
end;

procedure TTestDefinition.TestSequence;
begin
  CreateSequence('short',false);
  TestDef('typedef sequence <short> A',True);
  TestDef('sequence <short>',False);
  CreateSequence('short',True);
  TestDef('[Me] typedef sequence <short> A',True);
  TestDef('sequence <short>',False);
end;

procedure TTestDefinition.TestPromise;
begin
  CreatePromise('short',false);
  TestDef('typedef promise <short> A',True);
  TestDef('promise <short>',False);
  CreatePromise('short',true);
  TestDef('[Me] typedef promise <short> A',True);
  TestDef('promise <short>',False);
  CreatePromise('short',False).AllowNull:=True;
  TestDef('typedef promise <short>? A',True);
  TestDef('promise <short>?',False);
end;

procedure TTestDefinition.TestMapLike;
begin
  CreateMapLike('short','string',false);
  TestDef('maplike <short,string>',True);
  TestDef('maplike <short,string>',False);
  CreateMapLike('short','string', True);
  TestDef('[Me] maplike <short,string>',True);
  TestDef('maplike <short,string>',False);
  CreateMapLike('short','string' ,false).IsReadOnly:=True;
  TestDef('readonly maplike <short,string>',True);
  TestDef('readonly maplike <short,string>',False);
end;

procedure TTestDefinition.TestSetLike;
begin
  CreateSetLike('string',false);
  TestDef('setlike <string>',True);
  TestDef('setlike <string>',False);
  CreateSetLike('string', True);
  TestDef('[Me] setlike <string>',True);
  TestDef('setlike <string>',False);
  CreateSetLike('string' ,false).IsReadOnly:=True;
  TestDef('readonly setlike <string>',True);
  TestDef('readonly setlike <string>',False);
end;

procedure TTestDefinition.TestRecord;
begin
  CreateRecord('short','string',false);
  TestDef('typedef record <short,string>',True);
  TestDef('record <short,string>',False);
  CreateRecord('short','string', True);
  TestDef('[Me] typedef record <short,string>',True);
  TestDef('record <short,string>',False);
end;

initialization
  RegisterTests([TTestDefinition])
end.

