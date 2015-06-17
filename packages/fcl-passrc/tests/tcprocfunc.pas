unit tcprocfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fpcunit, pastree, pscanner, pparser, tcbaseparser,testregistry;

type

  { TTestProcedureFunction }

  TTestProcedureFunction= class(TTestParser)
  private
    FAddComment: Boolean;
    FFunc: TPasFunction;
    FHint: String;
    FProc: TPasProcedure;
    procedure AddDeclaration(const ASource: string; const AHint: String='');
    procedure AssertArg(ProcType: TPasProcedureType; AIndex: Integer;
      AName: String; AAccess: TArgumentAccess; const TypeName: String;
      AValue: String='');
    procedure AssertArrayArg(ProcType: TPasProcedureType; AIndex: Integer;
      AName: String; AAccess: TArgumentAccess; const ElementTypeName: String);
    procedure AssertFunc(Mods: TProcedureModifiers; CC: TCallingConvention; ArgCount: Integer; P: TPasFunction=nil);
    procedure AssertProc(Mods: TProcedureModifiers; CC: TCallingConvention; ArgCount: Integer; P: TPasProcedure=nil);
    function BaseAssertArg(ProcType: TPasProcedureType; AIndex: Integer;
      AName: String; AAccess: TArgumentAccess; AValue: String=''): TPasArgument;
    function GetFT: TPasFunctionType;
    function GetPT: TPasProcedureType;
    Procedure ParseProcedure;
    function ParseProcedure(const ASource: string; const AHint: String=''): TPasProcedure;
    Procedure ParseFunction;
    function ParseFunction(const ASource : String; AResult: string = ''; const AHint: String=''; CC : TCallingConvention = ccDefault): TPasProcedure;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure AssertComment;
    Property AddComment : Boolean Read FAddComment Write FAddComment;
    Property Hint : String Read FHint Write FHint;
    Property Proc : TPasProcedure Read FProc;
    Property ProcType : TPasProcedureType Read GetPT;
    Property Func : TPasFunction Read FFunc;
    Property FuncType : TPasFunctionType Read GetFT;
  published
    procedure TestEmptyProcedure;
    procedure TestEmptyProcedureComment;
    Procedure TestEmptyFunction;
    Procedure TestEmptyFunctionComment;
    procedure TestEmptyProcedureDeprecated;
    Procedure TestEmptyFunctionDeprecated;
    procedure TestEmptyProcedurePlatform;
    Procedure TestEmptyFunctionPlatform;
    procedure TestEmptyProcedureExperimental;
    Procedure TestEmptyFunctionExperimental;
    procedure TestEmptyProcedureUnimplemented;
    Procedure TestEmptyFunctionUnimplemented;
    procedure TestProcedureOneArg;
    Procedure TestFunctionOneArg;
    procedure TestProcedureOneVarArg;
    Procedure TestFunctionOneVarArg;
    procedure TestProcedureOneConstArg;
    Procedure TestFunctionOneConstArg;
    procedure TestProcedureOneOutArg;
    Procedure TestFunctionOneOutArg;
    procedure TestProcedureOneConstRefArg;
    Procedure TestFunctionOneConstRefArg;
    procedure TestProcedureTwoArgs;
    Procedure TestFunctionTwoArgs;
    procedure TestProcedureTwoArgsSeparate;
    Procedure TestFunctionTwoArgsSeparate;
    procedure TestProcedureOneArgDefault;
    Procedure TestFunctionOneArgDefault;
    procedure TestProcedureOneArgDefaultSet;
    Procedure TestFunctionOneArgDefaultSet;
    procedure TestProcedureOneArgDefaultExpr;
    Procedure TestFunctionOneArgDefaultExpr;
    procedure TestProcedureTwoArgsDefault;
    Procedure TestFunctionTwoArgsDefault;
    procedure TestProcedureOneUntypedVarArg;
    Procedure TestFunctionOneUntypedVarArg;
    procedure TestProcedureTwoUntypedVarArgs;
    Procedure TestFunctionTwoUntypedVarArgs;
    procedure TestProcedureOneUntypedConstArg;
    Procedure TestFunctionOneUntypedConstArg;
    procedure TestProcedureTwoUntypedConstArgs;
    Procedure TestFunctionTwoUntypedConstArgs;
    procedure TestProcedureOpenArrayArg;
    Procedure TestFunctionOpenArrayArg;
    procedure TestProcedureTwoOpenArrayArgs;
    Procedure TestFunctionTwoOpenArrayArgs;
    procedure TestProcedureConstOpenArrayArg;
    Procedure TestFunctionConstOpenArrayArg;
    procedure TestProcedureVarOpenArrayArg;
    Procedure TestFunctionVarOpenArrayArg;
    procedure TestProcedureArrayOfConstArg;
    Procedure TestFunctionArrayOfConstArg;
    procedure TestProcedureConstArrayOfConstArg;
    Procedure TestFunctionConstArrayOfConstArg;
    Procedure TestProcedureCdecl;
    Procedure TestFunctionCdecl;
    Procedure TestProcedureCdeclDeprecated;
    Procedure TestFunctionCdeclDeprecated;
    Procedure TestProcedureSafeCall;
    Procedure TestFunctionSafeCall;
    //ccDefault,ccRegister,ccPascal,ccCDecl,ccStdCall,ccOldFPCCall,ccSafeCall);
    Procedure TestProcedurePascal;
    Procedure TestFunctionPascal;
    Procedure TestProcedureStdCall;
    Procedure TestFunctionStdCall;
    Procedure TestProcedureOldFPCCall;
    Procedure TestFunctionOldFPCCall;
    Procedure TestProcedurePublic;
    Procedure TestProcedurePublicIdent;
    Procedure TestFunctionPublic;
    Procedure TestProcedureCdeclPublic;
    Procedure TestFunctionCdeclPublic;
    Procedure TestProcedureOverload;
    Procedure TestFunctionOverload;
    Procedure TestProcedureVarargs;
    Procedure TestFunctionVarArgs;
    Procedure TestProcedureCDeclVarargs;
    Procedure TestFunctionCDeclVarArgs;
    Procedure TestProcedureForwardInterface;
    Procedure TestFunctionForwardInterface;
    Procedure TestProcedureForward;
    Procedure TestFunctionForward;
    Procedure TestProcedureCdeclForward;
    Procedure TestFunctionCDeclForward;
    Procedure TestProcedureCompilerProc;
    Procedure TestFunctionCompilerProc;
    Procedure TestProcedureCDeclCompilerProc;
    Procedure TestFunctionCDeclCompilerProc;
    Procedure TestProcedureAssembler;
    Procedure TestFunctionAssembler;
    Procedure TestProcedureCDeclAssembler;
    Procedure TestFunctionCDeclAssembler;
    Procedure TestProcedureExport;
    Procedure TestFunctionExport;
    Procedure TestProcedureCDeclExport;
    Procedure TestFunctionCDeclExport;
    Procedure TestProcedureExternal;
    Procedure TestFunctionExternal;
    Procedure TestProcedureExternalLibName;
    Procedure TestFunctionExternalLibName;
    Procedure TestProcedureExternalLibNameName;
    Procedure TestFunctionExternalLibNameName;
    Procedure TestProcedureExternalName;
    Procedure TestFunctionExternalName;
    Procedure TestProcedureCdeclExternal;
    Procedure TestFunctionCdeclExternal;
    Procedure TestProcedureCdeclExternalLibName;
    Procedure TestFunctionCdeclExternalLibName;
    Procedure TestProcedureCdeclExternalLibNameName;
    Procedure TestFunctionCdeclExternalLibNameName;
    Procedure TestProcedureCdeclExternalName;
    Procedure TestFunctionCdeclExternalName;
  end;

implementation


procedure TTestProcedureFunction.AddDeclaration(const ASource: string;
  const AHint: String);

Var
  D : String;
begin
  Hint:=AHint;
  D:=ASource;
  If Hint<>'' then
    D:=D+' '+Hint;
  if (D[Length(D)]<>';') then
    D:=D+';';
  Add(D);
end;

function TTestProcedureFunction.GetPT: TPasProcedureType;
begin
  AssertNotNull('have procedure to get type from',Proc);
  Result:=Proc.ProcType;
end;

function TTestProcedureFunction.ParseProcedure(const ASource: string;
  const AHint: String): TPasProcedure;


begin
  If AddComment then
    begin
    Add('// A comment');
    Engine.NeedComments:=True;
    end;
  AddDeclaration('procedure A '+ASource,AHint);
  Self.ParseProcedure;
  Result:=Fproc;
  If AddComment then
    AssertComment;
end;

Procedure TTestProcedureFunction.ParseProcedure;

begin
  //  Writeln(source.text);
  ParseDeclarations;
  AssertEquals('One variable definition',1,Declarations.Functions.Count);
  AssertEquals('First declaration is procedure declaration.',TPasProcedure,TObject(Declarations.Functions[0]).ClassType);
  FProc:=TPasProcedure(Declarations.Functions[0]);
  Definition:=FProc;
  AssertEquals('First declaration has correct name.','A',FProc.Name);
  if (Hint<>'') then
    CheckHint(TPasMemberHint(Getenumvalue(typeinfo(TPasMemberHint),'h'+Hint)));
end;

function TTestProcedureFunction.ParseFunction(const ASource : String;AResult: string = ''; const AHint: String = ''; CC : TCallingConvention = ccDefault): TPasProcedure;
Var
  D :String;
begin
  if (AResult='') then
    AResult:='Integer';
  D:='Function A '+ASource+' : '+AResult;
  If (cc<>ccDefault) then
    D:=D+'; '+cCallingConventions[cc]+';';
  AddDeclaration(D,AHint);
  Self.ParseFunction;
  Result:=FFunc;
  AssertNotNull('Have function result element',FuncType.ResultEl);
  AssertNotNull('Have function result type element',FuncType.ResultEl.ResultType);
  AssertEquals('Correct function result type name',AResult,FuncType.ResultEl.ResultType.Name);
end;

Procedure TTestProcedureFunction.ParseFunction;
begin
  //  Writeln(source.text);
  ParseDeclarations;
  AssertEquals('One variable definition',1,Declarations.Functions.Count);
  AssertEquals('First declaration is function declaration.',TPasFunction,TObject(Declarations.Functions[0]).ClassType);
  FFunc:=TPasFunction(Declarations.Functions[0]);
  Definition:=FFunc;
  AssertEquals('First declaration has correct name.','A',FFunc.Name);
  if (Hint<>'') then
    CheckHint(TPasMemberHint(Getenumvalue(typeinfo(TPasMemberHint),'h'+Hint)));
end;

procedure TTestProcedureFunction.AssertProc(Mods : TProcedureModifiers; CC : TCallingConvention; ArgCount : Integer; P : TPasProcedure = Nil);

begin
  If P=Nil then
    P:=Proc;
  AssertNotNull('No proc to assert',P);
  AssertEquals('Procedure modifiers',Mods,P.Modifiers);
  AssertEquals('Procedue calling convention',CC,P.CallingConvention);
  AssertEquals('No message name','',p.MessageName);
  AssertEquals('No message type',pmtNone,P.MessageType);
  AssertNotNull('Have procedure type to assert',P.ProcType);
  AssertEquals('Correct number of arguments',ArgCount,P.ProcType.Args.Count);
  AssertEquals('Not of object',False,P.ProcType.IsOfObject);
  AssertEquals('Not is nested',False,P.ProcType.IsNested);
end;

procedure TTestProcedureFunction.AssertFunc(Mods : TProcedureModifiers; CC : TCallingConvention; ArgCount : Integer; P : TPasFunction = Nil);

begin
  If P=Nil then
    P:=Func;
  AssertNotNull('No func to assert',P);
  AssertEquals('Procedure modifiers',Mods,P.Modifiers);
  AssertEquals('Procedue calling convention',CC,P.CallingConvention);
  AssertEquals('No message name','',p.MessageName);
  AssertEquals('No message type',pmtNone,P.MessageType);
  AssertNotNull('Have procedure type to assert',P.ProcType);
  AssertEquals('Correct number of arguments',ArgCount,P.ProcType.Args.Count);
  AssertEquals('Not of object',False,P.ProcType.IsOfObject);
  AssertEquals('Not is nested',False,P.ProcType.IsNested);
end;

function TTestProcedureFunction.BaseAssertArg(ProcType: TPasProcedureType;
  AIndex: Integer; AName: String; AAccess: TArgumentAccess; AValue: String
  ): TPasArgument;

Var
  A : TPasArgument;
  N : String;

begin
  AssertNotNull('Have proctype to test argument',ProcType);
  if AIndex>=Proctype.Args.Count then
    Fail(Format('Cannot test argument: index %d is larger than the number of arguments (%d).',[AIndex,Proctype.Args.Count]));
  AssertNotNull('Have argument at position '+IntToStr(AIndex),Proctype.Args[AIndex]);
  AssertEquals('Have argument type at position '+IntToStr(AIndex),TPasArgument,TObject(Proctype.Args[AIndex]).ClassType);
  N:='Argument '+IntToStr(AIndex+1)+' : ';
  A:=TPasArgument(Proctype.Args[AIndex]);
  AssertEquals(N+'Argument name',AName,A.Name);
  AssertEquals(N+'Argument access',AAccess,A.Access);
  if (AValue='') then
    AssertNull(N+' no default value',A.ValueExpr)
  else
    begin
    AssertNotNull(N+' Have default value',A.ValueExpr);
    AssertEquals(N+' Default value',AValue,A.Value);
    end;
  Result:=A;
end;

procedure TTestProcedureFunction.AssertArg(ProcType: TPasProcedureType;
  AIndex: Integer; AName: String; AAccess: TArgumentAccess;
  const TypeName: String; AValue: String);

Var
  A : TPasArgument;
  N : String;

begin
  A:=BaseAssertArg(ProcType,AIndex,ANAme,AAccess,AValue);
  N:='Argument '+IntToStr(AIndex+1)+' : ';
  if (TypeName='') then
    AssertNull(N+' No argument type',A.ArgType)
  else
    begin
    AssertNotNull(N+' Have argument type',A.ArgType);
    AssertEquals(N+' Correct argument type name',TypeName,A.ArgType.Name);
    end;
end;

procedure TTestProcedureFunction.AssertArrayArg(ProcType: TPasProcedureType;
  AIndex: Integer; AName: String; AAccess: TArgumentAccess;
  const ElementTypeName: String);
Var
  A : TPasArgument;
  AT : TPasArrayType;
  N : String;

begin
  A:=BaseAssertArg(ProcType,AIndex,ANAme,AAccess,'');
  N:='Argument '+IntToStr(AIndex+1)+' : ';
  AssertNotNull(N+' Have argument type',A.ArgType);
  AssertEquals(N+' is arrayType',TPasArrayType,A.ArgType.ClassType);
  AT:=TPasArrayType(A.ArgType);
  if (ElementTypeName='') then
    AssertNull(N+' No array element type',AT.ElType)
  else
    begin
    AssertNotNull(N+' Have array element type',AT.ElType);
    AssertEquals(N+' Correct array element type name',ElementTypeName,AT.ElType.Name);
    end;
end;

function TTestProcedureFunction.GetFT: TPasFunctionType;
begin
  AssertNotNull('have function to get type from',Func);
  AssertEquals('Function type is correct type',TPasFunctionType,Func.ProcType.ClassType);
  Result:=Func.FuncType;
end;

//TProcedureMessageType

procedure TTestProcedureFunction.TestEmptyProcedure;
begin
  ParseProcedure('');
  AssertProc([],ccDefault,0);
end;

procedure TTestProcedureFunction.TestEmptyProcedureComment;
begin
  AddComment:=True;
  TestEmptyProcedure;
end;

Procedure TTestProcedureFunction.TestEmptyFunction;
begin
  ParseFunction('');
  AssertFunc([],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestEmptyFunctionComment;
begin
  AddComment:=True;
  TestEmptyProcedure;
end;

procedure TTestProcedureFunction.TestEmptyProcedureDeprecated;
begin
  ParseProcedure('','deprecated');
  AssertProc([],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestEmptyFunctionDeprecated;
begin
  ParseFunction('','deprecated');
  AssertFunc([],ccDefault,0);
end;

procedure TTestProcedureFunction.TestEmptyProcedurePlatform;
begin
  ParseProcedure('','platform');
  AssertProc([],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestEmptyFunctionPlatform;
begin
  ParseFunction('','platform');
  AssertFunc([],ccDefault,0);
end;

procedure TTestProcedureFunction.TestEmptyProcedureExperimental;
begin
  ParseProcedure('','experimental');
  AssertProc([],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestEmptyFunctionExperimental;
begin
  ParseFunction('','experimental');
  AssertFunc([],ccDefault,0);
end;

procedure TTestProcedureFunction.TestEmptyProcedureUnimplemented;
begin
  ParseProcedure('','unimplemented');
  AssertProc([],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestEmptyFunctionUnimplemented;
begin
  ParseFunction('','unimplemented');
  AssertFunc([],ccDefault,0);

end;



procedure TTestProcedureFunction.TestProcedureOneArg;
begin
  ParseProcedure('(B : Integer)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argDefault,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionOneArg;
begin
  ParseFunction('(B : Integer)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argDefault,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureOneVarArg;
begin
  ParseProcedure('(Var B : Integer)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argVar,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionOneVarArg;
begin
  ParseFunction('(Var B : Integer)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argVar,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureOneConstArg;
begin
  ParseProcedure('(Const B : Integer)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argConst,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionOneConstArg;
begin
  ParseFunction('(Const B : Integer)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argConst,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureOneOutArg;
begin
  ParseProcedure('(Out B : Integer)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argOut,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionOneOutArg;
begin
  ParseFunction('(Out B : Integer)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argOut,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureOneConstRefArg;
begin
  ParseProcedure('(Constref B : Integer)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argConstRef,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionOneConstRefArg;
begin
  ParseFunction('(ConstRef B : Integer)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argConstref,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureTwoArgs;
begin
  ParseProcedure('(B,C : Integer)');
  AssertProc([],ccDefault,2);
  AssertArg(ProcType,0,'B',argDefault,'Integer','');
  AssertArg(ProcType,1,'C',argDefault,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionTwoArgs;
begin
  ParseFunction('(B,C : Integer)');
  AssertFunc([],ccDefault,2);
  AssertArg(FuncType,0,'B',argDefault,'Integer','');
  AssertArg(FuncType,1,'C',argDefault,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureTwoArgsSeparate;
begin
  ParseProcedure('(B : Integer; C : Integer)');
  AssertProc([],ccDefault,2);
  AssertArg(ProcType,0,'B',argDefault,'Integer','');
  AssertArg(ProcType,1,'C',argDefault,'Integer','');
end;

Procedure TTestProcedureFunction.TestFunctionTwoArgsSeparate;
begin
  ParseFunction('(B : Integer;C : Integer)');
  AssertFunc([],ccDefault,2);
  AssertArg(FuncType,0,'B',argDefault,'Integer','');
  AssertArg(FuncType,1,'C',argDefault,'Integer','');
end;

procedure TTestProcedureFunction.TestProcedureOneArgDefault;
begin
  ParseProcedure('(B : Integer = 1)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argDefault,'Integer','1');
end;

Procedure TTestProcedureFunction.TestFunctionOneArgDefault;
begin
  ParseFunction('(B : Integer = 1)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argDefault,'Integer','1');
end;

procedure TTestProcedureFunction.TestProcedureOneArgDefaultSet;
begin
  ParseProcedure('(B : MySet = [1,2])');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argDefault,'MySet','[1, 2]');
end;

Procedure TTestProcedureFunction.TestFunctionOneArgDefaultSet;
begin
  ParseFunction('(B : MySet = [1,2])');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argDefault,'MySet','[1, 2]');
end;

procedure TTestProcedureFunction.TestProcedureOneArgDefaultExpr;
begin
  ParseProcedure('(B : Integer = 1 + 2)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argDefault,'Integer','1 + 2');
end;

Procedure TTestProcedureFunction.TestFunctionOneArgDefaultExpr;
begin
  ParseFunction('(B : Integer = 1 + 2)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argDefault,'Integer','1 + 2');
end;

procedure TTestProcedureFunction.TestProcedureTwoArgsDefault;
begin
  ParseProcedure('(B : Integer = 1; C : Integer = 2)');
  AssertProc([],ccDefault,2);
  AssertArg(ProcType,0,'B',argDefault,'Integer','1');
  AssertArg(ProcType,1,'C',argDefault,'Integer','2');
end;

Procedure TTestProcedureFunction.TestFunctionTwoArgsDefault;
begin
  ParseFunction('(B : Integer = 1; C : Integer = 2)');
  AssertFunc([],ccDefault,2);
  AssertArg(FuncType,0,'B',argDefault,'Integer','1');
  AssertArg(FuncType,1,'C',argDefault,'Integer','2');
end;

procedure TTestProcedureFunction.TestProcedureOneUntypedVarArg;
begin
  ParseProcedure('(Var B)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argVar,'','');
end;

Procedure TTestProcedureFunction.TestFunctionOneUntypedVarArg;
begin
  ParseFunction('(Var B)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argVar,'','');
end;

procedure TTestProcedureFunction.TestProcedureTwoUntypedVarArgs;
begin
  ParseProcedure('(Var B; Var C)');
  AssertProc([],ccDefault,2);
  AssertArg(ProcType,0,'B',argVar,'','');
  AssertArg(ProcType,1,'C',argVar,'','');
end;

Procedure TTestProcedureFunction.TestFunctionTwoUntypedVarArgs;
begin
  ParseFunction('(Var B; Var C)');
  AssertFunc([],ccDefault,2);
  AssertArg(FuncType,0,'B',argVar,'','');
  AssertArg(FuncType,1,'C',argVar,'','');
end;

procedure TTestProcedureFunction.TestProcedureOneUntypedConstArg;
begin
  ParseProcedure('(Const B)');
  AssertProc([],ccDefault,1);
  AssertArg(ProcType,0,'B',argConst,'','');
end;

Procedure TTestProcedureFunction.TestFunctionOneUntypedConstArg;
begin
  ParseFunction('(Const B)');
  AssertFunc([],ccDefault,1);
  AssertArg(FuncType,0,'B',argConst,'','');
end;

procedure TTestProcedureFunction.TestProcedureTwoUntypedConstArgs;
begin
  ParseProcedure('(Const B; Const C)');
  AssertProc([],ccDefault,2);
  AssertArg(ProcType,0,'B',argConst,'','');
  AssertArg(ProcType,1,'C',argConst,'','');
end;

Procedure TTestProcedureFunction.TestFunctionTwoUntypedConstArgs;
begin
  ParseFunction('(Const B; Const C)');
  AssertFunc([],ccDefault,2);
  AssertArg(FuncType,0,'B',argConst,'','');
  AssertArg(FuncType,1,'C',argConst,'','');
end;

procedure TTestProcedureFunction.TestProcedureOpenArrayArg;
begin
  ParseProcedure('(B : Array of Integer)');
  AssertProc([],ccDefault,1);
  AssertArrayArg(ProcType,0,'B',argDefault,'Integer');
end;

Procedure TTestProcedureFunction.TestFunctionOpenArrayArg;
begin
  ParseFunction('(B : Array of Integer)');
  AssertFunc([],ccDefault,1);
  AssertArrayArg(FuncType,0,'B',argDefault,'Integer');
end;

procedure TTestProcedureFunction.TestProcedureTwoOpenArrayArgs;
begin
  ParseProcedure('(B : Array of Integer;C : Array of Integer)');
  AssertProc([],ccDefault,2);
  AssertArrayArg(ProcType,0,'B',argDefault,'Integer');
  AssertArrayArg(ProcType,1,'C',argDefault,'Integer');
end;

Procedure TTestProcedureFunction.TestFunctionTwoOpenArrayArgs;
begin
  ParseFunction('(B : Array of Integer;C : Array of Integer)');
  AssertFunc([],ccDefault,2);
  AssertArrayArg(FuncType,0,'B',argDefault,'Integer');
  AssertArrayArg(FuncType,1,'C',argDefault,'Integer');
end;

procedure TTestProcedureFunction.TestProcedureConstOpenArrayArg;
begin
  ParseProcedure('(Const B : Array of Integer)');
  AssertProc([],ccDefault,1);
  AssertArrayArg(ProcType,0,'B',argConst,'Integer');
end;

Procedure TTestProcedureFunction.TestFunctionConstOpenArrayArg;
begin
  ParseFunction('(Const B : Array of Integer)');
  AssertFunc([],ccDefault,1);
  AssertArrayArg(FuncType,0,'B',argConst,'Integer');
end;

procedure TTestProcedureFunction.TestProcedureVarOpenArrayArg;
begin
  ParseProcedure('(Var B : Array of Integer)');
  AssertProc([],ccDefault,1);
  AssertArrayArg(ProcType,0,'B',argVar,'Integer');
end;

Procedure TTestProcedureFunction.TestFunctionVarOpenArrayArg;
begin
  ParseFunction('(Var B : Array of Integer)');
  AssertFunc([],ccDefault,1);
  AssertArrayArg(FuncType,0,'B',argVar,'Integer');
end;

procedure TTestProcedureFunction.TestProcedureArrayOfConstArg;
begin
  ParseProcedure('(B : Array of Const)');
  AssertProc([],ccDefault,1);
  AssertArrayArg(ProcType,0,'B',argDefault,'');
end;

Procedure TTestProcedureFunction.TestFunctionArrayOfConstArg;
begin
  ParseFunction('(B : Array of Const)');
  AssertFunc([],ccDefault,1);
  AssertArrayArg(FuncType,0,'B',argDefault,'');
end;

procedure TTestProcedureFunction.TestProcedureConstArrayOfConstArg;
begin
  ParseProcedure('(Const B : Array of Const)');
  AssertProc([],ccDefault,1);
  AssertArrayArg(ProcType,0,'B',argConst,'');
end;

Procedure TTestProcedureFunction.TestFunctionConstArrayOfConstArg;
begin
  ParseFunction('(Const B : Array of Const)');
  AssertFunc([],ccDefault,1);
  AssertArrayArg(FuncType,0,'B',argConst,'');
end;

Procedure TTestProcedureFunction.TestProcedureCdecl;
begin
  ParseProcedure('; cdecl');
  AssertProc([],ccCdecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCdecl;
begin
  ParseFunction('','','',ccCdecl);
  AssertFunc([],ccCdecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureCdeclDeprecated;
begin
  ParseProcedure('; cdecl;','deprecated');
  AssertProc([],ccCdecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCdeclDeprecated;
begin
  ParseFunction('','','deprecated',ccCdecl);
  AssertFunc([],ccCdecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureSafeCall;
begin
  ParseProcedure('; safecall;','');
  AssertProc([],ccSafeCall,0);
end;

Procedure TTestProcedureFunction.TestFunctionSafeCall;
begin
  ParseFunction('','','',ccSafecall);
  AssertFunc([],ccSafecall,0);
end;

Procedure TTestProcedureFunction.TestProcedurePascal;
begin
  ParseProcedure('; pascal;','');
  AssertProc([],ccPascal,0);
end;

Procedure TTestProcedureFunction.TestFunctionPascal;
begin
  ParseFunction('','','',ccPascal);
  AssertFunc([],ccPascal,0);
end;

Procedure TTestProcedureFunction.TestProcedureStdCall;
begin
  ParseProcedure('; stdcall;','');
  AssertProc([],ccstdcall,0);
end;

Procedure TTestProcedureFunction.TestFunctionStdCall;
begin
  ParseFunction('','','',ccStdCall);
  AssertFunc([],ccStdCall,0);
end;

Procedure TTestProcedureFunction.TestProcedureOldFPCCall;
begin
  ParseProcedure('; oldfpccall;','');
  AssertProc([],ccoldfpccall,0);
end;

Procedure TTestProcedureFunction.TestFunctionOldFPCCall;
begin
  ParseFunction('','','',ccOldFPCCall);
  AssertFunc([],ccOldFPCCall,0);
end;

Procedure TTestProcedureFunction.TestProcedurePublic;
begin
  ParseProcedure('; public name ''myfunc'';','');
  AssertProc([pmPublic],ccDefault,0);
  AssertExpression('Public name',Proc.PublicName,pekString,'''myfunc''');
end;

Procedure TTestProcedureFunction.TestProcedurePublicIdent;
begin
  ParseProcedure('; public name exportname;','');
  AssertProc([pmPublic],ccDefault,0);
  AssertExpression('Public name',Proc.PublicName,pekIdent,'exportname');
end;

Procedure TTestProcedureFunction.TestFunctionPublic;
begin
  AddDeclaration('function A : Integer; public name exportname');
  ParseFunction;
  AssertFunc([pmPublic],ccDefault,0);
  AssertExpression('Public name',Func.PublicName,pekIdent,'exportname');
end;

Procedure TTestProcedureFunction.TestProcedureCdeclPublic;
begin
  ParseProcedure('; cdecl; public name exportname;','');
  AssertProc([pmPublic],ccCDecl,0);
  AssertExpression('Public name',Proc.PublicName,pekIdent,'exportname');
end;

Procedure TTestProcedureFunction.TestFunctionCdeclPublic;
begin
  AddDeclaration('function A : Integer; cdecl; public name exportname');
  ParseFunction;
  AssertFunc([pmPublic],ccCDecl,0);
  AssertExpression('Public name',Func.PublicName,pekIdent,'exportname');
end;

Procedure TTestProcedureFunction.TestProcedureOverload;
begin
  ParseProcedure('; overload;','');
  AssertProc([pmOverload],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestFunctionOverload;
begin
  AddDeclaration('function A : Integer; overload');
  ParseFunction;
  AssertFunc([pmOverload],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestProcedureVarargs;
begin
  ParseProcedure('; varargs;','');
  AssertProc([pmVarArgs],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestFunctionVarArgs;
begin
  AddDeclaration('function A : Integer; varargs');
  ParseFunction;
  AssertFunc([pmVarArgs],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestProcedureCDeclVarargs;
begin
  ParseProcedure(';cdecl; varargs;','');
  AssertProc([pmVarArgs],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCDeclVarArgs;
begin
  AddDeclaration('function A : Integer; cdecl; varargs');
  ParseFunction;
  AssertFunc([pmVarArgs],ccCdecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureForwardInterface;
begin
  AddDeclaration('procedure A; forward;');
  AssertException(EParserError,@ParseProcedure);
end;

Procedure TTestProcedureFunction.TestFunctionForwardInterface;
begin
  AddDeclaration('function A : integer; forward;');
  AssertException(EParserError,@ParseFunction);
end;

Procedure TTestProcedureFunction.TestProcedureForward;
begin
  UseImplementation:=True;
  AddDeclaration('procedure A; forward;');
  ParseProcedure;
  AssertProc([pmforward],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestFunctionForward;
begin
  UseImplementation:=True;
  AddDeclaration('function A : integer; forward;');
  ParseFunction;
  AssertFunc([pmforward],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestProcedureCdeclForward;
begin
  UseImplementation:=True;
  AddDeclaration('procedure A; cdecl; forward;');
  ParseProcedure;
  AssertProc([pmforward],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCDeclForward;
begin
  UseImplementation:=True;
  AddDeclaration('function A : integer; cdecl; forward;');
  ParseFunction;
  AssertFunc([pmforward],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureCompilerProc;
begin
  ParseProcedure(';compilerproc;','');
  AssertProc([pmCompilerProc],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestFunctionCompilerProc;
begin
  AddDeclaration('function A : Integer; compilerproc');
  ParseFunction;
  AssertFunc([pmCompilerProc],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestProcedureCDeclCompilerProc;
begin
  ParseProcedure(';cdecl;compilerproc;','');
  AssertProc([pmCompilerProc],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCDeclCompilerProc;
begin
  AddDeclaration('function A : Integer; cdecl; compilerproc');
  ParseFunction;
  AssertFunc([pmCompilerProc],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureAssembler;
begin
  ParseProcedure(';assembler;','');
  AssertProc([pmAssembler],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestFunctionAssembler;
begin
  AddDeclaration('function A : Integer; assembler');
  ParseFunction;
  AssertFunc([pmAssembler],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestProcedureCDeclAssembler;
begin
  ParseProcedure(';cdecl;assembler;','');
  AssertProc([pmAssembler],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCDeclAssembler;
begin
  AddDeclaration('function A : Integer; cdecl; assembler');
  ParseFunction;
  AssertFunc([pmAssembler],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureExport;
begin
  ParseProcedure(';export;','');
  AssertProc([pmExport],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestFunctionExport;
begin
  AddDeclaration('function A : Integer; export');
  ParseFunction;
  AssertFunc([pmExport],ccDefault,0);
end;

Procedure TTestProcedureFunction.TestProcedureCDeclExport;
begin
  ParseProcedure('cdecl;export;','');
  AssertProc([pmExport],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestFunctionCDeclExport;
begin
  AddDeclaration('function A : Integer; cdecl; export');
  ParseFunction;
  AssertFunc([pmExport],ccCDecl,0);
end;

Procedure TTestProcedureFunction.TestProcedureExternal;
begin
  ParseProcedure(';external','');
  AssertProc([pmExternal],ccDefault,0);
  AssertNull('No Library name expression',Proc.LibraryExpr);
end;

Procedure TTestProcedureFunction.TestFunctionExternal;
begin
  AddDeclaration('function A : Integer; external');
  ParseFunction;
  AssertFunc([pmExternal],ccDefault,0);
  AssertNull('No Library name expression',Func.LibraryExpr);
end;

Procedure TTestProcedureFunction.TestProcedureExternalLibName;
begin
  ParseProcedure(';external ''libname''','');
  AssertProc([pmExternal],ccDefault,0);
  AssertExpression('Library name expression',Proc.LibraryExpr,pekString,'''libname''');
end;

Procedure TTestProcedureFunction.TestFunctionExternalLibName;
begin
  AddDeclaration('function A : Integer; external ''libname''');
  ParseFunction;
  AssertFunc([pmExternal],ccDefault,0);
  AssertExpression('Library name expression',Func.LibraryExpr,pekString,'''libname''');
end;

Procedure TTestProcedureFunction.TestProcedureExternalLibNameName;
begin
  ParseProcedure(';external ''libname'' name ''symbolname''','');
  AssertProc([pmExternal],ccDefault,0);
  AssertExpression('Library name expression',Proc.LibraryExpr,pekString,'''libname''');
  AssertExpression('Library symbol expression',Proc.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestFunctionExternalLibNameName;
begin
  AddDeclaration('function A : Integer; external ''libname'' name ''symbolname''');
  ParseFunction;
  AssertFunc([pmExternal],ccDefault,0);
  AssertExpression('Library name expression',Func.LibraryExpr,pekString,'''libname''');
  AssertExpression('Library symbol expression',Func.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestProcedureExternalName;
begin
  ParseProcedure(';external name ''symbolname''','');
  AssertProc([pmExternal],ccDefault,0);
  AssertNull('No Library name expression',Proc.LibraryExpr);
  AssertExpression('Library symbol expression',Proc.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestFunctionExternalName;
begin
  AddDeclaration('function A : Integer; external name ''symbolname''');
  ParseFunction;
  AssertFunc([pmExternal],ccDefault,0);
  AssertNull('No Library name expression',Func.LibraryExpr);
  AssertExpression('Library symbol expression',Func.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestProcedureCdeclExternal;
begin
  ParseProcedure('; cdecl; external','');
  AssertProc([pmExternal],ccCdecl,0);
  AssertNull('No Library name expression',Proc.LibraryExpr);
end;

Procedure TTestProcedureFunction.TestFunctionCdeclExternal;
begin
  AddDeclaration('function A : Integer; cdecl; external');
  ParseFunction;
  AssertFunc([pmExternal],ccCdecl,0);
  AssertNull('No Library name expression',Func.LibraryExpr);
end;

Procedure TTestProcedureFunction.TestProcedureCdeclExternalLibName;
begin
  ParseProcedure('; cdecl; external ''libname''','');
  AssertProc([pmExternal],ccCdecl,0);
  AssertExpression('Library name expression',Proc.LibraryExpr,pekString,'''libname''');
end;

Procedure TTestProcedureFunction.TestFunctionCdeclExternalLibName;
begin
  AddDeclaration('function A : Integer; cdecl; external ''libname''');
  ParseFunction;
  AssertFunc([pmExternal],ccCdecl,0);
  AssertExpression('Library name expression',Func.LibraryExpr,pekString,'''libname''');
end;

Procedure TTestProcedureFunction.TestProcedureCdeclExternalLibNameName;
begin
  ParseProcedure('; cdecl; external ''libname'' name ''symbolname''','');
  AssertProc([pmExternal],ccCdecl,0);
  AssertExpression('Library name expression',Proc.LibraryExpr,pekString,'''libname''');
  AssertExpression('Library symbol expression',Proc.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestFunctionCdeclExternalLibNameName;
begin
  AddDeclaration('function A : Integer; cdecl; external ''libname'' name ''symbolname''');
  ParseFunction;
  AssertFunc([pmExternal],ccCdecl,0);
  AssertExpression('Library name expression',Func.LibraryExpr,pekString,'''libname''');
  AssertExpression('Library symbol expression',Func.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestProcedureCdeclExternalName;
begin
  ParseProcedure('; cdecl; external name ''symbolname''','');
  AssertProc([pmExternal],ccCdecl,0);
  AssertNull('No Library name expression',Proc.LibraryExpr);
  AssertExpression('Library symbol expression',Proc.LibrarySymbolName,pekString,'''symbolname''');
end;

Procedure TTestProcedureFunction.TestFunctionCdeclExternalName;
begin
  AddDeclaration('function A : Integer; cdecl; external name ''symbolname''');
  ParseFunction;
  AssertFunc([pmExternal],ccCdecl,0);
  AssertNull('No Library name expression',Func.LibraryExpr);
  AssertExpression('Library symbol expression',Func.LibrarySymbolName,pekString,'''symbolname''');
end;

procedure TTestProcedureFunction.SetUp;
begin
   Inherited;
end;

procedure TTestProcedureFunction.TearDown;
begin
   Inherited;
end;

Procedure TTestProcedureFunction.AssertComment;
begin
  AssertEquals('Correct comment',' A comment'+sLineBreak,FProc.DocComment);
end;

initialization

  RegisterTest(TTestProcedureFunction);
end.

