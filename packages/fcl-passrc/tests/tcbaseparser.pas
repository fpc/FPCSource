unit tcbaseparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, pscanner, pparser, testregistry;

Type
  { TTestEngine }

  TTestEngine = Class(TPasTreeContainer)
  Private
    FList : TFPList;
  public
    Destructor Destroy; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;
  TTestPasParser = Class(TPasParser);

  { TTestParser }

  TTestParser= class(TTestCase)
  Private
    FDeclarations: TPasDeclarations;
    FDefinition: TPasElement;
    FEngine : TTestEngine;
    FModule: TPasModule;
    FParseResult: TPasElement;
    FScanner : TPascalScanner;
    FResolver : TStreamResolver;
    FParser : TTestPasParser;
    FSource: TStrings;
    FFileName : string;
    FIsUnit : Boolean;
    FImplementation : Boolean;
    FEndSource: Boolean;
    FUseImplementation: Boolean;
    function GetPL: TPasLibrary;
    function GetPP: TPasProgram;
    procedure CleanupParser;
    procedure SetupParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure StartUnit(AUnitName : String);
    Procedure StartProgram(AFileName : String; AIn : String = ''; AOut : String = '');
    Procedure StartLibrary(AFileName : String);
    Procedure UsesClause(Units : Array of string);
    Procedure StartImplementation;
    Procedure EndSource;
    Procedure Add(Const ALine : String);
    Procedure StartParsing;
    Procedure ParseDeclarations;
    Procedure ParseModule;
    procedure ResetParser;
    Procedure CheckHint(AHint : TPasMemberHint);
    Function AssertExpression(Const Msg: String; AExpr : TPasExpr; aKind : TPasExprKind; AClass : TClass) : TPasExpr;
    Function AssertExpression(Const Msg: String; AExpr : TPasExpr; aKind : TPasExprKind; AValue : String) : TPrimitiveExpr;
    Procedure AssertExportSymbol(Const Msg: String; AIndex : Integer; AName,AExportName : String; AExportIndex : Integer = -1);
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TPasExprKind); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TLoopType); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TPasObjKind); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TexprOpcode); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TPasMemberHint); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TCallingConvention); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TArgumentAccess); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TVariableModifier); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TVariableModifiers); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TPasMemberVisibility); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TProcedureModifier); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TProcedureModifiers); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TAssignKind); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TProcedureMessageType); overload;
    Procedure AssertEquals(Const Msg : String; AExpected, AActual: TOperatorType); overload;
    Procedure HaveHint(AHint : TPasMemberHint; AHints : TPasMemberHints);
    Property Resolver : TStreamResolver Read FResolver;
    Property Scanner : TPascalScanner Read FScanner;
    Property Engine : TTestEngine read FEngine;
    Property Parser : TTestPasParser read FParser ;
    Property Source : TStrings Read FSource;
    Property Module : TPasModule Read FModule;
    Property PasProgram : TPasProgram Read GetPP;
    Property PasLibrary : TPasLibrary Read GetPL;
    Property Declarations : TPasDeclarations read FDeclarations Write FDeclarations;
    Property Definition : TPasElement Read FDefinition Write FDefinition;
    // If set, Will be freed in teardown
    Property ParseResult : TPasElement Read FParseResult Write FParseResult;
    Property UseImplementation : Boolean Read FUseImplementation Write FUseImplementation;
  end;

implementation

uses typinfo;
{ TTestEngine }

destructor TTestEngine.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TTestEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
  if NeedComments and Assigned(CurrentParser) then
    begin
//    Writeln('Saving comment : ',CurrentParser.SavedComments);
    Result.DocComment:=CurrentParser.SavedComments;
    end;
  If not Assigned(FList) then
    FList:=TFPList.Create;
  FList.Add(Result);
end;

function TTestEngine.FindElement(const AName: String): TPasElement;

Var
  I : Integer;

begin
  Result:=Nil;
  if Assigned(FList) then
    begin
    I:=FList.Count-1;
    While (Result=Nil) and (I>=0) do
      begin
      if CompareText(TPasElement(FList[I]).Name,AName)=0 then
        Result:=TPasElement(Flist[i]);
      Dec(i);
      end;
    end;
end;

function TTestParser.GetPP: TPasProgram;
begin
  Result:=Module as TPasProgram;
end;

function TTestParser.GetPL: TPasLibrary;
begin
  Result:=Module as TPasLibrary;
end;

procedure TTestParser.SetupParser;

begin
  FResolver:=TStreamResolver.Create;
  FResolver.OwnsStreams:=True;
  FScanner:=TPascalScanner.Create(FResolver);
  FEngine:=TTestEngine.Create;
  FParser:=TTestPasParser.Create(FScanner,FResolver,FEngine);
  FSource:=TStringList.Create;
  FModule:=Nil;
  FDeclarations:=Nil;
  FEndSource:=False;
  FImplementation:=False;
  FIsUnit:=False;
end;

procedure TTestParser.CleanupParser;

begin
  if Not Assigned(FModule) then
    FreeAndNil(FDeclarations)
  else
    FDeclarations:=Nil;
  FImplementation:=False;
  FEndSource:=False;
  FIsUnit:=False;
  FreeAndNil(FModule);
  FreeAndNil(FSource);
  FreeAndNil(FParseResult);
  FreeAndNil(FParser);
  FreeAndNil(FEngine);
  FreeAndNil(FScanner);
  FreeAndNil(FResolver);
end;

procedure TTestParser.ResetParser;

begin
  CleanupParser;
  SetupParser;
end;

procedure TTestParser.SetUp;
begin
  Inherited;
  SetupParser;
end;

procedure TTestParser.TearDown;
begin
  CleanupParser;
  Inherited;
end;

procedure TTestParser.StartUnit(AUnitName: String);
begin
  FIsUnit:=True;
  If (AUnitName='') then
    AUnitName:='afile';
  Add('unit '+aUnitName+';');
  Add('');
  Add('interface');
  Add('');
  FFileName:=AUnitName+'.pp';
end;

procedure TTestParser.StartProgram(AFileName : String; AIn : String = ''; AOut : String = '');
begin
  FIsUnit:=False;
  If (AFileName='') then
    AFileName:='proga';
  FFileName:=AFileName+'.pp';
  If (AIn<>'') then
    begin
    AFileName:=AFileName+'('+AIn;
    if (AOut<>'') then
      AFileName:=AFIleName+','+AOut;
    AFileName:=AFileName+')';
    end;
  Add('program '+AFileName+';');
  FImplementation:=True;
end;

procedure TTestParser.StartLibrary(AFileName: String);
begin
  FIsUnit:=False;
  If (AFileName='') then
    AFileName:='liba';
  FFileName:=AFileName+'.pp';
  Add('library '+AFileName+';');
  FImplementation:=True;
end;

procedure TTestParser.UsesClause(Units: array of string);

Var
  S : String;
  I : integer;

begin
  S:='';
  For I:=Low(units) to High(units) do
    begin
    If (S<>'') then
        S:=S+', ';
    S:=S+Units[i];
    end;
  Add('uses '+S+';');
  Add('');
end;

procedure TTestParser.StartImplementation;
begin
  if Not FImplementation then
    begin
    if UseImplementation then
      begin
      FSource.Insert(0,'');
      FSource.Insert(0,'Implementation');
      FSource.Insert(0,'');
      end
    else
      begin
      Add('');
      Add('Implementation');
      Add('');
      end;
    FImplementation:=True;
    end;
end;

procedure TTestParser.EndSource;
begin
  if Not FEndSource then
    begin
    Add('end.');
    FEndSource:=True;
    end;
end;

procedure TTestParser.Add(const ALine: String);
begin
  FSource.Add(ALine);
end;

procedure TTestParser.StartParsing;

begin
  If FIsUnit then
    StartImplementation;
  EndSource;
  If (FFileName='') then
    FFileName:='afile.pp';
  FResolver.AddStream(FFileName,TStringStream.Create(FSource.text));
  FScanner.OpenFile(FFileName);
  Writeln('// Test : ',Self.TestName);
  Writeln(FSource.Text);
end;

procedure TTestParser.ParseDeclarations;
begin
  if UseImplementation then
    StartImplementation;
  FSource.Insert(0,'');
  FSource.Insert(0,'interface');
  FSource.Insert(0,'');
  FSource.Insert(0,'unit afile;');
  if Not UseImplementation then
    StartImplementation;
  EndSource;
  ParseModule;
  if UseImplementation then
    FDeclarations:=Module.ImplementationSection
  else
    FDeclarations:=Module.InterfaceSection;
end;

procedure TTestParser.ParseModule;
begin
  StartParsing;
  FParser.ParseMain(FModule);
  AssertNotNull('Module resulted in Module',FModule);
  AssertEquals('modulename',ChangeFileExt(FFileName,''),Module.Name);
end;

procedure TTestParser.CheckHint(AHint: TPasMemberHint);
begin
  HaveHint(AHint,Definition.Hints);
end;

function TTestParser.AssertExpression(const Msg: String; AExpr: TPasExpr;
  aKind: TPasExprKind; AClass: TClass): TPasExpr;
begin
  AssertEquals(Msg+': Correct expression kind',aKind,AExpr.Kind);
  AssertEquals(Msg+': Correct expression class',AClass,AExpr.ClassType);
  Result:=AExpr;
end;

function TTestParser.AssertExpression(const Msg: String; AExpr: TPasExpr;
  aKind: TPasExprKind; AValue: String): TPrimitiveExpr;
begin
  Result:=AssertExpression(Msg,AExpr,aKind,TPrimitiveExpr) as TPrimitiveExpr;
  AssertEquals(Msg+': Primitive expression value',AValue,TPrimitiveExpr(AExpr).Value);
end;

procedure TTestParser.AssertExportSymbol(const Msg: String; AIndex: Integer;
  AName, AExportName: String; AExportIndex: Integer);

Var
  E: TPasExportSymbol;

begin
  AssertNotNull(Msg+'Have export symbols list',PasLibrary.LibrarySection.ExportSymbols);
  if AIndex>=PasLibrary.LibrarySection.ExportSymbols.Count then
    Fail(Format(Msg+'%d not a valid export list symbol',[AIndex]));
  AssertNotNull(Msg+'Have export symbol',PasLibrary.LibrarySection.ExportSymbols[Aindex]);
  AssertEquals(Msg+'Correct export symbol class',TPasExportSymbol,TObject(PasLibrary.LibrarySection.ExportSymbols[Aindex]).ClassType);
  E:=TPasExportSymbol(PasLibrary.LibrarySection.ExportSymbols[Aindex]);
  AssertEquals(Msg+'Correct export symbol name',AName,E.Name);
  if (AExportName='') then
    AssertNull(Msg+'No export name',E.ExportName)
  else
    begin
    AssertNotNull(Msg+'Export name symbol',E.ExportName);
    AssertEquals(Msg+'TPrimitiveExpr',TPrimitiveExpr,E.ExportName.CLassType);
    AssertEquals(Msg+'Correct export symbol export name ',''''+AExportName+'''',TPrimitiveExpr(E.ExportName).Value);
    end;
  If AExportIndex=-1 then
    AssertNull(Msg+'No export name',E.ExportIndex)
  else
    begin
    AssertNotNull(Msg+'Export name symbol',E.ExportIndex);
    AssertEquals(Msg+'TPrimitiveExpr',TPrimitiveExpr,E.ExportIndex.CLassType);
    AssertEquals(Msg+'Correct export symbol export index',IntToStr(AExportindex),TPrimitiveExpr(E.ExportIndex).Value);
    end;
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TPasExprKind);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TPasExprKind),Ord(AExpected)),
                   GetEnumName(TypeInfo(TPasExprKind),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TLoopType);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TLoopType),Ord(AExpected)),
                   GetEnumName(TypeInfo(TLoopType),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TPasObjKind);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TexprOpcode),Ord(AExpected)),
                   GetEnumName(TypeInfo(TexprOpcode),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TexprOpcode);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TexprOpcode),Ord(AExpected)),
                   GetEnumName(TypeInfo(TexprOpcode),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TPasMemberHint);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TPasMemberHint),Ord(AExpected)),
                   GetEnumName(TypeInfo(TPasMemberHint),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TCallingConvention);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TCallingConvention),Ord(AExpected)),
                   GetEnumName(TypeInfo(TCallingConvention),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TArgumentAccess);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TArgumentAccess),Ord(AExpected)),
                   GetEnumName(TypeInfo(TArgumentAccess),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TVariableModifier);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TVariableModifier),Ord(AExpected)),
                   GetEnumName(TypeInfo(TVariableModifier),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TVariableModifiers);

 Function sn (S : TVariableModifiers) : string;

 Var
   M : TVariableModifier;

 begin
   Result:='';
   For M:=Low(TVariableModifier) to High(TVariableModifier) do
     if M in S then
       begin
       if (Result<>'') then
         Result:=Result+',';
       end;
   Result:='['+Result+']';
 end;

begin
  AssertEquals(Msg,Sn(AExpected),Sn(AActual));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TPasMemberVisibility);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TPasMemberVisibility),Ord(AExpected)),
                   GetEnumName(TypeInfo(TPasMemberVisibility),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TProcedureModifier);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TProcedureModifier),Ord(AExpected)),
                   GetEnumName(TypeInfo(TProcedureModifier),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TProcedureModifiers);

  Function Sn (S : TProcedureModifiers) : String;

  Var
    m : TProcedureModifier;
  begin
    Result:='';
    For M:=Low(TProcedureModifier) to High(TProcedureModifier) do
      If (m in S) then
        begin
        If (Result<>'') then
           Result:=Result+',';
        Result:=Result+GetEnumName(TypeInfo(TProcedureModifier),Ord(m))
        end;
  end;
begin
  AssertEquals(Msg,Sn(AExpected),SN(AActual));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TAssignKind);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TAssignKind),Ord(AExpected)),
                   GetEnumName(TypeInfo(TAssignKind),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TProcedureMessageType);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TProcedureMessageType),Ord(AExpected)),
                   GetEnumName(TypeInfo(TProcedureMessageType),Ord(AActual)));
end;

procedure TTestParser.AssertEquals(const Msg: String; AExpected,
  AActual: TOperatorType);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TOperatorType),Ord(AExpected)),
                   GetEnumName(TypeInfo(TOperatorType),Ord(AExpected)));
end;

procedure TTestParser.HaveHint(AHint: TPasMemberHint; AHints: TPasMemberHints);
begin
  If not (AHint in AHints) then
    Fail(GetEnumName(TypeInfo(TPasMemberHint),Ord(AHint))+'hint expected.');
end;

end.

