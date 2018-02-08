{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript precompile class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
   ./testpas2js --suite=TTestPrecompile.TestPC_EmptyUnit
}
unit tcfiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, PScanner, PasResolver, PasResolveEval, PParser,
  FPPas2Js, Pas2JsFiler,
  tcmodules;

type

  { TCustomTestPrecompile }

  TCustomTestPrecompile = Class(TCustomTestModule)
  private
    FInitialFlags: TPJUInitialFlags;
    FPJUReader: TPJUReader;
    FPJUWriter: TPJUWriter;
    procedure OnFilerGetSrc(Sender: TObject; aFilename: string; out p: PChar;
      out Count: integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure WriteReadUnit; virtual;
    procedure StartParsing; override;
    procedure CheckRestoredResolver(Original, Restored: TPas2JSResolver); virtual;
    procedure CheckRestoredDeclarations(const Path: string; Orig, Rest: TPasDeclarations); virtual;
    procedure CheckRestoredSection(const Path: string; Orig, Rest: TPasSection); virtual;
    procedure CheckRestoredModule(const Path: string; Orig, Rest: TPasModule); virtual;
    procedure CheckRestoredModuleScope(const Path: string; Orig, Rest: TPasModuleScope); virtual;
    procedure CheckRestoredIdentifierScope(const Path: string; Orig, Rest: TPasIdentifierScope); virtual;
    procedure CheckRestoredSectionScope(const Path: string; Orig, Rest: TPasSectionScope); virtual;
    procedure CheckRestoredCustomData(const Path: string; El: TPasElement; Orig, Rest: TObject); virtual;
    procedure CheckRestoredElement(const Path: string; Orig, Rest: TPasElement); virtual;
    procedure CheckRestoredConst(const Path: string; Orig, Rest: TPasConst); virtual;
    procedure CheckRestoredVariable(const Path: string; Orig, Rest: TPasVariable); virtual;
    procedure CheckRestoredPrimitiveExpr(const Path: string; Orig, Rest: TPrimitiveExpr); virtual;
    procedure CheckRestoredExpr(const Path: string; Orig, Rest: TPasExpr); virtual;
    procedure CheckRestoredReference(const Path: string; Orig, Rest: TPasElement); virtual;
  public
    property PJUWriter: TPJUWriter read FPJUWriter write FPJUWriter;
    property PJUReader: TPJUReader read FPJUReader write FPJUReader;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
  end;

  { TTestPrecompile }

  TTestPrecompile = class(TCustomTestPrecompile)
  published
    procedure Test_Base256VLQ;
    procedure TestPC_EmptyUnit;

    procedure TestPC_Const;
  end;

implementation

{ TCustomTestPrecompile }

procedure TCustomTestPrecompile.OnFilerGetSrc(Sender: TObject;
  aFilename: string; out p: PChar; out Count: integer);
var
  i: Integer;
  aModule: TTestEnginePasResolver;
  Src: String;
begin
  for i:=0 to ResolverCount-1 do
    begin
    aModule:=Resolvers[i];
    if aModule.Filename<>aFilename then continue;
    Src:=aModule.Source;
    p:=PChar(Src);
    Count:=length(Src);
    end;
end;

procedure TCustomTestPrecompile.SetUp;
begin
  inherited SetUp;
  FInitialFlags:=TPJUInitialFlags.Create;
end;

procedure TCustomTestPrecompile.TearDown;
begin
  FreeAndNil(FPJUWriter);
  FreeAndNil(FPJUReader);
  FreeAndNil(FInitialFlags);
  inherited TearDown;
end;

procedure TCustomTestPrecompile.WriteReadUnit;
var
  ms: TMemoryStream;
  PJU: string;
  ReadResolver: TTestEnginePasResolver;
  ReadFileResolver: TFileResolver;
  ReadScanner: TPascalScanner;
  ReadParser: TPasParser;
begin
  ConvertUnit;

  FPJUWriter:=TPJUWriter.Create;
  FPJUReader:=TPJUReader.Create;
  ms:=TMemoryStream.Create;
  ReadParser:=nil;
  ReadScanner:=nil;
  ReadResolver:=nil;
  ReadFileResolver:=nil;
  try
    try
      PJUWriter.OnGetSrc:=@OnFilerGetSrc;
      PJUWriter.WritePJU(Engine,InitialFlags,ms);
    except
      on E: Exception do
      begin
        {$IFDEF VerbosePas2JS}
        writeln('TCustomTestPrecompile.WriteReadUnit WRITE failed');
        {$ENDIF}
        Fail('Write failed('+E.ClassName+'): '+E.Message);
      end;
    end;

    try
      SetLength(PJU,ms.Size);
      System.Move(ms.Memory^,PJU[1],length(PJU));

      writeln('TCustomTestPrecompile.WriteReadUnit PJU START-----');
      writeln(PJU);
      writeln('TCustomTestPrecompile.WriteReadUnit PJU END-------');

      ReadFileResolver:=TFileResolver.Create;
      ReadScanner:=TPascalScanner.Create(ReadFileResolver);
      InitScanner(ReadScanner);
      ReadResolver:=TTestEnginePasResolver.Create;
      ReadResolver.Filename:=Engine.Filename;
      ReadResolver.AddObjFPCBuiltInIdentifiers(btAllJSBaseTypes,bfAllJSBaseProcs);
      //ReadResolver.OnFindUnit:=@OnPasResolverFindUnit;
      ReadParser:=TPasParser.Create(ReadScanner,ReadFileResolver,ReadResolver);
      ReadParser.Options:=po_tcmodules;
      ReadResolver.CurrentParser:=ReadParser;
      ms.Position:=0;
      PJUReader.ReadPJU(ReadResolver,ms);
    except
      on E: Exception do
      begin
        {$IFDEF VerbosePas2JS}
        writeln('TCustomTestPrecompile.WriteReadUnit READ failed');
        {$ENDIF}
        Fail('Read failed('+E.ClassName+'): '+E.Message);
      end;
    end;

    CheckRestoredResolver(Engine,ReadResolver);
  finally
    ReadParser.Free;
    ReadScanner.Free;
    ReadResolver.Free; // free parser before resolver
    ReadFileResolver.Free;
    ms.Free;
  end;
end;

procedure TCustomTestPrecompile.StartParsing;
begin
  inherited StartParsing;
  FInitialFlags.ParserOptions:=Parser.Options;
  FInitialFlags.ModeSwitches:=Scanner.CurrentModeSwitches;
  FInitialFlags.BoolSwitches:=Scanner.CurrentBoolSwitches;
  FInitialFlags.ConverterOptions:=Converter.Options;
  FInitialFlags.TargetPlatform:=Converter.TargetPlatform;
  FInitialFlags.TargetProcessor:=Converter.TargetProcessor;
  // ToDo: defines
end;

procedure TCustomTestPrecompile.CheckRestoredResolver(Original,
  Restored: TPas2JSResolver);
begin
  AssertNotNull('CheckRestoredResolver Original',Original);
  AssertNotNull('CheckRestoredResolver Restored',Restored);
  if Original.ClassType<>Restored.ClassType then
    Fail('CheckRestoredResolver Original='+Original.ClassName+' Restored='+Restored.ClassName);
  CheckRestoredElement('RootElement',Original.RootElement,Restored.RootElement);
end;

procedure TCustomTestPrecompile.CheckRestoredDeclarations(const Path: string;
  Orig, Rest: TPasDeclarations);
var
  i: Integer;
  OrigDecl, RestDecl: TPasElement;
  SubPath: String;
begin
  for i:=0 to Orig.Declarations.Count-1 do
    begin
    OrigDecl:=TPasElement(Orig.Declarations[i]);
    if i>=Rest.Declarations.Count then
      AssertEquals(Path+': Declarations.Count',Orig.Declarations.Count,Rest.Declarations.Count);
    RestDecl:=TPasElement(Rest.Declarations[i]);
    SubPath:=Path+'['+IntToStr(i)+']';
    if OrigDecl.Name<>'' then
      SubPath:=SubPath+'"'+OrigDecl.Name+'"'
    else
      SubPath:=SubPath+'?noname?';
    CheckRestoredElement(SubPath,OrigDecl,RestDecl);
    end;
  AssertEquals(Path+': Declarations.Count',Orig.Declarations.Count,Rest.Declarations.Count);
end;

procedure TCustomTestPrecompile.CheckRestoredSection(const Path: string; Orig,
  Rest: TPasSection);
begin
  if length(Orig.UsesClause)>0 then
    ; // ToDo
  CheckRestoredDeclarations(Path,Rest,Orig);
end;

procedure TCustomTestPrecompile.CheckRestoredModule(const Path: string; Orig,
  Rest: TPasModule);
begin
  if not (Orig.CustomData is TPasModuleScope) then
    Fail(Path+'.CustomData is not TPasModuleScope'+GetObjName(Orig.CustomData));

  CheckRestoredElement(Path+'.InterfaceSection',Orig.InterfaceSection,Rest.InterfaceSection);
  CheckRestoredElement(Path+'.ImplementationSection',Orig.ImplementationSection,Rest.ImplementationSection);
  if Orig is TPasProgram then
    CheckRestoredElement(Path+'.ProgramSection',TPasProgram(Orig).ProgramSection,TPasProgram(Rest).ProgramSection)
  else if Orig is TPasLibrary then
    CheckRestoredElement(Path+'.LibrarySection',TPasLibrary(Orig).LibrarySection,TPasLibrary(Rest).LibrarySection);
  CheckRestoredElement(Path+'.InitializationSection',Orig.InitializationSection,Rest.InitializationSection);
  CheckRestoredElement(Path+'.FinalizationSection',Orig.FinalizationSection,Rest.FinalizationSection);
end;

procedure TCustomTestPrecompile.CheckRestoredModuleScope(const Path: string;
  Orig, Rest: TPasModuleScope);
begin
  AssertEquals(Path+': FirstName',Orig.FirstName,Rest.FirstName);
  if Orig.Flags<>Rest.Flags then
    Fail(Path+': Flags');
  if Orig.BoolSwitches<>Rest.BoolSwitches then
    Fail(Path+': BoolSwitches');
  CheckRestoredReference(Path+'.AssertClass',Orig.AssertClass,Rest.AssertClass);
  CheckRestoredReference(Path+'.AssertDefConstructor',Orig.AssertDefConstructor,Rest.AssertDefConstructor);
  CheckRestoredReference(Path+'.AssertMsgConstructor',Orig.AssertMsgConstructor,Rest.AssertMsgConstructor);
  CheckRestoredReference(Path+'.RangeErrorClass',Orig.RangeErrorClass,Rest.RangeErrorClass);
  CheckRestoredReference(Path+'.RangeErrorConstructor',Orig.RangeErrorConstructor,Rest.RangeErrorConstructor);
end;

procedure TCustomTestPrecompile.CheckRestoredIdentifierScope(
  const Path: string; Orig, Rest: TPasIdentifierScope);
var
  OrigList: TFPList;
  i: Integer;
  OrigIdentifier, RestIdentifier: TPasIdentifier;
begin
  OrigList:=nil;
  try
    OrigList:=Orig.GetLocalIdentifiers;
    for i:=0 to OrigList.Count-1 do
    begin
      OrigIdentifier:=TPasIdentifier(OrigList[i]);
      RestIdentifier:=Rest.FindLocalIdentifier(OrigIdentifier.Identifier);
      if RestIdentifier=nil then
        Fail(Path+'.Local['+OrigIdentifier.Identifier+'] Missing RestIdentifier Orig='+OrigIdentifier.Identifier);
      repeat
        AssertEquals(Path+'.Local.Identifier',OrigIdentifier.Identifier,RestIdentifier.Identifier);
        CheckRestoredReference(Path+'.Local',OrigIdentifier.Element,RestIdentifier.Element);
        if OrigIdentifier.Kind<>RestIdentifier.Kind then
          Fail(Path+'.Local['+OrigIdentifier.Identifier+'] Orig='+PJUIdentifierKindNames[OrigIdentifier.Kind]+' Rest='+PJUIdentifierKindNames[RestIdentifier.Kind]);
        if OrigIdentifier.NextSameIdentifier=nil then
        begin
          if RestIdentifier.NextSameIdentifier<>nil then
            Fail(Path+'.Local['+OrigIdentifier.Identifier+'] Too many RestIdentifier.NextSameIdentifier='+GetObjName(RestIdentifier.Element));
          break;
        end
        else begin
          if RestIdentifier.NextSameIdentifier=nil then
            Fail(Path+'.Local['+OrigIdentifier.Identifier+'] Missing RestIdentifier.NextSameIdentifier Orig='+GetObjName(OrigIdentifier.NextSameIdentifier.Element));
        end;
        if CompareText(OrigIdentifier.Identifier,OrigIdentifier.NextSameIdentifier.Identifier)<>0 then
          Fail(Path+'.Local['+OrigIdentifier.Identifier+'] Cur.Identifier<>Next.Identifier '+OrigIdentifier.Identifier+'<>'+OrigIdentifier.NextSameIdentifier.Identifier);
        OrigIdentifier:=OrigIdentifier.NextSameIdentifier;
        RestIdentifier:=RestIdentifier.NextSameIdentifier;
      until false;
    end;
  finally
    OrigList.Free;
  end;
end;

procedure TCustomTestPrecompile.CheckRestoredSectionScope(const Path: string;
  Orig, Rest: TPasSectionScope);
var
  i: Integer;
  OrigUses, RestUses: TPasSectionScope;
begin
  AssertEquals(Path+' UsesScopes.Count',Orig.UsesScopes.Count,Rest.UsesScopes.Count);
  for i:=0 to Orig.UsesScopes.Count-1 do
    begin
    OrigUses:=TPasSectionScope(Orig.UsesScopes[i]);
    if not (TObject(Rest.UsesScopes[i]) is TPasSectionScope) then
      Fail(Path+': Uses['+IntToStr(i)+'] Rest='+GetObjName(TObject(Rest.UsesScopes[i])));
    RestUses:=TPasSectionScope(Rest.UsesScopes[i]);
    if OrigUses.ClassType<>RestUses.ClassType then
      Fail(Path+': Uses['+IntToStr(i)+'] Orig='+GetObjName(OrigUses)+' Rest='+GetObjName(RestUses));
    CheckRestoredReference(Path+': Uses['+IntToStr(i)+']',OrigUses.Element,RestUses.Element);
    end;
  AssertEquals(Path+': Finished',Orig.Finished,Rest.Finished);
  CheckRestoredIdentifierScope(Path,Orig,Rest);
end;

procedure TCustomTestPrecompile.CheckRestoredCustomData(const Path: string;
  El: TPasElement; Orig, Rest: TObject);
var
  C: TClass;
begin
  if Orig=nil then
    begin
    if Rest<>nil then
      Fail(Path+': Orig=nil Rest='+GetObjName(Rest));
    exit;
    end
  else if Rest=nil then
    Fail(Path+': Orig='+GetObjName(Orig)+' Rest=nil');
  if Orig.ClassType<>Rest.ClassType then
    Fail(Path+': Orig='+GetObjName(Orig)+' Rest='+GetObjName(Rest));

  C:=Orig.ClassType;
  if C=TPasModuleScope then
    CheckRestoredModuleScope(Path+'[TPasModuleScope]',TPasModuleScope(Orig),TPasModuleScope(Rest))
  else if C=TPasSectionScope then
    CheckRestoredSectionScope(Path+'[TPasSectionScope]',TPasSectionScope(Orig),TPasSectionScope(Rest))
  else
    Fail(Path+': unknown CustomData "'+GetObjName(Orig)+'" El='+GetObjName(El));
end;

procedure TCustomTestPrecompile.CheckRestoredElement(const Path: string; Orig,
  Rest: TPasElement);
var
  C: TClass;
begin
  if Orig=nil then
    begin
    if Rest<>nil then
      Fail(Path+': Orig=nil Rest='+GetObjName(Rest));
    exit;
    end
  else if Rest=nil then
    Fail(Path+': Orig='+GetObjName(Orig)+' Rest=nil');
  if Orig.ClassType<>Rest.ClassType then
    Fail(Path+': Orig='+GetObjName(Orig)+' Rest='+GetObjName(Rest));

  AssertEquals(Path+': Name',Orig.Name,Rest.Name);
  AssertEquals(Path+': SourceFilename',Orig.SourceFilename,Rest.SourceFilename);
  AssertEquals(Path+': SourceLinenumber',Orig.SourceLinenumber,Rest.SourceLinenumber);
  //AssertEquals(Path+': SourceEndLinenumber',Orig.SourceEndLinenumber,Rest.SourceEndLinenumber);
  if Orig.Visibility<>Rest.Visibility then
    Fail(Path+': Visibility '+PJUMemberVisibilityNames[Orig.Visibility]+' '+PJUMemberVisibilityNames[Rest.Visibility]);
  if Orig.Hints<>Rest.Hints then
    Fail(Path+': Hints');
  AssertEquals(Path+': HintMessage',Orig.HintMessage,Rest.HintMessage);

  if Orig.Parent=nil then
    begin
    if Rest.Parent<>nil then
      Fail(Path+': Orig.Parent=nil Rest.Parent='+GetObjName(Rest.Parent));
    end
  else if Rest.Parent=nil then
    Fail(Path+': Orig.Parent='+GetObjName(Orig.Parent)+' Rest.Parent=nil')
  else if Orig.Parent.ClassType<>Rest.Parent.ClassType then
    Fail(Path+': Orig.Parent='+GetObjName(Orig.Parent)+' Rest.Parent='+GetObjName(Rest.Parent));

  CheckRestoredCustomData(Path+'.CustomData',Rest,Orig.CustomData,Rest.CustomData);

  C:=Orig.ClassType;
  if (C=TPasModule)
      or (C=TPasProgram)
      or (C=TPasLibrary) then
    CheckRestoredModule(Path,TPasModule(Orig),TPasModule(Rest))
  else if C.InheritsFrom(TPasSection) then
    CheckRestoredSection(Path,TPasSection(Orig),TPasSection(Rest))
  else if C=TPasConst then
    CheckRestoredConst(Path,TPasConst(Orig),TPasConst(Rest))
  else if C=TPasVariable then
    CheckRestoredVariable(Path,TPasVariable(Orig),TPasVariable(Rest))
  else if C=TPrimitiveExpr then
    CheckRestoredPrimitiveExpr(Path,TPrimitiveExpr(Orig),TPrimitiveExpr(Rest))
  else
    Fail(Path+': unknown class '+C.ClassName);
end;

procedure TCustomTestPrecompile.CheckRestoredConst(const Path: string; Orig,
  Rest: TPasConst);
begin
  AssertEquals(Path+': IsConst',Orig.IsConst,Rest.IsConst);
  CheckRestoredVariable(Path,Orig,Rest);
end;

procedure TCustomTestPrecompile.CheckRestoredVariable(const Path: string; Orig,
  Rest: TPasVariable);
begin
  CheckRestoredElement(Path+'.VarType',Orig.VarType,Rest.VarType);
  if Orig.VarModifiers<>Rest.VarModifiers then
    Fail(Path+'.VarModifiers');
  CheckRestoredElement(Path+'.LibraryName',Orig.LibraryName,Rest.LibraryName);
  CheckRestoredElement(Path+'.ExportName',Orig.ExportName,Rest.ExportName);
  CheckRestoredElement(Path+'.AbsoluteExpr',Orig.AbsoluteExpr,Rest.AbsoluteExpr);
  CheckRestoredElement(Path+'.Expr',Orig.Expr,Rest.Expr);
end;

procedure TCustomTestPrecompile.CheckRestoredPrimitiveExpr(const Path: string;
  Orig, Rest: TPrimitiveExpr);
begin
  AssertEquals(Path+'.Value',Orig.Value,Rest.Value);
  CheckRestoredExpr(Path,Orig,Rest);
end;

procedure TCustomTestPrecompile.CheckRestoredExpr(const Path: string; Orig,
  Rest: TPasExpr);
begin
  if Orig.Kind<>Rest.Kind then
    Fail(Path+'.Kind');
  if Orig.OpCode<>Rest.OpCode then
    Fail(Path+'.OpCode');
  CheckRestoredElement(Path+'.Format1',Orig.format1,Rest.format1);
  CheckRestoredElement(Path+'.Format2',Orig.format2,Rest.format2);
end;

procedure TCustomTestPrecompile.CheckRestoredReference(const Path: string;
  Orig, Rest: TPasElement);
begin
  if Orig=nil then
    begin
    if Rest<>nil then
      Fail(Path+': Orig=nil Rest='+GetObjName(Rest));
    exit;
    end
  else if Rest=nil then
    Fail(Path+': Orig='+GetObjName(Orig)+' Rest=nil');
  if Orig.ClassType<>Rest.ClassType then
    Fail(Path+': Orig='+GetObjName(Orig)+' Rest='+GetObjName(Rest));
  AssertEquals(Path+': Name',Orig.Name,Rest.Name);

  if Orig is TPasUnresolvedSymbolRef then
    exit; // compiler types and procs are the same in every unit -> skip checking unit

  CheckRestoredReference(Path+'.Parent',Orig.Parent,Rest.Parent);
end;

{ TTestPrecompile }

procedure TTestPrecompile.Test_Base256VLQ;

  procedure Test(i: MaxPrecInt);
  var
    s: String;
    p: PByte;
    j: NativeInt;
  begin
    s:=EncodeVLQ(i);
    p:=PByte(s);
    j:=DecodeVLQ(p);
    if i<>j then
      Fail('Encode/DecodeVLQ OrigIndex='+IntToStr(i)+' Code="'+s+'" NewIndex='+IntToStr(j));
  end;

  procedure TestStr(i: MaxPrecInt; Expected: string);
  var
    Actual: String;
  begin
    Actual:=EncodeVLQ(i);
    AssertEquals('EncodeVLQ('+IntToStr(i)+')',Expected,Actual);
  end;

var
  i: Integer;
begin
  TestStr(0,#0);
  TestStr(1,#2);
  TestStr(-1,#3);
  for i:=-8200 to 8200 do
    Test(i);
  Test(High(MaxPrecInt));
  Test(High(MaxPrecInt)-1);
  Test(Low(MaxPrecInt)+2);
  Test(Low(MaxPrecInt)+1);
  //Test(Low(MaxPrecInt)); such a high number is not needed by pastojs
end;

procedure TTestPrecompile.TestPC_EmptyUnit;
begin
  StartUnit(false);
  Add([
  'interface',
  'implementation']);
  WriteReadUnit;
end;

procedure TTestPrecompile.TestPC_Const;
begin
  StartUnit(false);
  Add([
  'interface',
  'const c = 3;',
  'implementation']);
  WriteReadUnit;
end;

Initialization
  RegisterTests([TTestPrecompile]);
end.

