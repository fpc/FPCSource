{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    Data Dictionary Code Generator Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpcgtiopf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpddcodegen;
  
TYpe
  TClassOption = (caCreateClass,caConstructor,caDestructor,caCreateList,caListAddMethod,caListItemsProperty);
  TClassOptions = Set of TClassOption;
  TVisitorOption = (voRead,voReadList,voCreate,voDelete,voUpdate);
  TVisitorOptions = set of TVisitorOption;
  
  { TTiOPFCodeOptions }

  TTiOPFCodeOptions = Class (TClassCodeGeneratorOptions)
  Private
    FClassOptions: TClassOptions;
    FListAncestorName: String;
    FListClassName : String;
    FVisitorOptions: TVisitorOptions;
    FTableName : String;
    function GetListClassName: String;
    procedure SetListAncestorName(const AValue: String);
    procedure SetListClassName(const AValue: String);
  Public
    Constructor Create; override;
    Procedure Assign(ASource : TPersistent); override;
  Published
    Property ClassOptions : TClassOptions Read FClassOptions Write FClassOptions;
    Property VisitorOptions : TVisitorOptions Read FVisitorOptions Write FVisitorOptions;
    Property ListAncestorName : String Read FListAncestorName Write SetListAncestorName;
    Property ListClassName : String Read GetListClassName Write SetListClassName;
    Property AncestorClass;
    Property TableName : String Read FTableName Write FTableName;
  end;
  
  { TTiOPFCodeGenerator }

  TTiOPFCodeGenerator = Class(TDDClassCodeGenerator)
    procedure CreateListImplementation(Strings: TStrings; const ObjectClassName, ListClassName: String);
    function BeginInit(Strings: TStrings; const AClass: String): String;
    function BeginAcceptVisitor(Strings: TStrings; const AClass, ObjectClassName: String): String;
    function BeginSetupParams(Strings: TStrings; const AClass,ObjectClassName: String; DeclareObject : Boolean): String;
    function BeginMapRowToObject(Strings: TStrings; const AClass, ObjectClassName : String): String;
    procedure DeclareObjectvariable(Strings: TStrings;
      const ObjectClassName: String);
  private
    Function CreateSQLStatement(V: TVisitorOption) : String;
    function GetOpt: TTiOPFCodeOptions;
    procedure WriteCreateVisitor(Strings: TStrings; const ObjectClassName: String);
    procedure WriteDeleteVisitor(Strings: TStrings; const ObjectClassName: String);
    procedure WriteFieldAssign(Strings: TStrings; F: TFieldPropDef);
    procedure WriteAssignToParam(Strings: TStrings; F: TFieldPropDef);
    procedure WriteReadListVisitor(Strings: TStrings; const ObjectClassName: String);
    procedure WriteReadVisitor(Strings: TStrings; const ObjectClassName: String );
    procedure WriteSetSQL(Strings: TStrings; const ASQL: String);
    procedure WriteSQLConstants(Strings: TStrings);
    procedure WriteUpdateVisitor(Strings: TStrings; const ObjectClassName: String);
    procedure WriteVisitorDeclaration(Strings: TStrings; V: TVisitorOption; const ObjectClassName: String);
    procedure WriteVisitorImplementation(Strings: TStrings; V: TVisitorOption; const ObjectClassName: String);
  Protected
    // Not to be overridden.
    procedure WriteListAddObject(Strings: TStrings; const ListClassName, ObjectClassName: String);
    // Overrides of parent objects
    function AllowPropertyDeclaration(F: TFieldPropDef; AVisibility: TVisibilities): Boolean; override;
    Function GetInterfaceUsesClause : string; override;
    Procedure DoGenerateInterface(Strings: TStrings); override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
    Function NeedsConstructor : Boolean; override;
    Function NeedsDestructor : Boolean; override;
    Class Function NeedsFieldDefs : Boolean; override;
    Function CreateOptions : TCodeGeneratorOptions; override;
    //
    //  New methods
    //
    // Override to add declarations to list declaration
    procedure DoCreateListDeclaration(Strings: TStrings; const ObjectClassName, ListClassName, ListAncestorName: String); virtual;
  Public
    procedure CreateListDeclaration(Strings: TStrings; const ObjectClassName, ListClassName, ListAncestorName: String);
    Property TiOPFOptions : TTiOPFCodeOptions Read GetOpt;
  end;

Const
  SOID = 'OID';              // OID property.
  SDefTableName = 'MYTABLE'; // Default table name.
  
implementation

{ TTiOPFCodeOptions }

function TTiOPFCodeOptions.GetListClassName: String;
begin
  Result:=FListClassName;
  If (Result='') then
    Result:=ObjectClassName+'List';
end;

procedure TTiOPFCodeOptions.SetListAncestorName(const AValue: String);
begin
  CheckIdentifier(AValue,False);
  FListAncestorName:=AValue;
end;

procedure TTiOPFCodeOptions.SetListClassName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FListClassName:=AValue;
end;

constructor TTiOPFCodeOptions.Create;
begin
  inherited Create;
  FListAncestorName:='TtiObjectList';
  AncestorClass:='TtiObject';
  ObjectClassName:='MyObject';
  TableName:=SDefTableName;
  FVisitorOptions:=[voRead,voCreate,voDelete,voUpdate];
  FClassOptions:=[caCreateList,caListAddMethod,caListItemsProperty];
end;

procedure TTiOPFCodeOptions.Assign(ASource: TPersistent);

Var
  OC : TTiOPFCodeOptions;
  
begin
  If ASource is TTiOPFCodeOptions then
    begin
    OC:=ASource as TTiOPFCodeOptions;
    FListAncestorName:=OC.FListAncestorName;
    AncestorClass:=OC.AncestorClass;
    FVisitorOptions:=OC.FVisitorOptions;
    FClassOptions:=OC.FClassOptions;
    FTableName:=OC.TableName;
    end;
  inherited Assign(ASource);
end;

{ TTiOPFCodeGenerator }

{ ---------------------------------------------------------------------
  General overrides
  ---------------------------------------------------------------------}

function TTiOPFCodeGenerator.NeedsConstructor: Boolean;
begin
  Result:=inherited NeedsConstructor;
  Result:=Result or (caConstructor in TiOPFOptions.ClassOptions);
end;

function TTiOPFCodeGenerator.NeedsDestructor: Boolean;
begin
  Result:=inherited NeedsDestructor;
  Result:=Result or (caDestructor in TiOPFOptions.ClassOptions);
end;

class function TTiOPFCodeGenerator.NeedsFieldDefs: Boolean;
begin
  Result:=True;
end;

function TTiOPFCodeGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TTiOPFCodeOptions.Create;
end;
function TTiOPFCodeGenerator.GetOpt: TTiOPFCodeOptions;
begin
  Result:=CodeOptions as TTiOPFCodeOptions;
end;

function TTiOPFCodeGenerator.GetInterfaceUsesClause: string;
begin
  Result:=inherited GetInterfaceUsesClause;
  If (Result<>'') then
    Result:=Result+', ';
  Result:=Result+'tiVisitor, tiVisitorDB, tiObject';
end;

procedure TTiOPFCodeGenerator.DoGenerateInterface(Strings: TStrings);

Var
  V : TVisitorOption;

begin
  If (caCreateClass in TiOPFOptions.ClassOptions) then
    inherited DoGenerateInterface(Strings)
  else
    begin
    Addln(Strings,'Type');
    Addln(Strings);
    end;
  With TiOPFOptions do
    begin
    IncIndent;
    try
      If caCreateList in ClassOptions then
        CreateListDeclaration(Strings,ObjectClassName,ListClassName,ListAncestorName);
      For V:=Low(TVisitorOption) to High(TVisitorOption) do
        If V in VisitorOptions then
          WriteVisitorDeclaration(Strings,V,ObjectClassName);
    Finally
      DecIndent;
    end;
    end;
end;

Function StripType(S : String) : string;

begin
  Result:=S;
  If (Result<>'') and (Result[1]='T') then
    Delete(Result,1,1);
end;

procedure TTiOPFCodeGenerator.WriteVisitorDeclaration(Strings : TStrings; V : TVisitorOption; Const ObjectClassName : String);

Var
  S,T,A : string;

begin
  Case V of
    voRead     : S:='Read';
    voReadList : S:='ReadList';
    voCreate   : S:='Create';
    voDelete   : S:='Delete';
    voUpdate   : S:='Update';
  end;
  If V in [voCreate,voDelete,voUpdate] then
    A:='Update'
  else
    A:='Select';
  S:=Format('T%s%sVisitor',[S,StripType(ObjectClassName)]);
  AddLn(Strings,'{ %s }',[S]);
  AddlN(Strings,'%s = Class(TtiVisitor%s)',[S,A]);
  AddlN(Strings,'Protected');
  IncIndent;
  Try
    AddLn(Strings,'Procedure Init; override;');
    AddLn(Strings,'Function AcceptVisitor : Boolean; override;');
    AddLn(Strings,'Procedure SetupParams; override;');
    If Not (V in [voCreate,voDelete,voUpdate]) then
      AddLn(Strings,'Procedure MapRowToObject; override;');
  Finally
    DecIndent;
  end;
  AddlN(Strings,'end;');
  AddlN(Strings);
end;

Function TTiOPFCodeGenerator.CreateSQLStatement(V : TVisitorOption) : String;

  Function AddToS(Const S,Add : String) : string;
  
  begin
    Result:=S;
    If (Result<>'') then
      Result:=Result+', ';
    Result:=Result+Add;
  end;

Var
  I : integer;
  W,S,VS,TN : String;
  F : TFieldPropDef;

begin
  TN:=TiOPFOptions.TableName;
  If (TN='') then 
    TN:=SDefTableName;
  S:='';
  VS:='';
  W:='Your condition here';
  Result:='';
  Case V of
   voRead,
   voReadList : begin
                Result:='SELECT ';
                For I:=0 to Fields.Count-1 do
                  begin
                  F:=Fields[i];
                  If F.Enabled then
                    begin
                    S:=AddToS(S,F.FieldName);
                    If (V=voRead) and (F.PropertyName=SOID) then
                      W:=Format('%s = :%s',[F.FieldName,F.FieldName]);
                    end;
                  end;
                Result:=Result+S+Format(' FROM %s WHERE (%s);',[TN,W]);
                end;
   voCreate : begin
              Result:=Format('INSERT INTO %s (',[TN]);
              For I:=0 to Fields.Count-1 do
                begin
                F:=Fields[i];
                If F.Enabled then
                  begin
                  S:=AddToS(S,F.FieldName);
                  VS:=AddToS(VS,':'+F.FieldName);
                  end;
                end;
              Result:=Result+S+') VALUES ('+VS+');';
              end;
   voDelete : begin
              For I:=0 to Fields.Count-1 do
                begin
                F:=Fields[i];
                If (F.PropertyName=SOID) then
                  W:=Format('%s = :%s',[F.FieldName,F.FieldName]);
                end;
              Result:=Format('DELETE FROM %s WHERE (%s);',[TN,W]);
              end;
   voUpdate : begin
              Result:=Format('UPDATE %s SET ',[TN]);
              For I:=0 to Fields.Count-1 do
                 begin
                  F:=Fields[i];
                  If F.Enabled then
                    If (F.PropertyName=SOID) then
                      W:=Format('%s = :%s',[F.FieldName,F.FieldName])
                    else
                      S:=AddToS(S,F.FieldName+' = :'+F.FieldName);
                  end;
              Result:=Result+S+Format(' WHERE (%s);',[W]);
              end;
  end;
end;

procedure TTiOPFCodeGenerator.WriteSQLConstants(Strings : TStrings);

Const
  VisSQL : Array [TVisitorOption] of string
         = ('Read','ReadList','Create','Delete','Update');

Var
  OCN,S : String;
  V : TVisitorOption;

begin
  AddLn(Strings,'Const');
  IncIndent;
  try
    OCN:=StripType(TiOPFOptions.ObjectClassName);
    For V:=Low(TVisitorOption) to High(TVisitorOption) do
      If V in TiOPFOptions.VisitorOptions then
        begin
        S:=CreateSQLStatement(V);
        S:=Format('SQL%s%s = ''%s'';',[VisSQL[V],OCN,S]);
        AddLn(Strings,S);
        end;
  finally
    DecIndent;
  end;
  AddLn(Strings,'');
end;


procedure TTiOPFCodeGenerator.DoGenerateImplementation(Strings: TStrings);

Var
  V : TVisitorOption;

begin
  If (caCreateClass in TiOPFOptions.ClassOptions) then
    inherited DoGenerateImplementation(Strings);
  With TiOPFOptions do
    begin
    If (VisitorOptions<>[])   then
      WriteSQLConstants(Strings);
    If caCreateList in ClassOptions then
      CreateListImplementation(Strings,ObjectClassName,ListClassName);
    For V:=Low(TVisitorOption) to High(TVisitorOption) do
      If V in VisitorOptions then
        WriteVisitorImplementation(Strings,V,ObjectClassName);
    end;
end;

{ ---------------------------------------------------------------------
  Visitor helper routines
  ---------------------------------------------------------------------}

procedure TTiOPFCodeGenerator.WriteVisitorImplementation(Strings : TStrings; V : TVisitorOption; Const ObjectClassName : String);

begin
  Case V of
    voRead     : WriteReadVisitor(Strings,ObjectClassName);
    voReadList : WriteReadListVisitor(Strings,ObjectClassName);
    voCreate   : WriteCreateVisitor(Strings,ObjectClassName);
    voDelete   : WriteDeleteVisitor(Strings,ObjectClassName);
    voUpdate   : WriteUpdateVisitor(Strings,ObjectClassName);
  end;
end;

Function TTiOPFCodeGenerator.BeginInit(Strings : TStrings; const AClass : String) : String;

begin
  Result:=Format('Procedure %s.Init;',[AClass]);
  BeginMethod(Strings,Result);
  AddLn(Strings,'begin');
  IncIndent;
end;

Function TTiOPFCodeGenerator.BeginAcceptVisitor(Strings : TStrings; Const AClass, ObjectClassName: String) : String;

begin
  Result:=Format('Function %s.AcceptVisitor : Boolean;',[AClass]);
  BeginMethod(Strings,Result);
  AddLn(Strings,'begin');
  IncIndent;
  AddLn(Strings,'Result:=Visited is %s;',[ObjectClassName]);
end;

Function TTiOPFCodeGenerator.BeginSetupParams(Strings : TStrings; const AClass,ObjectClassName : String; DeclareObject : Boolean) : String;

begin
  Result:=Format('Procedure %s.SetupParams;',[AClass]);
  BeginMethod(Strings,Result);
  If DeclareObject Then
    DeclareObjectVariable(Strings,ObjectClassName);
  AddLn(Strings,'begin');
  IncIndent;
  If DeclareObject Then
    Addln(Strings,'O:=%s(Visited);',[ObjectClassName]);
end;

Procedure TTiOPFCodeGenerator.DeclareObjectvariable(Strings : TStrings; Const ObjectClassName : String);

begin
  AddLn(Strings,'var');
  IncIndent;
  try
    AddLn(Strings,'O : %s;',[ObjectClassName]);
    AddLn(Strings);
  finally
    DecIndent;
  end;
end;

Function TTiOPFCodeGenerator.BeginMapRowToObject(Strings : TStrings; Const AClass,ObjectClassName : String) : String;

begin
  Result:=Format('Procedure %s.MapRowToObject;',[AClass]);
  BeginMethod(Strings,Result);
  DeclareObjectVariable(Strings,ObjectClassName);
  AddLn(Strings,'begin');
  IncIndent;
end;

{ ---------------------------------------------------------------------
  Read Visitor
  ---------------------------------------------------------------------}

procedure TTiOPFCodeGenerator.WriteReadVisitor(Strings : TStrings; Const ObjectClassName : String);

Var
  OCN,CS,C,S : String;
  I : Integer;
  F : TFieldPropDef;

begin
  OCN:=StripType(ObjectClassName);
  CS:=Format('SQLRead%s',[OCN]);
  C:=Format('TRead%sVisitor',[OCN]);
  Addln(Strings,'{ %s }',[C]);
  Addln(Strings);
  // Init
  S:=BeginInit(Strings,C);
  WriteSetSQL(Strings,CS);
  DecIndent;
  EndMethod(Strings,S);
  // AcceptVisitor
  S:=BeginAcceptVisitor(Strings,C,ObjectClassName);
  DecIndent;
  EndMethod(Strings,S);
  // AcceptSetupParams
  F:=Fields.FindPropName('OID');
  S:=BeginSetupParams(Strings,C,ObjectClassName,F<>Nil);
  If (F<>Nil) then
    WriteAssignToParam(Strings,F)
  else
    AddLn(Strings,'// Set up as needed');
  DecIndent;
  EndMethod(Strings,S);
  // MapRowToObject
  S:=BeginMapRowToObject(Strings,C,ObjectClassName);
  Addln(Strings,'O:=%s(Visited);',[ObjectClassName]);
  Addln(Strings,'With Query do',[ObjectClassName]);
  IncINdent;
  try
    Addln(Strings,'begin');
    For I:=0 to Fields.Count-1 do
      If Fields[i].Enabled then
        WriteFieldAssign(Strings,Fields[i]);
    Addln(Strings,'end;');
  finally
    DecIndent;
  end;
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TTiOPFCodeGenerator.WriteFieldAssign(Strings : TStrings; F : TFieldPropDef);

Var
  PN,FN,SFN,R,S : String;

begin
  PN:=F.PropertyName;
  FN:=F.FieldName;
  SFN:=CreateString(FN);
  If (PN=SOID) then
    R:=Format('O.OID.AssignFromTIQuery(''%s'',Query);',[FN])
  else
    Case F.PropertyType of
      ptBoolean :
        S:='AsBoolean';
      ptShortint, ptByte,
      ptSmallInt, ptWord,
      ptLongint, ptCardinal :
        S:='AsInteger';
      ptInt64, ptQWord:
        If F.FieldType=ftLargeInt then
          R:=Format('O.%s:=(FieldByName(%s) as TLargeIntField).AsLargeInt;',[PN,SFN])
        else
          S:='AsInteger';
      ptShortString, ptAnsiString, ptWideString :
        S:='AsString';
      ptSingle, ptDouble, ptExtended, ptComp :
        S:='AsFloat';
      ptCurrency :
        S:='AsCurrency';
      ptDateTime :
        S:='AsDateTime';
      ptEnumerated :
        R:=Format('Integer(O.%s):=FieldAsInteger[%s];',[PN,SFN]);
      ptSet :
        S:=Format('// Add custom set loading code here for %s from %s',[PN,FN]);
      ptStream :
        R:=Format('AssignFieldAsStream(%s,O.%s);',[SFN,PN]);
      ptTStrings :
        R:=Format('O.%s.Text:=FieldAsString[%s];',[PN,SFN]);
      ptCustom :
        R:=Format('// Add custom loading code here for %s from %s',[PN,FN]);
    end;
  If (S<>'') then
    R:=Format('O.%s:=Field%s[%s];',[PN,S,SFN]);
  AddLn(Strings,R);
end;

procedure TTiOPFCodeGenerator.WriteAssignToParam(Strings : TStrings; F : TFieldPropDef);

Var
  PN,FN,SFN,R,S : String;

begin
  PN:=F.PropertyName;
  FN:=F.FieldName;
  SFN:=CreateString(FN);
  If (PN=SOID) then
    R:=Format('O.OID.AssignToTIQuery(''%s'',Query);',[FN])
  else
    Case F.PropertyType of
      ptBoolean :
        S:='AsBoolean';
      ptShortint, ptByte,
      ptSmallInt, ptWord,
      ptLongint, ptCardinal :
        S:='AsInteger';
      ptInt64, ptQWord:
        If F.FieldType=ftLargeInt then
          R:=Format('O.%s:=(Name(%s) as TLargeIntField).AsLargeInt;',[PN,SFN])
        else
          S:='AsInteger';
      ptShortString, ptAnsiString, ptWideString :
        S:='AsString';
      ptSingle, ptDouble, ptExtended, ptComp :
        S:='AsFloat';
      ptCurrency :
        S:='AsCurrency';
      ptDateTime :
        S:='AsDateTime';
      ptEnumerated :
        R:=Format('ParamAsInteger[%s]:=Integer(O.%s);',[SFN,PN]);
      ptSet :
        S:=Format('// Add custom set loading code here for %s from %s',[PN,FN]);
      ptStream :
        R:=Format('AssignParamFromStream(%s,O.%s);',[SFN,PN]);
      ptTStrings :
        R:=Format('ParamAsString[%s]:=O.%s.Text;',[SFN,PN]);
      ptCustom :
        R:=Format('// Add custom loading code here for %s from %s',[PN,FN]);
    end;
  If (S<>'') then
    R:=Format('Param%s[%s]:=O.%s;',[S,SFN,PN]);
  AddLn(Strings,R);
end;

{ ---------------------------------------------------------------------
  List Read Visitor
  ---------------------------------------------------------------------}

procedure TTiOPFCodeGenerator.WriteReadListVisitor(Strings : TStrings; Const ObjectClassName : String);

Var
  OCN,CS,C,S,LN : String;
  I : Integer;

begin
  LN:=tiOPFOptions.ListClassName;
  OCN:=StripType(ObjectClassName);
  CS:=Format('SQLReadList%s',[OCN]);
  C:=Format('TReadList%sVisitor',[StripType(OCN)]);
  Addln(Strings,'{ %s }',[C]);
  Addln(Strings);
  // Init
  S:=BeginInit(Strings,C);
  WriteSetSQL(Strings,CS);
  DecIndent;
  EndMethod(Strings,C);
  // AcceptVisitor
  S:=BeginAcceptVisitor(Strings,C,LN);
  DecIndent;
  EndMethod(Strings,S);
  // AcceptSetupParams
  S:=BeginSetupParams(Strings,C,'',False);
  DecIndent;
  EndMethod(Strings,S);
  // MapRowToObject
  S:=BeginMapRowToObject(Strings,C,ObjectClassName);
  Addln(Strings,'O:=%s.Create;',[ObjectClassName]);
  Addln(Strings,'With Query do',[ObjectClassName]);
  IncINdent;
  try
    Addln(Strings,'begin');
    For I:=0 to Fields.Count-1 do
      If Fields[i].Enabled then
        WriteFieldAssign(Strings,Fields[i]);
    Addln(Strings,'end;');
  finally
    DecIndent;
  end;
  Addln(Strings,'O.ObjectState:=posClean;');
  Addln(Strings,'%s(Visited).Add(O);',[LN]);
  DecIndent;
  EndMethod(Strings,S);
end;

{ ---------------------------------------------------------------------
  Create Visitor
  ---------------------------------------------------------------------}

procedure TTiOPFCodeGenerator.WriteCreateVisitor(Strings : TStrings; Const ObjectClassName : String);


Var
  OCN,CS,C,S : String;
  I : Integer;

begin
  OCN:=StripType(ObjectClassName);
  CS:=Format('SQLCreate%s',[OCN]);
  C:=Format('TCreate%sVisitor',[OCN]);
  Addln(Strings,'{ %s }',[C]);
  Addln(Strings);
  // Init
  S:=BeginInit(Strings,C);
  WriteSetSQL(Strings,CS);
  DecIndent;
  EndMethod(Strings,S);
  // AcceptVisitor
  S:=BeginAcceptVisitor(Strings,C,ObjectClassName);
  AddLn(Strings,'Result:=Result and (Visited.ObjectState=posCreate);');
  DecIndent;
  EndMethod(Strings,S);
  // SetupParams
  S:=BeginSetupParams(Strings,C,ObjectClassName,True);
  Addln(Strings,'With Query do',[ObjectClassName]);
  IncINdent;
  try
    Addln(Strings,'begin');
    For I:=0 to Fields.Count-1 do
      If Fields[i].Enabled then
        WriteAssignToParam(Strings,Fields[i]);
    Addln(Strings,'end;');
  finally
    DecIndent;
  end;
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TTiOPFCodeGenerator.WriteSetSQL(Strings : TStrings; Const ASQL : String);

begin
  Addln(Strings,Format('Query.SQLText:=%s;',[ASQL]));
end;

procedure TTiOPFCodeGenerator.WriteDeleteVisitor(Strings : TStrings; Const ObjectClassName : String);

Var
  OCN,CS, C,S : String;
  F : TFieldPropDef;
  
begin
  OCN:=StripType(ObjectClassName);
  CS:=Format('SQLDelete%s',[OCN]);
  C:=Format('TDelete%sVisitor',[StripType(ObjectClassName)]);
  Addln(Strings,'{ %s }',[C]);
  // Init
  S:=BeginInit(Strings,C);
  WriteSetSQL(Strings,CS);
  DecIndent;
  EndMethod(Strings,S);
  // AcceptVisitor
  S:=BeginAcceptVisitor(Strings,C,ObjectClassName);
  AddLn(Strings,'Result:=Result and (Visited.ObjectState=posDelete);');
  DecIndent;
  EndMethod(Strings,S);
  // SetupParams
  S:=BeginSetupParams(Strings,C,ObjectClassName,True);
  F:=Fields.FindPropName('OID');
  If (F<>Nil) then
    WriteAssignToParam(Strings,F)
  else
    AddLn(Strings,'// Add parameter setup code here ');
  DecIndent;
  EndMethod(Strings,S);
end;

procedure TTiOPFCodeGenerator.WriteUpdateVisitor(Strings : TStrings; Const ObjectClassName : String);

Var
  OCN,CS,C,S : String;
  I : Integer;

begin
  OCN:=StripType(ObjectClassName);
  CS:=Format('SQLUpdate%s',[OCN]);
  C:=Format('TUpdate%sVisitor',[OCN]);
  Addln(Strings,'{ %s }',[C]);
  Addln(Strings);
  // Init
  S:=BeginInit(Strings,C);
  WriteSetSQl(Strings,CS);
  DecIndent;
  EndMethod(Strings,S);
  // AcceptVisitor
  S:=BeginAcceptVisitor(Strings,C,ObjectClassName);
  AddLn(Strings,'Result:=Result and (Visited.ObjectState=posUpdate);');
  DecIndent;
  EndMethod(Strings,S);
  // SetupParams
  S:=BeginSetupParams(Strings,C,ObjectClassName,True);
  Addln(Strings,'With Query do',[ObjectClassName]);
  IncINdent;
  try
    Addln(Strings,'begin');
    For I:=0 to Fields.Count-1 do
      If Fields[i].Enabled then
        WriteAssignToParam(Strings,Fields[i]);
    Addln(Strings,'end;');
  finally
    DecIndent;
  end;
  DecIndent;
  EndMethod(Strings,S);
end;




{ ---------------------------------------------------------------------
  List object commands
  ---------------------------------------------------------------------}

procedure TTiOPFCodeGenerator.DoCreateListDeclaration(Strings: TStrings;
  const ObjectClassName, ListClassName, ListAncestorName: String);
begin
  If caListItemsProperty in tiOPFOptions.ClassOptions then
    begin
    AddLn(Strings,'Private');
    IncIndent;
    Try
      AddLn(Strings,'Function GetObj(AIndex : Integer) : %s;',[ObjectClassname]);
      AddLn(Strings,'Procedure SetObj(AIndex : Integer; AValue : %s);',[ObjectClassname]);
    Finally
      DecIndent;
    end;
    end;
  If (caListAddMethod in tiOPFOptions.ClassOptions) then
    begin
    AddLn(Strings,'Public');
    IncIndent;
    Try
      Addln(Strings,'Function Add(AnItem : %s) : Integer; reintroduce;',[ObjectClassName]);
    Finally
      DecIndent;
    end;
    end;
  If (caListItemsProperty in tiOPFOptions.ClassOptions) then
    begin
    If Not (caListAddMethod in tiOPFOptions.ClassOptions) then
      AddLn(Strings,'Public');
    IncIndent;
    Try
      AddLn(Strings,'Property Items[Index : Integer] : %s Read GetObj Write SetObj; Default;',[ObjectClassname]);
    Finally
      DecIndent;
    end;
    end;
end;

procedure TTiOPFCodeGenerator.CreateListDeclaration(Strings: TStrings;
  const ObjectClassName, ListClassName, ListAncestorName: String);
begin
  Addln(Strings);
  Addln(Strings,'{ %s }',[ListClassName]);
  Addln(Strings);
  Addln(Strings,'%s = Class(%s)',[ListClassName,ListAncestorName]);
  DoCreateListDeclaration(Strings,ObjectClassName,ListClassName,ListAncestorName);
  AddLn(Strings,'end;');
  Addln(Strings);
end;

procedure TTiOPFCodeGenerator.WriteListAddObject(Strings: TStrings;
  const ListClassName, ObjectClassName: String);
  
Var
  S : String;
  
begin
   S:=Format('Function %s.Add(AnItem : %s) : Integer;',[ListClassName,ObjectClassName]);
   BeginMethod(Strings,S);
   Addln(Strings,'begin');
   IncIndent;
   try
     Addln(Strings,'Result:=inherited Add(AnItem);');
   finally
     DecIndent;
   end;
   EndMethod(Strings,S);
   Addln(Strings);
end;

function TTiOPFCodeGenerator.AllowPropertyDeclaration(F: TFieldPropDef;
  AVisibility: TVisibilities): Boolean;
begin
  If F.PropertyName=SOID then
    Result:=False
  else
    Result:=inherited AllowPropertyDeclaration(F, AVisibility);
end;


procedure TTiOPFCodeGenerator.CreateListImplementation(Strings: TStrings; const ObjectClassName, ListClassName: String);

Var
  S : String;

begin
  If caListItemsProperty in tiOPFOptions.ClassOptions then
    begin
    AddLn(Strings,'{ %s }',[ListClassName]);
    AddLn(Strings);
    S:=Format('Function %s.GetObj(AIndex : Integer) : %s;',[ListClassName,ObjectClassname]);
    BeginMethod(Strings,S);
    AddLn(Strings,'begin');
    IncIndent;
    try
      AddLn(Strings,'Result:=%s(Inherited Items[AIndex]);',[ObjectClassname]);
    finally
      DecIndent;
    end;
    EndMethod(Strings,S);
    Addln(Strings);
    S:=Format('Procedure %s.SetObj(AIndex : Integer; AValue : %s);',[ListClassName,ObjectClassname]);
    BeginMethod(Strings,S);
    AddLn(Strings,'begin');
    IncIndent;
    try
      AddLn(Strings,'Inherited Items[AIndex]:=AValue;');
    finally
      DecIndent;
    end;
    EndMethod(Strings,S);
    Addln(Strings);
    end;
  If (caListAddMethod in tiOPFOptions.ClassOptions) then
    WriteListAddObject(Strings,ListClassName,ObjectClassName);
end;

Initialization
  RegisterCodeGenerator('tiOPF','tiOPF class and visitors for the data',TTiOPFCodeGenerator);

Finalization
  UnRegisterCodeGenerator(TTiOPFCodeGenerator);
end.

