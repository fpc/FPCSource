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
unit fpcgdbcoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpddcodegen;
  
Type
  TListMode = (lmNone,lmList,lmObjectList,lmCollection,lmDBCollection,lmGenericList);
  TClassOption = (coCreateLoader,coUseFieldMap,coCreateArrayProperty,coCreateAssign, coRecord);
  TClassOptions = Set of TClassOption;
  
  { TDBCollOptions }

  TDBCollOptions = Class(TClassCodeGeneratorOptions)
  private
    FClassOptions: TClassOptions;
    FListMode: TListMode;
    FListAncestorName: String;
    FListClassName: String;
    FArrayPropName: String;
    FMapAncestorName: String;
    FMapClassName: String;
    function GetArrayPropName: String;
    function GetListClassName: String;
    function GetMapName: String;
    procedure SetArrayPropName(const AValue: String);
    procedure SetClassOptions(AValue: TClassOptions);
    procedure SetListAncestorName(const AValue: String);
    procedure SetListClassName(const AValue: String);
    procedure SetListMode(const AValue: TListMode);
    procedure SetMapAncestorName(const AValue: String);
    procedure SetMapClassName(const AValue: String);
  Public
    Constructor Create; override;
    Procedure Assign(ASource : TPersistent); override;
    Function CreateLoader : Boolean;
    Function UseFieldMap : Boolean;
    Function CreateArrayProperty : Boolean;
    Function CreateAssign : Boolean;
  Published
    Property ClassOptions : TClassOptions Read FClassOptions Write SetClassOptions;
    Property ListMode : TListMode Read FListMode Write SetListMode;
    Property ListAncestorName : String Read FListAncestorName Write SetListAncestorName;
    Property ListClassName : String Read GetListClassName Write SetListClassName;
    Property MapAncestorName : String Read FMapAncestorName Write SetMapAncestorName;
    Property MapClassName : String Read GetMapName Write SetMapClassName;
    Property ArrayPropName : String Read GetArrayPropName Write SetArrayPropName;
    Property AncestorClass;
  end;
  
  { TDDDBCollCodeGenerator }

  TDDDBCollCodeGenerator = Class(TDDClassCodeGenerator)
  private
    function GetOpt: TDBCollOptions;
  Protected
    function IsRecord: Boolean;
    procedure CreateClassHead(Strings: TStrings); override;
    procedure CreateObjectAssign(Strings: TStrings; const ObjectClassName: String); virtual;
    // Not to be overridden.
    procedure CreateFieldMapImplementation(Strings: TStrings; const ObjectClassName, MapClassName: String);
    procedure CreateListImplementation(Strings: TStrings; ListMode: TListMode;  const ObjectClassName, ListClassName: String);
    procedure WriteFieldMapAssign(Strings: TStrings; F: TFieldPropDef);
    procedure WriteMapInitFields(Strings: TStrings; const ObjectClassName, MapClassName: String);
    procedure WriteListLoad(Strings: TStrings; ListMode: TListMode; const ObjectClassName, ListClassName: String; FromMap: Boolean);
    procedure WriteListAddObject(Strings: TStrings; ListMode: TListMode; const InstanceName, ObjectClassName: String);
    // Overrides of parent objects
    Function GetInterfaceUsesClause : string; override;
    Procedure DoGenerateInterface(Strings: TStrings); override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
    procedure WriteVisibilityStart(V: TVisibility; Strings: TStrings); override;
    procedure CreateImplementation(Strings: TStrings); override;
    Class Function NeedsFieldDefs : Boolean; override;
    Function CreateOptions : TCodeGeneratorOptions; override;
    //
    //  New methods
    //
    // Override to add declarations to list declaration
    procedure DoCreateListDeclaration(Strings: TStrings; ListMode: TListMode; const ObjectClassName, ListClassName, ListAncestorName: String); virtual;
    // Override to add declarations to fieldmap declaration
    procedure DoCreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String); virtual;
    // Override to add statements to the FieldMap Load implementation
    procedure DoWriteMapLoad(Strings: TStrings; const ObjectClassName, MapClassName: String); virtual;
    // Override to add statements to the FieldMap LoadObject implementation
    procedure DoWriteMapLoadObject(Strings: TStrings; const ObjectClassName, MapClassName: String);virtual;
    // Create an object that should be added to the list.
    procedure WriteListCreateObject(Strings: TStrings; ListMode: TListMode;  const InstanceName, ObjectClassName: String);
    // Write LoadFromDataset implementation for List object
    procedure WriteListLoadFromDataset(Strings: TStrings; ListMode: TListMode; const ObjectClassName, ListClassName: String);
    // Write LoadFromMap implementation for List object
    procedure WriteListLoadFromMap(Strings: TStrings; ListMode: TListMode; const ObjectClassName, ListClassName: String);
    // Object load from map;
    procedure CreateObjectLoadFromMap(Strings: TStrings; const ObjectClassName: String); virtual;
    // Create assign statement for a property from a dataset field, in object itself (not in map).
    procedure WriteFieldDatasetAssign(Strings: TStrings; F: TFieldPropDef); virtual;
    // Copy a property from one instance to another in Assign()
    procedure WriteFieldAssign(Strings: TStrings; F: TFieldPropDef; SrcName: String); virtual;
    // Code to Load object from fataset (should check usefieldmap)
    procedure CreateObjectLoadFromDataset(Strings: TStrings; const ObjectClassName: String); virtual;
  Public
    procedure CreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName,
      MapAncestorName: String);
    procedure CreateListDeclaration(Strings: TStrings; ListMode: TListMode;
      const ObjectClassName, ListClassName, ListAncestorName: String);
    Property DBCollOptions : TDBColLOptions Read GetOpt;
  end;

Const
  NonDBCollList = [lmList,lmObjectList,lmCollection,lmGenericList];

implementation

{ TDBCollOptions }

procedure TDBCollOptions.SetListMode(const AValue: TListMode);
begin
  if FListMode=AValue then exit;
  FListMode:=AValue;
  Case ListMode of
    lmNone :
      begin
      Exclude(FClassOptions,coCreateArrayProperty);
      end;
    lmList :
      begin
      AncestorClass:='TPersistent';
      ListAncestorName:='TList';
      end;
    lmObjectList :
      begin
      AncestorClass:='TPersistent';
      ListAncestorName:='TObjectList';
      end;
    lmGenericList :
      begin
      if coRecord in ClassOptions then
        begin
        AncestorClass:='TObject';
        ListAncestorName:='TList';
        end
      else
        begin
        AncestorClass:='TObject';
        ListAncestorName:='TObjectList';
        end;
      // Avoic recursion, bypass setter
      Exclude(FClassOptions,coCreateArrayProperty);
      end;
    lmCollection :
      begin
      AncestorClass:='TCollectionItem';
      ListAncestorName:='TCollection';
      end;
    lmDBCollection :
      begin
      AncestorClass:='TDBCollectionItem';
      ListAncestorName:='TDBCollection';
      Include(FClassoptions,coUseFieldMap);
      end;
  end;
end;

procedure TDBCollOptions.SetMapAncestorName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FMapAncestorName:=AValue;
end;

procedure TDBCollOptions.SetMapClassName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FMapClassName:=AValue;
end;

function TDBCollOptions.GetListClassName: String;
begin
  Result:=FListClassName;
  If (Result='') then
    Result:=ObjectClassName+'List';
end;

function TDBCollOptions.GetArrayPropName: String;
begin
  Result:=FArrayPropName;
  If (Result='') then
    begin
    Result:=ObjectClassName;
    If (Result<>'') and (Upcase(Result[1])='T') then
      Delete(Result,1,1);
    Result:=Result+'s';
    end;
end;

function TDBCollOptions.GetMapName: String;
begin
  Result:=FMapClassName;
  If (Result='') then
    Result:=ObjectClassName+'Map';
end;

procedure TDBCollOptions.SetArrayPropName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FArrayPropName:=AValue;
end;

procedure TDBCollOptions.SetClassOptions(AValue: TClassOptions);
begin
  if FClassOptions=AValue then Exit;
  FClassOptions:=AValue;
  if (coRecord in FCLassOptions) and (not (ListMode in [lmNone,lmGenericList])) then
    ListMode:=lmGenericList;
end;

procedure TDBCollOptions.SetListAncestorName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FListAncestorName:=AValue;
end;

procedure TDBCollOptions.SetListClassName(const AValue: String);
begin
  CheckIdentifier(AValue,True);
  FListClassName:=AValue;
end;

constructor TDBCollOptions.Create;
begin
  inherited Create;
  FClassOptions:=[coCreateLoader,coUseFieldMap,coCreateAssign];
  AncestorClass:='TPersistent';
  FListAncestorName:='TList';
  ObjectClassName:='TMyObject';
  FMapAncestorName:='TFieldMap';
end;

procedure TDBCollOptions.Assign(ASource: TPersistent);

Var
  DC : TDBCollOptions;

begin
  If ASource is TDBCollOptions then
    begin
    DC:=ASource as TDBCollOptions;
    ListMode:=DC.ListMode;
    FClassOptions:=DC.FClassOptions;
    FListAncestorName:=DC.FListAncestorName;
    FListClassName:=DC.FListClassName;
    FMapAncestorName:=DC.FMapAncestorName;
    FMapClassName:=DC.FMapClassName;
    FArrayPropName:=DC.FArrayPropName;
    end;
  inherited Assign(ASource);
end;

function TDBCollOptions.CreateLoader: Boolean;
begin
  Result:=coCreateLoader in ClassOptions;
end;

function TDBCollOptions.UseFieldMap: Boolean;
begin
  Result:=coUseFieldMap in ClassOptions;
end;

function TDBCollOptions.CreateArrayProperty: Boolean;
begin
  Result:=coCreateArrayProperty in ClassOptions;
end;

function TDBCollOptions.CreateAssign: Boolean;
begin
  Result:=coCreateAssign in ClassOptions;
end;

{ TDDDBCollCodeGenerator }

function TDDDBCollCodeGenerator.GetOpt: TDBCollOptions;
begin
  Result:=CodeOptions as TDBColLOptions
end;

procedure TDDDBCollCodeGenerator.CreateClassHead(Strings: TStrings);
begin
  if Not IsRecord then
    inherited CreateClassHead(Strings)
  else
    begin
    Addln(Strings,'{ %s }',[ClassOptions.ObjectClassName]);
    AddLn(Strings);
    AddLn(Strings,'%s = Record',[ClassOptions.ObjectClassName])
    end;
end;

procedure TDDDBCollCodeGenerator.DoGenerateInterface(Strings: TStrings);
begin
  inherited DoGenerateInterface(Strings);
  With DBCollOptions do
    begin
    If CreateLoader then
      begin
      if UseFieldMap then
        CreateFieldMapDeclaration(Strings,ObjectClassName,MapClassName,MapAncestorName);
      end;
    if ListMode<>lmNone then
      CreateListDeclaration(Strings,ListMode,ObjectClassName,ListClassName,ListAncestorName);
    end;
end;

procedure TDDDBCollCodeGenerator.DoGenerateImplementation(Strings: TStrings);
begin
  inherited DoGenerateImplementation(Strings);
  With DBCollOptions do
    begin
    If CreateLoader and UseFieldMap then
      CreateFieldMapImplementation(Strings,ObjectClassName,MapClassName);
    if ListMode<>lmNone then
      CreateListImplementation(Strings,ListMode,ObjectClassName,ListClassName);
    end;

end;

procedure TDDDBCollCodeGenerator.WriteVisibilityStart(V: TVisibility;
  Strings: TStrings);
begin
  inherited WriteVisibilityStart(V, Strings);
  If (V=vPublic) then
    With DBCollOptions do
      begin
      If CreateLoader and (ListMode in NonDBCollList) then
        begin
        If UseFieldMap Then
          AddLn(Strings,'Procedure LoadFromMap(Map : TFieldMap);');
        AddLn(Strings,'Procedure LoadFromDataset(ADataset : TDataset);');
        end;
      If CreateAssign then
        if IsRecord then
          AddLn(Strings,'Procedure Assign(ASource : %s); ',[ObjectClassName])
        else
          AddLn(Strings,'Procedure Assign(ASource : TPersistent); override;');
      end;
end;

procedure TDDDBCollCodeGenerator.CreateImplementation(Strings: TStrings);

Var
  S : String;

begin
  inherited CreateImplementation(Strings);
  With DBCOlloptions do
    begin
    If CreateLoader and (ListMode in NonDBCollList) then
      begin
      if UseFieldMap then
        begin
        S:=Format('Procedure %s.LoadFromMap(Map : TFieldMap);',[ObjectClassName]);
        BeginMethod(Strings,S);
        CreateObjectLoadFromMap(Strings,ObjectClassName);
        EndMethod(Strings,S);
        end;
      S:=Format('Procedure %s.LoadFromDataset(ADataset : TDataset);',[ObjectClassName]);
      BeginMethod(Strings,S);
      CreateObjectLoadFromDataset(Strings,ObjectClassName);
      EndMethod(Strings,S);
      end;
    If CreateAssign then
      begin
      if IsRecord then
          S:=Format('Procedure %s.Assign(ASource : %s);',[ObjectClassName,ObjectClassName])
       else
          S:=Format('Procedure %s.Assign(ASource : TPersistent);',[ObjectClassName]);
      BeginMethod(Strings,S);
      CreateObjectAssign(Strings,ObjectClassName);
      EndMethod(Strings,S);
      end;
    end;
end;

procedure TDDDBCollCodeGenerator.CreateObjectAssign(Strings: TStrings; const ObjectClassName: String);

Var
  I : Integer;
  F : TFieldPropDef;

begin
  if IsRecord then
    begin
    Addln(Strings,'begin');
    IncIndent;
    For I:=0 to Fields.Count-1 do
      begin
      F:=Fields[i];
      If F.Enabled Then
        WriteFieldAssign(Strings,F,'ASource');
      end;
    decIndent;
    end
  else
    begin
    AddLn(Strings,'var');
    IncIndent;
    Try
      AddLn(Strings,'O : %s ;',[ObjectClassName]);
      Addln(Strings,'');
    Finally
      DecIndent;
    end;
    Addln(Strings,'begin');
    IncIndent;
    Try
      AddLn(Strings,'If (ASource is %s) then',[ObjectClassName]);
      IncIndent;
      Try
        Addln(Strings,'begin');
        Addln(Strings,'O:=(ASource as %s);',[ObjectClassName]);
        For I:=0 to Fields.Count-1 do
          begin
          F:=Fields[i];
          If F.Enabled Then
            WriteFieldAssign(Strings,F,'O');
          end;
        Addln(Strings,'end');
      Finally
        DecIndent;
      end;
      AddLn(Strings,'else');
      IncIndent;
      Try
        AddLn(Strings,'Inherited;');
      Finally
        DecIndent;
      end;
    Finally
      DecIndent;
    end;
    end;
end;

procedure TDDDBCollCodeGenerator.WriteFieldAssign(Strings : TStrings; F : TFieldPropDef; SrcName : String);

Var
  S : String;

begin
  Case F.PropertyType of
    ptStream: S:=Format('%s.CopyFrom(%s.%s,0);',[F.ObjPasReadDef,SrcName,F.ObjPasReadDef]);
    ptTStrings: S:=Format('%s.Assign(%s.%s,0);',[F.ObjPasReadDef,SrcName,F.ObjPasReadDef]);
    ptCustom: S:=Format('// Custom code to assign %s from %s.%s',[F.ObjPasReadDef,SrcName,F.ObjPasReadDef]);
  else
    S:=Format('%s:=%s.%s;',[F.ObjPasReadDef,SrcName,F.ObjPasReadDef]);
  end;
  AddLn(Strings,S);
end;

procedure TDDDBCollCodeGenerator.CreateObjectLoadFromMap(Strings: TStrings; const ObjectClassName: String);

begin
  Addln(Strings,'begin');
  IncIndent;
  Try
    if IsRecord then
      AddLn(Strings,'(Map as %s).DoLoad(Self);',[DBCollOptions.MapClassName])
    else
      AddLn(Strings,'Map.LoadObject(Self);');
  Finally
    DecIndent;
  end;
end;

procedure TDDDBCollCodeGenerator.CreateObjectLoadFromDataset(Strings: TStrings; const ObjectClassName: String);

Var
  I : Integer;

begin
  AddLn(Strings,'begin');
  Incindent;
  try
    If DBColloptions.UseFieldMap then
      begin
      AddLn(Strings,'With %s.Create(ADataset) do',[DBCollOptions.MapClassName]);
      IncIndent;
      Try
        Addln(Strings,'try');
        IncIndent;
        Try
          if IsRecord then
            Addln(Strings,'DoLoad(Self);')
          else
            Addln(Strings,'LoadObject(Self);');
        Finally
          DecIndent;
        end;
        Addln(Strings,'Finally');
        IncIndent;
        Try
          Addln(Strings,'Free;');
        Finally
          DecIndent;
        end;
        Addln(Strings,'end;');
      Finally
        Decindent;
      end;
      end
    else
      begin
      AddLn(Strings,'With ADataset do');
      IncIndent;
      Try
        AddLn(Strings,'begin');
        For I:=0 to Fields.Count-1 do
          If Fields[i].Enabled then
            WriteFieldDatasetAssign(Strings,Fields[i]);
        AddLn(Strings,'end;');
      Finally
        DecIndent;
      end;
      end;
  Finally
    Decindent;
  end;
end;

procedure TDDDBCollCodeGenerator.WriteFieldDatasetAssign(Strings : TStrings; F : TFieldPropDef);

Var
  FN,PN,S,R : String;

begin
  PN:=F.PropertyName;
  FN:=F.FieldName;
  Case F.PropertyType of
    ptBoolean :
      S:='AsBoolean';
    ptShortint, ptByte,
    ptSmallInt, ptWord,
    ptLongint, ptCardinal :
      S:='AsInteger';
    ptInt64, ptQWord:
      If F.FieldType=ftLargeInt then
        R:=Format('%s:=(FieldByName(%s) as TLargeIntField).AsLargeInt;',[PN,CreateString(FN)])
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
      R:=Format('Integer(%s):=FieldByName(%s).AsInteger;',[PN,CreateString(FN)]);
    ptSet :
      S:=Format('// Add custom set loading code here for %s from %s',[PN,FN]);
    ptStream :
      R:=Format('FieldByName(%s).SaveToStream(%s);',[CreateString(FN),PN]);
    ptTStrings :
      R:=Format('%s.Text:=FieldByName(%s).AsString;',[PN,CreateString(FN),PN]);
    ptCustom :
      R:=Format('// Add custom loading code here for %s from %s',[PN,FN]);
  end;
  If (S<>'') then
    R:=Format('%s:=FieldByName(%s).%s;',[PN,CreateString(FN),s]);
  AddLn(Strings,R);
end;


{ FieldMap interface generation routines}

procedure TDDDBCollCodeGenerator.CreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName, MapClassName,
  MapAncestorName: String);


begin
  Addln(Strings);
  IncIndent;
  try
    Addln(Strings,'{ %s }',[MapClassName]);
    Addln(Strings);
    Addln(Strings,'%s = Class(%s)',[MapClassName,MapAncestorName]);
    DoCreateFieldMapDeclaration(Strings,ObjectClassName,MapClassName,MapAncestorName);
    AddLn(Strings,'end;');
  Finally
    DecIndent;
  end;
end;

procedure TDDDBCollCodeGenerator.DoCreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName, MapClassName,
  MapAncestorName: String);

Var
  I : Integer;
  F : TFieldPropDef;

begin
  AddLn(Strings,'Private');
  IncIndent;
  Try
    For I:=0 to Fields.Count-1 do
      begin
      F:=Fields[I];
      If F.Enabled then
        AddLn(Strings,'F%s : TField;',[F.FieldName]);
      end;
    if IsRecord then
      AddLn(Strings,'Procedure DoLoad(var AObject : %s);',[ObjectClassName])
    else
      AddLn(Strings,'Procedure DoLoad(AObject : %s);',[ObjectClassName]);
  Finally
    DecIndent;
  end;
  AddLn(Strings,'Public');
  IncIndent;
  Try
    AddLn(Strings,'Procedure InitFields; Override;');
    if not IsRecord then
      AddLn(Strings,'Procedure LoadObject(AObject : TObject); Override;');
  Finally
    DecIndent;
  end;
end;

{ FieldMap implementation generation routines}
Function TDDDBCollCodeGenerator.IsRecord : Boolean;

begin
  Result:=CoRecord in DBCollOptions.ClassOptions;
end;

procedure TDDDBCollCodeGenerator.CreateFieldMapImplementation(Strings: TStrings; const ObjectClassName, MapClassName: String);

Var
  S : String;

begin
  AddLn(Strings,' { %s }',[MapClassName]);
  AddLn(Strings);
  if IsRecord then
    S:=Format('Procedure %s.DoLoad(Var AObject : %s);',[MapClassName,ObjectClassName])
  else
    S:=Format('Procedure %s.DoLoad(AObject : %s);',[MapClassName,ObjectClassName]);
  BeginMethod(Strings,S);
  Try
    DoWriteMapLoad(Strings,ObjectClassName,MapClassName);
  Finally
    EndMethod(Strings,S);
  end;
  if not IsRecord then
    begin
    S:=Format('Procedure %s.LoadObject(AObject : TObject);',[MapClassName]);
    BeginMethod(Strings,S);
    Try
      DoWriteMapLoadObject(Strings,ObjectClassName,MapClassName);
    Finally
      EndMethod(Strings,S);
    end;
    end;
  S:=Format('Procedure %s.InitFields;',[MapClassName]);
  BeginMethod(Strings,S);
  Try
    WriteMapInitFields(Strings,ObjectClassName,MapClassName);
  Finally
    EndMethod(Strings,S);
  end;
end;

procedure TDDDBCollCodeGenerator.DoWriteMapLoad(Strings: TStrings; const ObjectClassName, MapClassName: String);

Var
  I : Integer;

begin
  AddLn(Strings,'begin');
  IncIndent;
  try
     AddLn(Strings,'With AObject do');
     IncIndent;
     try
       AddLn(Strings,'begin');
       For I:=0 to Fields.Count-1 do
         If Fields[i].Enabled then
           WriteFieldMapAssign(Strings,Fields[i]);
       AddLn(Strings,'end;');
     finally
       DecIndent;
     end;
  finally
    DecIndent;
  end;
end;

procedure TDDDBCollCodeGenerator.DoWriteMapLoadObject(Strings: TStrings; const ObjectClassName, MapClassName: String);

begin
  Addln(Strings,'begin');
  IncIndent;
  try
    Addln(Strings,'DoLoad(AObject as %s);',[ObjectClassName]);
  finally
    DecIndent;
  end;
end;


procedure TDDDBCollCodeGenerator.WriteFieldMapAssign(Strings : TStrings; F : TFieldPropDef);

Var
  FN,PN,S : String;
  
begin
  PN:=F.PropertyName;
  FN:='Self.F'+F.FieldName;
  Case F.PropertyType of
    ptBoolean :
      S:=Format('%s:=GetFromField(%s,%s);',[PN,FN,PN]);
    ptShortint, ptByte,
    ptSmallInt, ptWord,
    ptLongint, ptCardinal :
      S:=Format('%s:=GetFromField(%s,%s);',[PN,FN,PN]);
    ptInt64, ptQWord,
    ptShortString, ptAnsiString, ptWideString :
      S:=Format('%s:=GetFromField(%s,%s);',[PN,FN,PN]);
    ptSingle, ptDouble, ptExtended, ptComp, ptCurrency :
      S:=Format('%s:=GetFromField(%s,%s);',[PN,FN,PN]);
    ptDateTime :
      S:=Format('%s:=GetFromField(%s,%s);',[PN,FN,PN]);
    ptEnumerated :
      S:=Format('Integer(%s):=GetFromField(%s,Ord(%s));',[PN,FN,PN]);
    ptSet :
      S:=Format('// Add custom set loading code here for %s from %s',[PN,FN]);
    ptStream :
      S:=Format('%s.SaveToStream(%s);',[FN,PN]);
    ptTStrings :
      S:=Format('%s.Text:=GetFromField(%s,%s.Text)',[PN,FN,PN]);
    ptCustom :
      S:=Format('// Add custom loading code here for %s from %s',[PN,FN]);
  end;
  AddLn(Strings,S);
end;

procedure TDDDBCollCodeGenerator.WriteMapInitFields(Strings: TStrings; const ObjectClassName, MapClassName: String);

Var
  I: Integer;
  F : TFieldPropDef;

begin
  AddLn(Strings,'begin');
  IncIndent;
  try
    For I:=0 to Fields.Count-1 Do
      begin
      F:=Fields[i];
      If F.Enabled then
        AddLn(Strings,'F%s:=FindField(%s);',[F.FieldName,CreateString(F.FieldName)]);
      end;
  Finally
    DecIndent;
  end;
end;

function TDDDBCollCodeGenerator.GetInterfaceUsesClause: string;
begin
  Result:=inherited GetInterfaceUsesClause;
  With DBColloptions do
    if CreateLoader or (ListMode=lmDBCollection) then
      begin
      If (Result<>'') then
        Result:=Result+', ';
      Result:=Result+'db';
      If (ListMode=lmObjectList) then
        Result:=Result+', contnrs'
      else if (ListMode=lmGenericList) then
        Result:=Result+', Generics.Collections';
      if UseFieldMap then
        Result:=Result+', FieldMap';
      If (ListMode=lmDBCollection) then
        Result:=Result+', dbcoll';
      end;
end;



{ List class generation routines }

procedure TDDDBCollCodeGenerator.CreateListDeclaration(Strings: TStrings; ListMode: TListMode; const ObjectClassName,
  ListClassName, ListAncestorName: String);

Var
  LAN : String;

begin
  IncIndent;
  try
    Addln(Strings);
    Addln(Strings,'{ %s }',[ListClassName]);
    Addln(Strings);
    if ListMode<>lmGenericList then
      LAN:=ListAncestorName
    else
      LAN:=Format('%s<%s>',[ListAncestorName,ObjectClassName]);
    Addln(Strings,'%s = Class(%s)',[ListClassName,LAN]);
    DoCreateListDeclaration(Strings,ListMode,ObjectClassName,ListClassName,LAN);
    AddLn(Strings,'end;');
  Finally
    DecIndent;
  end;
end;


procedure TDDDBCollCodeGenerator.DoCreateListDeclaration(Strings: TStrings; ListMode: TListMode; const ObjectClassName,
  ListClassName, ListAncestorName: String);

Var
  S : String;
  DoArray : Boolean;

begin
  DoArray:=DBCollOptions.CreateArrayProperty and (ListMode<>lmGenericList);
  if DoArray then
    begin
    AddLn(Strings,'Private');
    IncIndent;
    Try
      AddLn(Strings,'Function GetObj(Index : Integer) : %s;',[ObjectClassname]);
      AddLn(Strings,'Procedure SetObj(Index : Integer; AValue : %s);',[ObjectClassname]);
    Finally
      DecIndent;
    end;
    end;
  AddLn(Strings,'Public');
  IncIndent;
  Try
    If (ListMode in NonDBCollList) and DBCollOptions.CreateLoader then
      begin
      If DBColloptions.UseFieldMap then
        AddLn(Strings,'Procedure LoadFromMap(Map : TFieldMap);');
      AddLn(Strings,'Procedure LoadFromDataset(Dataset : TDataset);');
      end
  Finally
    DecIndent;
  end;
  if DoArray then
    begin
    IncIndent;
    Try
      S:=DBCollOptions.ArrayPropName;
      AddLn(Strings,'Property %s[Index : Integer] : %s Read GetObj Write SetObj; Default;',[S,ObjectClassname]);
    Finally
      DecIndent;
    end;
    end;
end;

procedure TDDDBCollCodeGenerator.CreateListImplementation(Strings: TStrings; ListMode: TListMode; const ObjectClassName,
  ListClassName: String);

Var
  S : String;
  DoArray : Boolean;

begin
  DoArray:=DBCollOptions.CreateArrayProperty and (ListMode<>lmGenericList);
  AddLn(Strings,'{ %s }',[ListClassName]);
  if DoArray then
    begin
    S:=Format('Function %s.GetObj(Index : Integer) : %s;',[ListClassName,ObjectClassname]);
    BeginMethod(Strings,S);
    AddLn(Strings,'begin');
    IncIndent;
    try
      AddLn(Strings,'Result:=%s(Items[Index]);',[ObjectClassname]);
    finally
      DecIndent;
    end;
    EndMethod(Strings,S);
    S:=Format('Procedure %s.SetObj(Index : Integer; AValue : %s);',[ListClassName,ObjectClassname]);
    BeginMethod(Strings,S);
    AddLn(Strings,'begin');
    IncIndent;
    try
      AddLn(Strings,'Items[Index]:=AValue;');
    finally
      DecIndent;
    end;
    EndMethod(Strings,S);
    end;
  if DBCollOptions.CreateLoader then
    begin
    If DBCollOptions.UseFieldMap then
      begin
      AddLn(Strings);
      S:=Format('Procedure %s.LoadFromMap(Map : TFieldMap);',[ListClassName]);
      BeginMethod(Strings,S);
      WriteListLoadFromMap(Strings,Listmode,ObjectClassName,ListClassName);
      EndMethod(Strings,S);
      end;
    AddLn(Strings);
    S:=Format('Procedure %s.LoadFromDataset(Dataset : TDataset);',[ListClassName]);
    BeginMethod(Strings,S);
    WriteListLoadFromDataset(Strings,Listmode,ObjectClassName,ListClassName);
    EndMethod(Strings,S);
    end;
end;

procedure TDDDBCollCodeGenerator.WriteListLoadFromMap(Strings: TStrings; ListMode: TListMode; const ObjectClassName,
  ListClassName: String);

begin
  WriteListLoad(Strings,ListMode,ObjectClassName,ListClassName,True);
end;

procedure TDDDBCollCodeGenerator.WriteListLoadFromDataset(Strings: TStrings; ListMode: TListMode; const ObjectClassName,
  ListClassName: String);


Var
  M : String;

begin
  If Not DBCollOptions.UseFieldMap then
    WriteListLoad(Strings,ListMode,ObjectClassName,ListClassName,False)
  else
    begin
    M:=DBCollOptions.MapClassName;
    AddLn(Strings);
    AddLn(Strings,'Var');
    IncIndent;
    try
      AddLn(Strings,'Map : %s;',[M]);
    Finally
      DecIndent;
    end;
    AddLn(Strings);
    AddLn(Strings,'begin');
    IncIndent;
    try
      AddLn(Strings,'Map:=%s.Create(Dataset);',[M]);
      AddLn(Strings,'Try');
      IncIndent;
      try
        AddLn(Strings,'LoadFromMap(Map);');
      finally
        DecIndent;
      end;
      AddLn(Strings,'Finally');
      IncIndent;
      try
        AddLn(Strings,'FreeAndNil(Map);');
      finally
        DecIndent;
      end;
      AddLn(Strings,'end;');
    finally
      DecIndent;
    end;
    end;
end;

procedure TDDDBCollCodeGenerator.WriteListLoad(Strings: TStrings; ListMode: TListMode; const ObjectClassName,
  ListClassName: String; FromMap: Boolean);

begin
  AddLn(Strings);
  AddLn(Strings,'Var');
  IncIndent;
  try
    AddLn(Strings,'Obj : %s;',[ObjectClassName]);
  Finally
    DecIndent;
  end;
  AddLn(Strings);
  AddLn(Strings,'begin');
  IncIndent;
  try
    If FromMap then
      begin
      if IsRecord then
        AddLn(Strings,'With Map as %s do',[DBCollOptions.MapClassName])
      else
        AddLn(Strings,'With Map do');
      IncIndent;
      end;
    Try
      AddLn(Strings,'While not Dataset.EOF do');
      IncIndent;
      Try
        AddLn(Strings,'begin');
        WriteListCreateObject(Strings,ListMode,'Obj',ObjectClassName);
        AddLn(Strings,'Try');
        IncIndent;
        Try
          If FromMap then
            begin
            if IsRecord then
              AddLn(Strings,'DoLoad(Obj);')
            else
              AddLn(Strings,'LoadObject(Obj);')
            end
          else
            AddLn(Strings,'Obj.LoadFromDataset(Dataset);');
          WriteListAddObject(Strings,ListMode,'Obj',ObjectClassName);
        Finally
          DecIndent;
        end;
        AddLn(Strings,'Except');
        IncIndent;
        Try
          if  IsRecord then
            AddLn(Strings,'Obj:=Default(%s);',[ObjectClassName])
          else
            AddLn(Strings,'FreeAndNil(Obj);');
          AddLn(Strings,'Raise;');
        Finally
          DecIndent;
        end;
        AddLn(Strings,'end;');
        AddLn(Strings,'Dataset.Next;');
        AddLn(Strings,'end;');
      Finally
        DecIndent;
      end;
    finally
      If FromMap then
        DecIndent;
    end;
  finally
    DecIndent;
  end;
end;

procedure TDDDBCollCodeGenerator.WriteListCreateObject(Strings: TStrings; ListMode: TListMode; const InstanceName,
  ObjectClassName: String);

Var
  S : String;

begin
  if IsRecord then
    S:=Format('%s:=Default(%s);',[InstanceName,ObjectClassName])
  else If (ListMode in [lmList,lmObjectList,lmGenericList]) then
    S:=Format('%s:=%s.Create;',[InstanceName,ObjectClassName])
  else
    S:=Format('%s:=Self.Add as %s;',[InstanceName,ObjectClassName]);
  AddLn(Strings,S);
end;

procedure TDDDBCollCodeGenerator.WriteListAddObject(Strings: TStrings; ListMode: TListMode; const InstanceName,
  ObjectClassName: String);

Var
  S : String;

begin
  If ListMode in [lmList,lmObjectList,lmGenericList] then
    begin
    S:=Format('Add(%s);',[InstanceName]);
    AddLn(Strings,S);
    end;
end;


class function TDDDBCollCodeGenerator.NeedsFieldDefs: Boolean;
begin
  Result:=True;
end;

function TDDDBCollCodeGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TDBCollOptions.Create;
end;


Initialization
  RegisterCodeGenerator('DBColl','Simple object/record collection/list for the data',TDDDBCollCodeGenerator);
  
Finalization
  UnRegisterCodeGenerator(TDDDBCollCodeGenerator);
end.

