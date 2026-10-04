{%MainUnit pasresolver.pp}
{ **********************************************************************

    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - Scope handling (except finish)
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

{ TPasResolverHub }

constructor TPasResolverHub.Create(TheOwner: TObject);
begin
  FOwner:=TheOwner;
end;

procedure TPasResolverHub.Reset;
begin
  FinishedInterfaceCount:=0;
end;

{ TPRSpecializedItem }

destructor TPRSpecializedItem.Destroy;
var
  i: Integer;
begin
  // Destroy does not free child elements: free each synthetic const's Expr first.
  if SyntheticConsts<>nil then
    for i:=0 to SyntheticConsts.Count-1 do
      TPasElement(SyntheticConsts[i]).FreeChildren(false);
  FreeAndNil(SyntheticConsts);
  SetLength(ConstExprs,0);
  SetLength(SpecializedConstraints,0);
  inherited Destroy;
end;

{ TPRSpecializedTypeItem }

procedure TPRSpecializedTypeItem.SetSpecializedType(AValue: TPasGenericType);
begin
  if FSpecializedType=AValue then Exit;
  FSpecializedEl:=AValue;
  FSpecializedType:=AValue;
end;

destructor TPRSpecializedTypeItem.Destroy;
begin
  if ImplProcs<>nil then
    begin
    ImplProcs.Free;
    ImplProcs:=nil;
    end;
  HeaderScope.Free;
  HeaderScope:=nil;
  SpecializedType:=nil;
  inherited Destroy;
end;

{ TPRSpecializedProcItem }

procedure TPRSpecializedProcItem.SetSpecializedProc(const AValue: TPasProcedure
  );
begin
  if FSpecializedProc=AValue then Exit;
  FSpecializedEl:=AValue;
  FSpecializedProc:=AValue;
end;

destructor TPRSpecializedProcItem.Destroy;
begin
  if ImplProc<>nil then
    begin
    ImplProc:=nil;
    end;
  SpecializedProc:=nil;
  inherited Destroy;
end;


{ TPasGenericScope }

destructor TPasGenericScope.Destroy;
begin
  if SpecializedItems<>nil then
    begin
    SpecializedItems.Free;
    SpecializedItems:=nil;
    end;
  inherited Destroy;
end;

{ TPasInheritedScope }

function TPasInheritedScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  aClassScope: TPasClassScope;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then exit;
  aClassScope:=AncestorScope;
  while aClassScope<>nil do
    begin
    Result:=aClassScope.FindIdentifier(Identifier);
    if Result<>nil then exit;
    aClassScope:=aClassScope.AncestorScope;
    end;
end;

procedure TPasInheritedScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  aClassScope: TPasClassScope;
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  aClassScope:=AncestorScope;
  while aClassScope<>nil do
    begin
    aClassScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
    if Abort then exit;
    aClassScope:=aClassScope.AncestorScope;
    end;
end;

procedure TPasInheritedScope.WriteIdentifiers(Prefix: string);
var
  aClassScope: TPasClassScope;
begin
  inherited WriteIdentifiers(Prefix);
  aClassScope:=AncestorScope;
  while aClassScope<>nil do
    begin
    aClassScope.WriteIdentifiers(Prefix);
    aClassScope:=aClassScope.AncestorScope;
    end;
end;

{ TPasDotEnumTypeScope }

function TPasDotEnumTypeScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=EnumScope.FindLocalIdentifier(Identifier);
  if Result<>nil then exit;
  Result:=inherited FindIdentifier(Identifier);
end;

procedure TPasDotEnumTypeScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  EnumScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasDotEnumTypeScope.WriteIdentifiers(Prefix: string);
begin
  EnumScope.WriteIdentifiers(Prefix);
  inherited WriteIdentifiers(Prefix);
end;

{ TPasRecordCompositionScope }

constructor TPasRecordCompositionScope.Create;
begin
  inherited Create;
  Items:=TFPList.Create;
end;

destructor TPasRecordCompositionScope.Destroy;
begin
  FreeAndNil(Items);
  inherited Destroy;
end;

class function TPasRecordCompositionScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

class function TPasRecordCompositionScope.FreeOnPop: boolean;
begin
  Result:=false;
end;

function TPasRecordCompositionScope.AddComposition(const aName: string;
  El: TPasElement; const aPath: TPasVariableArray;
  aVisibility: TPasMemberVisibility): TPasCompositionIdentifier;
begin
  Result:=TPasCompositionIdentifier.Create;
  Result.Identifier:=aName;
  Result.Element:=El;
  if El is TPasProcedure then
    Result.Kind:=pikProc
  else
    Result.Kind:=pikSimple;
  Result.Path:=aPath;
  Result.Visibility:=aVisibility;
  InternalAdd(Result);
  Items.Add(Result);
end;

function TPasRecordCompositionScope.FindComposition(El: TPasElement
  ): TPasCompositionIdentifier;
var
  Item: TPasIdentifier;
begin
  Item:=FindLocalIdentifier(El.Name);
  while Item<>nil do
    begin
    if Item.Element=El then
      exit(Item as TPasCompositionIdentifier);
    Item:=Item.NextSameIdentifier;
    end;
  Result:=nil;
end;

{ TPasRecordScope }

destructor TPasRecordScope.Destroy;
begin
  FreeAndNil(CompositionScope);
  inherited Destroy;
end;

{ TPasGroupScope }

procedure TPasGroupScope.Add(Scope: TPasIdentifierScope);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if Scopes[i]=Scope then exit; // already added
  if Scope.FreeOnPop then
    raise Exception.Create('TPasGroupScope.Add '+GetObjName(Scope)+' '+GetObjName(Scope.Element));
  if Count=length(Scopes) then
    SetLength(Scopes,Count*2+4);
  Scopes[Count]:=Scope;
  inc(Count);
end;

destructor TPasGroupScope.Destroy;
begin
  Scopes:=nil;
  Count:=0;
  inherited Destroy;
end;

function TPasGroupScope.GetFirstNonHelperScope: TPasIdentifierScope;
var
  i: Integer;
  Scope: TPasIdentifierScope;
begin
  for i:=0 to Count-1 do
    begin
    Scope:=Scopes[i];
    if (Scope.ClassType<>TPasClassScope)
        or (TPasClassType(Scope.Element).HelperForType=nil) then
      exit(Scope);
    end;
  Result:=nil;
end;

class function TPasGroupScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

function TPasGroupScope.FindAncestorIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  i: Integer;
begin
  for i:=1 to Count-1 do
    begin
    Result:=Scopes[i].FindIdentifier(Identifier);
    if Result<>nil then exit;
    end;
  Result:=nil;
end;

function TPasGroupScope.FindAncestorElement(const Identifier: String
  ): TPasElement;
var
  Item: TPasIdentifier;
begin
  Item:=FindAncestorIdentifier(Identifier);
  if Item<>nil then
    Result:=Item.Element
  else
    Result:=nil;
end;

function TPasGroupScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    begin
    Result:=Scopes[i].FindIdentifier(Identifier);
    if Result<>nil then exit;
    end;
  Result:=nil;
end;

procedure TPasGroupScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    begin
    Scopes[i].IterateElements(aName,StartScope,OnIterateElement,Data,Abort);
    if Abort then exit;
    end;
end;

procedure TPasGroupScope.WriteIdentifiers(Prefix: string);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Scopes[i].WriteIdentifiers(Prefix+'Group['+IntToStr(i)+'/'+IntToStr(Count)+']');
end;

{$ifdef pas2js}
{ TPasResHashList }

constructor TPasResHashList.Create;
begin
  FItems:=TJSObject.new;
end;

procedure TPasResHashList.Add(const aName: string; Item: Pointer);
begin
  FItems['%'+aName]:=Item;
end;

function TPasResHashList.Find(const aName: string): Pointer;
begin
  if FItems.hasOwnProperty('%'+aName) then
    Result:=Pointer(FItems['%'+aName])
  else
    Result:=nil;
end;

procedure TPasResHashList.ForEachCall(const Proc: TPasResIterate; Arg: Pointer);
var
  key: string;
begin
  for key in FItems do
    if FItems.hasOwnProperty(key) then
      Proc(Pointer(FItems[key]),Arg);
end;

procedure TPasResHashList.Clear;
begin
  FItems:=TJSObject.new;
end;

procedure TPasResHashList.Remove(const aName: string);
begin
  if FItems.hasOwnProperty('%'+aName) then
    JSDelete(FItems,'%'+aName);
end;
{$endif}

{ TResElDataBuiltInProc }

destructor TResElDataBuiltInProc.Destroy;
begin
  Proc:=nil;
  inherited Destroy;
end;

{ TPasClassIntfMap }

destructor TPasClassIntfMap.Destroy;
begin
  Element:=nil;
  Intf:=nil;
  FreeAndNil(Procs);
  FreeAndNil(AncestorMap);
  inherited Destroy;
end;

{ TPasInitialFinalizationScope }

function TPasInitialFinalizationScope.AddReference(El: TPasElement;
  Access: TPSRefAccess): TPasScopeReference;
begin
  if References=nil then
    References:=TPasScopeReferences.Create(Self);
  Result:=References.Add(El,Access);
end;

destructor TPasInitialFinalizationScope.Destroy;
begin
  FreeAndNil(References);
  inherited Destroy;
end;

{ TPasScopeReference }

procedure TPasScopeReference.SetElement(const AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  FElement:=AValue;
end;

destructor TPasScopeReference.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcScopeReference.Destroy START ',ClassName,' "',GetObjName(Element),'"');
  {$ENDIF}
  Element:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcScopeReference.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasScopeReferences }

procedure TPasScopeReferences.OnClearItem(Item, Dummy: pointer);
var
  Ref: TPasScopeReference absolute Item;
  Ref2: TPasScopeReference;
begin
  if Dummy=nil then ;
  //writeln('TPasProcedureScope.OnClearReferenceItem ',GetObjName(Ref.Element));
  while Ref<>nil do
    begin
    Ref2:=Ref;
    Ref:=Ref.NextSameName;
    Ref2.Free;
    end;
end;

procedure TPasScopeReferences.OnCollectItem(Item, aList: pointer);
var
  Ref: TPasScopeReference absolute Item;
  List: TFPList absolute aList;
begin
  while Ref<>nil do
    begin
    List.Add(Ref);
    Ref:=Ref.NextSameName;
    end;
end;

constructor TPasScopeReferences.Create(aScope: TPasScope);
begin
  References:=TPasResHashList.Create;
  FScope:=aScope;
end;

destructor TPasScopeReferences.Destroy;
begin
  Clear;
  {$ifdef pas2js}
  References:=nil;
  {$else}
  FreeAndNil(References);
  {$endif}
  inherited Destroy;
end;

procedure TPasScopeReferences.Clear;
begin
  if References=nil then exit;
  References.ForEachCall(@OnClearItem,nil);
  References.Clear;
end;

function TPasScopeReferences.Add(El: TPasElement; Access: TPSRefAccess
  ): TPasScopeReference;
var
  LoName: String;
  OldItem, Item, LastItem: TPasScopeReference;
begin
  LoName:=lowercase(El.Name);
  OldItem:=TPasScopeReference(References.Find(LoName));
  Item:=OldItem;
  LastItem:=nil;
  while Item<>nil do
    begin
    if Item.Element=El then
      begin
      // already marked as used -> combine access
      case Access of
      psraNone: ;
      psraRead:
        case Item.Access of
          psraNone: Item.Access:=Access;
          //psraRead: ;
          psraWrite: Item.Access:=psraWriteRead;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraWrite:
        case Item.Access of
          psraNone: Item.Access:=Access;
          psraRead: Item.Access:=psraReadWrite;
          //psraWrite: ;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraReadWrite:
        case Item.Access of
          psraNone: Item.Access:=Access;
          psraRead: Item.Access:=psraReadWrite;
          psraWrite: Item.Access:=psraWriteRead;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraWriteRead:
        case Item.Access of
          psraNone: Item.Access:=Access;
          psraRead: Item.Access:=psraReadWrite;
          psraWrite: Item.Access:=psraWriteRead;
          //psraReadWrite: ;
          //psraWriteRead: ;
          //psraTypeInfo: ;
        end;
      psraTypeInfo: Item.Access:=psraTypeInfo;
      else
        raise EPasResolve.Create(GetObjName(El)+' unknown Access');
      end;
      exit(Item);
      end;
    LastItem:=Item;
    Item:=Item.NextSameName;
    end;
  // new reference
  Item:=TPasScopeReference.Create;
  Item.Element:=El;
  Item.Access:=Access;
  if LastItem=nil then
    begin
    References.Add(LoName,Item);
    {$IFDEF VerbosePCUFiler}
    if TPasScopeReference(References.Find(LoName))<>Item then
      raise EPasResolve.Create('20180219230028');
    {$ENDIF}
    end
  else
    LastItem.NextSameName:=Item;
  Result:=Item;
end;

function TPasScopeReferences.Find(const aName: string): TPasScopeReference;
var
  LoName: String;
begin
  if References=nil then exit(nil);
  LoName:=lowercase(aName);
  Result:=TPasScopeReference(References.Find(LoName));
end;

function TPasScopeReferences.GetList: TFPList;
begin
  Result:=TFPList.Create;
  if References=nil then exit;
  References.ForEachCall(@OnCollectItem,Result);
end;

{ TPasPropertyScope }

destructor TPasPropertyScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasPropertyScope.Destroy START ',ClassName);
  {$ENDIF}
  AncestorProp:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasPropertyScope.Destroy END',ClassName);
  {$ENDIF}
end;

{ TPasEnumTypeScope }

destructor TPasEnumTypeScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasEnumTypeScope.Destroy START ',ClassName);
  {$ENDIF}
  CanonicalSet:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasEnumTypeScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasDotBaseScope }

function TPasDotBaseScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=GroupScope.FindIdentifier(Identifier);
end;

procedure TPasDotBaseScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  GroupScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasDotBaseScope.WriteIdentifiers(Prefix: string);
begin
  GroupScope.WriteIdentifiers(Prefix);
end;

destructor TPasDotBaseScope.Destroy;
begin
  FreeAndNil(GroupScope);
  inherited Destroy;
end;

{ TPasWithExprScope }

class function TPasWithExprScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

class function TPasWithExprScope.FreeOnPop: boolean;
begin
  Result:=false;
end;

procedure TPasWithExprScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  Scope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasWithExprScope.WriteIdentifiers(Prefix: string);
begin
  {AllowWriteln}
  writeln(Prefix+'WithExpr: '+GetTreeDbg(Expr,length(Prefix)));
  Scope.WriteIdentifiers(Prefix);
  {AllowWriteln-}
end;

destructor TPasWithExprScope.Destroy;
begin
  FreeAndNil(Scope);
  inherited Destroy;
end;

{ TPasWithScope }

constructor TPasWithScope.Create;
begin
  inherited Create;
  ExpressionScopes:=TObjectList.Create(true);
end;

destructor TPasWithScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasWithScope.Destroy START ',ClassName);
  {$ENDIF}
  FreeAndNil(ExpressionScopes);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasWithScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasProcedureScope }

function TPasProcedureScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=inherited FindIdentifier(Identifier);
  if (Result<>nil) or (GroupScope=nil) then exit;
  Result:=GroupScope.FindIdentifier(Identifier);
end;

procedure TPasProcedureScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  if GroupScope=nil then exit;
  GroupScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

function TPasProcedureScope.GetSelfScope: TPasProcedureScope;
var
  Proc: TPasProcedure;
  El: TPasElement;
begin
  Result:=Self;
  repeat
    if Result.ClassRecScope<>nil then exit;
    Proc:=TPasProcedure(Result.Element);
    El:=Proc.Parent;
    repeat
      if El=nil then exit(nil);
      if El is TProcedureBody then break;
      El:=El.Parent;
    until false;
    Proc:=El.Parent as TPasProcedure;
    Result:=TPasProcedureScope(Proc.CustomData);
  until false;
end;

procedure TPasProcedureScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  if GroupScope<>nil then
    GroupScope.WriteIdentifiers(Prefix+'GS ');
end;

destructor TPasProcedureScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcedureScope.Destroy START ',ClassName);
  {$ENDIF}
  FreeAndNil(References);
  FreeAndNil(GroupScope);
  NestedMembersScope:=nil; // NestedMembersScope is auto freed
  inherited Destroy;
  SelfArg:=nil;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcedureScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasProcedureScope.AddReference(El: TPasElement; Access: TPSRefAccess
  ): TPasScopeReference;
begin
  if References=nil then
    References:=TPasScopeReferences.Create(Self);
  Result:=References.Add(El,Access);
end;

function TPasProcedureScope.GetReferences: TFPList;
begin
  if References=nil then
    Result:=TFPList.Create
  else
    Result:=References.GetList;
end;

{ TPasClassScope }

destructor TPasClassScope.Destroy;
var
  i: Integer;
  o: TObject;
begin
  if Interfaces<>nil then
    begin
    for i:=0 to Interfaces.Count-1 do
      begin
      o:=TObject(Interfaces[i]);
      if o=nil then
      else if o is TPasProperty then
      else if o is TPasClassIntfMap then
        o.Free
      else
        raise Exception.Create('[20180322132757] '+GetElementDbgPath(Element)+' i='+IntToStr(i)+' '+GetObjName(o));
      end;
    FreeAndNil(Interfaces);
    end;
  CanonicalClassOf:=nil;
  inherited Destroy;
end;

{ TPasIdentifier }

procedure TPasIdentifier.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  FElement:=AValue;
end;

destructor TPasIdentifier.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifier.Destroy START ',ClassName,' "',Identifier,'"');
  {$ENDIF}
  Element:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifier.Destroy END ',ClassName);
  {$ENDIF}
end;

{ EPasResolve }

procedure EPasResolve.SetPasElement(AValue: TPasElement);
begin
  if FPasElement=AValue then Exit;
  FPasElement:=AValue;
end;

destructor EPasResolve.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('EPasResolve.Destroy START ',ClassName);
  {$ENDIF}
  PasElement:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('EPasResolve.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TResolvedReference }

procedure TResolvedReference.SetDeclaration(AValue: TPasElement);
begin
  if FDeclaration=AValue then Exit;
  FDeclaration:=AValue;
end;

destructor TResolvedReference.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolvedReference.Destroy START ',ClassName);
  {$ENDIF}
  Declaration:=nil;
  FreeAndNil(Context);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolvedReference.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasSubExprScope }

class function TPasSubExprScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasModuleDotScope }

procedure TPasModuleDotScope.OnInternalIterate(El: TPasElement; ElScope,
  StartScope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if (El.ClassType=TPasModule) or (El.ClassType=TPasUsesUnit) then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,ElScope,StartScope,FilterData^.Data,Abort);
end;

procedure TPasModuleDotScope.SetModule(AValue: TPasModule);
begin
  if FModule=AValue then Exit;
  FModule:=AValue;
end;

destructor TPasModuleDotScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSubModuleScope.Destroy START ',ClassName);
  {$ENDIF}
  Module:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSubModuleScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasModuleDotScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;

  function Find(Scope: TPasIdentifierScope): boolean;
  var
    Found: TPasIdentifier;
    C: TClass;
  begin
    if Scope=nil then exit(false);
    Found:=Scope.FindLocalIdentifier(Identifier);
    FindIdentifier:=Found;
    if Found=nil then exit(false);
    C:=Found.Element.ClassType;
    Result:=(C<>TPasModule) and (C<>TPasUsesUnit);
  end;

begin
  Result:=nil;
  if Find(ImplementationScope) then exit;
  if Find(InterfaceScope) then exit;
  Find(SystemScope);
end;

procedure TPasModuleDotScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  FilterData: TPasIterateFilterData;

  function Iterate(Scope: TPasIdentifierScope): boolean;
  begin
    if Scope=nil then exit(false);
    Scope.IterateLocalElements(aName,StartScope,@OnInternalIterate,@FilterData,Abort);
    Result:=Abort;
  end;

begin
  FilterData.OnIterate:=OnIterateElement;
  FilterData.Data:=Data;
  if Iterate(ImplementationScope) then exit;
  if Iterate(InterfaceScope) then exit;
  Iterate(SystemScope);
end;

procedure TPasModuleDotScope.WriteIdentifiers(Prefix: string);
begin
  if ImplementationScope<>nil then
    ImplementationScope.WriteIdentifiers(Prefix+'  ');
  if InterfaceScope<>nil then
    InterfaceScope.WriteIdentifiers(Prefix+'  ');
  if SystemScope<>nil then
    SystemScope.WriteIdentifiers(Prefix+'  ');
end;

{ TPasSectionScope }

procedure TPasSectionScope.OnInternalIterate(El: TPasElement; ElScope,
  StartScope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if (El.ClassType=TPasModule) or (El.ClassType=TPasUsesUnit) then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,ElScope,StartScope,FilterData^.Data,Abort);
end;

constructor TPasSectionScope.Create;
begin
  inherited Create;
  UsesScopes:=TFPList.Create;
end;

destructor TPasSectionScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSectionScope.Destroy START ',ClassName);
  {$ENDIF}
  ClearHelperList(Helpers);
  FreeAndNil(UsesScopes);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSectionScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasSectionScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;

  procedure SearchUses(aList: TFPList; var aFound: TPasIdentifier; out Done: boolean);
  var
    i: Integer;
    UsesScope: TPasIdentifierScope;
    C: TClass;
    Cur: TPasIdentifier;
  begin
    Done:=false;
    if aList=nil then exit;
    for i:=aList.Count-1 downto 0 do
      begin
      UsesScope:=TPasIdentifierScope(aList[i]);
      {$IFDEF VerbosePasResolver}
      writeln('TPasSectionScope.FindIdentifier "',Identifier,'" in used unit ',GetObjName(UsesScope.Element));
      {$ENDIF}
      Cur:=UsesScope.FindLocalIdentifier(Identifier);
      if Cur<>nil then
        begin
        aFound:=Cur;
        C:=Cur.Element.ClassType;
        if (C<>TPasModule) and (C<>TPasUsesUnit) then
          begin
          Done:=true;
          exit;
          end;
        end;
      end;
  end;

var
  Done: boolean;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then
    exit;
  // the units used by the implementation section are searched together with the
  // interface section, so that the unit's own declarations shadow them
  if Element.ClassType=TImplementationSection then
    exit;
  SearchUses(ImplUsesScopes,Result,Done);
  if Done then exit;
  SearchUses(UsesScopes,Result,Done);
end;

procedure TPasSectionScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  FilterData: TPasIterateFilterData;

  procedure IterateUses(aList: TFPList);
  var
    i: Integer;
    UsesScope: TPasSectionScope;
  begin
    if aList=nil then exit;
    for i:=aList.Count-1 downto 0 do
      begin
      UsesScope:=TPasSectionScope(aList[i]);
      {$IFDEF VerbosePasResolver}
      writeln('TPasSectionScope.IterateElements "',aName,'" in used unit ',UsesScope.Element.ParentPath,':',GetObjName(UsesScope.Element));
      {$ENDIF}
      UsesScope.IterateLocalElements(aName,StartScope,@OnInternalIterate,@FilterData,Abort);
      if Abort then exit;
      end;
  end;

begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  // the units used by the implementation section are searched together with the
  // interface section, so that the unit's own declarations shadow them
  if Element.ClassType=TImplementationSection then
    exit;
  FilterData.OnIterate:=OnIterateElement;
  FilterData.Data:=Data;
  IterateUses(ImplUsesScopes);
  if Abort then exit;
  IterateUses(UsesScopes);
end;

procedure TPasSectionScope.WriteIdentifiers(Prefix: string);
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
  SubPrefix: String;
begin
  {AllowWriteln}
  inherited WriteIdentifiers(Prefix);
  SubPrefix:=Prefix+'    ';
  for i:=UsesScopes.Count-1 downto 0 do
    begin
    UsesScope:=TPasIdentifierScope(UsesScopes[i]);
    writeln(Prefix+'  Uses: '+GetObjName(UsesScope.Element)+' "'+UsesScope.Element.GetModule.Name+'"');
    UsesScope.FItems.ForEachCall(@OnWriteItem,Pointer(SubPrefix));
    end;
  {AllowWriteln-}
end;

{ TPasModuleScope }

procedure TPasModuleScope.SetAssertClass(const AValue: TPasClassType);
begin
  if FAssertClass=AValue then Exit;
  FAssertClass:=AValue;
end;

procedure TPasModuleScope.SetAssertDefConstructor(const AValue: TPasConstructor
  );
begin
  if FAssertDefConstructor=AValue then Exit;
  FAssertDefConstructor:=AValue;
end;

procedure TPasModuleScope.SetAssertMsgConstructor(const AValue: TPasConstructor
  );
begin
  if FAssertMsgConstructor=AValue then Exit;
  FAssertMsgConstructor:=AValue;
end;

procedure TPasModuleScope.SetRangeErrorClass(const AValue: TPasClassType);
begin
  if FRangeErrorClass=AValue then Exit;
  FRangeErrorClass:=AValue;
end;

procedure TPasModuleScope.SetRangeErrorConstructor(const AValue: TPasConstructor
  );
begin
  if FRangeErrorConstructor=AValue then Exit;
  FRangeErrorConstructor:=AValue;
end;

procedure TPasModuleScope.SetSystemTVarRec(const AValue: TPasRecordType);
begin
  if FSystemTVarRec=AValue then Exit;
  FSystemTVarRec:=AValue;
end;

constructor TPasModuleScope.Create;
begin
  inherited Create;
  PendingResolvers:=TFPList.Create;
end;

destructor TPasModuleScope.Destroy;
begin
  AssertClass:=nil;
  AssertDefConstructor:=nil;
  AssertMsgConstructor:=nil;
  RangeErrorClass:=nil;
  RangeErrorConstructor:=nil;
  SystemTVarRec:=nil;
  FreeAndNil(PendingResolvers);
  inherited Destroy;
end;

procedure TPasModuleScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  if CompareText(aName,FirstName)<>0 then exit;
  OnIterateElement(Element,Self,StartScope,Data,Abort);
end;

{ TPasDefaultScope }

class function TPasDefaultScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasScope }

class function TPasScope.IsStoredInElement: boolean;
begin
  Result:=true;
end;

class function TPasScope.FreeOnPop: boolean;
begin
  Result:=not IsStoredInElement;
end;

procedure TPasScope.IterateElements(const aName: string; StartScope: TPasScope;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
begin
  if aName='' then ;
  if StartScope=nil then ;
  if Data=nil then ;
  if OnIterateElement=nil then ;
  if Abort then ;
end;

procedure TPasScope.WriteIdentifiers(Prefix: string);
begin
  {AllowWriteln}
  writeln(Prefix,'(',ClassName,') Element: ',GetObjName(Element));
  {AllowWriteln-}
end;

{ TPasIdentifierScope }

// inline
function TPasIdentifierScope.FindLocalIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=TPasIdentifier(FItems.Find(lowercase(Identifier)));
end;

procedure TPasIdentifierScope.OnClearItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Ident: TPasIdentifier;
begin
  if Dummy=nil then ;
  //writeln('TPasIdentifierScope.OnClearItem ',PasIdentifier.Identifier+':'+PasIdentifier.ClassName);
  while PasIdentifier<>nil do
    begin
    Ident:=PasIdentifier;
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    Ident.Free;
    end;
end;

procedure TPasIdentifierScope.OnCollectItem(Item, List: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  FPList: TFPList absolute List;
begin
  FPList.Add(PasIdentifier);
end;

procedure TPasIdentifierScope.OnWriteItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Prefix: String;
begin
  {AllowWriteln}
  Prefix:=String(Dummy);
  while PasIdentifier<>nil do
    begin
    writeln(Prefix,'Identifier="',PasIdentifier.Identifier,'" Element=',GetObjName(PasIdentifier.Element));
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    end;
  {AllowWriteln-}
end;

procedure TPasIdentifierScope.InternalAdd(Item: TPasIdentifier);
var
  OldItem: TPasIdentifier;
  LoName: string;
  {$ifdef pas2js}
  {$ELSE}
  Index: Integer;
  {$ENDIF}
begin
  LoName:=lowercase(Item.Identifier);
  {$ifdef pas2js}
  OldItem:=TPasIdentifier(FItems.Find(LoName));
  if OldItem<>nil then
    begin
    // insert LIFO - last in, first out
    Item.NextSameIdentifier:=OldItem;
    end;
  FItems.Add(LoName,Item);
  {$IFDEF VerbosePasResolver}
  if Item.Owner<>nil then
    raise Exception.Create('20160925184110');
  Item.Owner:=Self;
  {$ENDIF}
  {$IFDEF VerbosePasResolver}
  if FindIdentifier(Item.Identifier)<>Item then
    raise Exception.Create('20181018173201');
  {$ENDIF}
  {$else}
  Index:=FItems.FindIndexOf(LoName);
  {$IFDEF VerbosePasResolver}
  if Item.Owner<>nil then
    raise Exception.Create('20160925184110');
  Item.Owner:=Self;
  {$ENDIF}
  //writeln('  Index=',Index);
  if Index>=0 then
    begin
    // insert LIFO - last in, first out
    {$IF FPC_FULLVERSION<30204}
    OldItem:=TPasIdentifier(FItems.List^[Index].Data);
    {$ELSE}
    OldItem:=TPasIdentifier(FItems.List[Index].Data);
    {$ENDIF}
    {$IFDEF VerbosePasResolver}
    if lowercase(OldItem.Identifier)<>LoName then
      raise Exception.Create('20160925183438');
    {$ENDIF}
    Item.NextSameIdentifier:=OldItem;
    {$IF FPC_FULLVERSION<30204}
    FItems.List^[Index].Data:=Item;
    {$ELSE}
    FItems.List[Index].Data:=Item;
    {$ENDIF}
    end
  else
    begin
    FItems.Add(LoName, Item);
    {$IFDEF VerbosePasResolver}
    if FindIdentifier(Item.Identifier)<>Item then
      raise Exception.Create('20160925183849');
    {$ENDIF}
    end;
  {$endif}
end;

constructor TPasIdentifierScope.Create;
begin
  FItems:=TPasResHashList.Create;
end;

destructor TPasIdentifierScope.Destroy;
begin
  ClearIdentifiers(true);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Destroy END ',ClassName);
  {$ENDIF}
end;

procedure TPasIdentifierScope.ClearIdentifiers(FreeItems: boolean);
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Clear START ',ClassName);
  {$ENDIF}

  FItems.ForEachCall(@OnClearItem,nil);

  {$ifdef pas2js}
  if FreeItems then
    FItems:=nil
  else
    FItems.Clear;
  {$else}
  FItems.Clear;
  if FreeItems then
    FreeAndNil(FItems);
  {$endif}

  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Clear END ',ClassName);
  {$ENDIF}
end;

function TPasIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=FindLocalIdentifier(Identifier);
  {$IFDEF VerbosePasResolver}
  {AllowWriteln}
  if (Result<>nil) and (Result.Owner<>Self) then
    begin
    writeln('TPasIdentifierScope.FindIdentifier Result.Owner<>Self Owner='+GetObjName(Result.Owner));
    raise Exception.Create('20160925184159');
    end;
  {AllowWriteln-}
  {$ENDIF}
end;

function TPasIdentifierScope.RemoveLocalIdentifier(El: TPasElement): boolean;
var
  Identifier, PrevIdentifier: TPasIdentifier;
  LoName: string;
begin
  LoName:=lowercase(El.Name);
  Identifier:=TPasIdentifier(FItems.Find(LoName));
  FindLocalIdentifier(El.Name);
  PrevIdentifier:=nil;
  Result:=false;
  while Identifier<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    if (Identifier.Owner<>Self) then
      raise Exception.Create('20160925184159');
    {$ENDIF}
    if Identifier.Element=El then
      begin
      if PrevIdentifier<>nil then
        begin
        PrevIdentifier.NextSameIdentifier:=Identifier.NextSameIdentifier;
        Identifier.Free;
        Identifier:=PrevIdentifier.NextSameIdentifier;
        end
      else
        begin
        FItems.Remove({$ifdef pas2js}LoName{$else}Identifier{$endif});
        PrevIdentifier:=Identifier;
        Identifier:=Identifier.NextSameIdentifier;
        PrevIdentifier.Free;
        PrevIdentifier:=nil;
        if Identifier<>nil then
          FItems.Add(LoName,Identifier);
        end;
      Result:=true;
      continue;
      end;
    PrevIdentifier:=Identifier;
    Identifier:=Identifier.NextSameIdentifier;
    end;
end;

function TPasIdentifierScope.AddIdentifier(const Identifier: String;
  El: TPasElement; const Kind: TPasIdentifierKind): TPasIdentifier;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.AddIdentifier Identifier="',Identifier,'" El=',GetObjName(El));
  Item:=TPasIdentifier.Create;
  Item.Identifier:=Identifier;
  Item.Element:=El;
  Item.Kind:=Kind;

  InternalAdd(Item);
  //writeln('TPasIdentifierScope.AddIdentifier END');
  Result:=Item;
end;

function TPasIdentifierScope.FindElement(const aName: string): TPasElement;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.FindElement "',aName,'"');
  Item:=FindIdentifier(aName);
  if Item=nil then
    Result:=nil
  else
    Result:=Item.Element;
  //writeln('TPasIdentifierScope.FindElement Found="',GetObjName(Result),'"');
end;

procedure TPasIdentifierScope.IterateLocalElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  Item: TPasIdentifier;
  {$IFDEF VerbosePasResolver}
  OldElement: TPasElement;
  {$ENDIF}
begin
  Item:=FindLocalIdentifier(aName);
  while Item<>nil do
    begin
    //writeln('TPasIdentifierScope.IterateLocalElements ',ClassName,' ',Item.Identifier,' ',GetObjName(Item.Element));
    {$IFDEF VerbosePasResolver}
    OldElement:=Item.Element;
    {$ENDIF}
    OnIterateElement(Item.Element,Self,StartScope,Data,Abort);
    {$IFDEF VerbosePasResolver}
    if OldElement<>Item.Element then
      raise Exception.Create('20160925183503');
    {$ENDIF}
    if Abort then exit;
    Item:=Item.NextSameIdentifier;
    end;
end;

procedure TPasIdentifierScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  IterateLocalElements(aName,StartScope,OnIterateElement,Data,Abort);
end;

procedure TPasIdentifierScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  WriteLocalIdentifiers(Prefix+'  ');
end;

procedure TPasIdentifierScope.WriteLocalIdentifiers(Prefix: string);
begin
  FItems.ForEachCall(@OnWriteItem,Pointer(Prefix));
end;

function TPasIdentifierScope.GetLocalIdentifiers: TFPList;
begin
  Result:=TFPList.Create;
  FItems.ForEachCall(@OnCollectItem,Result);
end;
