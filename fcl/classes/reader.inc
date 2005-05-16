{
    $Id: reader.inc,v 1.4 2005/02/14 17:13:11 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                       TBinaryObjectReader                                *}
{****************************************************************************}

constructor TBinaryObjectReader.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  GetMem(FBuffer, BufSize);
end;

destructor TBinaryObjectReader.Destroy;
begin
  { Seek back the amount of bytes that we didn't process until now: }
  FStream.Seek(Integer(FBufPos) - Integer(FBufEnd), soFromCurrent);

  if Assigned(FBuffer) then
    FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

function TBinaryObjectReader.ReadValue: TValueType;
begin
  Result := vaNull; { Necessary in FPC as TValueType is larger than 1 byte! }
  Read(Result, 1);
end;

function TBinaryObjectReader.NextValue: TValueType;
begin
  Result := ReadValue;
  { We only 'peek' at the next value, so seek back to unget the read value: }
  Dec(FBufPos);
end;

procedure TBinaryObjectReader.BeginRootComponent;
var
  Signature: LongInt;
begin
  { Read filer signature }
  Read(Signature, 4);
  if Signature <> LongInt(FilerSignature) then
    raise EReadError.Create(SInvalidImage);
end;

procedure TBinaryObjectReader.BeginComponent(var Flags: TFilerFlags;
  var AChildPos: Integer; var CompClassName, CompName: String);
var
  Prefix: Byte;
  ValueType: TValueType;
begin
  { Every component can start with a special prefix: }
  Flags := [];
  if (Byte(NextValue) and $f0) = $f0 then
  begin
    Prefix := Byte(ReadValue);
    Flags := TFilerFlags(Prefix and $0f);
    if ffChildPos in Flags then
    begin
      ValueType := NextValue;
      case ValueType of
        vaInt8:
          AChildPos := ReadInt8;
        vaInt16:
          AChildPos := ReadInt16;
        vaInt32:
          AChildPos := ReadInt32;
        else
          raise EReadError.Create(SInvalidPropertyValue);
      end;
    end;
  end;

  CompClassName := ReadStr;
  CompName := ReadStr;
end;

function TBinaryObjectReader.BeginProperty: String;
begin
  Result := ReadStr;
end;

procedure TBinaryObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  BinSize: LongInt;
begin
  Read(BinSize, 4);
  DestData.Size := BinSize;
  Read(DestData.Memory^, BinSize);
end;

function TBinaryObjectReader.ReadFloat: Extended;
begin
  Read(Result, SizeOf(Extended))
end;

function TBinaryObjectReader.ReadSingle: Single;
begin
  Read(Result, SizeOf(Single))
end;

{!!!: function TBinaryObjectReader.ReadCurrency: Currency;
begin
  Read(Result, SizeOf(Currency))
end;}

function TBinaryObjectReader.ReadDate: TDateTime;
begin
  Read(Result, SizeOf(TDateTime))
end;

function TBinaryObjectReader.ReadIdent(ValueType: TValueType): String;
var
  i: Byte;
begin
  case ValueType of
    vaIdent:
      begin
        Read(i, 1);
        SetLength(Result, i);
        Read(Pointer(@Result[1])^, i);
      end;
    vaNil:
      Result := 'nil';
    vaFalse:
      Result := 'False';
    vaTrue:
      Result := 'True';
    vaNull:
      Result := 'Null';
  end;
end;

function TBinaryObjectReader.ReadInt8: ShortInt;
begin
  Read(Result, 1);
end;

function TBinaryObjectReader.ReadInt16: SmallInt;
begin
  Read(Result, 2);
end;

function TBinaryObjectReader.ReadInt32: LongInt;
begin
  Read(Result, 4);
end;

function TBinaryObjectReader.ReadInt64: Int64;
begin
  Read(Result, 8);
end;

function TBinaryObjectReader.ReadSet(EnumType: Pointer): Integer;
var
  Name: String;
  Value: Integer;
begin
  try
    Result := 0;
    while True do
    begin
      Name := ReadStr;
      if Length(Name) = 0 then
        break;
      Value := GetEnumValue(PTypeInfo(EnumType), Name);
      if Value = -1 then
        raise EReadError.Create(SInvalidPropertyValue);
      Result := Result or (1 shl Value);
    end;
  except
    SkipSetBody;
    raise;
  end;
end;

function TBinaryObjectReader.ReadStr: String;
var
  i: Byte;
begin
  Read(i, 1);
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i);
end;

function TBinaryObjectReader.ReadString(StringType: TValueType): String;
var
  i: Integer;
begin
  case StringType of
    vaString:
      begin
        i := 0;
        Read(i, 1);
      end;
    vaLString:
      Read(i, 4);
  end;
  SetLength(Result, i);
  if i > 0 then
    Read(Pointer(@Result[1])^, i);
end;

{!!!: function TBinaryObjectReader.ReadWideString: WideString;
var
  i: Integer;
begin
  FDriver.Read(i, 4);
  SetLength(Result, i);
  if i > 0 then
    Read(PWideChar(Result), i * 2);
end;}

procedure TBinaryObjectReader.SkipComponent(SkipComponentInfos: Boolean);
var
  Flags: TFilerFlags;
  Dummy: Integer;
  CompClassName, CompName: String;
begin
  if SkipComponentInfos then
    { Skip prefix, component class name and component object name }
    BeginComponent(Flags, Dummy, CompClassName, CompName);

  { Skip properties }
  while NextValue <> vaNull do
    SkipProperty;
  ReadValue;

  { Skip children }
  while NextValue <> vaNull do
    SkipComponent(True);
  ReadValue;
end;

procedure TBinaryObjectReader.SkipValue;

  procedure SkipBytes(Count: LongInt);
  var
    Dummy: array[0..1023] of Byte;
    SkipNow: Integer;
  begin
    while Count > 0 do
    begin
      if Count > 1024 then
        SkipNow := 1024
      else
        SkipNow := Count;
      Read(Dummy, SkipNow);
      Dec(Count, SkipNow);
    end;
  end;

var
  Count: LongInt;
begin
  case ReadValue of
    vaNull, vaFalse, vaTrue, vaNil: ;
    vaList:
      begin
        while NextValue <> vaNull do
          SkipValue;
        ReadValue;
      end;
    vaInt8:
      SkipBytes(1);
    vaInt16:
      SkipBytes(2);
    vaInt32:
      SkipBytes(4);
    vaExtended:
      SkipBytes(SizeOf(Extended));
    vaString, vaIdent:
      ReadStr;
    vaBinary, vaLString, vaWString:
      begin
        Read(Count, 4);
        SkipBytes(Count);
      end;
    vaSet:
      SkipSetBody;
    vaCollection:
      begin
        while NextValue <> vaNull do
        begin
          { Skip the order value if present }
          if NextValue in [vaInt8, vaInt16, vaInt32] then
            SkipValue;
          SkipBytes(1);
          while NextValue <> vaNull do
            SkipProperty;
          ReadValue;
        end;
        ReadValue;
      end;
    vaSingle:
      SkipBytes(Sizeof(Single));
    {!!!: vaCurrency:
      SkipBytes(SizeOf(Currency));}
    vaDate:
      SkipBytes(Sizeof(TDateTime));
    vaInt64:
      SkipBytes(8);
  end;
end;

{ private methods }

procedure TBinaryObjectReader.Read(var Buf; Count: LongInt);
var
  CopyNow: LongInt;
  Dest: Pointer;
begin
  Dest := @Buf;
  while Count > 0 do
  begin
    if FBufPos >= FBufEnd then
    begin
      FBufEnd := FStream.Read(FBuffer^, FBufSize);
      if FBufEnd = 0 then
        raise EReadError.Create(SReadError);
      FBufPos := 0;
    end;
    CopyNow := FBufEnd - FBufPos;
    if CopyNow > Count then
      CopyNow := Count;
    Move(PChar(FBuffer)[FBufPos], Dest^, CopyNow);
    Inc(FBufPos, CopyNow);
    Inc(Dest, CopyNow);
    Dec(Count, CopyNow);
  end;
end;

procedure TBinaryObjectReader.SkipProperty;
begin
  { Skip property name, then the property value }
  ReadStr;
  SkipValue;
end;

procedure TBinaryObjectReader.SkipSetBody;
begin
  while Length(ReadStr) > 0 do;
end;



{****************************************************************************}
{*                             TREADER                                      *}
{****************************************************************************}


// This may be better put somewhere else:

type

  TFieldInfo = packed record
    FieldOffset: LongWord;
    ClassTypeIndex: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Word;
    Entries: array[Word] of TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    FieldCount: Word;
    ClassTable: PFieldClassTable;
    // Fields: array[Word] of TFieldInfo;  Elements have variant size!
  end;


function GetFieldClass(Instance: TObject; const ClassName: string): TPersistentClass;
var
  UClassName: String;
  ClassType: TClass;
  ClassTable: PFieldClassTable;
  i: Integer;
  FieldTable: PFieldTable;
begin
  // At first, try to locate the class in the class tables
  UClassName := UpperCase(ClassName);
  ClassType := Instance.ClassType;
  while ClassType <> TPersistent do
  begin
    FieldTable := PFieldTable((Pointer(ClassType) + vmtFieldTable)^);
    ClassTable := PFieldTable((Pointer(ClassType) + vmtFieldTable)^)^.ClassTable;
    if Assigned(ClassTable) then
      for i := 0 to ClassTable^.Count - 1 do
      begin
        Result := ClassTable^.Entries[i];
        if UpperCase(Result.ClassName) = UClassName then
          exit;
      end;
     // Try again with the parent class type
     ClassType := ClassType.ClassParent;
  end;
  Result := Classes.GetClass(ClassName);
end;


constructor TReader.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FDriver := TBinaryObjectReader.Create(Stream, BufSize);
end;

destructor TReader.Destroy;
begin
  FDriver.Free;
  inherited Destroy;
end;

procedure TReader.BeginReferences;
begin
  FLoaded := TList.Create;
  try
    FFixups := TList.Create;
  except
    FLoaded.Free;
    raise;
  end;
end;

procedure TReader.CheckValue(Value: TValueType);
begin
  if FDriver.NextValue <> Value then
    raise EReadError.Create(SInvalidPropertyValue)
  else
    FDriver.ReadValue;
end;

procedure TReader.DefineProperty(const Name: String; AReadData: TReaderProc;
  WriteData: TWriterProc; HasData: Boolean);
begin
  if Assigned(AReadData) and (UpperCase(Name) = UpperCase(FPropName)) then
  begin
    AReadData(Self);
    SetLength(FPropName, 0);
  end;
end;

procedure TReader.DefineBinaryProperty(const Name: String;
  AReadData, WriteData: TStreamProc; HasData: Boolean);
var
  MemBuffer: TMemoryStream;
begin
  if Assigned(AReadData) and (UpperCase(Name) = UpperCase(FPropName)) then
  begin
    { Check if the next property really is a binary property}
    if FDriver.NextValue <> vaBinary then
    begin
      FDriver.SkipValue;
      FCanHandleExcepts := True;
      raise EReadError.Create(SInvalidPropertyValue);
    end else
      FDriver.ReadValue;

    MemBuffer := TMemoryStream.Create;
    try
      FDriver.ReadBinary(MemBuffer);
      FCanHandleExcepts := True;
      AReadData(MemBuffer);
    finally
      MemBuffer.Free;
    end;
    SetLength(FPropName, 0);
  end;
end;

function TReader.EndOfList: Boolean;
begin
  Result := FDriver.NextValue = vaNull;
end;

procedure TReader.EndReferences;
begin
  FreeFixups;
  FLoaded.Free;
  FLoaded := nil;
end;

function TReader.Error(const Message: String): Boolean;
begin
  Result := False;
  if Assigned(FOnError) then
    FOnError(Self, Message, Result);
end;

function TReader.FindMethod(ARoot: TComponent; const AMethodName: String): Pointer;
var
  ErrorResult: Boolean;
begin
  Result := ARoot.MethodAddress(AMethodName);
  ErrorResult := Result = nil;

  { always give the OnFindMethod callback a chance to locate the method }
  if Assigned(FOnFindMethod) then
    FOnFindMethod(Self, AMethodName, Result, ErrorResult);

  if ErrorResult then
    raise EReadError.Create(SInvalidPropertyValue);
end;

procedure RemoveGlobalFixup(Fixup: TPropFixup);
var
  i: Integer;
begin
  with GlobalFixupList.LockList do
    try
      for i := Count - 1 downto 0 do
        with TPropFixup(Items[i]) do
          if (FInstance = Fixup.FInstance) and
            (FPropInfo = Fixup.FPropInfo) then
          begin
            Free;
            Delete(i);
          end;
    finally
      GlobalFixupList.UnlockList;
    end;
end;

procedure TReader.DoFixupReferences;
var
  i: Integer;
  CurFixup: TPropFixup;
  CurName: String;
  Target: Pointer;
begin
  if Assigned(FFixups) then
    try
      for i := 0 to FFixups.Count - 1 do
      begin
        CurFixup := TPropFixup(FFixups[i]);
        CurName := CurFixup.FName;
        if Assigned(FOnReferenceName) then
          FOnReferenceName(Self, CurName);
        Target := FindNestedComponent(CurFixup.FInstanceRoot, CurName);
        RemoveGlobalFixup(CurFixup);
        if (not Assigned(Target)) and CurFixup.MakeGlobalReference then
        begin
          GlobalFixupList.Add(CurFixup);
          FFixups[i] := nil;
        end else
          SetOrdProp(CurFixup.FInstance, CurFixup.FPropInfo, LongInt(Target));
      end;
    finally
      FreeFixups;
    end;
end;

procedure TReader.FixupReferences;
var
  i: Integer;
begin
  DoFixupReferences;
  GlobalFixupReferences;
  for i := 0 to FLoaded.Count - 1 do
    TComponent(FLoaded[I]).Loaded;
end;

procedure TReader.FreeFixups;
var
  i: Integer;
begin
  if Assigned(FFixups) then
  begin
    for i := 0 to FFixups.Count - 1 do
      TPropFixup(FFixups[I]).Free;
    FFixups.Free;
    FFixups := nil;
  end;
end;

function TReader.NextValue: TValueType;
begin
  Result := FDriver.NextValue;
end;

procedure TReader.PropertyError;
begin
  FDriver.SkipValue;
  raise EReadError.Create(SUnknownProperty);
end;

function TReader.ReadBoolean: Boolean;
var
  ValueType: TValueType;
begin
  ValueType := FDriver.ReadValue;
  if ValueType = vaTrue then
    Result := True
  else if ValueType = vaFalse then
    Result := False
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

function TReader.ReadChar: Char;
var
  s: String;
begin
  s := ReadString;
  if Length(s) = 1 then
    Result := s[1]
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

procedure TReader.ReadCollection(Collection: TCollection);
var
  Item: TPersistent;
begin
  Collection.BeginUpdate;
  try
    if not EndOfList then
      Collection.Clear;
    while not EndOfList do
    begin
      if FDriver.NextValue in [vaInt8, vaInt16, vaInt32] then
        ReadInteger;            { Skip order value }
      Item := Collection.Add;
      ReadListBegin;
      while not EndOfList do
        ReadProperty(Item);
      ReadListEnd;
    end;
    ReadListEnd;
  finally
    Collection.EndUpdate;
  end;
end;

function TReader.ReadComponent(Component: TComponent): TComponent;
var
  Flags: TFilerFlags;

  function Recover(var Component: TComponent): Boolean;
  begin
    Result := False;
    if ExceptObject.InheritsFrom(Exception) then
    begin
      if not ((ffInherited in Flags) or Assigned(Component)) then
        Component.Free;
      Component := nil;
      FDriver.SkipComponent(False);
      Result := Error(Exception(ExceptObject).Message);
    end;
  end;

var
  CompClassName, Name: String;
  ChildPos: Integer;
  SavedParent, SavedLookupRoot: TComponent;
  ComponentClass: TComponentClass;
  NewComponent: TComponent;
begin
  FDriver.BeginComponent(Flags, ChildPos, CompClassName, Name);
  SavedParent := Parent;
  SavedLookupRoot := FLookupRoot;
  try
    Result := Component;
    if not Assigned(Result) then
      try
        if ffInherited in Flags then
        begin
          { Try to locate the existing ancestor component }

          if Assigned(FLookupRoot) then
            Result := FLookupRoot.FindComponent(Name)
          else
            Result := nil;

          if not Assigned(Result) then
          begin
            if Assigned(FOnAncestorNotFound) then
              FOnAncestorNotFound(Self, Name,
                FindComponentClass(CompClassName), Result);
            if not Assigned(Result) then
              raise EReadError.CreateFmt(SAncestorNotFound, [Name]);
          end;

          Parent := Result.GetParentComponent;
          if not Assigned(Parent) then
            Parent := Root;
        end else
        begin
          Result := nil;
          ComponentClass := FindComponentClass(CompClassName);
          if Assigned(FOnCreateComponent) then
            FOnCreateComponent(Self, ComponentClass, Result);
          if not Assigned(Result) then
          begin
            NewComponent := TComponent(ComponentClass.NewInstance);
            if ffInline in Flags then
              NewComponent.FComponentState :=
                NewComponent.FComponentState + [csLoading, csInline];
            NewComponent.Create(Owner);

            { Don't set Result earlier because else we would come in trouble
              with the exception recover mechanism! (Result should be NIL if
              an error occured) }
            Result := NewComponent;
          end;
          Include(Result.FComponentState, csLoading);
        end;
      except
        if not Recover(Result) then
          raise;
      end;

    if Assigned(Result) then
      try
        Include(Result.FComponentState, csLoading);
        if not (ffInherited in Flags) then
          try
            Result.SetParentComponent(Parent);
            if Assigned(FOnSetName) then
              FOnSetName(Self, Result, Name);
            Result.Name := Name;
            if Assigned(FindGlobalComponent) and
              (FindGlobalComponent(Name) = Result) then
              Include(Result.FComponentState, csInline);
          except
            if not Recover(Result) then
              raise;
          end;
        if not Assigned(Result) then
          exit;
        if csInline in Result.ComponentState then
          FLookupRoot := Result;

        { Read the component state }
        Include(Result.FComponentState, csReading);
        Result.ReadState(Self);
        Exclude(Result.FComponentState, csReading);

        if ffChildPos in Flags then
          Parent.SetChildOrder(Result, ChildPos);

        { Add component to list of loaded components, if necessary }
        if (not ((ffInherited in Flags) or (csInline in Result.ComponentState))) or
          (FLoaded.IndexOf(Result) < 0) then
          FLoaded.Add(Result);
      except
        if ((ffInherited in Flags) or Assigned(Component)) then
          Result.Free;
        raise;
      end;
  finally
    Parent := SavedParent;
    FLookupRoot := SavedLookupRoot;
  end;
end;

procedure TReader.ReadData(Instance: TComponent);
var
  DoFreeFixups: Boolean;
  SavedOwner, SavedParent: TComponent;
begin
  if not Assigned(FFixups) then
  begin
    FFixups := TList.Create;
    DoFreeFixups := True;
  end else
    DoFreeFixups := False;

  try
    { Read properties }
    while not EndOfList do
      ReadProperty(Instance);
    ReadListEnd;

    { Read children }
    SavedOwner := Owner;
    SavedParent := Parent;
    try
      Owner := Instance.GetChildOwner;
      if not Assigned(Owner) then
        Owner := Root;
      Parent := Instance.GetChildParent;

      while not EndOfList do
        ReadComponent(nil);
      ReadListEnd;
    finally
      Owner := SavedOwner;
      Parent := SavedParent;
    end;

    { Fixup references if necessary (normally only if this is the root) }
    if DoFreeFixups then
      DoFixupReferences;

  finally
    if DoFreeFixups then
      FreeFixups;
  end;
end;

function TReader.ReadFloat: Extended;
begin
  if FDriver.NextValue = vaExtended then
  begin
    ReadValue;
    Result := FDriver.ReadFloat
  end else
    Result := ReadInteger;
end;

function TReader.ReadSingle: Single;
begin
  if FDriver.NextValue = vaSingle then
  begin
    FDriver.ReadValue;
    Result := FDriver.ReadSingle;
  end else
    Result := ReadInteger;
end;

{!!!: function TReader.ReadCurrency: Currency;
begin
  if FDriver.NextValue = vaCurrency then
  begin
    FDriver.ReadValue;
    Result := FDriver.ReadCurrency;
  end else
    Result := ReadInteger;
end;}

function TReader.ReadDate: TDateTime;
begin
  if FDriver.NextValue = vaDate then
  begin
    FDriver.ReadValue;
    Result := FDriver.ReadDate;
  end else
    Result := ReadInteger;
end;

function TReader.ReadIdent: String;
var
  ValueType: TValueType;
begin
  ValueType := FDriver.ReadValue;
  if ValueType in [vaIdent, vaNil, vaFalse, vaTrue, vaNull] then
    Result := FDriver.ReadIdent(ValueType)
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;


function TReader.ReadInteger: LongInt;
begin
  case FDriver.ReadValue of
    vaInt8:
      Result := FDriver.ReadInt8;
    vaInt16:
      Result := FDriver.ReadInt16;
    vaInt32:
      Result := FDriver.ReadInt32;
  else
    raise EReadError.Create(SInvalidPropertyValue);
  end;
end;

function TReader.ReadInt64: Int64;
begin
  if FDriver.NextValue = vaInt64 then
  begin
    FDriver.ReadValue;
    Result := FDriver.ReadInt64;
  end else
    Result := ReadInteger;
end;

procedure TReader.ReadListBegin;
begin
  CheckValue(vaList);
end;

procedure TReader.ReadListEnd;
begin
  CheckValue(vaNull);
end;

procedure TReader.ReadProperty(AInstance: TPersistent);
var
  Path: String;
  Instance: TPersistent;
  DotPos, NextPos: PChar;
  PropInfo: PPropInfo;
  Obj: TObject;
  Name: String;
  Skip: Boolean;
  Handled: Boolean;
  OldPropName: String;

  function HandleMissingProperty(IsPath: Boolean): boolean;
  begin
    Result:=true;
    if Assigned(OnPropertyNotFound) then begin
      // user defined property error handling
      OldPropName:=FPropName;
      Handled:=false;
      Skip:=false;
      OnPropertyNotFound(Self,Instance,FPropName,IsPath,Handled,Skip);
      if Handled and (not Skip) and (OldPropName<>FPropName) then
        // try alias property
        PropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
      if Skip then begin
        FDriver.SkipValue;
        Result:=false;
        exit;
      end;
    end;
  end;

begin
  try
    Path := FDriver.BeginProperty;
    try
      Instance := AInstance;
      FCanHandleExcepts := True;
      DotPos := PChar(Path);
      while True do
      begin
        NextPos := StrScan(DotPos, '.');
        if Assigned(NextPos) then
          FPropName := Copy(String(DotPos), 1, Integer(NextPos - DotPos))
        else
        begin
          FPropName := DotPos;
          break;
        end;
        DotPos := NextPos + 1;

        PropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
        if not Assigned(PropInfo) then begin
          if not HandleMissingProperty(true) then exit;
          if not Assigned(PropInfo) then
            PropertyError;
        end;

        if PropInfo^.PropType^.Kind = tkClass then
          Obj := TObject(GetOrdProp(Instance, PropInfo))
        else
          Obj := nil;

        if not Obj.InheritsFrom(TPersistent) then
        begin
          { All path elements must be persistent objects! }
          FDriver.SkipValue;
          raise EReadError.Create(SInvalidPropertyPath);
        end;
        Instance := TPersistent(Obj);
      end;

      PropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
      if not Assigned(PropInfo) then
        if not HandleMissingProperty(false) then exit;
      if Assigned(PropInfo) then
        ReadPropValue(Instance, PropInfo)
      else
      begin
        FCanHandleExcepts := False;
        Instance.DefineProperties(Self);
        FCanHandleExcepts := True;
        if Length(FPropName) > 0 then
          PropertyError;
      end;
    except
      on e: Exception do
      begin
        SetLength(Name, 0);
        if AInstance.InheritsFrom(TComponent) then
          Name := TComponent(AInstance).Name;
        if Length(Name) = 0 then
          Name := AInstance.ClassName;
        raise EReadError.CreateFmt(SPropertyException,
          [Name, DotSep, Path, e.Message]);
      end;
    end;
  except
    on e: Exception do
      if not FCanHandleExcepts or not Error(E.Message) then
        raise;
  end;
end;

procedure TReader.ReadPropValue(Instance: TPersistent; PropInfo: Pointer);
const
  NullMethod: TMethod = (Code: nil; Data: nil);
var
  PropType: PTypeInfo;
  Value: LongInt;
  IdentToIntFn: TIdentToInt;
  Ident: String;
  Method: TMethod;
  Handled: Boolean;
  TmpStr: String;
begin
  if not Assigned(PPropInfo(PropInfo)^.SetProc) then
    raise EReadError.Create(SReadOnlyProperty);

  PropType := PPropInfo(PropInfo)^.PropType;
  case PropType^.Kind of
    tkInteger:
      if FDriver.NextValue = vaIdent then
      begin
        Ident := ReadIdent;
        if GlobalIdentToInt(Ident,Value) then
          SetOrdProp(Instance, PropInfo, Value)
        else
          raise EReadError.Create(SInvalidPropertyValue);
      end else
        SetOrdProp(Instance, PropInfo, ReadInteger);
    tkBool:
      SetOrdProp(Instance, PropInfo, Ord(ReadBoolean));
    tkChar:
      SetOrdProp(Instance, PropInfo, Ord(ReadChar));
    tkEnumeration:
      begin
        Value := GetEnumValue(PropType, ReadIdent);
        if Value = -1 then
          raise EReadError.Create(SInvalidPropertyValue);
        SetOrdProp(Instance, PropInfo, Value);
      end;
    tkFloat:
      SetFloatProp(Instance, PropInfo, ReadFloat);
    tkSet:
      begin
        CheckValue(vaSet);
        SetOrdProp(Instance, PropInfo,
          FDriver.ReadSet(GetTypeData(PropType)^.CompType));
      end;
    tkMethod:
      if FDriver.NextValue = vaNil then
      begin
        FDriver.ReadValue;
        SetMethodProp(Instance, PropInfo, NullMethod);
      end else
      begin
        Handled:=false;
        Ident:=ReadIdent;
        if Assigned(OnSetMethodProperty) then
          OnSetMethodProperty(Self,Instance,PPropInfo(PropInfo),Ident,
                              Handled);
        if not Handled then begin
          Method.Code := FindMethod(Root, Ident);
          Method.Data := Root;
          if Assigned(Method.Code) then
            SetMethodProp(Instance, PropInfo, Method);
        end;
      end;
    tkSString, tkLString, tkAString, tkWString:
      begin
        TmpStr:=ReadString;
        if Assigned(FOnReadStringProperty) then
          FOnReadStringProperty(Self,Instance,PropInfo,TmpStr);
        SetStrProp(Instance, PropInfo, TmpStr);
      end;
    {!!!: tkVariant}
    tkClass:
      case FDriver.NextValue of
        vaNil:
          begin
            FDriver.ReadValue;
            SetOrdProp(Instance, PropInfo, 0)
          end;
        vaCollection:
          begin
            FDriver.ReadValue;
            ReadCollection(TCollection(GetOrdProp(Instance, PropInfo)));
          end
        else
          FFixups.Add(TPropFixup.Create(Instance, Root, PropInfo, '', ReadIdent));
      end;
    tkInt64: SetInt64Prop(Instance, PropInfo, ReadInt64);
    else
      raise EReadError.CreateFmt(SUnknownPropertyType, [Ord(PropType^.Kind)]);
  end;
end;

function TReader.ReadRootComponent(ARoot: TComponent): TComponent;
var
  Dummy, i: Integer;
  Flags: TFilerFlags;
  CompClassName, CompName, ResultName: String;
begin
  FDriver.BeginRootComponent;
  Result := nil;
  {!!!: GlobalNameSpace.BeginWrite;  // Loading from stream adds to name space
  try}
    try
      FDriver.BeginComponent(Flags, Dummy, CompClassName, CompName);
      if not Assigned(ARoot) then
      begin
        { Read the class name and the object name and create a new object: }
        Result := TComponentClass(FindClass(CompClassName)).Create(nil);
        Result.Name := CompName;
      end else
      begin
        Result := ARoot;

        if not (csDesigning in Result.ComponentState) then
        begin
          Result.FComponentState :=
            Result.FComponentState + [csLoading, csReading];

          if Assigned(FindGlobalComponent) then
          begin
            { We need an unique name }
            i := 0;
            { Don't use Result.Name directly, as this would influence
              FindGlobalComponent in successive loop runs }
            ResultName := CompName;
            while Assigned(FindGlobalComponent(ResultName)) do
            begin
              Inc(i);
              ResultName := CompName + '_' + IntToStr(i);
            end;
            Result.Name := ResultName;
          end else
            Result.Name := '';
        end;
      end;

      FRoot := Result;
      FLookupRoot := Result;
      if Assigned(GlobalLoaded) then
        FLoaded := GlobalLoaded
      else
        FLoaded := TList.Create;

      try
        if FLoaded.IndexOf(FRoot) < 0 then
          FLoaded.Add(FRoot);
        FOwner := FRoot;
        FRoot.FComponentState := FRoot.FComponentState + [csLoading, csReading];
        FRoot.ReadState(Self);
        Exclude(FRoot.FComponentState, csReading);

        if not Assigned(GlobalLoaded) then
          for i := 0 to FLoaded.Count - 1 do
            TComponent(FLoaded[i]).Loaded;

      finally
        if not Assigned(GlobalLoaded) then
          FLoaded.Free;
        FLoaded := nil;
      end;
      GlobalFixupReferences;
    except
      RemoveFixupReferences(ARoot, '');
      if not Assigned(ARoot) then
        Result.Free;
      raise;
    end;
  {finally
    GlobalNameSpace.EndWrite;
  end;}
end;

procedure TReader.ReadComponents(AOwner, AParent: TComponent;
  Proc: TReadComponentsProc);
var
  Component: TComponent;
begin
  Root := AOwner;
  Owner := AOwner;
  Parent := AParent;
  BeginReferences;
  try
    while not EndOfList do
    begin
      FDriver.BeginRootComponent;
      Component := ReadComponent(nil);
      if Assigned(Proc) then
        Proc(Component);
    end;
    ReadListEnd;
    FixupReferences;
  finally
    EndReferences;
  end;
end;


function TReader.ReadString: String;
var
  StringType: TValueType;
begin
  StringType := FDriver.ReadValue;
  if StringType in [vaString, vaLString] then
    Result := FDriver.ReadString(StringType)
  else
    raise EReadError.Create(SInvalidPropertyValue);
end;

{!!!: function TReader.ReadWideString: WideString;
begin
  CheckValue(vaWString);
  Result := FDriver.ReadWideString;
end;}

function TReader.ReadValue: TValueType;
begin
  Result := FDriver.ReadValue;
end;

procedure TReader.CopyValue(Writer: TWriter);

  procedure CopyBytes(Count: Integer);
  var
    Buffer: array[0..1023] of Byte;
  begin
{!!!:    while Count > 1024 do
    begin
      FDriver.Read(Buffer, 1024);
      Writer.Driver.Write(Buffer, 1024);
      Dec(Count, 1024);
    end;
    if Count > 0 then
    begin
      FDriver.Read(Buffer, Count);
      Writer.Driver.Write(Buffer, Count);
    end;}
  end;

var
  s: String;
  Count: LongInt;
begin
  case FDriver.NextValue of
    vaNull:
      Writer.WriteIdent('NULL');
    vaFalse:
      Writer.WriteIdent('FALSE');
    vaTrue:
      Writer.WriteIdent('TRUE');
    vaNil:
      Writer.WriteIdent('NIL');
    {!!!: vaList, vaCollection:
      begin
        Writer.WriteValue(FDriver.ReadValue);
        while not EndOfList do
          CopyValue(Writer);
        ReadListEnd;
        Writer.WriteListEnd;
      end;}
    vaInt8, vaInt16, vaInt32:
      Writer.WriteInteger(ReadInteger);
    vaExtended:
      Writer.WriteFloat(ReadFloat);
    {!!!: vaString:
      Writer.WriteStr(ReadStr);}
    vaIdent:
      Writer.WriteIdent(ReadIdent);
    {!!!: vaBinary, vaLString, vaWString:
      begin
        Writer.WriteValue(FDriver.ReadValue);
        FDriver.Read(Count, SizeOf(Count));
        Writer.Driver.Write(Count, SizeOf(Count));
        CopyBytes(Count);
      end;}
    {!!!: vaSet:
      Writer.WriteSet(ReadSet);}
    vaSingle:
      Writer.WriteSingle(ReadSingle);
    {!!!: vaCurrency:
      Writer.WriteCurrency(ReadCurrency);}
    vaDate:
      Writer.WriteDate(ReadDate);
    vaInt64:
      Writer.WriteInteger(ReadInt64);
  end;
end;

function TReader.FindComponentClass(const AClassName: String): TComponentClass;
begin
  TPersistentClass(Result) := GetFieldClass(Root, AClassName);
  if not Assigned(Result) and Assigned(FLookupRoot) and (FLookupRoot <> Root) then
    TPersistentClass(Result) := GetFieldClass(FLookupRoot, AClassName);
  if Assigned(FOnFindComponentClass) then
    FOnFindComponentClass(Self, AClassName, Result);
  if not (Assigned(Result) and Result.InheritsFrom(TComponent)) then
    raise EClassNotFound.CreateFmt(SClassNotFound, [AClassName]);
end;


{
  $Log: reader.inc,v $
  Revision 1.4  2005/02/14 17:13:11  peter
    * truncate log

}
