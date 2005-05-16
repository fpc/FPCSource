{
    $Id: classes.inc,v 1.3 2005/02/14 17:13:11 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
 *       Class implementations are in separate files.                 *
 **********************************************************************}

var
  ClassList : TThreadlist;
  ClassAliasList : TStringList;

{
 Include all message strings

 Add a language with IFDEF LANG_NAME
 just befor the final ELSE. This way English will always be the default.
}

{$IFDEF LANG_GERMAN}
{$i constsg.inc}
{$ELSE}
{$IFDEF LANG_SPANISH}
{$i constss.inc}
{$ELSE}
{$i constse.inc}
{$ENDIF}
{$ENDIF}

{ Utility routines }
{$i util.inc}

{ TBits implementation }
{$i bits.inc}

{ All streams implementations: }
{ Tstreams THandleStream TFileStream TResourcseStreams TStringStream }
{ TCustomMemoryStream TMemoryStream }
{$i streams.inc}

{ TParser implementation}
{$i parser.inc}

{ TCollection and TCollectionItem implementations }
{$i collect.inc}

{ TList and TThreadList implementations }
{$i lists.inc}

{ TStrings and TStringList implementations }
{$i stringl.inc}

{$ifndef VER1_0}
{ TThread implementation }
{$i tthread.inc}
{$endif}

{ TPersistent implementation }
{$i persist.inc }

{ TComponent implementation }
{$i compon.inc}

{ TBasicAction implementation }
{$i action.inc}

{ TDataModule implementation }
{$i dm.inc}

{ Class and component registration routines }
{$I cregist.inc}



{ Interface related stuff }
{$ifdef HASINTF}
{$I intf.inc}
{$endif HASINTF}

{**********************************************************************
 *       Miscellaneous procedures and functions                       *
 **********************************************************************}

{ Point and rectangle constructors }

function Point(AX, AY: Integer): TPoint;

begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;


function SmallPoint(AX, AY: SmallInt): TSmallPoint;

begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;


function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;

begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;


function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom :=  ATop + AHeight;
  end;
end;





{ Object filing routines }

var
  IntConstList: TThreadList;


type
  TIntConst = class
    IntegerType: PTypeInfo;             // The integer type RTTI pointer
    IdentToIntFn: TIdentToInt;          // Identifier to Integer conversion
    IntToIdentFn: TIntToIdent;          // Integer to Identifier conversion
    constructor Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
      AIntToIdent: TIntToIdent);
  end;

constructor TIntConst.Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
begin
  IntegerType := AIntegerType;
  IdentToIntFn := AIdentToInt;
  IntToIdentFn := AIntToIdent;
end;

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToIntFn: TIdentToInt;
  IntToIdentFn: TIntToIdent);
begin
  IntConstList.Add(TIntConst.Create(IntegerType, IdentToIntFn, IntToIdentFn));
end;

function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
var
  i: Integer;
begin
  with IntConstList.LockList do
  try
    for i := 0 to Count - 1 do
      if TIntConst(Items[i]).IntegerType = AIntegerType then
        exit(TIntConst(Items[i]).IntToIdentFn);
    Result := nil;
  finally
    IntConstList.UnlockList;
  end;
end;

function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
var
  i: Integer;
begin
  with IntConstList.LockList do
  try
    for i := 0 to Count - 1 do
      with TIntConst(Items[I]) do
        if TIntConst(Items[I]).IntegerType = AIntegerType then
          exit(IdentToIntFn);
    Result := nil;
  finally
    IntConstList.UnlockList;
  end;
end;

function IdentToInt(const Ident: String; var Int: LongInt;
  const Map: array of TIdentMapEntry): Boolean;
var
  i: Integer;
begin
  for i := Low(Map) to High(Map) do
    if CompareText(Map[i].Name, Ident) = 0 then
    begin
      Int := Map[i].Value;
      exit(True);
    end;
  Result := False;
end;

function IntToIdent(Int: LongInt; var Ident: String;
  const Map: array of TIdentMapEntry): Boolean;
var
  i: Integer;
begin
  for i := Low(Map) to High(Map) do
    if Map[i].Value = Int then
    begin
      Ident := Map[i].Name;
      exit(True);
    end;
  Result := False;
end;

function GlobalIdentToInt(const Ident: String; var Int: LongInt):boolean;
var
  i : Integer;
begin
  with IntConstList.LockList do
    try
      for i := 0 to Count - 1 do
        if TIntConst(Items[I]).IdentToIntFn(Ident, Int) then
          Exit(True);
      Result := false;
    finally
      IntConstList.UnlockList;
    end;
end;

{ TPropFixup }

type
  TPropFixup = class
    FInstance: TPersistent;
    FInstanceRoot: TComponent;
    FPropInfo: PPropInfo;
    FRootName: string;
    FName: string;
    constructor Create(AInstance: TPersistent; AInstanceRoot: TComponent;
      APropInfo: PPropInfo; const ARootName, AName: String);
    function MakeGlobalReference: Boolean;
  end;

var
  GlobalFixupList: TThreadList;

constructor TPropFixup.Create(AInstance: TPersistent; AInstanceRoot: TComponent;
  APropInfo: PPropInfo; const ARootName, AName: String);
begin
  FInstance := AInstance;
  FInstanceRoot := AInstanceRoot;
  FPropInfo := APropInfo;
  FRootName := ARootName;
  FName := AName;
end;

function TPropFixup.MakeGlobalReference: Boolean;
var
  i: Integer;
  s, p: PChar;
begin
  i := Pos('.', FName);
  if i = 0 then
    exit(False);
  FRootName := Copy(FName, 1, i - 1);
  FName := Copy(FName, i + 1, Length(FName));
  Result := True;
end;

Type
  TInitHandler = Class(TObject)
    AHandler : TInitComponentHandler;
    AClass : TComponentClass;
  end;

Var
  InitHandlerList : TList;

procedure RegisterInitComponentHandler(ComponentClass: TComponentClass;   Handler: TInitComponentHandler);

Var
  I : Integer;
  H: TInitHandler;

begin
  If (InitHandlerList=Nil) then
    InitHandlerList:=TList.Create;
  H:=TInitHandler.Create;
  H.Aclass:=ComponentClass;
  H.AHandler:=Handler;
  With InitHandlerList do
    begin
    I:=0;
    While (I<Count) and not H.AClass.InheritsFrom(TInitHandler(Items[i]).AClass) do
      Inc(I);
    InitHandlerList.Insert(I,H);
    end;
end;

function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

Var
  I : Integer;

begin
  I:=0;
  Result:=False;
  With InitHandlerList do
    begin
    I:=0;
    // Instance is the normally the lowest one, so that one should be used when searching.
    While Not result and (I<Count) do
      begin
      If (Instance.InheritsFrom(TInitHandler(Items[i]).AClass)) then
        Result:=TInitHandler(Items[i]).AHandler(Instance,RootAncestor);
      Inc(I);
      end;
    end;
end;


function InitComponentRes(const ResName: String; Instance: TComponent): Boolean;

begin
  { !!!: Too Win32-specific }
  InitComponentRes := False;
end;


function ReadComponentRes(const ResName: String; Instance: TComponent): TComponent;

begin
  { !!!: Too Win32-specific }
  ReadComponentRes := nil;
end;


function ReadComponentResEx(HInstance: THandle; const ResName: String): TComponent;

begin
  { !!!: Too Win32-specific in VCL }
  ReadComponentResEx := nil;
end;


function ReadComponentResFile(const FileName: String; Instance: TComponent): TComponent;
var
  FileStream: TStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead {!!!:or fmShareDenyWrite});
  try
    Result := FileStream.ReadComponentRes(Instance);
  finally
    FileStream.Free;
  end;
end;


procedure WriteComponentResFile(const FileName: String; Instance: TComponent);
var
  FileStream: TStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteComponentRes(Instance.ClassName, Instance);
  finally
    FileStream.Free;
  end;
end;


procedure GlobalFixupReferences;
var
  GlobalList, DoneList, ToDoList: TList;
  I, Index: Integer;
  Root: TComponent;
  Instance: TPersistent;
  Reference: Pointer;
begin
  if not Assigned(FindGlobalComponent) then
    exit;

  {!!!: GlobalNameSpace.BeginWrite;
  try}
    GlobalList := GlobalFixupList.LockList;
    try
      if GlobalList.Count > 0 then
      begin
        ToDoList := nil;
        DoneList := TList.Create;
        ToDoList := TList.Create;
        try
          i := 0;
          while i < GlobalList.Count do
            with TPropFixup(GlobalList[i]) do
            begin
              Root := FindGlobalComponent(FRootName);
              if Assigned(Root) or (GetOrdProp(FInstance, FPropInfo) <> 0) then
              begin
                if Assigned(Root) then
                begin
                  Reference := FindNestedComponent(Root, FName);
                  SetOrdProp(FInstance, FPropInfo, Longint(Reference));
                end;
                // Move component to list of done components, if necessary
                if (DoneList.IndexOf(FInstance) < 0) and
                  (ToDoList.IndexOf(FInstance) >= 0) then
                  DoneList.Add(FInstance);
                GlobalList.Delete(i);
                Free;   // ...the fixup
              end else
              begin
                // Move component to list of components to process, if necessary
                Index := DoneList.IndexOf(FInstance);
                if Index <> -1 then
                  DoneList.Delete(Index);
                if ToDoList.IndexOf(FInstance) < 0 then
                  ToDoList.Add(FInstance);
                Inc(i);
              end;
            end;
            for i := 0 to DoneList.Count - 1 do
            begin
              Instance := TPersistent(DoneList[I]);
              if Instance.InheritsFrom(TComponent) then
                Exclude(TComponent(Instance).FComponentState, csFixups);
            end;
          finally
            ToDoList.Free;
            DoneList.Free;
          end;
        end;
    finally
      GlobalFixupList.UnlockList;
    end;
  {finally
    GlobalNameSpace.EndWrite;
  end;}
end;


function IsStringInList(const AString: String; AList: TStrings): Boolean;
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    if CompareText(AList[i], AString) = 0 then
      exit(True);
  Result := False;
end;


procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);
var
  i: Integer;
  CurFixup: TPropFixup;
begin
  with GlobalFixupList.LockList do
    try
      for i := 0 to Count - 1 do
      begin
        CurFixup := TPropFixup(Items[i]);
        if ((not Assigned(Root)) or (CurFixup.FInstanceRoot = Root)) and
          not IsStringInList(CurFixup.FRootName, Names) then
          Names.Add(CurFixup.FRootName);
      end;
    finally
      GlobalFixupList.UnlockList;
    end;
end;


procedure GetFixupInstanceNames(Root: TComponent;
  const ReferenceRootName: string; Names: TStrings);
var
  i: Integer;
  CurFixup: TPropFixup;
begin
  with GlobalFixupList.LockList do
    try
      for i := 0 to Count - 1 do
      begin
        CurFixup := TPropFixup(Items[i]);
        if (CurFixup.FInstanceRoot = Root) and
          (UpperCase(ReferenceRootName) = UpperCase(CurFixup.FRootName)) and
          not IsStringInList(CurFixup.FName, Names) then
          Names.Add(CurFixup.FName);
      end;
    finally
      GlobalFixupList.UnlockList;
    end;
end;


procedure RedirectFixupReferences(Root: TComponent; const OldRootName,
  NewRootName: string);
var
  i: Integer;
  CurFixup: TPropFixup;
begin
  with GlobalFixupList.LockList do
    try
      for i := 0 to Count - 1 do
      begin
        CurFixup := TPropFixup(Items[i]);
        if ((not Assigned(Root)) or (CurFixup.FInstanceRoot = Root)) and
          (UpperCase(OldRootName) = UpperCase(CurFixup.FRootName)) then
          CurFixup.FRootName := NewRootName;
      end;
      GlobalFixupReferences;
    finally
      GlobalFixupList.Unlocklist;
    end;
end;


procedure RemoveFixupReferences(Root: TComponent; const RootName: string);
var
  i: Integer;
  CurFixup: TPropFixup;
begin
  if not Assigned(GlobalFixupList) then
    exit;

  with GlobalFixupList.LockList do
    try
      for i := Count - 1 downto 0 do
      begin
        CurFixup := TPropFixup(Items[i]);
        if ((not Assigned(Root)) or (CurFixup.FInstanceRoot = Root)) and
          ((Length(RootName) = 0) or
          (UpperCase(RootName) = UpperCase(CurFixup.FRootName))) then
        begin
          Delete(i);
          CurFixup.Free;
        end;
      end;
    finally
      GlobalFixupList.UnlockList;
    end;
end;


procedure RemoveFixups(Instance: TPersistent);
var
  i: Integer;
  CurFixup: TPropFixup;
begin
  if not Assigned(GlobalFixupList) then
    exit;

  with GlobalFixupList.LockList do
    try
      for i := Count - 1 downto 0 do
      begin
        CurFixup := TPropFixup(Items[i]);
        if (CurFixup.FInstance = Instance) then
        begin
          Delete(i);
          CurFixup.Free;
        end;
      end;
    finally
      GlobalFixupList.UnlockList;
    end;
end;


function FindNestedComponent(Root: TComponent; const NamePath: string): TComponent;
var
  Current, Found: TComponent;
  s, p: PChar;
  Name: String;
begin
  Result := nil;
  if Length(NamePath) > 0 then
  begin
    Current := Root;
    p := PChar(NamePath);
    while p[0] <> #0 do
    begin
      s := p;
      while not (p^ in ['.', '-', #0]) do
        Inc(p);
      SetString(Name, s, p - s);
      Found := Current.FindComponent(Name);
      if (not Assigned(Found)) and (UpperCase(Name) = 'OWNER') then
        Found := Current;
      if not Assigned(Found) then exit;

      // Remove the dereference operator from the name
      if p[0] = '.' then
        Inc(P);
      if p[0] = '-' then
        Inc(P);
      if p[0] = '>' then
        Inc(P);

      Current := Found;
    end;
  end;
  Result := Current;
end;

{!!!: Should be threadvar  -  doesn't work for all platforms yet!}
var
  GlobalLoaded, GlobalLists: TList;


procedure BeginGlobalLoading;

begin
  if not Assigned(GlobalLists) then
    GlobalLists := TList.Create;
  GlobalLists.Add(GlobalLoaded);
  GlobalLoaded := TList.Create;
end;


{ Notify all global components that they have been loaded completely }
procedure NotifyGlobalLoading;
var
  i: Integer;
begin
  for i := 0 to GlobalLoaded.Count - 1 do
    TComponent(GlobalLoaded[i]).Loaded;
end;


procedure EndGlobalLoading;
begin
  { Free the memory occupied by BeginGlobalLoading }
  GlobalLoaded.Free;
  GlobalLoaded := TList(GlobalLists.Last);
  GlobalLists.Delete(GlobalLists.Count - 1);
  if GlobalLists.Count = 0 then
  begin
    GlobalLists.Free;
    GlobalLists := nil;
  end;
end;


function CollectionsEqual(C1, C2: TCollection): Boolean;
begin
  // !!!: Implement this
  CollectionsEqual:=false;
end;



{ Object conversion routines }

procedure ObjectBinaryToText(Input, Output: TStream);

  procedure OutStr(s: String);
  begin
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure OutLn(s: String);
  begin
    OutStr(s + #10);
  end;

  procedure OutString(s: String);
  var
    res, NewStr: String;
    i: Integer;
    InString, NewInString: Boolean;
  begin
    res := '';
    InString := False;
    for i := 1 to Length(s) do begin
      NewInString := InString;
      case s[i] of
        #0..#31: begin
            if InString then
              NewInString := False;
            NewStr := '#' + IntToStr(Ord(s[i]));
          end;
        '''':
            if InString then NewStr := ''''''
            else NewStr := '''''''';
        else begin
          if not InString then
            NewInString := True;
          NewStr := s[i];
        end;
      end;
      if NewInString <> InString then begin
        NewStr := '''' + NewStr;
        InString := NewInString;
      end;
      res := res + NewStr;
    end;
    if InString then res := res + '''';
    OutStr(res);
  end;

  function ReadInt(ValueType: TValueType): LongInt;
  begin
    case ValueType of
      vaInt8: Result := ShortInt(Input.ReadByte);
      vaInt16: Result := SmallInt(Input.ReadWord);
      vaInt32: Result := LongInt(Input.ReadDWord);
    end;
  end;

  function ReadInt: LongInt;
  begin
    Result := ReadInt(TValueType(Input.ReadByte));
  end;

  function ReadSStr: String;
  var
    len: Byte;
  begin
    len := Input.ReadByte;
    SetLength(Result, len);
    Input.Read(Result[1], len);
  end;

  procedure ReadPropList(indent: String);

    procedure ProcessValue(ValueType: TValueType; Indent: String);

      procedure Stop(s: String);
      begin
        WriteLn(s);
        Halt;
      end;

      procedure ProcessBinary;
      var
        ToDo, DoNow, i: LongInt;
        lbuf: array[0..31] of Byte;
        s: String;
      begin
        ToDo := Input.ReadDWord;
        OutLn('{');
        while ToDo > 0 do begin
          DoNow := ToDo;
          if DoNow > 32 then DoNow := 32;
          Dec(ToDo, DoNow);
          s := Indent + '  ';
          Input.Read(lbuf, DoNow);
          for i := 0 to DoNow - 1 do
            s := s + IntToHex(lbuf[i], 2);
          OutLn(s);
        end;
        OutLn(indent + '}');
      end;

    var
      s: String;
      len: LongInt;
      IsFirst: Boolean;
      ext: Extended;

    begin
      case ValueType of
        vaList: begin
            OutStr('(');
            IsFirst := True;
            while True do begin
              ValueType := TValueType(Input.ReadByte);
              if ValueType = vaNull then break;
              if IsFirst then begin
                OutLn('');
                IsFirst := False;
              end;
              OutStr(Indent + '  ');
              ProcessValue(ValueType, Indent + '  ');
            end;
            OutLn(Indent + ')');
          end;
        vaInt8: OutLn(IntToStr(ShortInt(Input.ReadByte)));
        vaInt16: OutLn( IntToStr(SmallInt(Input.ReadWord)));
        vaInt32: OutLn(IntToStr(LongInt(Input.ReadDWord)));
        vaExtended: begin
            Input.Read(ext, SizeOf(ext));
            OutLn(FloatToStr(ext));
          end;
        vaString: begin
            OutString(ReadSStr);
            OutLn('');
          end;
        vaIdent: OutLn(ReadSStr);
        vaFalse: OutLn('False');
        vaTrue: OutLn('True');
        vaBinary: ProcessBinary;
        vaSet: begin
            OutStr('[');
            IsFirst := True;
            while True do begin
              s := ReadSStr;
              if Length(s) = 0 then break;
              if not IsFirst then OutStr(', ');
              IsFirst := False;
              OutStr(s);
            end;
            OutLn(']');
          end;
        vaLString:
          Stop('!!LString!!');
        vaNil:
          OutLn('nil');
        vaCollection: begin
            OutStr('<');
            while Input.ReadByte <> 0 do begin
              OutLn(Indent);
              Input.Seek(-1, soFromCurrent);
              OutStr(indent + '  item');
              ValueType := TValueType(Input.ReadByte);
              if ValueType <> vaList then
                OutStr('[' + IntToStr(ReadInt(ValueType)) + ']');
              OutLn('');
              ReadPropList(indent + '    ');
              OutStr(indent + '  end');
            end;
            OutLn('>');
          end;
        {vaSingle: begin OutLn('!!Single!!'); exit end;
        vaCurrency: begin OutLn('!!Currency!!'); exit end;
        vaDate: begin OutLn('!!Date!!'); exit end;
        vaWString: begin OutLn('!!WString!!'); exit end;}
        else
          Stop(IntToStr(Ord(ValueType)));
      end;
    end;

  begin
    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      OutStr(indent + ReadSStr + ' = ');
      ProcessValue(TValueType(Input.ReadByte), Indent);
    end;
  end;

  procedure ReadObject(indent: String);
  var
    b: Byte;
    ObjClassName, ObjName: String;
    ChildPos: LongInt;
  begin
    // Check for FilerFlags
    b := Input.ReadByte;
    if (b and $f0) = $f0 then begin
      if (b and 2) <> 0 then ChildPos := ReadInt;
    end else begin
      b := 0;
      Input.Seek(-1, soFromCurrent);
    end;

    ObjClassName := ReadSStr;
    ObjName := ReadSStr;

    OutStr(Indent);
    if (b and 1) <> 0 then OutStr('inherited')
    else OutStr('object');
    OutStr(' ');
    if ObjName <> '' then
      OutStr(ObjName + ': ');
    OutStr(ObjClassName);
    if (b and 2) <> 0 then OutStr('[' + IntToStr(ChildPos) + ']');
    OutLn('');

    ReadPropList(indent + '  ');

    while Input.ReadByte <> 0 do begin
      Input.Seek(-1, soFromCurrent);
      ReadObject(indent + '  ');
    end;
    OutLn(indent + 'end');
  end;

type
  PLongWord = ^LongWord;
const
  signature: PChar = 'TPF0';
begin
  if Input.ReadDWord <> PLongWord(Pointer(signature))^ then
    raise EReadError.Create('Illegal stream image' {###SInvalidImage});
  ReadObject('');
end;


procedure ObjectTextToBinary(Input, Output: TStream);
var
  parser: TParser;

  procedure WriteString(s: String);
  begin
    Output.WriteByte(Length(s));
    if Length(s) > 0 then
      Output.Write(s[1], Length(s));
  end;

  procedure WriteInteger(value: LongInt);
  begin
    if (value >= -128) and (value <= 127) then begin
      Output.WriteByte(Ord(vaInt8));
      Output.WriteByte(Byte(value));
    end else if (value >= -32768) and (value <= 32767) then begin
      Output.WriteByte(Ord(vaInt16));
      Output.WriteWord(Word(value));
    end else begin
      Output.WriteByte(ord(vaInt32));
      Output.WriteDWord(LongWord(value));
    end;
  end;

  procedure ProcessProperty; forward;

  procedure ProcessValue;
  var
    flt: Extended;
    s: String;
    stream: TMemoryStream;
  begin
    case parser.Token of
      toInteger:
        begin
          WriteInteger(parser.TokenInt);
          parser.NextToken;
        end;
      toFloat:
        begin
          Output.WriteByte(Ord(vaExtended));
          flt := Parser.TokenFloat;
          Output.Write(flt, SizeOf(flt));
          parser.NextToken;
        end;
      toString:
        begin
          s := parser.TokenString;
          while parser.NextToken = '+' do
          begin
            parser.NextToken;   // Get next string fragment
            parser.CheckToken(toString);
            s := s + parser.TokenString;
          end;
          Output.WriteByte(Ord(vaString));
          WriteString(s);
        end;
      toSymbol:
        begin
          if CompareText(parser.TokenString, 'True') = 0 then
            Output.WriteByte(Ord(vaTrue))
          else if CompareText(parser.TokenString, 'False') = 0 then
            Output.WriteByte(Ord(vaFalse))
          else if CompareText(parser.TokenString, 'nil') = 0 then
            Output.WriteByte(Ord(vaNil))
          else
          begin
            Output.WriteByte(Ord(vaIdent));
            WriteString(parser.TokenComponentIdent);
          end;
          Parser.NextToken;
        end;
      // Set
      '[':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaSet));
          if parser.Token <> ']' then
            while True do
            begin
              parser.CheckToken(toSymbol);
              WriteString(parser.TokenString);
              parser.NextToken;
              if parser.Token = ']' then
                break;
              parser.CheckToken(',');
              parser.NextToken;
            end;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // List
      '(':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaList));
          while parser.Token <> ')' do
            ProcessValue;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // Collection
      '<':
        begin
          parser.NextToken;
          Output.WriteByte(Ord(vaCollection));
          while parser.Token <> '>' do
          begin
            parser.CheckTokenSymbol('item');
            parser.NextToken;
            // ConvertOrder
            Output.WriteByte(Ord(vaList));
            while not parser.TokenSymbolIs('end') do
              ProcessProperty;
            parser.NextToken;   // Skip 'end'
            Output.WriteByte(0);
          end;
          Output.WriteByte(0);
          parser.NextToken;
        end;
      // Binary data
      '{':
        begin
          Output.WriteByte(Ord(vaBinary));
          stream := TMemoryStream.Create;
          try
            parser.HexToBinary(stream);
            Output.WriteDWord(stream.Size);
            Output.Write(Stream.Memory^, stream.Size);
          finally
            stream.Free;
          end;
          parser.NextToken;
        end;
      else
        parser.Error(SInvalidProperty);
    end;
  end;

  procedure ProcessProperty;
  var
    name: String;
  begin
    // Get name of property
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
    while True do begin
      parser.NextToken;
      if parser.Token <> '.' then break;
      parser.NextToken;
      parser.CheckToken(toSymbol);
      name := name + '.' + parser.TokenString;
    end;
    WriteString(name);
    parser.CheckToken('=');
    parser.NextToken;
    ProcessValue;
  end;

  procedure ProcessObject;
  var
    IsInherited: Boolean;
    ObjectName, ObjectType: String;
  begin
    if parser.TokenSymbolIs('OBJECT') then
      IsInherited := False
    else begin
      parser.CheckTokenSymbol('INHERITED');
      IsInherited := True;
    end;
    parser.NextToken;
    parser.CheckToken(toSymbol);
    ObjectName := '';
    ObjectType := parser.TokenString;
    parser.NextToken;
    if parser.Token = ':' then begin
      parser.NextToken;
      parser.CheckToken(toSymbol);
      ObjectName := ObjectType;
      ObjectType := parser.TokenString;
      parser.NextToken;
    end;
    WriteString(ObjectType);
    WriteString(ObjectName);

    // Convert property list
    while not (parser.TokenSymbolIs('END') or
      parser.TokenSymbolIs('OBJECT') or
      parser.TokenSymbolIs('INHERITED')) do
      ProcessProperty;
    Output.WriteByte(0);        // Terminate property list

    // Convert child objects
    while not parser.TokenSymbolIs('END') do ProcessObject;
    parser.NextToken;           // Skip end token
    Output.WriteByte(0);        // Terminate property list
  end;

const
  signature: PChar = 'TPF0';
begin
  parser := TParser.Create(Input);
  try
    Output.Write(signature[0], 4);
    ProcessObject;
  finally
    parser.Free;
  end;
end;


procedure ObjectResourceToText(Input, Output: TStream);
begin
  Input.ReadResHeader;
  ObjectBinaryToText(Input, Output);
end;


procedure ObjectTextToResource(Input, Output: TStream);
var
  StartPos, SizeStartPos, BinSize: LongInt;
  parser: TParser;
  name: String;
begin
  // Get form type name
  StartPos := Input.Position;
  parser := TParser.Create(Input);
  try
    if not parser.TokenSymbolIs('OBJECT') then parser.CheckTokenSymbol('INHERITED');
    parser.NextToken;
    parser.CheckToken(toSymbol);
    parser.NextToken;
    parser.CheckToken(':');
    parser.NextToken;
    parser.CheckToken(toSymbol);
    name := parser.TokenString;
  finally
    parser.Free;
    Input.Position := StartPos;
  end;

  // Write resource header
  name := UpperCase(name);
  Output.WriteByte($ff);
  Output.WriteByte(10);
  Output.WriteByte(0);
  Output.Write(name[1], Length(name) + 1);      // Write null-terminated form type name
  Output.WriteWord($1030);
  SizeStartPos := Output.Position;
  Output.WriteDWord(0);                 // Placeholder for data size
  ObjectTextToBinary(Input, Output);    // Convert the stuff!
  BinSize := Output.Position - SizeStartPos - 4;
  Output.Position := SizeStartPos;
  Output.WriteDWord(BinSize);           // Insert real resource data size
end;



{ Utility routines }

function LineStart(Buffer, BufPos: PChar): PChar;

begin
  Result := BufPos;
  while Result > Buffer do begin
    Dec(Result);
    if Result[0] = #10 then break;
  end;
end;

procedure CommonInit;
begin
  InitHandlerList:=Nil;
  IntConstList := TThreadList.Create;
  GlobalFixupList := TThreadList.Create;
  ClassList := TThreadList.Create;
  ClassAliasList := TStringList.Create;
end;

procedure CommonCleanup;
var
  i: Integer;
begin
  // !!!: GlobalNameSpace.BeginWrite;
  with IntConstList.LockList do
    try
      for i := 0 to Count - 1 do
        TIntConst(Items[I]).Free;
    finally
      IntConstList.UnlockList;
    end;
    IntConstList.Free;
  ClassList.Free;
  ClassAliasList.Free;
  RemoveFixupReferences(nil, '');
  GlobalFixupList.Free;
  GlobalFixupList := nil;
  GlobalLists.Free;
  ComponentPages.Free;
  {!!!: GlobalNameSpace.Free;
  GlobalNameSpace := nil;}
  InitHandlerList.Free;
  InitHandlerList:=Nil;
end;



{ TFiler implementation }
{$i filer.inc}

{ TReader implementation }
{$i reader.inc}

{ TWriter implementations }
{$i writer.inc}
{$i twriter.inc}


{
  $Log: classes.inc,v $
  Revision 1.3  2005/02/14 17:13:11  peter
    * truncate log

}
