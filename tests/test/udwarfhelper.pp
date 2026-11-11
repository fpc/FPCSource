unit udwarfhelper;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
interface

uses
  udwarfdump, SysUtils, fgl;

{ Nested DWARF TAG / attribute container.

  - TDwarfTag is the container for one DIE (tag). The root (e.g.
    DW_TAG_compile_unit) has Parent = nil.
  - AddTag creates a child tag and returns its container.
  - Attributes are kept in insertion order, with lookup by name.
  - for-in over a tag        -> direct children
  - for-in over .Descendants -> all nested tags, depth-first pre-order
  - for-in over .Attrs       -> the attributes
  Children are owned (freed) by their parent. }

type
  TDwarfTag = class;

  TDwarfAttribute = class
  private
    FName: string;
    FForm: string;
    FValue: string;
  public
    constructor Create(const AName, AValue: string; const AForm: string = '');
    property Name: string read FName;
    property Form: string read FForm;
    property Value: string read FValue write FValue;
  end;

  TDwarfAttributeList = specialize TFPGObjectList<TDwarfAttribute>;
  TDwarfTagList = specialize TFPGObjectList<TDwarfTag>;

  { direct children }
  TDwarfChildEnumerator = record
  private
    FList: TDwarfTagList;
    FIndex: Integer;
    function GetCurrent: TDwarfTag;
  public
    function MoveNext: Boolean;
    property Current: TDwarfTag read GetCurrent;
  end;

  TDwarfAttrEnumerator = record
  private
    FList: TDwarfAttributeList;
    FIndex: Integer;
    function GetCurrent: TDwarfAttribute;
  public
    function MoveNext: Boolean;
    property Current: TDwarfAttribute read GetCurrent;
  end;

  TDwarfAttrEnumerable = record
  private
    FList: TDwarfAttributeList;
  public
    function GetEnumerator: TDwarfAttrEnumerator;
  end;

  TDwarfRecursiveEnumerator = record
  private
    FStackTags: array of TDwarfTag;   // tag whose children are being walked
    FStackIdx: array of Integer;      // next child index for that tag
    FDepth: Integer;
    FCurrent: TDwarfTag;
    procedure Init(ARoot: TDwarfTag);
  public
    function MoveNext: Boolean;
    property Current: TDwarfTag read FCurrent;
  end;

  TDwarfRecursiveEnumerable = record
  private
    FRoot: TDwarfTag;
  public
    function GetEnumerator: TDwarfRecursiveEnumerator;
  end;

  { TDwarfTag }

  TDwarfTag = class
  private
    FParent: TDwarfTag;
    FTagName: string;
    FOffset: string;
    FOffsetAddr: string;
    FChildren: TDwarfTagList;
    FAttributes: TDwarfAttributeList;
    function GetAttrByIndex(AIndex: Integer): TDwarfAttribute;
    function GetAttrValue(const AName: string): string;
    function GetChild(AIndex: Integer): TDwarfTag;
    function GetChildCount: Integer;
    function GetAttrCount: Integer;
    function GetDepth: Integer;
  public
    constructor Create(AParent: TDwarfTag; const ATagName: string);
    destructor Destroy; override;
    function IsTagNameIn(AName: String; ACaseSense: boolean = False): boolean; // ; separated
    function IsAtOffs(AnOffset: string): boolean;

    { children }
    function AddTag(const ATagName: string; AnOffs: String; AnOffsAddr: string=''): TDwarfTag;
    function FindTagAt(AnOffs: string): TDwarfTag;
    function FindTag(AName: string; ATagName: string=''): TDwarfTag;
    function FindNestedTag(AName: string; ATagName: string=''): TDwarfTag;

    { attributes }
    function AddAttribute(const AName, AValue: string;
      const AForm: string = ''): TDwarfAttribute;
    function FindAttribute(const AName: string): TDwarfAttribute; // nil if missing
    function HasAttribute(const AName: string): Boolean;
    function TryGetAttribute(const AName: string; out AValue: string): Boolean;

    { enumeration }
    function GetEnumerator: TDwarfChildEnumerator;        // direct children
    function Descendants: TDwarfRecursiveEnumerable;      // all nested tags
    function Attrs: TDwarfAttrEnumerable;                 // attributes

    property Parent: TDwarfTag read FParent;               // nil for the root
    property TagName: string read FTagName;
    property Offset: string read FOffset write FOffset;
    property OffsetAddr: string read FOffsetAddr write FOffsetAddr;
    property Depth: Integer read GetDepth;

    property ChildCount: Integer read GetChildCount;
    property Children[AIndex: Integer]: TDwarfTag read GetChild;

    property AttrCount: Integer read GetAttrCount;
    property AttrByIndex[AIndex: Integer]: TDwarfAttribute read GetAttrByIndex;
    property AttrValue[const AName: string]: string read GetAttrValue; default;
  end;



procedure ResProc(AddLine: String);
procedure DoError(AText: string);

type
  TTestTypeFlag = (
    ttWantAlias,          // require DW_TAG_typedef
    ttNoAlias,            // require  *NO*  DW_TAG_typedef
    ttAllowRefBYPointer,  // allow a DW_TAG_pointer_type for types with internal pointer
    {ttAllowRef, }
    ttAnyType,
    ttAnyTypeNoName   // real pointer may have name only in DW_TAG_typedef
  );
  TTestTypeFlags = set of TTestTypeFlag;
  TCheckPropertyFlag = (
    cpFieldOwned,
    cpFieldInherited,
    cpHasFormalParam
  );
  TCheckPropertyFlags = set of TCheckPropertyFlag;
  TDwarfTagArray = array of TDwarfTag;


(* Get nodes // assert a node was found *)
function GetNodeByName(ATag, AName: String; AParentNode: TDwarfTag=nil): TDwarfTag;
function GetNodeAtOffs(AnOffs: String): TDwarfTag;
function GetNodeAtOffs(AnOffs: String; out AName: String): TDwarfTag;
function GetDirectChildNode(AParentNode: TDwarfTag; AnChildTag: String): TDwarfTag;

(* Get attribute values // assert the attribute exists *)
function GetAttribFromNode(ANode: TDwarfTag; AnAttr: String): String;
function GetNameFromNode(ANode: TDwarfTag): String;
function GetTypeOffsFromNode(ANode: TDwarfTag): String;
function GetAttribFromNamedTag(ATag, AName, AnAttr: String; AParentNode: TDwarfTag=nil): String;
function GetTypeOffsFromMember(AName: String; AParentNode: TDwarfTag): String;




//CheckType returns the type-tag
function CheckType(ATypeOffs: String; AnExpName: string; ATestTypeFlags: TTestTypeFlags;
  var AnExpNotEqual: TDwarfTagArray; AnAddToNotExp: boolean;
  ATypeTag: string = ''): TDwarfTag; overload;
function CheckType(ATypeOffs: String; AnExpName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string = ''): TDwarfTag; overload;
// CheckVar: returns type node
function CheckVar(AVarName, AnExpTypeName: String; ATestTypeFlags: TTestTypeFlags;
  var AnExpNotEqual: TDwarfTagArray; AnAddToNotExp: boolean;
  ATypeTag: string = ''): TDwarfTag; overload;
function CheckVar(AVarName, AnExpTypeName: String; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string = ''): TDwarfTag; overload;

// CheckMember: returns type node
function CheckMember(ANode: TDwarfTag; AName: String; AnExpTypeName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string = ''): TDwarfTag;
// CheckMember: returns proc node // test return type
function CheckMethod(ANode: TDwarfTag; AName: String; AnExpTypeName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string = ''): TDwarfTag;
// CheckMethodParam return type
function CheckMethodParam(ANode: TDwarfTag; AName: String; AnExpTypeName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string = ''): TDwarfTag;
// CheckInheritance returns type node
function CheckInheritance(ANode: TDwarfTag; AnExpTypeName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string = ''): TDwarfTag;

function CheckProperty(AStructNode: TDwarfTag; APropName, AnExpTypeName, AnExpReadField, AnExpReadStruct: String; AFlags:TCheckPropertyFlags = []): TDwarfTag;
procedure CheckPropertySetter(APropNode: TDwarfTag; AnExpWriteField, AnExpWriteStruct: String);

procedure CheckLinkageName(ANode: TDwarfTag; AnExpStructName, AnExpMethodName: string);

var
  ErrInfo: String;
  RootTag: TDwarfTag;

implementation

var
  CU_Idx: integer=0;

function IsTextInList(AText, AList: String; ACaseSense: boolean=false): boolean;
var
  n: TAnsiStringArray;
  i: integer;
begin
  if not ACaseSense then begin
    AList := UpperCase(AList);
    AText := UpperCase(AText);
  end;
  n := AList.Split(';');
  i := Length(n)-1;
  while (i>=0) and (not SameText(AText, n[i])) do
    dec(i);
  Result := i >= 0;
end;

function ZeroTrim(s: string): string;
begin
  Result := s;
  while (Length(Result) > 1) and (Result[1] = '0') do delete(Result,1,1);
end;

{ TDwarfAttribute }

constructor TDwarfAttribute.Create(const AName, AValue: string; const AForm: string);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
  FForm := AForm;
end;

{ TDwarfChildEnumerator }

function TDwarfChildEnumerator.GetCurrent: TDwarfTag;
begin
  Result := FList[FIndex];
end;

function TDwarfChildEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TDwarfAttrEnumerator }

function TDwarfAttrEnumerator.GetCurrent: TDwarfAttribute;
begin
  Result := FList[FIndex];
end;

function TDwarfAttrEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TDwarfAttrEnumerable }

function TDwarfAttrEnumerable.GetEnumerator: TDwarfAttrEnumerator;
begin
  Result.FList := FList;
  Result.FIndex := -1;
end;

{ TDwarfRecursiveEnumerator }

procedure TDwarfRecursiveEnumerator.Init(ARoot: TDwarfTag);
begin
  SetLength(FStackTags, 16);
  SetLength(FStackIdx, 16);
  FStackTags[0] := ARoot;
  FStackIdx[0] := 0;
  FDepth := 1;
  FCurrent := nil;
end;

function TDwarfRecursiveEnumerator.MoveNext: Boolean;
var
  Top: Integer;
  T: TDwarfTag;
begin
  Result := False;
  while FDepth > 0 do
  begin
    Top := FDepth - 1;
    if FStackIdx[Top] < FStackTags[Top].ChildCount then
    begin
      T := FStackTags[Top].Children[FStackIdx[Top]];
      Inc(FStackIdx[Top]);
      { descend into T on the following calls }
      if FDepth = Length(FStackTags) then
      begin
        SetLength(FStackTags, FDepth * 2);
        SetLength(FStackIdx, FDepth * 2);
      end;
      FStackTags[FDepth] := T;
      FStackIdx[FDepth] := 0;
      Inc(FDepth);
      FCurrent := T;
      Exit(True);
    end
    else
      Dec(FDepth);   // children exhausted, go back up
  end;
  FCurrent := nil;
end;

{ TDwarfRecursiveEnumerable }

function TDwarfRecursiveEnumerable.GetEnumerator: TDwarfRecursiveEnumerator;
begin
  Result.Init(FRoot);
end;

{ TDwarfTag }

constructor TDwarfTag.Create(AParent: TDwarfTag; const ATagName: string);
begin
  inherited Create;
  FParent := AParent;
  FTagName := ATagName;
  FChildren := TDwarfTagList.Create(True);          // owns children
  FAttributes := TDwarfAttributeList.Create(True);  // owns attributes
end;

destructor TDwarfTag.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TDwarfTag.IsTagNameIn(AName: String; ACaseSense: boolean): boolean;
begin
  Result := IsTextInList(FTagName, AName, ACaseSense);
end;

function TDwarfTag.IsAtOffs(AnOffset: string): boolean;
begin
  AnOffset := ZeroTrim(AnOffset);
  Result := SameText(AnOffset, ZeroTrim(FOffset)) or
            SameText(AnOffset, ZeroTrim(FOffsetAddr));
end;

function TDwarfTag.AddTag(const ATagName: string; AnOffs: String; AnOffsAddr: string): TDwarfTag;
begin
  Result := TDwarfTag.Create(Self, ATagName);
  Result.Offset := AnOffs;
  Result.OffsetAddr := AnOffsAddr;
  FChildren.Add(Result);
end;

function TDwarfTag.FindTagAt(AnOffs: string): TDwarfTag;
var
  i: Integer;
begin
  AnOffs := UpperCase(ZeroTrim(AnOffs));
  if SameText(AnOffs, UpperCase(ZeroTrim(FOffset))) or
     SameText(AnOffs, UpperCase(ZeroTrim(FOffsetAddr)))
  then
    exit(Self);

  for i := 0 to FChildren.Count - 1 do begin
    Result := FChildren[i].FindTagAt(AnOffs);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TDwarfTag.FindTag(AName: string; ATagName: string): TDwarfTag;
var
  i: Integer;
  c: udwarfhelper.TDwarfTag;
begin
  AName := UpperCase(AName);
  for i := 0 to FChildren.Count - 1 do begin
    c := FChildren[i];
    if ((ATagName = '') or c.IsTagNameIn(ATagName)) and
       ((AName = '') or SameText(c.AttrValue['DW_AT_name'], AName))
    then
      Exit(c);
  end;
  Result := nil;
end;

function TDwarfTag.FindNestedTag(AName: string; ATagName: string): TDwarfTag;
var
  i: Integer;
  c: udwarfhelper.TDwarfTag;
begin
  AName := UpperCase(AName);
  for i := 0 to FChildren.Count - 1 do begin
    c := FChildren[i];
    if ((ATagName = '') or c.IsTagNameIn(ATagName)) and
       ((AName = '') or IsTextInList(c.AttrValue['DW_AT_name'], AName))
    then
      Exit(c);
    Result := c.FindNestedTag(AName, ATagName);
    if Result <> nil then
      exit;
  end;
  Result := nil;
end;

function TDwarfTag.AddAttribute(const AName, AValue: string; const AForm: string): TDwarfAttribute;
begin
  Result := TDwarfAttribute.Create(AName, AValue, AForm);
  FAttributes.Add(Result);
end;

function TDwarfTag.FindAttribute(const AName: string): TDwarfAttribute;
var
  i: Integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    if SameText(FAttributes[i].Name, AName) then
      Exit(FAttributes[i]);
  Result := nil;
end;

function TDwarfTag.HasAttribute(const AName: string): Boolean;
begin
  Result := FindAttribute(AName) <> nil;
end;

function TDwarfTag.TryGetAttribute(const AName: string; out AValue: string): Boolean;
var
  A: TDwarfAttribute;
begin
  A := FindAttribute(AName);
  Result := A <> nil;
  if Result then AValue := A.Value else AValue := '';
end;

function TDwarfTag.GetAttrValue(const AName: string): string;
var
  A: TDwarfAttribute;
begin
  A := FindAttribute(AName);
  if A <> nil then Result := A.Value else Result := '';
end;

function TDwarfTag.GetAttrByIndex(AIndex: Integer): TDwarfAttribute;
begin
  Result := FAttributes[AIndex];
end;

function TDwarfTag.GetAttrCount: Integer;
begin
  Result := FAttributes.Count;
end;

function TDwarfTag.GetChild(AIndex: Integer): TDwarfTag;
begin
  Result := FChildren[AIndex];
end;

function TDwarfTag.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TDwarfTag.GetDepth: Integer;
var
  P: TDwarfTag;
begin
  Result := 0;
  P := FParent;
  while P <> nil do
  begin
    Inc(Result);
    P := P.FParent;
  end;
end;

function TDwarfTag.GetEnumerator: TDwarfChildEnumerator;
begin
  Result.FList := FChildren;
  Result.FIndex := -1;
end;

function TDwarfTag.Descendants: TDwarfRecursiveEnumerable;
begin
  Result.FRoot := Self;
end;

function TDwarfTag.Attrs: TDwarfAttrEnumerable;
begin
  Result.FList := FAttributes;
end;


var
  CurTag: TDwarfTag;
procedure ResProc(AddLine: String);
var
  i,j,k,d: integer;
  s: String;
begin
  i := 0;
  while (Length(AddLine)>i) and (AddLine[i+1] = ' ') do Inc(i);
  if pos('DW_TAG_', AddLine) > 0 then begin
    if pos('DW_TAG_compile_unit', AddLine) > 0 then
      inc(CU_Idx);

    d := i div 2;
    while CurTag.Depth > d do CurTag := CurTag.Parent;

    inc(i);
    j := pos('_',AddLine)-2;
    CurTag := CurTag.AddTag(
      copy(AddLine, j, Length(AddLine)-11-j),
      copy(AddLine, i, j-i-1)+'.'+IntToStr(CU_Idx),
      copy(AddLine, Length(AddLine)-10, 10)
    );

  end
  else begin
    j := pos('DW_FORM_ref', AddLine);
    s := '';
    if (j > 0) and (pos('DW_FORM_ref_addr', AddLine) < 1) then begin // not addr
      s := '.'+IntToStr(CU_Idx);
    end;

    inc(i);
    j := pos('(',AddLine);
    k := pos(')',AddLine);
    CurTag.AddAttribute(
      copy(AddLine, i, j-i-1),
      copy(AddLine, k+3, Length(AddLine))+s,
      copy(AddLine, j+1, k-j-1)
    );
  end;
end;

procedure DoError(AText: string);
begin
  writeln(StdErr, AText+' <<<< '+ErrInfo);
  halt(1);
end;

function GetNodeByName(ATag, AName: String; AParentNode: TDwarfTag): TDwarfTag;
var
  dummy: TDwarfTag;
begin
  if AParentNode = nil then AParentNode := RootTag;
  Result := AParentNode.FindNestedTag(AName,ATag);

  if Result = nil then
    DoError('Did not find '+ATag+' with name: '+AName);
end;

function GetNodeAtOffs(AnOffs: String): TDwarfTag;
begin
  Result := RootTag.FindTagAt(AnOffs);
  if result = nil then
    DoError('Did not find node at offset '+AnOffs);
end;

function GetNodeAtOffs(AnOffs: String; out AName: String): TDwarfTag;
begin
  Result := GetNodeAtOffs(AnOffs);
  AName := Result.AttrValue['DW_AT_name'];
end;

function GetDirectChildNode(AParentNode: TDwarfTag; AnChildTag: String): TDwarfTag;
begin
  Result := AParentNode.FindTag('', AnChildTag);
  if Result=nil then
    DoError('Did not find a tag '+AnChildTag);
end;

function GetAttribFromNode(ANode: TDwarfTag; AnAttr: String): String;
var
  a: TDwarfAttribute;
begin
  a := ANode.FindAttribute(AnAttr);
  if a = nil then
    DoError('Did not find a tag '+AnAttr);
  Result := a.Value;
end;

function GetNameFromNode(ANode: TDwarfTag): String;
begin
  Result := GetAttribFromNode(ANode, 'DW_AT_name');
end;

function GetTypeOffsFromNode(ANode: TDwarfTag): String;
begin
  Result := GetAttribFromNode(ANode, 'DW_AT_type');
end;

function GetAttribFromNamedTag(ATag, AName, AnAttr: String; AParentNode: TDwarfTag): String;
var
  t: TDwarfTag;
begin
  if AParentNode = nil then AParentNode := RootTag;
  t := AParentNode.FindNestedTag(AName, '');
  if t=nil then
    DoError('Did not find a tag '+ATag+' with name: '+AName + ' and attr '+AnAttr);
  Result := GetAttribFromNode(t, AnAttr);
end;

function GetTypeOffsFromMember(AName: String; AParentNode: TDwarfTag): String;
begin
  Result := GetAttribFromNamedTag('DW_TAG_member', AName, 'DW_AT_type', AParentNode);
end;


function CheckType(ATypeOffs: String; AnExpName: string; ATestTypeFlags: TTestTypeFlags;
  var AnExpNotEqual: TDwarfTagArray; AnAddToNotExp: boolean; ATypeTag: string): TDwarfTag;

  procedure CheckNotInList(n: TDwarfTag);
  var
    i: Integer;
  begin
    for i := 0 to Length(AnExpNotEqual) - 1 do
      if AnExpNotEqual[i] = n then
        DoError('Type @'+ATypeOffs+' with exp name '+AnExpName+' is in non exp list');
    if AnAddToNotExp then begin
      SetLength(AnExpNotEqual, Length(AnExpNotEqual)+1);
      AnExpNotEqual[Length(AnExpNotEqual)-1] := n;
    end;
  end;

var
  FndName, ri: string;
begin
  Result := nil;
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check Type - Exp: '+ATypeOffs;
  AnExpName := UpperCase(AnExpName);

  if ATypeTAG = '' then ATypeTAG := 'DW_TAG_structure_type';

  Result := GetNodeAtOffs(ATypeOffs, FndName);
  CheckNotInList(Result);
  if (UpperCase(FndName) <> AnExpName) then
    DoError('wrong type name in tag '+ATypeOffs+': '+Result.TagName+' -- exp name '+AnExpName + ' got '+FndName);

  if (Result.TagName = 'DW_TAG_typedef') then begin
    if not(ttNoAlias in ATestTypeFlags) then begin
      ATypeOffs := GetTypeOffsFromNode(Result);
      Result := GetNodeAtOffs(ATypeOffs, FndName);
      CheckNotInList(Result);
      ErrInfo := ErrInfo + ' -> TypeDef'
    end
    else
      DoError('Type @'+ATypeOffs+' with exp name '+AnExpName+' has DW_TAG_typedef');
  end
  else
  if not(ttWantAlias in ATestTypeFlags) then
    DoError('Type @'+ATypeOffs+' with exp name '+AnExpName+' has NOT DW_TAG_typedef');

  if (Result.TagName = 'DW_TAG_pointer_type') and
     not( (ttAnyType in ATestTypeFlags) or SameText(Result.TagName, ATypeTag) )
  then begin
    if (ttAllowRefBYPointer in ATestTypeFlags) then begin
      ATypeOffs := GetTypeOffsFromNode(Result);
      Result := GetNodeAtOffs(ATypeOffs, FndName);
      CheckNotInList(Result);
      ErrInfo := ErrInfo + ' -> Ptr'
    end
    else
      DoError('Type @'+ATypeOffs+' with exp name '+AnExpName+' has DW_TAG_pointer');
  end;

  if not (ttAnyType in ATestTypeFlags) then
    if (Result.TagName <> ATypeTag) then DoError('Type @'+ATypeOffs+' with exp name '+AnExpName+' has NOT '+ATypeTAG + ' got '+Result.TagName);
  if not (ttAnyTypeNoName in ATestTypeFlags) then
    if (UpperCase(FndName) <> AnExpName) then
      DoError('wrong type name(F) in tag '+ATypeOffs+': '+Result.TagName+' -- exp name '+AnExpName + ' got '+FndName);
  ErrInfo := ri;
end;

function CheckType(ATypeOffs: String; AnExpName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string): TDwarfTag;
var dummy: TDwarfTagArray;
begin
  result := CheckType(ATypeOffs, AnExpName, ATestTypeFlags, dummy, false, ATypeTag);
end;

function CheckVar(AVarName, AnExpTypeName: String; ATestTypeFlags: TTestTypeFlags;
  var AnExpNotEqual: TDwarfTagArray; AnAddToNotExp: boolean; ATypeTag: string): TDwarfTag;
var
  ri: String;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check Var '+AVarName;
  result := CheckType(
    //GetTypeOffs('DW_TAG_variable', AVarName),
    GetAttribFromNamedTag('DW_TAG_variable', AVarName, 'DW_AT_type'),
    UpperCase(AnExpTypeName),
    ATestTypeFlags,
    AnExpNotEqual,
    AnAddToNotExp,
    ATypeTag
  );
  ErrInfo := ri;
end;

function CheckVar(AVarName, AnExpTypeName: String; ATestTypeFlags: TTestTypeFlags; ATypeTag: string
  ): TDwarfTag;
var dummy: TDwarfTagArray;
begin
  result := CheckVar(AVarName, AnExpTypeName, ATestTypeFlags, dummy, false, ATypeTag);
end;

function CheckMember(ANode: TDwarfTag; AName: String; AnExpTypeName: string;
  ATestTypeFlags: TTestTypeFlags; ATypeTag: string): TDwarfTag;
var
  ri, o: String;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check member '+AName;
  o := GetAttribFromNamedTag('DW_TAG_member', AName, 'DW_AT_type', ANode);
  Result := CheckType(o, AnExpTypeName, ATestTypeFlags, ATypeTag);

  ErrInfo := ri;
end;

function CheckMethod(ANode: TDwarfTag; AName: String; AnExpTypeName: string;
  ATestTypeFlags: TTestTypeFlags; ATypeTag: string): TDwarfTag;
var
  ri, t: String;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check method '+AName;

  Result := GetNodeByName('DW_TAG_subprogram', AName, ANode);

  if AnExpTypeName <> '' then
    CheckType(GetTypeOffsFromNode(Result), AnExpTypeName, ATestTypeFlags, ATypeTag);

  ErrInfo := ri;
end;

function CheckMethodParam(ANode: TDwarfTag; AName: String; AnExpTypeName: string;
  ATestTypeFlags: TTestTypeFlags; ATypeTag: string): TDwarfTag;
var
  ri: String;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check method param '+AName;

  Result := GetNodeByName('DW_TAG_formal_parameter', AName, ANode);

  if AnExpTypeName <> '' then
    CheckType(GetTypeOffsFromNode(Result), AnExpTypeName, ATestTypeFlags, ATypeTag);

  ErrInfo := ri;
end;

function CheckInheritance(ANode: TDwarfTag; AnExpTypeName: string; ATestTypeFlags: TTestTypeFlags;
  ATypeTag: string): TDwarfTag;
var
  ri: String;
  t: TDwarfTag;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check inheritance  '+AnExpTypeName;
  t := GetDirectChildNode(ANode, 'DW_TAG_inheritance');
  Result := CheckType(GetTypeOffsFromNode(t), AnExpTypeName, ATestTypeFlags, ATypeTag);
  ErrInfo := ri;
end;

procedure CheckLinkageName(ANode: TDwarfTag; AnExpStructName, AnExpMethodName: string);
var
  ri, t, s: String;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check link-name '+AnExpStructName+'.'+AnExpMethodName;
  t := GetAttribFromNode(ANode, 'DW_AT_linkage_name');

  s := Format('_ZN%d%s%d%sEv', [Length(AnExpStructName), AnExpStructName, Length(AnExpMethodName), AnExpMethodName]);
  if UpperCase(t) <> UpperCase(s) then
    DoError('Wrong linkage name: got:'+ t);

  ErrInfo := ri;
end;

function CheckProperty(AStructNode: TDwarfTag; APropName, AnExpTypeName, AnExpReadField,
  AnExpReadStruct: String; AFlags: TCheckPropertyFlags): TDwarfTag;
var
  ri, ri2, Offs, FndName: String;
  dummy: TDwarfTagArray;
  GetterNode, TmpNode, InherNode: TDwarfTag;
  i: integer;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check Prop '+APropName;
  ri2 := ErrInfo;

  Result := GetNodeByName('DW_TAG_Property', APropName, AStructNode);

  if AnExpTypeName <> '' then begin
    ErrInfo := ErrInfo + ' / Type ';
    Offs := GetTypeOffsFromNode(Result);
    CheckType(Offs, UpperCase(AnExpTypeName), [ttAnyType], dummy, False);
  end;

  if AnExpReadField = '' then // TODO: check absence of reader tag
    exit;

  ErrInfo := ri2 + ' / READER ';
  GetterNode := GetDirectChildNode(Result, 'DW_TAG_Property_getter');
  Offs := GetAttribFromNode(GetterNode, 'DW_AT_property_forward');

  TmpNode := GetNodeAtOffs(Offs, FndName);
  if UpperCase(FndName) <> UpperCase(AnExpReadField) then
    DoError('wrong read field name. exp '+ AnExpReadField+ ' got '+FndName);

  TmpNode := TmpNode.Parent;
  if not TmpNode.IsTagNameIn('DW_TAG_structure_type;DW_TAG_class_type') then
    DoError('wrong read field struct TAG name. exp  got '+TmpNode.TagName);
  if AnExpReadStruct <> '' then begin
    FndName := GetAttribFromNode(TmpNode, 'DW_AT_name');
    if UpperCase(FndName) <> UpperCase(AnExpReadStruct) then
      DoError('wrong read field struct name. exp '+ AnExpReadStruct+ ' got '+FndName);
  end
  else
    if TmpNode.FindAttribute('DW_AT_name') <> nil then
      DoError('Did expect un-named struct, but got name ');

  if cpFieldOwned in AFlags then begin
    if TmpNode <> AStructNode then
      DoError('Field is not in same structure as property. exp(self) '+AStructNode.Offset +' got(fld) '+TmpNode.Offset);
  end;

  if cpFieldInherited in AFlags then begin
    InherNode := GetDirectChildNode(AStructNode, 'DW_TAG_inheritance');
    InherNode := GetNodeAtOffs(GetTypeOffsFromNode(InherNode));
    i := 0;
    while (TmpNode <> InherNode) and (i < 99) do begin
      if InherNode.TagName = 'DW_TAG_typedef' then begin
        InherNode := GetNodeAtOffs(GetTypeOffsFromNode(InherNode));
        if TmpNode = InherNode then
          break;
      end;

      InherNode := GetDirectChildNode(InherNode, 'DW_TAG_inheritance');
      InherNode := GetNodeAtOffs(GetTypeOffsFromNode(InherNode));
      inc(i);
    end;

    if TmpNode <> InherNode then
      DoError('Field does not inherit from structure of property. exp(self) '+AStructNode.Offset+' got(fld) '+TmpNode.Offset);
  end;

  if cpHasFormalParam in AFlags then begin
    GetDirectChildNode(GetterNode, 'DW_TAG_formal_parameter');
  end;

  ErrInfo := ri;
end;

procedure CheckPropertySetter(APropNode: TDwarfTag; AnExpWriteField, AnExpWriteStruct: String);
var
  ri, ri2, Offs, FndName: String;
  dummy: TDwarfTagArray;
  SetterNode, TmpNode: TDwarfTag;
begin
  ri := ErrInfo;
  ErrInfo := ErrInfo + ' / Check Prop-setter '+AnExpWriteField;
  ri2 := ErrInfo;

  SetterNode := GetDirectChildNode(APropNode, 'DW_TAG_Property_setter');
  Offs := GetAttribFromNode(SetterNode, 'DW_AT_property_forward');

  TmpNode := GetNodeAtOffs(Offs, FndName);
  if UpperCase(FndName) <> UpperCase(AnExpWriteField) then
    DoError('wrong read field name. exp '+ AnExpWriteField+ ' got '+FndName);

  TmpNode := TmpNode.Parent;
  if not TmpNode.IsTagNameIn('DW_TAG_structure_type;DW_TAG_class_type') then
    DoError('wrong read field struct TAG name. exp  got '+TmpNode.TagName);
  if AnExpWriteStruct <> '' then begin
    FndName := GetAttribFromNode(TmpNode, 'DW_AT_name');
    if UpperCase(FndName) <> UpperCase(AnExpWriteStruct) then
      DoError('wrong read field struct name. exp '+ AnExpWriteStruct+ ' got '+FndName);
  end
  else
    if TmpNode.FindAttribute('DW_AT_name') <> nil then
      DoError('Did expect un-named struct, but got name ');

  ErrInfo := ri;
end;

initialization
  RootTag := TDwarfTag.Create(nil,'');
  CurTag := RootTag;
finalization
  RootTag.Free;
end.
