unit utclasses;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  SysUtils, Classes, punit, utrtl;
  
implementation 
  
Function TestBytesStream : Ansistring;

var
  BS: TBytesStream;
  MS: TMemoryStream;
  B: TBytes;
begin
  Result:='';
  B := TBytes.Create(1, 2, 3);
  BS := TBytesStream.Create(B);
  // save it to regular memory stream
  MS := TMemoryStream.Create;
  try
    BS.SaveToStream(MS);
  finally
    BS.Free;
  end;

  // now restore and compare
  BS := TBytesStream.Create;
  try
    MS.Position := 0;
    BS.LoadFromStream(MS);
    B := BS.Bytes;
    if not AssertTrue('Bytes differ',not (Length(B) < 3) or (B[0] <> 1) or (B[1] <> 2) or (B[2] <> 3)) then
      Exit;
  finally
    BS.Free;
  end;
  MS.Free;
end;


type
 tenum = (eena,eenb,eenc,eend,eene,eenf,eeng,eenh,eeni);
 tset = set of tenum;

 ttestclass1 = class(tcomponent)
  private
   fprop1: tset;
  public
   property prop1: tset read fprop1 write fprop1 stored true;
 end;

 ttestclass2 = class(ttestclass1)
  published
   property prop1;
 end;

function TestStoredfalse : Ansistring;

var
 testclass2,testclass3: ttestclass2;
 stream1,stream2: tmemorystream;
 str1: ansistring;
begin
  Result:='';
  str1:='';
 testclass2:= ttestclass2.create(nil);
 testclass2.prop1:= [eenb,eend,eene,eenh,eeni];
 stream1:= tmemorystream.create;
 try
  stream1.writecomponent(testclass2);
  stream2:= tmemorystream.create;
  try
   stream1.position:= 0;
   objectbinarytotext(stream1,stream2);
   stream1.position:= 0;
   stream2.position:= 0;
   setlength(str1,stream2.size);
   move(stream2.memory^,str1[1],length(str1));
   testclass3:=ttestclass2.create(nil);
   stream1.readcomponent(testclass3);
   if not AssertTrue('Property set',testclass3.prop1=[eenb,eend,eene,eenh,eeni]) then
     Exit;
  finally
   stream2.free;
  end;
 finally
  stream1.free;
 end;
end;


type
  TMyStringList = class(TStringList)
  protected
    ExchangeCount: LongInt;
    procedure ExchangeItems(aLeft, aRight: Integer); override;
  end;
  
procedure TMyStringList.ExchangeItems(aLeft, aRight: Integer);
begin
  Inc(ExchangeCount);
  inherited ExchangeItems(aLeft, aRight);
end;

procedure FillStringList(aList: TStrings);
begin
  aList.Add('Beta');
  aList.Add('Gamma');
  aList.Add('Alpha');
  aList.Add('Delta');
end;

type
  TDummy = class
    ExchangeCount: LongInt;
    procedure Change(aSender: TObject);
  end;
  
procedure TDummy.Change(aSender: TObject);
begin
  Inc(ExchangeCount);
end;

Function Testtstringlistexchange : Ansistring;

var
  sl: TStringList;
  msl: TMyStringList;
  dummy: TDummy;
begin
  Result:='';
  dummy := TDummy.Create;
  try
    sl := TStringList.Create;
    try
      FillStringList(sl);
      sl.OnChange := @dummy.Change;
      sl.Sort;
      // only OnChange call in TStringList.Sort
      If not AssertEquals(' OnChange call in TStringList.Sort',1, dummy.ExchangeCount) then
        Exit;
    finally
      sl.Free;
    end;

    dummy.ExchangeCount := 0;
    
    msl := TMyStringList.Create;
    try
      FillStringList(msl);
      msl.OnChange := @dummy.Change;
      msl.Sort;
      // TMyStringList.ExchangeItems called 5 times
      if Not AssertEquals('TMyStringList.ExhangeItems call count',3,msl.ExchangeCount) then
        Exit;
      // OnChange called once in Sort
      if Not AssertEquals('Dummy.OnChange',1,dummy.ExchangeCount) then
        Exit
    finally
      msl.Free;
    end;
  finally
    dummy.Free;
  end;
end;


type
  TDummyVCLComObject = class(TInterfacedObject, IVCLComObject)
  public
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; override;
    procedure FreeOnRelease;
  end;
var
  c: TComponent;
  v: IVCLComObject;

procedure DoCreateVCLComObject(Component: TComponent);
begin
  Component.VCLComObject := Pointer(V);
end;

{ TDummyVCLComObject }

procedure TDummyVCLComObject.FreeOnRelease;
begin

end;


function TDummyVCLComObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;stdcall;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;stdcall;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.GetTypeInfoCount(out Count: Integer): HResult;stdcall;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;stdcall;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := E_UNEXPECTED;
end;

Function Testvclcomobject : Ansistring;

begin
  Result:='';
  v := TDummyVCLComObject.Create;
  CreateVCLComObjectProc := @DoCreateVCLComObject;
  c := TComponent.Create(nil);
  if c.ComObject = nil then
    Result:='No Comobject';
  c.Free;
  v := nil;
end;

Function TestLineBreak : String;

var
  tmp: TStrings;
begin
  tmp := TStringList.Create();
  try
    tmp.LineBreak := ',';
    tmp.Text := 'a,b,c';
    If tmp.Count<>3 then
      exit('Count needs to be 3');
    if tmp[0]<>'a' then
      exit('First element a');  
    if tmp[1]<>'b' then
      exit('Second element b');  
    if tmp[2]<>'c' then
      exit('Third element c');  
  finally
    tmp.Free;
  end;
end;

Function TestAlwaysQuote : String;

Const
  ResD  = 'ItemOne,ItemTwo,ItemThree,''Item With Spaces''';
  ResAQ = '''ItemOne'',''ItemTwo'',''ItemThree'',''Item With Spaces''';
  

Var 
  L : TStringList;
  
Begin
  L:=TStringList.Create;
  try
    With L do
      begin
      Add('ItemOne');
      Add('ItemTwo');
      Add('ItemThree');
      Add('Item With Spaces');
      QuoteChar := '''';
      if DelimitedText<>ResD then
        Exit('Default fails');
      AlwaysQuote := True;  
      if DelimitedText<>ResAQ then
        Exit('AlwaysQuote fails');
      end;                       
  finally
    L.Free;
  end;        
end;

Function TestGetNameValue : string;

var
  l: tstringlist;
begin
  l:= tstringlist.create;
  try
  l.add('bb');
  l.MissingNameValueSeparatorAction:=mnvaValue; 
  If not (l.ValueFromIndex[0]='bb') then
    Exit('mnvaValue value error');
  If not (l.Names[0]='') then
    Exit('mnvaValue name error');
  l.MissingNameValueSeparatorAction:=mnvaName; 
  If not (l.ValueFromIndex[0]='') then
    Exit('mnvaName value error');
  If not (l.Names[0]='bb') then
    Exit('mnvaName name error');
  l.MissingNameValueSeparatorAction:=mnvaEmpty; 
  If not (l.ValueFromIndex[0]='') then
    Exit('mnvaEmpty value error');
  If not (l.Names[0]='') then
    Exit('mnvaEmptyerror');
  l.MissingNameValueSeparatorAction:=mnvaError; 
  try 
    Writeln(l.ValueFromIndex[0]);
    Exit('mnvError value error');
  except
    // Ignore, expected
  end;
  finally
    L.free;
  end;   
end;


type
  TItem = class
  public
    Value: Integer;
    constructor Create(aValue: Integer);
  end;
  TSortParameter = class
  public
    Desc: Boolean;
  end;

  { TItem }

  constructor TItem.Create(aValue: Integer);
  begin
    inherited Create;
    Value := aValue;
  end;

  function Compare(Item1, Item2, Context: Pointer): Integer;
  var
    xItem1: TItem absolute Item1;
    xItem2: TItem absolute Item2;
    xParam: TSortParameter absolute Context;
  begin
    Result := xItem1.Value-xItem2.Value;
    if xParam.Desc then
      Result := -Result;
  end;



Function TestSortContext : String;

var
  L: TList;
  I: Integer;
  B: Boolean;
  P: TSortParameter;

  Procedure FreeItems;

  var
    I : integer;

  begin
    for I:=0 to L.Count-1 do
      TObject(L[i]).Free;
  end;

begin
  L := TList.Create;
  try
    for I := 1 to 5 do
      L.Add(TItem.Create(I));
    P := TSortParameter.Create;
    for B in Boolean do
      begin
      P.Desc := B;
      L.Sort(@Compare, P);
      if not B then
        begin
        For I:=1 to 5 do
          If (TItem(L[i-1]).Value<>i) then
             Exit(Format('ASC Error at %d',[I]));
        end
      else
        For I:=1 to 5 do
          If (TItem(L[i-1]).Value<>6-i) then
             Exit(Format('DESC Error at %d',[I]));
      end;

  finally
    P.Free;
    FreeItems;
    L.Free;
  end;
end;


Function TestStringListReverse1: String;

Var
  L,l2 : TStringList;
  I : Integer;

begin
  Result:='';
  L2:=Nil;
  L:=TStringList.Create;
  try
    L2:=TStringList.Create;
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    L.Reverse(L2);
    For I:=0 to 2 do
      if not AssertEquals('Item'+IntToStr(I),L[2-I],L2[I]) then exit;
  finally
    L.Free;
    L2.Free;
  end;

end;

Function TestStringListReverse2: String;

Var
  L,l2 : TStrings;
  I : Integer;

begin
  Result:='';
  L2:=Nil;
  L:=TStringList.Create;
  try
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    L2:=L.Reverse;
    if not AssertEquals('Classname',L.ClassName,L2.ClassName) then exit;
    For I:=0 to 2 do
      if not AssertEquals('Item'+IntToStr(I),L[2-I],L2[I]) then exit;
  finally
    L.Free;
    L2.Free;
  end;
end;

Function TestStringsIndexOfStartAt : String;

Var
  L : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    if not AssertEquals('Start at 0',2,L.IndexOf('3')) then exit;
    if not AssertEquals('Start at 1',2,L.IndexOf('3',1)) then exit;
    if not AssertEquals('Start at 2',2,L.IndexOf('3',2)) then exit;
    if not AssertEquals('Start at 3',5,L.IndexOf('3',3)) then exit;
    if not AssertEquals('Start at -1',5,L.IndexOf('3',-1)) then exit;
  finally
    L.Free;
  end;
end;

Function TestStringsLastIndexOfStartAt : String;

Var
  L : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    if not AssertEquals('Start at 0',5,L.LastIndexOf('3')) then exit;
    if not AssertEquals('Start at 1',-1,L.LastIndexOf('3',1)) then exit;
    if not AssertEquals('Start at 2',2,L.LastIndexOf('3',2)) then exit;
    if not AssertEquals('Start at 3',2,L.LastIndexOf('3',3)) then exit;
    if not AssertEquals('Start at -1',5,L.LastIndexOf('3',-1)) then exit;
    if not AssertEquals('Start at -2',2,L.LastIndexOf('3',-2)) then exit;
  finally
    L.Free;
  end;
end;

Function TestSlice: String;

Var
  L,l2 : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    L2:=TStringList.Create;
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    L.Slice(1,l2);
    if not AssertEquals('Item count',2,L2.Count) then exit;
    if not AssertEquals('Item 0','2',L2[0]) then exit;
    if not AssertEquals('Item 1','3',L2[1]) then exit;
  finally
    L2.Free;
    L.Free;
  end;
end;

Function TestSlice2 : String;

Var
  L,l2 : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 3 do
      L.Add(IntToStr(I));
    L2:=L.Slice(1);
    if not AssertEquals('Classname',L.ClassName,L2.ClassName) then exit;
    if not AssertEquals('Item count',2,L2.Count) then exit;
    if not AssertEquals('Item 0','2',L2[0]) then exit;
    if not AssertEquals('Item 1','3',L2[1]) then exit;
  finally
    L2.Free;
    L.Free;
  end;
end;

Function TestFill : String;

Var
  L : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L.Fill(' ',3,7);
    For I:=1 to 3 do
      AssertEquals(IntToStr(I),IntToStr(I),L[i-1]);
    For I:=3 to 7 do
      AssertEquals(IntToStr(I),' ',L[i]);
    For I:=9 to 10 do
      AssertEquals(IntToStr(I),IntToStr(I),L[i-1]);
  finally
    L.Free;
  end;
end;

Function TestFill2 : String;

Var
  L : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L.Fill(' ',3,-3);
    For I:=1 to 3 do
      AssertEquals(IntToStr(I),IntToStr(I),L[i-1]);
    For I:=3 to 7 do
      AssertEquals(IntToStr(I),' ',L[i]);
    For I:=9 to 10 do
      AssertEquals(IntToStr(I),IntToStr(I),L[i-1]);
  finally
    L.Free;
  end;
end;

Type
  TFilterStringList = Class(TStringList)
   function DoFilter (const s: string): boolean;
  end;

function TFilterStringList.DoFilter (const s: string): boolean;

begin
  Result:=StrToInt(S)<6;
end;


Function TestFilter : String;

Var
  L : TFilterStringList;
  L2 : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TFilterStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L2:=L.Filter(@L.DoFilter);
    if not AssertEquals('Classname',L.ClassName,L2.ClassName) then exit;
    if not AssertEquals('Count',5,L2.Count) then exit;
    For I:=1 to 5 do
      AssertEquals(IntToStr(I),IntToStr(I),L2[i-1]);
  finally
    L.Free;
    L2.Free;
  end;
end;

Function TestFilter2 : String;

Var
  L : TFilterStringList;
  L2 : TStrings;
  I : Integer;

begin
  Result:='';
  L2:=Nil;
  L:=TFilterStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L2:=TStringList.Create;
    L.Filter(@L.DoFilter,L2);
    if not AssertEquals('Classname',L.ClassName,L2.ClassName) then exit;
    if not AssertEquals('Count',5,L2.Count) then exit;
    For I:=1 to 5 do
      AssertEquals(IntToStr(I),IntToStr(I),L2[i-1]);
  finally
    L.Free;
    L2.Free;
  end;
end;

Type
  TMapStringList = Class(TStringList)
   function DoMap (const s: string): String;
  end;

function TMapStringList.DoMap (const s: string): string;

begin
  Result:=IntToStr(StrToInt(S)+10);
end;

Function TestMap : String;

Var
  L : TMapStringList;
  L2 : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TMapStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L2:=L.Map(@L.DoMap);
    if not AssertEquals('Classname',L.ClassName,L2.ClassName) then exit;
    if not AssertEquals('Count',10,L2.Count) then exit;
    For I:=1 to 10 do
      AssertEquals(IntToStr(I),IntToStr(I+10),L2[i-1]);
  finally
    L.Free;
    L2.Free;
  end;
end;


Function TestMap2 : String;

Var
  L : TMapStringList;
  L2 : TStrings;
  I : Integer;

begin
  Result:='';
  L:=TMapStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L2:=TStringList.Create;
    L.Map(@L.DoMap,L2);
    if not AssertEquals('Count',10,L2.Count) then exit;
    For I:=1 to 10 do
      AssertEquals(IntToStr(I),IntToStr(I+10),L2[i-1]);
  finally
    L.Free;
    L2.Free;
  end;
end;

Type
  TReduceStringList = Class(TStringList)
   function DoReduce (const s1,s2: string): String;
  end;

function TReduceStringList.DoReduce (const s1,s2: string): String;

begin
  Result:=IntToStr(StrToInt(S1)+StrToInt(S2));
end;

Function TestReduce : String;

Var
  L : TReduceStringList;
  S : String;
  I : Integer;

begin
  Result:='';
  L:=TReduceStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    S:=L.Reduce(@L.DoReduce,'0');
    If not AssertEquals('Correct','55',S) then exit;
  finally
    L.Free;
  end;
end;

Function TestPop : String;

Var
  L : TStringList;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    For I:=10 downto 1 do
      If not AssertEquals('Correct pop '+IntToStr(I),IntToStr(I),L.Pop) then exit;
    If not AssertEquals('Correct pop at last','',L.Pop) then exit;
  finally
    L.Free;
  end;
end;

Function TestShift : String;

Var
  L : TStringList;
  I : Integer;

begin
  Result:='';
  L:=TStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    For I:=1 to 10 do
      If not AssertEquals('Correct shift '+IntToStr(I),IntToStr(I),L.Shift) then exit;
    If not AssertEquals('Correct shift at last','',L.shift) then exit;
  finally
    L.Free;
  end;
end;

Type
  TForeachStringList = Class(TStringList)
   Public
   res : String;
   Procedure DoForeach (const s1: string);
  end;

Procedure TForeachStringList.DoForEach(Const S1 : String);

begin
  Res:=res+S1;
end;

Function TestForeach : String;

Var
  L : TForeachStringList;
  I : Integer;

begin
  Result:='';
  L:=TForeachStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L.Foreach(@L.DoForeach);
    If not AssertEquals('Correct','12345678910',L.Res) then exit;
  finally
    L.Free;
  end;
end;

Type
  TForeachExStringList = Class(TStringList)
   Public
   res : String;
   Procedure DoForeach (const s1: string; const aIndex : integer);
  end;

Procedure TForeachExStringList.DoForEach(Const S1 : String; const aIndex : integer);

begin
  Res:=res+S1+IntToStr(aIndex);
end;

Function TestForeachEx : String;

Var
  L : TForeachExStringList;
  I : Integer;

begin
  Result:='';
  L:=TForeachExStringList.Create;
  try
    For I:=1 to 10 do
      L.Add(IntToStr(I));
    L.Foreach(@L.DoForeach);
    If not AssertEquals('Correct','102132435465768798109',L.Res) then exit;
  finally
    L.Free;
  end;
end;

function CompareStringLists(Expected,TestSL : TStrings):string;

var
  I : Integer;

begin
  Result:='';
  if Expected.Count<>TestSL.Count then
    Exit('count mismatch: '+ inttostr(TestSL.Count)+' test strings; '+inttostr(Expected.Count)+' expected strings.');
  for i:=0 to TestSL.Count-1 do
    if (Expected.Count>i) and (TestSL[i]<>Expected[i]) then
      Exit('Line '+IntToStr(i)+' mismatch, expected *'+Expected[i]+'*, got: *'+TestSL[i]);
end;

function ReadStrictDelimFalse:string;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=false (default) when processing the delimitedtext
//
// Mainly check if reading quotes is according to Delphi sdf specs and works.
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='normal_string;"quoted_string";"quoted;delimiter";"quoted and space";"""quoted_and_starting_quote";"""quoted, starting quote, and space";"quoted_with_tab'+#9+'character";"quoted_multi'+LineEnding+
    'line";  UnquotedSpacesInfront;UnquotedSpacesAtTheEnd   ;  "Spaces before quoted string"';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('quoted_string');
    Expected.Add('quoted;delimiter');
    Expected.Add('quoted and space');
    Expected.Add('"quoted_and_starting_quote');
    Expected.Add('"quoted, starting quote, and space');
    Expected.Add('quoted_with_tab'+#9+'character');
    Expected.Add('quoted_multi'+LineEnding+'line');
    Expected.Add('UnquotedSpacesInfront');
    Expected.Add('UnquotedSpacesAtTheEnd');
    Expected.Add('Spaces before quoted string');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=false;
    TestSL.DelimitedText:=DelimText;
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function ReadStrictDelimTrue: string;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=true when processing the delimitedtext
//
// Mainly check if reading quotes is according to Delphi sdf specs and works.
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='normal_string;"quoted_string";"quoted;delimiter";"quoted and space";"""quoted_and_starting_quote";"""quoted, starting quote, and space";"quoted_with_tab'+#9+'character";"quoted_multi'+LineEnding+
    'line";  UnquotedSpacesInfront;UnquotedSpacesAtTheEnd   ;  "Spaces before quoted string"';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  result:='';
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('quoted_string');
    Expected.Add('quoted;delimiter');
    Expected.Add('quoted and space');
    Expected.Add('"quoted_and_starting_quote');
    Expected.Add('"quoted, starting quote, and space');
    Expected.Add('quoted_with_tab'+#9+'character');
    Expected.Add('quoted_multi'+LineEnding+'line');
    Expected.Add('  UnquotedSpacesInfront');
    Expected.Add('UnquotedSpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=true;
    TestSL.DelimitedText:=DelimText;
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function ReadStrictDelimFalseCornerCases: String;

// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=false (default) when processing the delimitedtext
//
// Has some corner cases that Delphi produces but are not evident from their
// documentation
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='"Spaces after quoted string"   ;';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  result:='';
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('Spaces after quoted string');
    Expected.Add('');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=false;
    TestSL.DelimitedText:=DelimText;
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function ReadStrictDelimTrueCornerCases: string;
// Test if input works with Delphi-compatible sdf output
// Strictdelimiter:=true when processing the delimitedtext
//
// Has some corner cases that Delphi produces but are not evident from their
// documentation
// Based on del4.zip in bug 19610
const
  // Matches del4.zip in bug 19610:
  DelimText='"Spaces after quoted string"   ;';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  Result:='';
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    // With delimiter true, we get 2 extra empty lines, also some spaces
    Expected.Add('Spaces after quoted string');
    Expected.Add('   ');
    Expected.Add('');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=true;
    TestSL.DelimitedText:=DelimText;
    //Test:
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function ReadStrictDelimTrueSafeQuote:string;
// Test if input works with sdf output that has always been quoted
// Delphi accepts this input even though it does not write it by default
// This is a more unambiguous format than unquoted
// Strictdelimiter:=true when processing the delimitedtext
//
const
  DelimText='"normal_string";"""quoted_string""";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"string_with_tab'+#9+'character";"multi'+LineEnding+
    'line";"  SpacesInfront";"SpacesAtTheEnd   ";"  ""Spaces before quoted string"""';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  result:='';
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('"quoted_string"');
    Expected.Add('"quoted;delimiter"');
    Expected.Add('"quoted and space"');
    Expected.Add('"starting_quote');
    Expected.Add('string_with_tab'+#9+'character');
    Expected.Add('multi'+LineEnding+
      'line');
    Expected.Add('  SpacesInfront');
    Expected.Add('SpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=true;
    TestSL.DelimitedText:=DelimText;
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function ReadStrictDelimFalseSafeQuote: string;
// Test if input works with sdf output that has always been quoted
// Delphi accepts this input even though it does not write it by default
// This is a more unambiguous format than unquoted
// Strictdelimiter:=false when processing the delimitedtext
//
const
  DelimText='"normal_string";"""quoted_string""";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"string_with_tab'+#9+'character";"multi'+LineEnding+
    'line";"  SpacesInfront";"SpacesAtTheEnd   ";"  ""Spaces before quoted string"""';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  Result:='';
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('"quoted_string"');
    Expected.Add('"quoted;delimiter"');
    Expected.Add('"quoted and space"');
    Expected.Add('"starting_quote');
    Expected.Add('string_with_tab'+#9+'character');
    Expected.Add('multi'+LineEnding+'line');
    Expected.Add('  SpacesInfront');
    Expected.Add('SpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';'; //Match example in bug 19610, del4.zip
    TestSL.StrictDelimiter:=false;
    TestSL.DelimitedText:=DelimText;
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

function ReadCommatext: string;

// Test if input works with Delphi-compatible commatext
const
  CommaText='normal_string,"quoted_string","quoted,delimiter","quoted and space","""quoted_and_starting_quote","""quoted, starting quote, and space","quoted_with_tab'+#9+'character","quoted_multi'+LineEnding+
    'line","  UnquotedSpacesInfront","UnquotedSpacesAtTheEnd   ","  ""Spaces before quoted string"""';

var
  TestSL: TStringList;
  Expected: TStringList;
begin
  result:='';
  //Expected values:
  Expected:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    Expected.Add('normal_string');
    Expected.Add('quoted_string');
    Expected.Add('quoted,delimiter');
    Expected.Add('quoted and space');
    Expected.Add('"quoted_and_starting_quote');
    Expected.Add('"quoted, starting quote, and space');
    Expected.Add('quoted_with_tab'+#9+'character');
    Expected.Add('quoted_multi'+LineEnding+
      'line');
    Expected.Add('  UnquotedSpacesInfront');
    Expected.Add('UnquotedSpacesAtTheEnd   ');
    Expected.Add('  "Spaces before quoted string"');
    TestSL.CommaText:=CommaText;
    //Test:
    Result:=CompareStringLists(Expected,TestSL);
  finally
    Expected.Free;
    TestSL.Free;
  end;
end;

Function CheckDelimited(TestSL : Tstrings; const Expected, ExpectedSafeQuote : string) : String;

begin
  if (TestSL.DelimitedText<>Expected) and (TestSL.DelimitedText<>ExpectedSafeQuote) then
    Exit('result: *'+TestSL.DelimitedText+'* while expected was: *'+Expected+'* - or, with safe quote output: *'+ExpectedSafeQuote+'*');
end;

function WriteStrictDelimFalse:string;

// Test if conversion stringlist=>delimitedtext gives the right data
// (right in this case: what Delphi outputs)
// Strictdelimiter:=false when processing the delimitedtext
const
  Expected='normal_string;"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";"with_tab'+#9+'character";"multi'+LineEnding+
    'line";"   UnquotedSpacesInfront";"UnquotedSpacesAtTheEnd  ";"  ""Spaces before quoted string"""';
  //If we choose to output the "safely quoted" version, we need to test for it:
  //Though this version is not the same output as Delphi, it leads to the
  //same input if imported again (see ReadStrictDelimFalseSafeQuote for corresponding tests)
  ExpectedSafeQuote='"normal_string";"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";"with_tab'+#9+'character";"multi'+LineEnding+
    'line";"   UnquotedSpacesInfront";"UnquotedSpacesAtTheEnd  ";"  ""Spaces before quoted string"""';
var
  TestSL: TStringList;
begin
  Result:='';
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('just;delimiter');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('single"quote');
    TestSL.Add('""quoted starting quote and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=false;
    Result:=CheckDelimited(TestSL,Expected,ExpectedSafeQuote);
  finally
    TestSL.Free;
  end;
end;



function WriteStrictDelimTrue:String;
// Test if conversion stringlist=>delimitedtext gives the right data
// (right in this case: what Delphi outputs)
// Strictdelimiter:=true when processing the delimitedtext
const
  Expected='normal_string;"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";with_tab'+#9+'character;multi'+LineEnding+
    'line;   UnquotedSpacesInfront;UnquotedSpacesAtTheEnd  ;"  ""Spaces before quoted string"""';
  //If we choose to output the "safely quoted" version, we need to test for it:
  //Though this version is not the same output as Delphi, it leads to the
  //same input if imported again (see ReadStrictDelimTrueSafeQuote for corresponding tests)
  ExpectedSafeQuote='"normal_string";"""quoted_string""";"just;delimiter";"""quoted;delimiter""";"""quoted and space""";"""starting_quote";"single""quote";"""""quoted starting quote and space""";"with_tab'+#9+'character";"multi'+LineEnding+
    'line";"   UnquotedSpacesInfront";"UnquotedSpacesAtTheEnd  ";"  ""Spaces before quoted string"""';

var
  TestSL: TStringList;
begin
  result:='';
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('just;delimiter');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('single"quote');
    TestSL.Add('""quoted starting quote and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=true;
    Result:=CheckDelimited(TestSL,Expected,ExpectedSafeQuote);
  finally
    TestSL.Free;
  end;
end;

function ReadWriteStrictDelimFalse:String;
// Test if conversion stringlist=>delimitedtext=>stringlist gives identical data
// Strictdelimiter:=false (default) when processing the delimitedtext

var
  TestSL: TStringList;
  ResultSL: TStringList;
begin
  result:='';
  ResultSL:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('""quoted, starting quote, and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=false;
    ResultSL.Delimiter:=';';
    ResultSL.StrictDelimiter:=false;
    ResultSL.DelimitedText:=TestSL.DelimitedText;
    Result:=CompareStringLists(ResultSL,TestSL);
  finally
    ResultSL.Free;
    TestSL.Free;
  end;
end;

function ReadWriteStrictDelimTrue:String;
// Test if conversion stringlist=>delimitedtext=>stringlist gives identical data
// Strictdelimiter:=true when processing the delimitedtext

var
  TestSL: TStringList;
  ResultSL: TStringList;
begin
  result:='';
  ResultSL:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('normal_string');
    TestSL.Add('"quoted_string"');
    TestSL.Add('"quoted;delimiter"');
    TestSL.Add('"quoted and space"');
    TestSL.Add('"starting_quote');
    TestSL.Add('""quoted, starting quote, and space"');
    TestSL.Add('with_tab'+#9+'character');
    TestSL.Add('multi'+LineEnding+
      'line');
    TestSL.Add('   UnquotedSpacesInfront');
    TestSL.Add('UnquotedSpacesAtTheEnd  ');
    TestSL.Add('  "Spaces before quoted string"');

    TestSL.Delimiter:=';';
    TestSL.StrictDelimiter:=false;
    ResultSL.Delimiter:=';';
    ResultSL.StrictDelimiter:=true;
    ResultSL.DelimitedText:=TestSL.DelimitedText;
    //Test:
    Result:=CompareStringLists(ResultSL,TestSL);
  finally
    ResultSL.Free;
    TestSL.Free;
  end;
end;

Function AddStrictDelimFalse : string;

var
  TestSL: TStringList;
  ResultSL: TStringList;

begin
  result:='';
  ResultSL:=TStringList.Create;
  TestSL:=TStringList.Create;
  try
    TestSL.Add('a');
    TestSL.Add('b');
    TestSL.Add('c');
    TestSL.StrictDelimiter:=false;
    TestSL.AddDelimitedtext('"quoted and space"');
    ResultSL.Add('a');
    ResultSL.Add('b');
    ResultSL.Add('c');
    ResultSL.Add('quoted and space');
    Result:=CompareStringLists(ResultSL,TestSL);
  finally
    ResultSL.Free;
    TestSL.Free;
  end;
end;


Procedure RegisterTests;

Var
  P : Psuite;
begin
  P:=EnsureSuite('Classes');
  AddTest('Testvclcomobject',@Testvclcomobject,P);
  AddTest('Testtstringlistexchange',@Testtstringlistexchange,P);
  AddTest('TestStoredfalse',@TestStoredfalse,P);
  AddTest('TestBytesStream',@TestBytesStream,P);
  AddTest('TestLineBreak',@TestLineBreak,P);
  AddTest('TestAlwaysQuote',@TestAlwaysQuote,P);
  AddTest('TestGetNameValue',@TestGetNameValue,P);
  AddTest('SortContext',@TestSortContext,P);
  AddTest('TestStringlistReverse1',@TestStringListReverse1,P);
  AddTest('TestStringlistReverse2',@TestStringListReverse2,P);
  AddTest('TestStringsIndexOfStartAt',@TestStringsIndexOfStartAt,P);
  AddTest('TestStringsLastIndexOfStartAt',@TestStringsLastIndexOfStartAt,P);
  AddTest('TestSlice',@TestSlice,P);
  AddTest('TestSlice2',@TestSlice2,P);
  AddTest('TestFill',@TestFill,P);
  AddTest('TestFill2',@TestFill2,P);
  AddTest('TestFilter',@TestFilter,P);
  AddTest('TestFilter2',@TestFilter,P);
  AddTest('TestMap',@TestMap,P);
  AddTest('TestMap2',@TestMap2,P);
  AddTest('TestReduce',@TestReduce,P);
  AddTest('TestPop',@TestPop,P);
  AddTest('TestShift',@TestShift,P);
  AddTest('TestForeach',@TestForeach,P);
  AddTest('TestForeachEx',@TestForeachEx,P);
  AddTest('ReadStrictDelimFalse',@ReadStrictDelimFalse,P);
  AddTest('ReadStrictDelimTrue',@ReadStrictDelimTrue,P);
  AddTest('ReadStrictDelimFalseCornerCases',@ReadStrictDelimFalseCornerCases,P);
  AddTest('ReadStrictDelimTrueCornerCases',@ReadStrictDelimTrueCornerCases,P);
  AddTest('ReadStrictDelimTrueSafeQuote',@ReadStrictDelimTrueSafeQuote,P);
  AddTest('ReadStrictDelimFalseSafeQuote',@ReadStrictDelimFalseSafeQuote,P);
  AddTest('ReadCommaText',@ReadCommaText,P);
  AddTest('WriteStrictDelimFalse',@WriteStrictDelimFalse,P);
  AddTest('WriteStrictDelimTrue',@WriteStrictDelimTrue,P);
  AddTest('ReadWriteStrictDelimFalse',@ReadWriteStrictDelimFalse,P);
  AddTest('ReadWriteStrictDelimTrue',@ReadWriteStrictDelimTrue,P);
  AddTest('AddStrictDelimFalse',@AddStrictDelimFalse,P);
end;

initialization
  RegisterTests;  
end.
