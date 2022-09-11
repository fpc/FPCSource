unit tcCSSTree;

{$mode ObjFPC}{$H+}

interface

uses
  TypInfo, Classes, SysUtils, fpcunit, testregistry, fpCSSTree;

type

  { TBaseCSSTreeTest }
  TAppendMode = (amReplace,amNone,amChild,amSelector,amKey);

  TBaseCSSTreeTest = Class(TTestCase)
  private
    FElement: TCSSElement;
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure AssertEquals(AMessage: String; AExpected, AActual: TCSSType);  overload;
    Function CreateElement(aClass : TCSSElementClass; aAppend : TAppendMode = amReplace) : TCSSElement;
    Function CreateIdentifier(const avalue : string; aAppend : TAppendMode = amNone): TCSSIdentifierElement;
    Function CreateDeclaration(const aKey,avalue : string; aAppend : TAppendMode = amNone): TCSSDeclarationElement;
    Function CreateBinaryOperation(aOp : TCSSBinaryOperation;const aLeft,aRight : string; aAppend : TAppendMode = amNone): TCSSBinaryElement;
    Function CreateUnaryOperation(aOp : TCSSUnaryOperation;const aRight : string; aAppend : TAppendMode = amNone): TCSSUnaryElement;
    Property Element : TCSSElement Read FElement Write FElement;
  end;

  { TCSSTreeTypeTest }

  TCSSTreeTypeTest = Class(TBaseCSSTreeTest)
  Published
    Procedure TestINTEGER;
    Procedure TestSTRING;
    Procedure TestFLOAT;
    Procedure TestIDENTIFIER;
    Procedure TestCLASSNAME;
    Procedure TestPSEUDOCLASS;
    Procedure TestCOMPOUND;
    Procedure TestRULE;
    Procedure TestDECLARATION;
    Procedure TestBINARYOP;
    Procedure TestCALL;
    Procedure TestUNARYOP;
    Procedure TestARRAY;
    Procedure TestURL;
    Procedure TestUNICODERANGE;
  end;

  { TCSSTreeAsStringTest }

  TCSSTreeAsStringTest = Class(TBaseCSSTreeTest)
  Published
    Procedure TestINTEGER;
    Procedure TestSTRING;
    Procedure TestFLOAT;
    Procedure TestIDENTIFIER;
    Procedure TestCLASSNAME;
    Procedure TestPSEUDOCLASS;
    Procedure TestCOMPOUND;
    Procedure TestRULE;
    Procedure TestRULE2Declarations;
    Procedure TestRULESelector;
    Procedure TestRULE2Selectors;
    Procedure TestRULE2SelectorCombined;
    Procedure TestAtRULE;
    Procedure TestDECLARATION;
    Procedure TestDECLARATIONList;
    Procedure TestBINARYOP;
    Procedure TestCALL;
    Procedure TestUNARYOP;
    Procedure TestARRAY;
    Procedure TestURL;
    Procedure TestUNICODERANGE;
  end;

  { TEnumVisitor }

  TEnumVisitor = Class (TCSSTreeVisitor)
  Private
    FList: TFPList;
  Public
    Constructor Create(aList : TFPList);
    procedure Visit(obj: TCSSElement); override;
  end;

  { TCSSTreeVisitorTest }

  TCSSTreeVisitorTest = Class(TBaseCSSTreeTest)
  Private
    FList: TFPList;
    FVisitor: TCSSTreeVisitor;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure CheckElement(aIndex : Integer; aElement : TCSSElement);
    Procedure CheckCount(aCount : Integer);
    Property List : TFPList Read FList;
    Property Visitor : TCSSTreeVisitor Read FVisitor;
  Published
    Procedure TestElement;
    Procedure TestINTEGER;
    Procedure TestSTRING;
    Procedure TestFLOAT;
    Procedure TestIDENTIFIER;
    Procedure TestCLASSNAME;
    Procedure TestPSEUDOCLASS;
    Procedure TestCOMPOUND;
    Procedure TestRULE;
    Procedure TestRULE2Declarations;
    Procedure TestRULESelector;
    Procedure TestRULE2Selectors;
    Procedure TestRULE2SelectorCombined;
    Procedure TestAtRULE;
    Procedure TestDECLARATION;
    Procedure TestDECLARATIONList;
    Procedure TestBINARYOP;
    Procedure TestCALL;
    Procedure TestUNARYOP;
    Procedure TestARRAY;
    Procedure TestURL;
    Procedure TestUNICODERANGE;
  end;

  { TCSSTreeOtherTest }

  TCSSTreeOtherTest = Class(TBaseCSSTreeTest)
  Published
    Procedure TestStringToIdentifier;
  end;

implementation

{ TCSSTreeOtherTest }

procedure TCSSTreeOtherTest.TestStringToIdentifier;
begin
  AssertEquals('Normal','abc',StringToIdentifier('abc'));
  AssertEquals('dash','-abc',StringToIdentifier('-abc'));
  AssertEquals('dashdash','--abc',StringToIdentifier('--abc'));
  AssertEquals('Underscore','abc_d',StringToIdentifier('abc_d'));
  AssertEquals('Numerical','abc_1',StringToIdentifier('abc_1'));
  AssertEquals('Weird','abc\(1\)',StringToIdentifier('abc(1)'));
end;

{ TCSSTreeVisitorTest }

procedure TCSSTreeVisitorTest.Setup;
begin
  inherited Setup;
  FList:=TFPList.Create;
  FVisitor:=TEnumVisitor.Create(FList);
end;

procedure TCSSTreeVisitorTest.TearDown;
begin
  FreeAndNil(FVisitor);
  FreeAndNil(FList);
  inherited TearDown;
end;

procedure TCSSTreeVisitorTest.CheckElement(aIndex: Integer; aElement: TCSSElement);
begin
  AssertTrue(Format('Index in range: %d in [0..%d[',[aIndex,FList.Count]),(aIndex>=0) and (aIndex<FList.Count));
  AssertSame(Format('Element %d is correct',[aIndex]),aElement,TObject(FList[aindex]));
end;

procedure TCSSTreeVisitorTest.CheckCount(aCount: Integer);
begin
  AssertEquals('Count is correct',aCount,FList.Count);
end;

procedure TCSSTreeVisitorTest.TestElement;


begin
  CreateElement(TCSSElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestINTEGER;
begin
  CreateElement(TCSSIntegerElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestSTRING;
begin
  CreateElement(TCSSStringElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestFLOAT;
begin
  CreateElement(TCSSFloatElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestIDENTIFIER;
begin
  CreateElement(TCSSIdentifierElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestCLASSNAME;
begin
  CreateElement(TCSSClassNameElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestPSEUDOCLASS;
begin
  CreateElement(TCSSPseudoClassElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestCOMPOUND;
begin
  CreateElement(TCSSCompoundElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestRULE;
begin
  CreateElement(TCSSRuleElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestRULE2Declarations;
begin

end;

procedure TCSSTreeVisitorTest.TestRULESelector;
begin

end;

procedure TCSSTreeVisitorTest.TestRULE2Selectors;
begin

end;

procedure TCSSTreeVisitorTest.TestRULE2SelectorCombined;
begin

end;

procedure TCSSTreeVisitorTest.TestAtRULE;
begin
  CreateElement(TCSSAtRuleElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestDECLARATION;

Var
  Decl: TCSSDeclarationElement;

begin
  Decl:=CreateDeclaration('a','b',amReplace);
  Element.Iterate(Visitor);
  CheckCount(3);
  CheckElement(0,Element);
  CheckElement(1,Decl.Keys[0]);
  CheckElement(2,Decl.Children[0]);
end;

procedure TCSSTreeVisitorTest.TestDECLARATIONList;
Var
  Decl: TCSSDeclarationElement;

begin
  Decl:=CreateDeclaration('a','b',amReplace);
  CreateIdentifier('c',amChild);
  Element.Iterate(Visitor);
  CheckCount(4);
  CheckElement(0,Element);
  CheckElement(1,Decl.Keys[0]);
  CheckElement(2,Decl.Children[0]);
  CheckElement(3,Decl.Children[1]);
end;

procedure TCSSTreeVisitorTest.TestBINARYOP;

Var
  Bin : TCSSBinaryElement;

begin
  Bin:=CreateBinaryOperation(boAnd,'a','b',amReplace);
  Element.Iterate(Visitor);
  CheckCount(3);
  CheckElement(0,Element);
  CheckElement(1,Bin.Right);
  CheckElement(2,Bin.Left);
end;

procedure TCSSTreeVisitorTest.TestCALL;

Var
  aEl : TCSSElement;

begin
  CreateElement(TCSSCallElement);
  aEl:=CreateIdentifier('a',amChild);
  Element.Iterate(Visitor);
  CheckCount(2);
  CheckElement(0,Element);
  CheckElement(1,aEl);
end;

procedure TCSSTreeVisitorTest.TestUNARYOP;

begin
  CreateUnaryOperation(uoDoubleColon,'a',amReplace);
  Element.Iterate(Visitor);
  CheckCount(2);
  CheckElement(0,Element);
  CheckElement(1,TCSSUnaryElement(Element).Right);
end;

procedure TCSSTreeVisitorTest.TestARRAY;
begin
  CreateElement(TCSSArrayElement);
  CreateIdentifier('a',amChild);
  CreateIdentifier('b',amChild);
  Element.Iterate(Visitor);
  CheckCount(3);
  CheckElement(0,Element);
  CheckElement(1,TCSSArrayElement(Element).Children[0]);
  CheckElement(2,TCSSArrayElement(Element).Children[1]);
end;

procedure TCSSTreeVisitorTest.TestURL;
begin
  CreateElement(TCSSURLElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

procedure TCSSTreeVisitorTest.TestUNICODERANGE;
begin
  CreateElement(TCSSUnicodeRangeElement);
  Element.Iterate(Visitor);
  CheckCount(1);
  CheckElement(0,Element);
end;

{ TEnumVisitor }

constructor TEnumVisitor.Create(aList: TFPList);
begin
  FList:=AList;
end;

procedure TEnumVisitor.Visit(obj: TCSSElement);
begin
  FList.Add(obj);
end;

{ TCSSTreeAsStringTest }

procedure TCSSTreeAsStringTest.TestINTEGER;
begin
  TCSSIntegerElement(CreateElement(TCSSIntegerElement)).Value:=123;
  AssertEquals('Value','123',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestSTRING;
begin
  TCSSStringElement(CreateElement(TCSSStringElement)).Value:='abc';
  AssertEquals('Value','"abc"',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestFLOAT;
begin
  TCSSFloatElement(CreateElement(TCSSFloatElement)).Value:=1.23;
  AssertEquals('Value','1.23',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestIDENTIFIER;
begin
  TCSSIdentifierElement(CreateElement(TCSSIdentifierElement)).Value:='abc';
  AssertEquals('Value','abc',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestCLASSNAME;
begin
  TCSSClassNameElement(CreateElement(TCSSClassNameElement)).Value:='.abc';
  AssertEquals('Value','.abc',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestPSEUDOCLASS;
begin
  TCSSPseudoClassElement(CreateElement(TCSSPseudoClassElement)).Value:=':abc';
  AssertEquals('Value',':abc',Element.AsString);
  TCSSPseudoClassElement(CreateElement(TCSSPseudoClassElement)).Value:='::abc';
  AssertEquals('Value','::abc',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestCOMPOUND;

Var
  aRule : TCSSRuleElement;

begin
  CreateElement(TCSSCompoundElement);
  aRule:=TCSSRuleElement(CreateElement(TCSSRuleElement,amChild));
  aRule.AddChild(CreateDeclaration('a','b',amNone));
  aRule:=TCSSRuleElement(CreateElement(TCSSRuleElement,amChild));
  aRule.AddChild(CreateDeclaration('c','d',amNone));
  aRule.AddSelector(CreateIdentifier('p',amNone));
  AssertEquals('Value','{ a : b; }p { c : d; }',Element.AsString);
  AssertEquals('Value','{'+sLineBreak+'  a : b;'+sLineBreak+'}'+sLineBReak+'p {'+sLineBReak+'  c : d;'+sLineBreak+'}',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestRULE;

begin
  CreateElement(TCSSRuleElement);
  CreateDeclaration('a','b',amChild);
  AssertEquals('Value','{ a : b; }',Element.AsString);
  AssertEquals('Value','{'+sLineBreak+'  a : b;'+sLineBreak+'}',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestRULE2Declarations;
begin
  CreateElement(TCSSRuleElement);
  CreateDeclaration('a','b',amChild);
  CreateDeclaration('c','d',amChild);
  AssertEquals('Value','{ a : b; c : d; }',Element.AsString);
  AssertEquals('Value','{'+sLineBreak+'  a : b;'+sLineBreak+'  c : d;'+sLineBreak+'}',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestRULESelector;
begin
  CreateElement(TCSSRuleElement);
  CreateDeclaration('a','b',amChild);
  CreateIdentifier('c',amSelector);
  AssertEquals('Value','c { a : b; }',Element.AsString);
  AssertEquals('Value','c {'+sLineBreak+'  a : b;'+sLineBreak+'}',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestRULE2Selectors;
begin
  CreateElement(TCSSRuleElement);
  CreateDeclaration('a','b',amChild);
  CreateIdentifier('c',amSelector);
  CreateIdentifier('d',amSelector);
  AssertEquals('Value','c, d { a : b; }',Element.AsString);
  AssertEquals('Value','c,'+sLineBreak+'d {'+sLineBreak+'  a : b;'+sLineBreak+'}',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestRULE2SelectorCombined;
Var
  aList : TCSSListElement;
  aIdent : TCSSIdentifierElement;

begin
  CreateElement(TCSSRuleElement);
  CreateDeclaration('a','b',amChild);
  aList:=TCSSListElement(CreateElement(TCSSListElement,amSelector));
  aIdent:=CreateIdentifier('c',amNone);
  aList.AddChild(aIdent);
  aIdent:=CreateIdentifier('d',amNone);
  aList.AddChild(aIdent);
  aIdent:=CreateIdentifier('e',amSelector);
  AssertEquals('Value','c d, e { a : b; }',Element.AsString);
  AssertEquals('Value','c d,'+sLineBreak+'e {'+sLineBreak+'  a : b;'+sLineBreak+'}',Element.AsFormattedString);

end;

procedure TCSSTreeAsStringTest.TestAtRULE;
Var
  aATRule : TCSSAtRuleElement;
  aURL : TCSSURLElement;

begin
  aATRule:=TCSSAtRuleElement(CreateElement(TCSSAtRuleElement));
  aATRule.atKeyWord:='@import';
  aURL:=TCSSURLElement(CreateElement(TCSSURLElement,amSelector));
  aURL.Value:='url("me.css")';
  AssertEquals('Value','@import url("me.css");',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestDECLARATION;
begin
  CreateDeclaration('a','b',amReplace);
  AssertEquals('Value','a : b',Element.AsString)
end;

procedure TCSSTreeAsStringTest.TestDECLARATIONList;

Var
  aList: TCSSListElement;

begin
  CreateElement(TCSSDeclarationElement);
  CreateIdentifier('a',amKey);
  aList:=TCSSListElement(CreateElement(TCSSListElement,amChild));
  aList.AddChild(CreateIdentifier('b',amNone));
  aList.AddChild(CreateIdentifier('c',amNone));
  CreateIdentifier('d',amChild);
  AssertEquals('Value','a : b c, d',Element.AsString)
end;

procedure TCSSTreeAsStringTest.TestBINARYOP;

Var
  Op : TCSSBinaryOperation;
  Sop : String;

begin
  For Op in TCSSBinaryOperation do
    begin
    CreateBinaryOperation(Op,'a','b',amReplace);
    Sop:=BinaryOperators[Op];
    if Not (Op in [boColon,boDoubleColon]) then
      Sop:=' '+Sop+' ';
    AssertEquals('Value '+Sop,'a'+sop+'b',Element.AsString)
    end;
end;


procedure TCSSTreeAsStringTest.TestCALL;

Var
  aCall : TCSSCallElement;

begin
  aCall:=TCSSCallElement(CreateElement(TCSSCallElement));
  aCall.Name:='me';
  CreateIdentifier('a',amChild);
  AssertEquals('Value','me(a)',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestUNARYOP;

Const
  MyUnaryOperators : Array[TCSSUnaryOperation] of string =
      ('::','-','+','/');
Var
  Op : TCSSUnaryOperation;
  Sop : String;

begin
  For Op in TCSSUnaryOperation do
    begin
    CreateUnaryOperation(op,'a',amReplace);
    Sop:=MyUnaryOperators[Op];
    if Not (Op in [uoDoubleColon]) then
      Sop:=Sop+' ';
    AssertEquals('Value '+Sop,sop+'a',Element.AsString)
    end;

end;

procedure TCSSTreeAsStringTest.TestARRAY;

begin
  CreateElement(TCSSArrayElement);
  CreateIdentifier('a',amChild);
  CreateIdentifier('b',amChild);
  AssertEquals('Value','[a b]',Element.AsString);
  AssertEquals('Value','[a b]',Element.AsFormattedString);
end;

procedure TCSSTreeAsStringTest.TestURL;

Var
  Url : TCSSURLElement;

begin
  Url:=TCSSURLElement(CreateElement(TCSSURLElement));
  Url.Value:='url("a.png")';
  AssertEquals('Value','url("a.png")',Element.AsString);
end;

procedure TCSSTreeAsStringTest.TestUNICODERANGE;
Var
  Url : TCSSUnicodeRangeElement;

begin
  Url:=TCSSUnicodeRangeElement(CreateElement(TCSSUnicodeRangeElement));
  Url.Value:='U+3588-488';
  AssertEquals('Value','U+3588-488',Element.AsString);
end;

{ TCSSTreeTypeTest }

procedure TCSSTreeTypeTest.TestINTEGER;
begin
  AssertEquals('Type',csstINTEGER,CreateElement(TCSSIntegerElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestSTRING;
begin
  AssertEquals('Type',csstString,CreateElement(TCSSStringElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestFLOAT;
begin
  AssertEquals('Type',csstFloat,CreateElement(TCSSFloatElement).CSSType);
end;


procedure TCSSTreeTypeTest.TestIDENTIFIER;
begin
  AssertEquals('Type',csstINTEGER,CreateElement(TCSSIntegerElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestCLASSNAME;
begin
  AssertEquals('Type',csstClassName,CreateElement(TCSSClassNameElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestPSEUDOCLASS;
begin
  AssertEquals('Type',csstPseudoClass,CreateElement(TCSSPseudoClassElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestCOMPOUND;
begin
  AssertEquals('Type',csstCompound,CreateElement(TCSSCompoundElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestRULE;
begin
  AssertEquals('Type',csstRule,CreateElement(TCSSRuleElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestDECLARATION;
begin
  AssertEquals('Type',csstDeclaration,CreateElement(TCSSDeclarationElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestBINARYOP;
begin
  AssertEquals('Type',csstBinaryOp,CreateElement(TCSSBinaryElement).CSSType);
end;


procedure TCSSTreeTypeTest.TestCALL;
begin
  AssertEquals('Type',csstCall,CreateElement(TCSSCallElement).CSSType);
end;


procedure TCSSTreeTypeTest.TestUNARYOP;
begin
  AssertEquals('Type',csstUnaryOp,CreateElement(TCSSUnaryElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestARRAY;
begin
  AssertEquals('Type',csstArray,CreateElement(TCSSArrayElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestURL;
begin
  AssertEquals('Type',csstURL,CreateElement(TCSSURLElement).CSSType);
end;

procedure TCSSTreeTypeTest.TestUNICODERANGE;
begin
  AssertEquals('Type',csstUnicodeRange,CreateElement(TCSSUnicodeRangeElement).CSSType);
end;

{ TBaseCSSTreeTest }

procedure TBaseCSSTreeTest.SetUp;
begin
  inherited SetUp;
  FreeAndNil(Felement);
end;

procedure TBaseCSSTreeTest.TearDown;
begin
  FreeAndNil(Felement);
  inherited TearDown;
end;

procedure TBaseCSSTreeTest.AssertEquals(AMessage : String; AExpected, AActual: TCSSType);

Var
  S,EN1,EN2 : String;

begin
  If (AActual<>AExpected) then
    begin
    EN1:=GetEnumName(TypeINfo(TCSSType),Ord(AExpected));
    EN2:=GetEnumName(TypeINfo(TCSSType),Ord(AActual));
    S:=Format('%s : %s <> %s',[AMessage,EN1,EN2]);
    Fail(S);
    end;
end;

function TBaseCSSTreeTest.CreateElement(aClass: TCSSElementClass; aAppend : TAppendMode = amReplace): TCSSElement;
begin
  Result:=aClass.Create(TestName+'.css',1,1);
  Case aAppend of
    amNone : ;
    amReplace :
      begin
      FreeAndNil(FElement);
      FElement:=Result;
      end;
    amChild:
      begin
      if FElement is TCSSChildrenElement then
        TCSSChildrenElement(FElement).AddChild(Result);
      end;
    amSelector:
      begin
      if FElement is TCSSRuleElement then
        TCSSRuleElement(FElement).AddSelector(Result);
      end;
    amKey:
      begin
      if FElement is TCSSDeclarationElement then
        TCSSDeclarationElement(FElement).AddKey(Result);
      end;
  end;
end;

function TBaseCSSTreeTest.CreateIdentifier(const avalue: string;
  aAppend: TAppendMode): TCSSIdentifierElement;
begin
  Result:=TCSSIdentifierElement(CreateElement(TCSSIdentifierElement,aAppend));
  Result.Value:=aValue;
end;

function TBaseCSSTreeTest.CreateDeclaration(const aKey, avalue: string; aAppend : TAppendMode = amNone): TCSSDeclarationElement;

var
  aIdent : TCSSIdentifierElement;

begin
  Result:=TCSSDeclarationElement(CreateElement(TCSSDeclarationElement,aAppend));
  aIdent:=CreateIdentifier(aKey,amNone);
  Result.AddKey(aIdent);
  aIdent:=CreateIdentifier(aValue,amNone);
  Result.AddChild(aIdent);
end;

function TBaseCSSTreeTest.CreateBinaryOperation(aOp : TCSSBinaryOperation; const aLeft, aRight: string;
  aAppend: TAppendMode): TCSSBinaryElement;

var
  aIdent : TCSSIdentifierElement;

begin
  Result:=TCSSBinaryElement(CreateElement(TCSSBinaryElement,aAppend));
  Result.Operation:=aOp;
  aIdent:=CreateIdentifier(aLeft,amNone);
  Result.Left:=aIdent;
  aIdent:=CreateIdentifier(aRight,amNone);
  Result.Right:=aIdent;
end;

function TBaseCSSTreeTest.CreateUnaryOperation(aOp: TCSSUnaryOperation;
  const aRight: string; aAppend: TAppendMode): TCSSUnaryElement;

var
  aIdent : TCSSIdentifierElement;

begin
  Result:=TCSSUnaryElement(CreateElement(TCSSUnaryElement,aAppend));
  Result.Operation:=aOp;
  aIdent:=CreateIdentifier(aRight,amNone);
  Result.Right:=aIdent;
end;

initialization
  RegisterTests([TCSSTreeTypeTest,TCSSTreeAsStringTest,TCSSTreeVisitorTest,TCSSTreeOtherTest]);
end.

