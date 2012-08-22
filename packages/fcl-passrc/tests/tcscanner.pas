unit tcscanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fpcunit, testregistry, pscanner;

type

  { TTestTokenFinder }

  TTestTokenFinder = class(TTestCase)
  Published
    Procedure TestFind;
  end;

  { TTestStreamLineReader }


  TTestStreamLineReader = class(TTestCase)
  Private
    FReader: TStreamLineReader;
  Protected
    procedure NewSource(Const Source : string);
    Procedure TestLine(Const ALine : String; ExpectEOF : Boolean = True);
    procedure TearDown; override;
  Published
    Procedure TestCreate;
    Procedure TestEOF;
    Procedure TestEmptyLine;
    Procedure TestEmptyLineCR;
    Procedure TestEmptyLineLF;
    Procedure TestEmptyLineCRLF;
    Procedure TestEmptyLineLFCR;
    Procedure TestOneLine;
    Procedure TestTwoLines;
  end;

  { TTestingPascalScanner }

  TTestingPascalScanner = Class(TPascalScanner)
  private
    FDoSpecial: Boolean;
  protected
    function HandleMacro(AIndex: integer): TToken;override;
  Public
    Property DoSpecial : Boolean Read FDoSpecial Write FDoSpecial;
  end;

  { TTestScanner }
  TTestScanner= class(TTestCase)
  Private
    FLI: String;
    FScanner : TPascalScanner;
    FResolver : TStreamResolver;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Function TokenToString(tk : TToken) : string;
    Procedure AssertEquals(Msg : String; Expected,Actual : TToken); overload;
    procedure NewSource(Const Source : string; DoClear : Boolean = True);
    Procedure DoTestToken(t : TToken; Const ASource : String; Const CheckEOF : Boolean = True);
    Procedure TestToken(t : TToken; Const ASource : String; Const CheckEOF : Boolean = True);
    Procedure TestTokens(t : array of TToken; Const ASource : String; Const CheckEOF : Boolean = True;Const DoClear : Boolean = True);
    Property LastIDentifier : String Read FLI Write FLi;
  published
    procedure TestEOF;
    procedure TestWhitespace;
    procedure TestComment1;
    procedure TestComment2;
    procedure TestComment3;
    procedure TestNestedComment1;
    procedure TestNestedComment2;
    procedure TestNestedComment3;
    procedure TestNestedComment4;
    procedure TestIdentifier;
    procedure TestString;
    procedure TestNumber;
    procedure TestChar;
    procedure TestBraceOpen;
    procedure TestBraceClose;
    procedure TestMul;
    procedure TestPlus;
    procedure TestComma;
    procedure TestMinus;
    procedure TestDot;
    procedure TestDivision;
    procedure TestColon;
    procedure TestSemicolon;
    procedure TestLessThan;
    procedure TestEqual;
    procedure TestGreaterThan;
    procedure TestAt;
    procedure TestSquaredBraceOpen;
    procedure TestSquaredBraceClose;
    procedure TestCaret;
    procedure TestBackslash;
    procedure TestDotDot;
    procedure TestAssign;
    procedure TestAssignPlus;
    procedure TestAssignMinus;
    procedure TestAssignMul;
    procedure TestAssignDivision;
    procedure TestNotEqual;
    procedure TestLessEqualThan;
    procedure TestGreaterEqualThan;
    procedure TestPower;
    procedure TestSymmetricalDifference;
    procedure TestAbsolute;
    procedure TestAnd;
    procedure TestArray;
    procedure TestAs;
    procedure TestAsm;
    procedure TestBegin;
    procedure TestBitpacked;
    procedure TestCase;
    procedure TestClass;
    procedure TestConst;
    procedure TestConstructor;
    procedure TestDestructor;
    procedure TestDiv;
    procedure TestDo;
    procedure TestDownto;
    procedure TestElse;
    procedure TestEnd;
    procedure TestExcept;
    procedure TestExports;
    procedure TestFalse;
    procedure TestFile;
    procedure TestFinalization;
    procedure TestFinally;
    procedure TestFor;
    procedure TestFunction;
    procedure TestGeneric;
    procedure TestGoto;
    Procedure TestHelper;
    procedure TestIf;
    procedure TestImplementation;
    procedure TestIn;
    procedure TestInherited;
    procedure TestInitialization;
    procedure TestInline;
    procedure TestInterface;
    procedure TestIs;
    procedure TestLabel;
    procedure TestLibrary;
    procedure TestMod;
    procedure TestNil;
    procedure TestNot;
    procedure TestObject;
    procedure TestOf;
    procedure TestOn;
    procedure TestOperator;
    procedure TestOr;
    procedure TestPacked;
    procedure TestProcedure;
    procedure TestProgram;
    procedure TestProperty;
    procedure TestRaise;
    procedure TestRecord;
    procedure TestRepeat;
    procedure TestResourceString;
    procedure TestSelf;
    procedure TestSet;
    procedure TestShl;
    procedure TestShr;
    procedure TestSpecialize;
    procedure TestThen;
    procedure TestThreadvar;
    procedure TestTo;
    procedure TestTrue;
    procedure TestTry;
    procedure TestType;
    procedure TestUnit;
    procedure TestUntil;
    procedure TestUses;
    procedure TestVar;
    procedure TestWhile;
    procedure TestWith;
    procedure TestXor;
    procedure TestLineEnding;
    procedure TestTab;
    Procedure TestTokenSeries;
    Procedure TestTokenSeriesNoWhiteSpace;
    Procedure TestTokenSeriesComments;
    Procedure TestTokenSeriesNoComments;
    Procedure TestDefine0;
    Procedure TestDefine1;
    Procedure TestDefine2;
    Procedure TestDefine3;
    Procedure TestDefine4;
    Procedure TestDefine5;
    Procedure TestDefine6;
    Procedure TestDefine7;
    Procedure TestDefine8;
    Procedure TestDefine9;
    Procedure TestDefine10;
    Procedure TestDefine11;
    Procedure TestDefine12;
    Procedure TestInclude;
    Procedure TestInclude2;
    Procedure TestUnDefine1;
    Procedure TestMacro1;
    procedure TestMacro2;
    procedure TestMacro3;
    procedure TestMacroHandling;
  end;

implementation

{ TTestingPascalScanner }

function TTestingPascalScanner.HandleMacro(AIndex: integer): TToken;
begin
  if DoSpecial then
    begin
    Result:=tkIdentifier;
    SetCurTokenstring('somethingweird');
    end
  else
    Result:=inherited HandleMacro(AIndex);
end;

{ TTestTokenFinder }

procedure TTestTokenFinder.TestFind;

Var
  tk,tkr : TToken;
  S : string;
  B : Boolean;

begin
  For tk:=tkAbsolute to tkXor do
    begin
    S:=tokenInfos[tk];
    B:=IsNamedToken(S,tkr);
    AssertEquals('Token '+S+' is a token',true,B);
    AssertEquals('Token '+S+' returns correct token',Ord(tk),Ord(tkr));
    end;
end;

{ TTestStreamLineReader }

procedure TTestStreamLineReader.NewSource(Const Source: string);
begin
  FReader:=TStringStreamLineReader.Create('afile',Source);
end;

procedure TTestStreamLineReader.TestLine(const ALine: String; ExpectEOF: Boolean);
begin
  AssertNotNull('Have reader',FReader);
  AssertEquals('Reading source line',ALine,FReader.ReadLine);
  if ExpectEOF then
    AssertEquals('End of file reached',True,FReader.IsEOF);
end;

procedure TTestStreamLineReader.TearDown;
begin
  inherited TearDown;
  If Assigned(FReader) then
    FreeAndNil(Freader);
end;

procedure TTestStreamLineReader.TestCreate;
begin
  FReader:=TStreamLineReader.Create('afile');
  AssertEquals('Correct filename','afile',FReader.FileName);
  AssertEquals('Initially empty',True,FReader.isEOF);
end;

procedure TTestStreamLineReader.TestEOF;
begin
  NewSource('');
  AssertEquals('Empty stream',True,FReader.IsEOF);
end;

procedure TTestStreamLineReader.TestEmptyLine;
begin
  NewSource('');
  TestLine('');
end;

procedure TTestStreamLineReader.TestEmptyLineCR;
begin
  NewSource(#13);
  TestLine('');
end;

procedure TTestStreamLineReader.TestEmptyLineLF;
begin
  NewSource(#10);
  TestLine('');
end;

procedure TTestStreamLineReader.TestEmptyLineCRLF;
begin
  NewSource(#13#10);
  TestLine('');
end;

procedure TTestStreamLineReader.TestEmptyLineLFCR;
begin
  NewSource(#10#13);
  TestLine('',False);
  TestLine('');
end;

procedure TTestStreamLineReader.TestOneLine;

Const
    S = 'a line with text';
begin
  NewSource(S);
  TestLine(S);
end;

procedure TTestStreamLineReader.TestTwoLines;
Const
    S = 'a line with text';
begin
  NewSource(S+sLineBreak+S);
  TestLine(S,False);
  TestLine(S);
end;

{ ---------------------------------------------------------------------
  TTestScanner
  ---------------------------------------------------------------------}

procedure TTestScanner.SetUp;
begin
  FResolver:=TStreamResolver.Create;
  FResolver.OwnsStreams:=True;
  FScanner:=TTestingPascalScanner.Create(FResolver);
  // Do nothing
end; 

procedure TTestScanner.TearDown; 
begin
  FreeAndNil(FScanner);
  FreeAndNil(FResolver);
end;

function TTestScanner.TokenToString(tk: TToken): string;
begin
  Result:=GetEnumName(TypeInfo(TToken),Ord(tk));
end;

procedure TTestScanner.AssertEquals(Msg: String; Expected, Actual: TToken);
begin
  AssertEquals(Msg,TokenToString(Expected),TokenToString(Actual));
end;

procedure TTestScanner.NewSource(const Source: string; DoClear : Boolean = True);
begin
  if DoClear then
    FResolver.Clear;
  FResolver.AddStream('afile.pp',TStringStream.Create(Source));
  FScanner.OpenFile('afile.pp');
end;

procedure TTestScanner.DoTestToken(t: TToken; const ASource: String;
  Const CheckEOF: Boolean);

Var
  tk : ttoken;

begin
  NewSource(ASource);
  tk:=FScanner.FetchToken;
  AssertEquals('Read token equals expected token.',t,tk);
  if CheckEOF then
    begin
    tk:=FScanner.FetchToken;
    if (tk=tkLineEnding) and not (t in [tkEOF,tkLineEnding]) then
      tk:=FScanner.FetchToken;
    AssertEquals('EOF reached.',tkEOF,FScanner.FetchToken);
    end;
end;

procedure TTestScanner.TestToken(t: TToken; const ASource: String; Const CheckEOF: Boolean);
Var
  S : String;
begin
  DoTestToken(t,ASource);
  if (ASource<>'') then
    begin
    S:=ASource;
    S[1]:=Upcase(S[1]);
    DoTestToken(t,S);
    end;
  DoTestToken(t,UpperCase(ASource));
  DoTestToken(t,LowerCase(ASource));
end;

procedure TTestScanner.TestTokens(t: array of TToken; const ASource: String;
  const CheckEOF: Boolean;Const DoClear : Boolean = True);
Var
  tk : ttoken;
  i : integer;

begin
  NewSource(ASource,DoClear);
  For I:=Low(t) to High(t) do
    begin
    tk:=FScanner.FetchToken;
    AssertEquals(Format('Read token %d equals expected token.',[i]),t[i],tk);
    if tk=tkIdentifier then
      LastIdentifier:=FScanner.CurtokenString;
    end;
  if CheckEOF then
    begin
    tk:=FScanner.FetchToken;
    if (tk=tkLineEnding) then
      tk:=FScanner.FetchToken;
    AssertEquals('EOF reached.',tkEOF,FScanner.FetchToken);
    end;
end;

procedure TTestScanner.TestEOF;
begin
  TestToken(tkEOF,'')
end;

procedure TTestScanner.TestWhitespace;

begin
  TestToken(tkWhitespace,' ');
  TestToken(tkWhitespace,' ');
end;


procedure TTestScanner.TestComment1;

begin
  TestToken(tkComment,'{ comment }');
end;


procedure TTestScanner.TestComment2;

begin
  TestToken(tkComment,'(* comment *)');
end;


procedure TTestScanner.TestComment3;

begin
  TestToken(tkComment,'//');
end;

procedure TTestScanner.TestNestedComment1;
begin
  TestToken(tkComment,'// { comment } ');
end;

procedure TTestScanner.TestNestedComment2;
begin
  TestToken(tkComment,'(* { comment } *)');
end;

procedure TTestScanner.TestNestedComment3;
begin
  TestToken(tkComment,'{ { comment } }');
end;

procedure TTestScanner.TestNestedComment4;
begin
  TestToken(tkComment,'{ (* comment *) }');
end;


procedure TTestScanner.TestIdentifier;

begin
  TestToken(tkIdentifier,'identifier');
end;


procedure TTestScanner.TestString;

begin
  TestToken(pscanner.tkString,'''A string''');
end;


procedure TTestScanner.TestNumber;

begin
  TestToken(tkNumber,'123');
end;


procedure TTestScanner.TestChar;

begin
  TestToken(pscanner.tkChar,'#65 ', false);
end;


procedure TTestScanner.TestBraceOpen;

begin
  TestToken(tkBraceOpen,'(');
end;


procedure TTestScanner.TestBraceClose;

begin
  TestToken(tkBraceClose,')');
end;


procedure TTestScanner.TestMul;

begin
  TestToken(tkMul,'*');
end;


procedure TTestScanner.TestPlus;

begin
  TestToken(tkPlus,'+');
end;


procedure TTestScanner.TestComma;

begin
  TestToken(tkComma,',');
end;


procedure TTestScanner.TestMinus;

begin
  TestToken(tkMinus,'-');
end;


procedure TTestScanner.TestDot;

begin
  TestToken(tkDot,'.');
end;


procedure TTestScanner.TestDivision;

begin
  TestToken(tkDivision,'/');
end;


procedure TTestScanner.TestColon;

begin
  TestToken(tkColon,':');
end;


procedure TTestScanner.TestSemicolon;

begin
  TestToken(tkSemicolon,';');
end;


procedure TTestScanner.TestLessThan;

begin
  TestToken(tkLessThan,'<');
end;


procedure TTestScanner.TestEqual;

begin
  TestToken(tkEqual,'=');
end;


procedure TTestScanner.TestGreaterThan;

begin
  TestToken(tkGreaterThan,'>');
end;


procedure TTestScanner.TestAt;

begin
  TestToken(tkAt,'@');
end;


procedure TTestScanner.TestSquaredBraceOpen;

begin
  TestToken(tkSquaredBraceOpen,'[');
end;


procedure TTestScanner.TestSquaredBraceClose;

begin
  TestToken(tkSquaredBraceClose,']');
end;


procedure TTestScanner.TestCaret;

begin
  TestToken(tkCaret,'^');
end;


procedure TTestScanner.TestBackslash;

begin
  TestToken(tkBackslash,'\');
end;


procedure TTestScanner.TestDotDot;

begin
  TestToken(tkDotDot,'..');
end;


procedure TTestScanner.TestAssign;

begin
  TestToken(tkAssign,':=');
end;

procedure TTestScanner.TestAssignPlus;
begin
  TestTokens([tkPlus,tkEqual],'+=');
  FScanner.Options:=[po_cassignments];
  TestToken(tkAssignPlus,'+=');
end;

procedure TTestScanner.TestAssignMinus;
begin
  TestTokens([tkMinus,tkEqual],'-=');
  FScanner.Options:=[po_cassignments];
  TestToken(tkAssignMinus,'-=');
end;

procedure TTestScanner.TestAssignMul;
begin
  TestTokens([tkMul,tkEqual],'*=');
  FScanner.Options:=[po_cassignments];
  TestToken(tkAssignMul,'*=');
end;

procedure TTestScanner.TestAssignDivision;
begin
  TestTokens([tkDivision,tkEqual],'/=');
  FScanner.Options:=[po_cassignments];
  TestToken(tkAssignDivision,'/=');
end;


procedure TTestScanner.TestNotEqual;

begin
  TestToken(tkNotEqual,'<>');
end;


procedure TTestScanner.TestLessEqualThan;

begin
  TestToken(tkLessEqualThan,'<=');
end;


procedure TTestScanner.TestGreaterEqualThan;

begin
  TestToken(tkGreaterEqualThan,'>=');
end;


procedure TTestScanner.TestPower;

begin
  TestToken(tkPower,'**');
end;


procedure TTestScanner.TestSymmetricalDifference;

begin
  TestToken(tkSymmetricalDifference,'><');
end;


procedure TTestScanner.TestAbsolute;

begin
  TestToken(tkabsolute,'absolute');
end;


procedure TTestScanner.TestAnd;

begin
  TestToken(tkand,'and');
end;


procedure TTestScanner.TestArray;

begin
  TestToken(tkarray,'array');
end;


procedure TTestScanner.TestAs;

begin
  TestToken(tkas,'as');
end;


procedure TTestScanner.TestAsm;

begin
  TestToken(tkasm,'asm');
end;


procedure TTestScanner.TestBegin;

begin
  TestToken(tkbegin,'begin');
end;


procedure TTestScanner.TestBitpacked;

begin
  TestToken(tkbitpacked,'bitpacked');
end;


procedure TTestScanner.TestCase;

begin
  TestToken(tkcase,'case');
end;


procedure TTestScanner.TestClass;

begin
  TestToken(tkclass,'class');
end;


procedure TTestScanner.TestConst;

begin
  TestToken(tkconst,'const');
end;


procedure TTestScanner.TestConstructor;

begin
  TestToken(tkconstructor,'constructor');
end;


procedure TTestScanner.TestDestructor;

begin
  TestToken(tkdestructor,'destructor');
end;


procedure TTestScanner.TestDiv;

begin
  TestToken(tkdiv,'div');
end;


procedure TTestScanner.TestDo;

begin
  TestToken(tkdo,'do');
end;


procedure TTestScanner.TestDownto;

begin
  TestToken(tkdownto,'downto');
end;


procedure TTestScanner.TestElse;

begin
  TestToken(tkelse,'else');
end;


procedure TTestScanner.TestEnd;

begin
  TestToken(tkend,'end');
end;


procedure TTestScanner.TestExcept;

begin
  TestToken(tkexcept,'except');
end;


procedure TTestScanner.TestExports;

begin
  TestToken(tkexports,'exports');
end;


procedure TTestScanner.TestFalse;

begin
  TestToken(tkfalse,'false');
end;


procedure TTestScanner.TestFile;

begin
  TestToken(tkfile,'file');
end;


procedure TTestScanner.TestFinalization;

begin
  TestToken(tkfinalization,'finalization');
end;


procedure TTestScanner.TestFinally;

begin
  TestToken(tkfinally,'finally');
end;


procedure TTestScanner.TestFor;

begin
  TestToken(tkfor,'for');
end;


procedure TTestScanner.TestFunction;

begin
  TestToken(tkfunction,'function');
end;


procedure TTestScanner.TestGeneric;

begin
  TestToken(tkgeneric,'generic');
end;


procedure TTestScanner.TestGoto;

begin
  TestToken(tkgoto,'goto');
end;

procedure TTestScanner.TestHelper;
begin
  TestToken(tkHelper,'helper');
end;


procedure TTestScanner.TestIf;

begin
  TestToken(tkif,'if');
end;


procedure TTestScanner.TestImplementation;

begin
  TestToken(tkimplementation,'implementation');
end;


procedure TTestScanner.TestIn;

begin
  TestToken(tkin,'in');
end;


procedure TTestScanner.TestInherited;

begin
  TestToken(tkinherited,'inherited');
end;


procedure TTestScanner.TestInitialization;

begin
  TestToken(tkinitialization,'initialization');
end;


procedure TTestScanner.TestInline;

begin
  TestToken(tkinline,'inline');
end;


procedure TTestScanner.TestInterface;

begin
  TestToken(tkinterface,'interface');
end;


procedure TTestScanner.TestIs;

begin
  TestToken(tkis,'is');
end;


procedure TTestScanner.TestLabel;

begin
  TestToken(tklabel,'label');
end;


procedure TTestScanner.TestLibrary;

begin
  TestToken(tklibrary,'library');
end;


procedure TTestScanner.TestMod;

begin
  TestToken(tkmod,'mod');
end;


procedure TTestScanner.TestNil;

begin
  TestToken(tknil,'nil');
end;


procedure TTestScanner.TestNot;

begin
  TestToken(tknot,'not');
end;


procedure TTestScanner.TestObject;

begin
  TestToken(tkobject,'object');
end;


procedure TTestScanner.TestOf;

begin
  TestToken(tkof,'of');
end;


procedure TTestScanner.TestOn;

begin
  TestToken(tkon,'on');
end;


procedure TTestScanner.TestOperator;

begin
  TestToken(tkoperator,'operator');
end;


procedure TTestScanner.TestOr;

begin
  TestToken(tkor,'or');
end;


procedure TTestScanner.TestPacked;

begin
  TestToken(tkpacked,'packed');
end;


procedure TTestScanner.TestProcedure;

begin
  TestToken(tkprocedure,'procedure');
end;


procedure TTestScanner.TestProgram;

begin
  TestToken(tkprogram,'program');
end;


procedure TTestScanner.TestProperty;

begin
  TestToken(tkproperty,'property');
end;


procedure TTestScanner.TestRaise;

begin
  TestToken(tkraise,'raise');
end;


procedure TTestScanner.TestRecord;

begin
  TestToken(tkrecord,'record');
end;


procedure TTestScanner.TestRepeat;

begin
  TestToken(tkrepeat,'repeat');
end;


procedure TTestScanner.TestResourceString;

begin
  TestToken(tkResourceString,'resourcestring');
end;


procedure TTestScanner.TestSelf;

begin
  TestToken(tkself,'self');
end;


procedure TTestScanner.TestSet;

begin
  TestToken(tkset,'set');
end;


procedure TTestScanner.TestShl;

begin
  TestToken(tkshl,'shl');
end;


procedure TTestScanner.TestShr;

begin
  TestToken(tkshr,'shr');
end;


procedure TTestScanner.TestSpecialize;

begin
  TestToken(tkspecialize,'specialize');
end;


procedure TTestScanner.TestThen;

begin
  TestToken(tkthen,'then');
end;


procedure TTestScanner.TestThreadvar;

begin
  TestToken(tkthreadvar,'threadvar');
end;


procedure TTestScanner.TestTo;

begin
  TestToken(tkto,'to');
end;


procedure TTestScanner.TestTrue;

begin
  TestToken(tktrue,'true');
end;


procedure TTestScanner.TestTry;

begin
  TestToken(tktry,'try');
end;


procedure TTestScanner.TestType;

begin
  TestToken(tktype,'type');
end;


procedure TTestScanner.TestUnit;

begin
  TestToken(tkunit,'unit');
end;


procedure TTestScanner.TestUntil;

begin
  TestToken(tkuntil,'until');
end;


procedure TTestScanner.TestUses;

begin
  TestToken(tkuses,'uses');
end;


procedure TTestScanner.TestVar;

begin
  TestToken(tkvar,'var');
end;


procedure TTestScanner.TestWhile;

begin
  TestToken(tkwhile,'while');
end;


procedure TTestScanner.TestWith;

begin
  TestToken(tkwith,'with');
end;


procedure TTestScanner.TestXor;

begin
  TestToken(tkxor,'xor');
end;


procedure TTestScanner.TestLineEnding;

begin
  TestToken(tkLineEnding,#10);
end;


procedure TTestScanner.TestTab;

begin
  TestToken(tkTab,#9);
end;

procedure TTestScanner.TestTokenSeries;
begin
  TestTokens([tkin,tkWhitespace,tkOf,tkWhiteSpace,tkthen,tkWhiteSpace,tkIdentifier],'in of then aninteger')
end;

procedure TTestScanner.TestTokenSeriesNoWhiteSpace;
begin
  FScanner.SkipWhiteSpace:=True;
  TestTokens([tkin,tkOf,tkthen,tkIdentifier],'in of then aninteger')
end;

procedure TTestScanner.TestTokenSeriesComments;
begin
  TestTokens([tkin,tkWhitespace,tkOf,tkWhiteSpace,tkComment,tkWhiteSpace,tkIdentifier],'in of {then} aninteger')
end;

procedure TTestScanner.TestTokenSeriesNoComments;
begin
  FScanner.SkipComments:=True;
  TestTokens([tkin,tkWhitespace,tkOf,tkWhiteSpace,tkWhiteSpace,tkIdentifier],'in of {then} aninteger')
end;

procedure TTestScanner.TestDefine0;
begin
  TestTokens([tkComment],'{$DEFINE NEVER}');
  If FSCanner.Defines.IndexOf('NEVER')=-1 then
    Fail('Define not defined');
end;

procedure TTestScanner.TestDefine1;
begin
  TestTokens([tkComment],'{$IFDEF NEVER} of {$ENDIF}');
end;

procedure TTestScanner.TestDefine2;

begin
  FSCanner.Defines.Add('ALWAYS');
  TestTokens([tkComment,tkWhitespace,tkOf,tkWhitespace,tkcomment],'{$IFDEF ALWAYS} of {$ENDIF}');
end;

procedure TTestScanner.TestDefine3;
begin
  FSCanner.Defines.Add('ALWAYS');
  TestTokens([tkComment,tkWhitespace,tkOf,tkWhitespace,tkcomment],'{$IFDEF ALWAYS} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestDefine4;
begin
  TestTokens([tkComment,tkWhitespace,tkin,tkWhitespace,tkcomment],'{$IFDEF ALWAYS} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestDefine5;
begin
  FScanner.SkipComments:=True;
  TestTokens([tkLineEnding],'{$IFDEF NEVER} of {$ENDIF}');
end;

procedure TTestScanner.TestDefine6;

begin
  FSCanner.Defines.Add('ALWAYS');
  FScanner.SkipComments:=True;
  TestTokens([tkWhitespace,tkOf,tkWhitespace],'{$IFDEF ALWAYS} of {$ENDIF}');
end;

procedure TTestScanner.TestDefine7;
begin
  FSCanner.Defines.Add('ALWAYS');
  FScanner.SkipComments:=True;
  TestTokens([tkWhitespace,tkOf,tkWhitespace],'{$IFDEF ALWAYS} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestDefine8;
begin
  FScanner.SkipComments:=True;
  TestTokens([tkWhitespace,tkin,tkWhitespace],'{$IFDEF ALWAYS} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestDefine9;
begin
  FScanner.SkipWhiteSpace:=True;
  TestTokens([],'{$IFDEF NEVER} of {$ENDIF}');
end;

procedure TTestScanner.TestDefine10;

begin
  FSCanner.Defines.Add('ALWAYS');
  FScanner.SkipComments:=True;
  TestTokens([tkWhitespace,tkOf,tkWhitespace],'{$IFDEF ALWAYS} of {$ENDIF}');
end;

procedure TTestScanner.TestDefine11;
begin
  FSCanner.Defines.Add('ALWAYS');
  FScanner.SkipComments:=True;
  FScanner.SkipWhiteSpace:=True;
  TestTokens([tkOf],'{$IFDEF ALWAYS} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestDefine12;
begin
  FScanner.SkipComments:=True;
  FScanner.SkipWhiteSpace:=True;
  TestTokens([tkin],'{$IFDEF ALWAYS} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestInclude;
begin
  FResolver.AddStream('myinclude.inc',TStringStream.Create('if true then'));
  FScanner.SkipWhiteSpace:=True;
  FScanner.SkipComments:=True;
  TestTokens([tkIf,tkTrue,tkThen],'{$I myinclude.inc}',True,False);
end;

procedure TTestScanner.TestInclude2;
begin
  FResolver.AddStream('myinclude.inc',TStringStream.Create('if true then'));
  FScanner.SkipWhiteSpace:=True;
  FScanner.SkipComments:=True;
  TestTokens([tkIf,tkTrue,tkThen,tkElse],'{$I myinclude.inc} else',True,False);
end;

procedure TTestScanner.TestUnDefine1;
begin
  FSCanner.Defines.Add('ALWAYS');
  TestTokens([tkComment],'{$UNDEF ALWAYS}');
  AssertEquals('No more define',-1,FScanner.Defines.INdexOf('ALWAYS'));
end;

procedure TTestScanner.TestMacro1;
begin
  FScanner.SkipWhiteSpace:=True;
  FScanner.SkipComments:=True;
  TestTokens([tkbegin,tkend,tkDot],'{$DEFINE MM:=begin end.}'#13#10'MM',True,False);
end;

procedure TTestScanner.TestMacro2;
begin
  FScanner.SkipWhiteSpace:=True;
  FScanner.SkipComments:=True;
  TestTokens([tkbegin,tkend,tkDot],'{$DEFINE MM:=begin end}'#13#10'MM .',True,False);
end;

procedure TTestScanner.TestMacro3;
begin
  FScanner.SkipComments:=True;
  FScanner.SkipWhiteSpace:=True;
  TestTokens([tkof],'{$DEFINE MM:=begin end}'#13#10'{$IFDEF MM} of {$ELSE} in {$ENDIF}');
end;

procedure TTestScanner.TestMacroHandling;
begin
  TTestingPascalScanner(FScanner).DoSpecial:=True;
  FScanner.SkipComments:=True;
  FScanner.SkipWhiteSpace:=True;
  TestTokens([tkIdentifier],'{$DEFINE MM:=begin end}'#13#10'MM');
  AssertEQuals('Correct identifier', 'somethingweird',LastIdentifier);
end;




initialization
  RegisterTests([TTestTokenFinder,TTestStreamLineReader,TTestScanner]);
end.

