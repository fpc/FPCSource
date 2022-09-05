{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains the tests for the CSS Scanner

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit tcCSSScanner;

{$mode objfpc}{$H+}

interface

uses
  TypInfo, Classes, SysUtils, fpcunit, testregistry, fpcssscanner;

type
  TTestLineReader = Class(TTestCase)
  Private
    FData: TStringStream;
    FReader : TStreamLineReader;
  protected
    Procedure CreateReader(AInput : String);
    procedure TearDown; override;
  published
    Procedure TestEmpty;
    Procedure TestReadLine;
    Procedure TestReadLines13;
    Procedure TestReadLines10;
    Procedure TestReadLines1310;
    procedure TestReadLinesEOF13;
    procedure TestReadLinesEOF10;
    procedure TestReadLinesEOF1310;
    procedure TestReadEmptyLines101010;
  end;

  { TTestCSSScanner }

  TTestCSSScanner= class(TTestCase)
  Private
    FPSeudoDisabled,
    FNeedWhiteSpace : Boolean;
    FStream : TStream;
    FLineReader : TLineReader;
    FScanner : TCSSScanner;
    FErrorSource : String;
    procedure AssertEquals(AMessage: String; AExpected, AActual : TCSSToken); overload;
    procedure CheckToken(AToken: TCSSToken; ASource: String);
    procedure CheckTokens(ASource: String; ATokens: array of TCSSToken);
    procedure DoTestFloat(F: Double);
    procedure DoTestFloat(F: Double; S: String);
    procedure DoTestString(S: String);
    procedure TestErrorSource;
  protected
    Function CreateScanner(AInput : String) : TCSSScanner;
    procedure FreeScanner;
    procedure SetUp; override;
    procedure TearDown; override;
    Property Scanner : TCSSScanner Read FScanner;
  published
    procedure TestEmpty;
    Procedure TestEOF;
    Procedure TestWhiteSpace;
    Procedure TestComment1;
    Procedure TestComment2;
    Procedure TestComment3;
    Procedure TestID1;
    Procedure TestIDUnderscore;
    Procedure TestIDMinus;
    Procedure TestIDMinusMinus;
    Procedure TestIDEscape;
    Procedure TestIDEscapeStart;
    procedure TestPSEUDO;
    procedure TestPSEUDO2;
    procedure TestPSEUDOMinus;
    procedure TestPSEUDO2Minus;
    procedure TestPSEUDODisabled;
    procedure TestUNICODERANGE;
    procedure TestUNICODERANGE2;
    procedure TestFUNCTION;
    procedure TestPSEUDOFUNCTION;
    procedure TestPSEUDOFUNCTION2;
    Procedure TestSEMI;
    Procedure TestLPAREN;
    Procedure TestRPAREN;
    Procedure TestLBRACE;
    Procedure TestRBRACE;
    Procedure TestLBRACKET;
    Procedure TestRBRACKET;
    Procedure TestSTR;
    Procedure TestStrEscapeUnicode;
    procedure TestStrEscapeUnicode2;
    Procedure TestCOMMA;
    Procedure TestEQUALS;
    Procedure TestAND;
    Procedure TestTILDE;
    Procedure TestPLUS;
    Procedure TestCOLON;
    Procedure TestDOUBLECOLON;
    Procedure TestDOT;
    Procedure TestINT;
    Procedure TestFLOAT;
    Procedure TestDOUBLE;
    Procedure TestHASH1;
    Procedure TestHASH2;
    Procedure TestDIV;
    Procedure TestSTATEMENT;
    Procedure TestGT;
    Procedure TestLT;
    Procedure TestPERCENTAGE;
    Procedure TestMINUS;
    Procedure TestIMPORTANT;
    Procedure TestCLASSNAME;
    Procedure TestSTAR;
    Procedure TestURL;
    Procedure TestURLAltQuote;
    Procedure TestURL2;
    Procedure TestBADURL;
    Procedure TestBADEOF;
    Procedure TestJUNK;
    Procedure TestSQUARED;
  end;

implementation

function TTestCSSScanner.CreateScanner(AInput: String): TCSSScanner;

begin
  FreeScanner;
  FStream:=TStringStream.Create(AInput);
  FLineReader:=TStreamLineReader.Create(Fstream);
  FScanner:=TCSSScanner.Create(FLineReader);
  FScanner.DisablePseudo:=FPSeudoDisabled;
  Result:=FScanner;
  if FNeedWhiteSpace then
    FScanner.ReturnWhiteSpace:=True;
end;

procedure TTestCSSScanner.FreeScanner;
begin
  FreeAndNil(FScanner);
  FreeAndNil(FLineReader);
  FreeAndNil(FStream);
end;

procedure TTestCSSScanner.SetUp;
begin
  inherited SetUp;
  FNeedWhiteSpace:=False;
  FPSeudoDisabled:=False;
end;

procedure TTestCSSScanner.TearDown;
begin
  FreeScanner;
  Inherited;
end;

procedure TTestCSSScanner.DoTestFloat(F : Double);

Var
  S : String;

begin
  Str(F,S);
  DoTestFloat(F,S);
end;

procedure TTestCSSScanner.DoTestFloat(F : Double; S : String);

Var
  J : TCSSToken;
  C : Double;
  I : integer;
  V : String;

begin
  CreateScanner(S);
  try
    J:=FScanner.FetchToken;
    AssertEquals(S+' is a number',ctkFloat,J);
    V:=FScanner.CurTokenString;
    If (Copy(V,1,2)='0x') then
      begin
      Flush(output);
      V:='$'+Copy(V,3,Length(V)-2);
      C:=StrToInt(V);
      end
    else
      begin
      Val(V,C,I);
      If (I<>0) then
        Fail(FScanner.CurTokenString+' does not contain a float value');
      end;
    AssertEquals('Parsed float equals original float',F,C);
  finally
    FreeScanner;
  end;
end;

procedure TTestCSSScanner.DoTestString(S: String);

Var
  J : TCSSToken;
begin
  CreateScanner(S);
  try
    J:=FScanner.FetchToken;
    AssertEquals(S+' is a string',ctkSTRING,J);
    If (Length(S)>0) and (S[1] in ['"','''','`']) then
      S:=Copy(S,2,Length(S)-2);
    AssertEquals('Correct string is returned',S,FScanner.CurTokenString);
  finally
    FreeScanner;
  end;
end;

procedure TTestCSSScanner.TestErrorSource;

begin
  CreateScanner(FErrorSource);
  try
    While (FScanner.FetchToken<>ctkEOF) do ;
  finally
    FreeScanner;
  end;
end;

procedure TTestCSSScanner.TestEmpty;

Var
  J : TCSSToken;

begin
  CreateScanner('');
  J:=Scanner.FetchToken;
  If (J<>ctkEOF) then
    Fail('Empty returns EOF');
end;

procedure TTestCSSScanner.TestEOF;

Var
  C : TCSSToken;

begin
  CreateScanner('');
  C:=Scanner.FetchToken;
  If (C<>ctkEOF) then
    Fail('Empty returns EOF');
end;

procedure TTestCSSScanner.TestWhiteSpace;
Var
  C : TCSSToken;

begin
  FNeedWhiteSpace:=True;
  CreateScanner(' ');
  C:=Scanner.FetchToken;
  If (C<>ctkWHITESPACE) then
    Fail('Empty returns EOF');
end;

procedure TTestCSSScanner.TestComment1;
begin
  CreateScanner('/* some comment string */');
  AssertEquals('Comment line is skipped',ctkEOF,FScanner.FetchToken);
end;

procedure TTestCSSScanner.TestComment2;
begin
  CreateScanner('/* some comment string */');
  FScanner.ReturnComments:=True;
  AssertEquals('Comment line is returned',ctkComment,FScanner.FetchToken);
  AssertEquals('Comment contents is returned',' some comment string ',FScanner.CurTokenString);
end;

procedure TTestCSSScanner.TestComment3;

begin
  CreateScanner('/* some multiline comment '#10' string */');
  FScanner.ReturnComments:=True;
  AssertEquals('Comment line is returned',ctkComment,FScanner.FetchToken);
  AssertEquals('Comment contents is returned',' some multiline comment '#10' string ',FScanner.CurTokenString);
end;

procedure TTestCSSScanner.TestID1;
begin
  CheckToken(ctkIDENTIFIER,'anid');
end;

procedure TTestCSSScanner.TestIDUnderscore;
begin
  CheckToken(ctkIDENTIFIER,'_anid');
end;

procedure TTestCSSScanner.TestIDMinus;
begin
  CheckToken(ctkIDENTIFIER,'-anid');
end;

procedure TTestCSSScanner.TestIDMinusMinus;
begin
  CheckToken(ctkIDENTIFIER,'--anid');
end;

procedure TTestCSSScanner.TestIDEscape;
begin
  CheckToken(ctkIDENTIFIER,'an\(id');
end;

procedure TTestCSSScanner.TestIDEscapeStart;
begin
  CheckToken(ctkIDENTIFIER,'\(anid');
end;


procedure TTestCSSScanner.TestSEMI;
begin
  CheckToken(ctkSEMICOLON,';');
end;

procedure TTestCSSScanner.TestLPAREN;
begin
  CheckToken(ctkLPARENTHESIS,'(');
end;

procedure TTestCSSScanner.TestRPAREN;
begin
  CheckToken(ctkRPARENTHESIS,')');
end;

procedure TTestCSSScanner.TestLBRACE;
begin
  CheckToken(ctkLBRACE,'{');
end;

procedure TTestCSSScanner.TestRBRACE;
begin
  CheckToken(ctkRBRACE,'}');
end;

procedure TTestCSSScanner.TestLBRACKET;
begin
  CheckToken(ctkLBRACKET,'[');
end;

procedure TTestCSSScanner.TestRBRACKET;
begin
  CheckToken(ctkRBRACKET,']');
end;

procedure TTestCSSScanner.TestSTR;
begin
  CheckToken(ctkSTRING,'"abc"');
end;

procedure TTestCSSScanner.TestStrEscapeUnicode;
begin
 CheckToken(ctkSTRING,'"\00a0\00a0\00a0\00a0"');
 CheckToken(ctkSTRING,'"\2a"');
end;

procedure TTestCSSScanner.TestStrEscapeUnicode2;
begin
 CheckToken(ctkSTRING,'"\2a"');
end;


procedure TTestCSSScanner.TestCOMMA;
begin
  CheckToken(ctkCOMMA,',');
end;

procedure TTestCSSScanner.TestEQUALS;
begin
  CheckToken(ctkEQUALS,'=');
end;

procedure TTestCSSScanner.TestAND;
begin
  CheckToken(ctkAND,'&');
end;

procedure TTestCSSScanner.TestTILDE;
begin
  CheckToken(ctkTILDE,'~');
end;

procedure TTestCSSScanner.TestPLUS;
begin
  CheckToken(ctkPLUS,'+');
end;

procedure TTestCSSScanner.TestCOLON;
begin
  CheckToken(ctkCOLON,':');
end;

procedure TTestCSSScanner.TestDOUBLECOLON;
begin
  CheckToken(ctkDOUBLECOLON,'::');
end;

procedure TTestCSSScanner.TestDOT;
begin
  CheckToken(ctkDOT,'.');
end;

procedure TTestCSSScanner.TestINT;
begin
  CheckToken(ctkINTEGER,'123');
end;

procedure TTestCSSScanner.TestFLOAT;
begin
  DoTestFloat(123.3,'123.3');
end;

procedure TTestCSSScanner.TestDOUBLE;
begin
  DoTestFloat(123.3,'123.3');
end;

procedure TTestCSSScanner.TestHASH1;
begin
  CheckToken(ctkHASH,'#1231');
end;

procedure TTestCSSScanner.TestHASH2;
begin
  CheckToken(ctkHASH,'#AFAFAF');
end;

procedure TTestCSSScanner.TestDIV;
begin
  CheckToken(ctkDIV,'/');
end;

procedure TTestCSSScanner.TestSTATEMENT;
begin
  CheckToken(ctkATKEYWORD,'@media');
end;

procedure TTestCSSScanner.TestGT;
begin
  CheckToken(ctkGT,'>');
end;

procedure TTestCSSScanner.TestLT;
begin
  CheckToken(ctkLT,'<');
end;

procedure TTestCSSScanner.TestPERCENTAGE;
begin
  CheckToken(ctkPERCENTAGE,'%');
end;

procedure TTestCSSScanner.TestMINUS;
begin
  CheckToken(ctkMinus,'-');
end;

procedure TTestCSSScanner.TestIMPORTANT;
begin
  CheckToken(ctkIMPORTANT,'!important');
end;

procedure TTestCSSScanner.TestCLASSNAME;
begin
  CheckToken(ctkCLASSNAME,'.classname');
end;

procedure TTestCSSScanner.TestSTAR;
begin
  CheckToken(ctkSTAR,'*');
end;

procedure TTestCSSScanner.TestURL;
begin
  CheckToken(ctkURL,'url("abc")');
end;

procedure TTestCSSScanner.TestURLAltQuote;
begin
  CheckToken(ctkURL,'url(''abc'')');
end;

procedure TTestCSSScanner.TestURL2;
begin
  CheckToken(ctkURL,'url(abc)');
end;

procedure TTestCSSScanner.TestBADURL;
begin
  CheckToken(ctkBADURL,'url(de f)');
end;

procedure TTestCSSScanner.TestBADEOF;
begin
  CheckToken(ctkEOF,'url(def');
end;


procedure TTestCSSScanner.TestPSEUDO;
begin
  CheckToken(ctkPSEUDO,':name');
end;

procedure TTestCSSScanner.TestPSEUDO2;
begin
  CheckToken(ctkPSEUDO,'::name');
end;

procedure TTestCSSScanner.TestPSEUDOMinus;
begin
  CheckToken(ctkPSEUDO,':-name');
end;

procedure TTestCSSScanner.TestPSEUDO2Minus;
begin
  CheckToken(ctkPSEUDO,'::-name');
end;

procedure TTestCSSScanner.TestPSEUDODisabled;
begin
  FPseudoDisabled:=True;
  CheckTokens(':me',[ctkColon,ctkIDENTIFIER]);
end;

procedure TTestCSSScanner.TestUNICODERANGE;
begin
  CheckToken(ctkUNICODERANGE,'U+124');
end;

procedure TTestCSSScanner.TestUNICODERANGE2;
begin
  CheckToken(ctkUNICODERANGE,'U+124-128');

end;

procedure TTestCSSScanner.TestFUNCTION;
begin
  CheckToken(ctkFUNCTION,'name(');
end;

procedure TTestCSSScanner.TestPSEUDOFUNCTION;
begin
  CheckToken(ctkPSEUDOFUNCTION,':name(');
end;

procedure TTestCSSScanner.TestPSEUDOFUNCTION2;
begin
  CheckToken(ctkPSEUDOFUNCTION,'::name(');
end;



procedure TTestCSSScanner.TestJUNK;
begin
  FErrorSource:='?';
  AssertException('Exception',ECSSScanner, @TestErrorSource);
end;

procedure TTestCSSScanner.TestSQUARED;
begin
  CheckToken(ctkSQUARED,'^');
end;

procedure TTestCSSScanner.AssertEquals(AMessage : String; AExpected, AActual: TCSSToken);

Var
  S,EN1,EN2 : String;

begin
  If (AActual<>AExpected) then
    begin
    EN1:=GetEnumName(TypeINfo(TCSSToken),Ord(AExpected));
    EN2:=GetEnumName(TypeINfo(TCSSToken),Ord(AActual));
    S:=Format('%s : %s <> %s',[AMessage,EN1,EN2]);
    Fail(S);
    end;
end;

procedure TTestCSSScanner.CheckToken(AToken: TCSSToken; ASource: String);

Var
  J : TCSSToken;
  EN2 : String;

begin
  CreateScanner(ASource);
  J:=Scanner.FetchToken;
  EN2:=GetEnumName(TypeINfo(TCSSToken),Ord(AToken));
  AssertEquals(Format('Source %s should result in %s.',[ASource,EN2]),AToken,J);
end;

procedure TTestCSSScanner.CheckTokens(ASource: String;
  ATokens: array of TCSSToken);

Var
  I : Integer;
  J : TCSSToken;
  S : String;

begin
  CreateScanner(ASource);
  For I:=Low(ATokens) to High(ATokens) do
    begin
    J:=FScanner.FetchToken;
    S:=GetEnumName(TypeINfo(TCSSToken),Ord(ATokens[i]));
    S:=Format('Source "%s", token %d (%s): expected %s',[ASource,I,FScanner.CurTokenString,S]);
    AssertEquals(S,ATokens[i],J);
    end;
end;



{ TTestLineReader }

procedure TTestLineReader.CreateReader(AInput: String);
begin
  FData:=TStringStream.Create(AInput);
  FReader:=TStreamLineReader.Create(FData);
end;


procedure TTestLineReader.TearDown;
begin
  FreeAndNil(FReader);
  FreeAndNil(FData);
end;

procedure TTestLineReader.TestEmpty;
begin
  CreateReader('');
  AssertEquals('Empty reader returns EOF',True,FReader.IsEOF);
  AssertEquals('Empty reader returns empty string','',FReader.ReadLine);
end;

procedure TTestLineReader.TestReadLine;
begin
  CreateReader('Something');
  AssertEquals('Reader with 1 line returns 1 line','Something',FReader.ReadLine);
  AssertEquals('EOF true after reading line',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadLines13;
begin
  CreateReader('Something'#13'else');
  AssertEquals('Reader with 2 lines returns 1st line','Something',FReader.ReadLine);
  AssertEquals('Reader with 2 lines returns 2nd line','else',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadLines10;
begin
  CreateReader('Something'#10'else');
  AssertEquals('Reader with 2 lines returns 1st line','Something',FReader.ReadLine);
  AssertEquals('Reader with 2 lines returns 2nd line','else',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadLines1310;
begin
  CreateReader('Something'#13#10'else');
  AssertEquals('Reader with 2 lines returns 1st line','Something',FReader.ReadLine);
  AssertEquals('Reader with 2 lines returns 2nd line','else',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadLinesEOF13;
begin
  CreateReader('Something'#13);
  AssertEquals('Reader with 2 lines + CR returns 1st line','Something',FReader.ReadLine);
  AssertEquals('Reader with 1 lines + CR returns empty 2nd line','',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadLinesEOF10;
begin
  CreateReader('Something'#10);
  AssertEquals('Reader with 2 lines + LF returns 1st line','Something',FReader.ReadLine);
  AssertEquals('Reader with 1 lines + LF returns empty 2nd line','',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadLinesEOF1310;
begin
  CreateReader('Something'#13#10);
  AssertEquals('Reader with 2 lines + CRLF returns 1st line','Something',FReader.ReadLine);
  AssertEquals('Reader with 1 lines + CRLF returns empty 2nd line','',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

procedure TTestLineReader.TestReadEmptyLines101010;

begin
  CreateReader('Something'#10#10#10);
  AssertEquals('Reader with 1 line + LFLFLF returns 1st line','Something',FReader.ReadLine);
  AssertEquals('EOF false after reading line 1',False,FReader.IsEOF);
  AssertEquals('Reader with 1 line + LFLFLF returns empty 2nd line','',FReader.ReadLine);
  AssertEquals('EOF false after reading line 2',False,FReader.IsEOF);
  AssertEquals('Reader with 1 line + LFLFLF returns empty 3nd line','',FReader.ReadLine);
  AssertEquals('EOF true after reading lines',True,FReader.IsEOF);
end;

initialization

  RegisterTests([TTestLineReader,TTestCSSScanner]);
end.

