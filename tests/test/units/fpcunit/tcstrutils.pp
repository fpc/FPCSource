unit tcstrutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, strutils;

type

  { TTestSearchBuf }

  TTestSearchBuf= class(TTestCase)
  Private
    Procedure TestSearch(Sub:String; Start : Integer; O : TStringSearchOptions; Expected : Integer);
  published
    procedure TestSimple;
    procedure TestSimpleNoRes;
    procedure TestSimpleDown;
    procedure TestSimpleDownNoRes;
    procedure TestNotExistDown;
    procedure TestNotExist;
    procedure TestSimpleDownPos;
    procedure TestSimplePos;
    procedure TestSimpleCaseSensitive;
    procedure TestSimpleCaseSensitiveDown;
    procedure TestSimpleWholeWord;
    procedure TestSimpleWholeWordDown;
    procedure TestSimplePartialend;
    procedure TestSimplePartialStart;
    procedure TestEndMatchDown;
    procedure TestEndMatch;
    procedure TestWholeWordAtStart;
    procedure TestWholeWordAtStartDown;
    procedure TestWholeWordAtEnd;
    procedure TestWholeWordAtEndDown;
    procedure TestEmptySearchString;
    procedure TestSelstartBeforeBuf;
    procedure testSelstartAfterBuf;
    Procedure TestDecodeSoundexInt;
  end;

implementation

Const
   // Don't move this comment, it indicates the positions.
   //           1         2         3         4
   //  1234567890123456789012345678901234567890123456789
  S = 'Some very long string with some words in it';
  SLen = Length(S);
  
{$define usenew}
{$ifdef usenew}
{$i searchbuf.inc}
const
  WhichSearchbuf = 'new';
{$else}
const
  WhichSearchbuf = 'old';
{$endif}

procedure TTestSearchBuf.TestSearch(Sub: String; Start: Integer;
  O: TStringSearchOptions; Expected: Integer);

Var
  P,PR : PChar;
  I : Integer;
  
begin
  P:=PChar(S);
  PR:=SearchBuf(P,Length(S),Start,0,Sub,O);
  If (PR=Nil) then
    begin
    If (Expected<>-1) then
      Fail(Format('Search for "%s" failed, expected result at %d',[Sub,Expected]));
    end
  else
    begin
    I:=(PR-P)+1;
    If (I<>Expected) then
      Fail(Format('Wrong result for search for "%s", expected result at %d, got %d',[Sub,Expected,I]));
    end;
end;

procedure TTestSearchBuf.TestSimpleNoRes;
begin
  TestSearch('very',0,[],-1);
end;

procedure TTestSearchBuf.TestSimple;
begin
  TestSearch('very',SLen,[],6);
end;

procedure TTestSearchBuf.TestSimpleDownNoRes;
begin
  TestSearch('very',0,[soDown],6);
end;

procedure TTestSearchBuf.TestSimpleDown;
begin
  TestSearch('very',SLen,[soDown],-1);
end;

procedure TTestSearchBuf.TestSimplePartialend;
begin
  TestSearch('its',0,[soDown],-1);
end;

procedure TTestSearchBuf.TestSimplePartialStart;
begin
  TestSearch('Tso',SLen,[],-1);
end;

procedure TTestSearchBuf.TestEndMatchDown;
begin
  TestSearch('it',30,[soDown],42);
end;

procedure TTestSearchBuf.TestEndMatch;
begin
  TestSearch('it',SLen,[],42);
end;

procedure TTestSearchBuf.TestWholeWordAtStart;
begin
  TestSearch('Some',20,[soWholeWord],1);
end;

procedure TTestSearchBuf.TestWholeWordAtStartDown;
begin
  TestSearch('Some',0,[soDown,soWholeWord],1);
end;

procedure TTestSearchBuf.TestWholeWordAtEnd;
begin
  TestSearch('it',SLen,[soWholeWord],42);
end;

procedure TTestSearchBuf.TestWholeWordAtEndDown;
begin
  TestSearch('it',30,[soDown,soWholeWord],42);
end;

procedure TTestSearchBuf.TestEmptySearchString;
begin
  TestSearch('',30,[],-1);
end;

procedure TTestSearchBuf.TestSelstartBeforeBuf;
begin
  TestSearch('very',-5,[soDown],-1);
end;

procedure TTestSearchBuf.testSelstartAfterBuf;
begin
  TestSearch('very',100,[],-1);
end;

procedure TTestSearchBuf.TestDecodeSoundexInt;

Const
  OrdA = Ord('A');
  Ord0 = Ord('0');

  Function CreateInt (Const S : String) : Integer;

  var
    I, Len : Integer;

  begin
    Result:=-1;
    Len:=Length(S);
    If Len>0 then
      begin
      Result:=Ord(S[1])-OrdA;
      if Len > 1 then
        begin
        Result:=Result*26+(Ord(S[2])-Ord0);
        for I:=3 to Len do
          Result:=(Ord(S[I])-Ord0)+Result*7;
        end;
      Result:=Len+Result*9;
      end;
  end;


  Procedure TestOneShot(S : String);

  Var
    R : String;

  begin
    R:=DecodeSoundexInt(CreateInt(S));
    AssertEquals('Decoded Soundexint equals original soundex result:',S,R);
  end;

Var
  C,J,K : Integer;
  S : String;

begin
  For C:=Ord('A') to Ord('Z') do
    begin
    S:=Char(C);
    TestOneShot(S);
    for J:=1 to 6 do
      begin
      S:=Char(C);
      For K:=1 to 6 do
        begin
        S:=S+Char(Ord('0')+k);
        TestOneShot(S);
        end;
      end;
    end;

end;

procedure TTestSearchBuf.TestSimpleDownPos;
begin
  TestSearch('it',30,[soDown],42);
end;

procedure TTestSearchBuf.TestSimplePos;
begin
  TestSearch('it',30,[],24);
end;

procedure TTestSearchBuf.TestNotExist;
begin
  TestSearch('quid',SLen,[],-1);
end;

procedure TTestSearchBuf.TestNotExistDown;
begin
  TestSearch('quid',0,[soDown],-1);
end;

procedure TTestSearchBuf.TestSimpleCaseSensitive;
begin
  TestSearch('Very',SLen,[soMatchCase],-1);
end;

procedure TTestSearchBuf.TestSimpleCaseSensitiveDown;
begin
  TestSearch('Very',0,[soMatchCase,soDown],-1);
end;

procedure TTestSearchBuf.TestSimpleWholeWord;
begin
  TestSearch('in',SLen,[soWholeWord],39);
end;

procedure TTestSearchBuf.TestSimpleWholeWordDown;
begin
  TestSearch('in',0,[soWholeWord,soDown],39);
end;

initialization
  RegisterTest(TTestSearchBuf);
  writeln ('Testing with ', WhichSearchbuf, ' implementation');
  writeln;
end.

