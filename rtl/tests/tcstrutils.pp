unit tcstrutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, strutils;

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
  end;

implementation

Const
   // Don't move this comment, it indicates the positions.
   //           1         2         3         4
   //  1234567890123456789012345678901234567890123456789
  S = 'Some very long string with some words in it';
  SLen = Length(S);
  Starts : Array[1..3] of Integer = (0,10,41);
  
{$define usenew}
{$ifdef usenew}
{$i searchbuf.inc}
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
end.

