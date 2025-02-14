unit utcstrutils;

{$mode ObjFPC}{$H+}
{$COPERATORS ON}

interface

uses
  sysutils, strutils, fpcunit, testregistry;

Type

  { TTestStrUtils }

  TTestStrUtils = class(TTestCase)
  Published
    Procedure TestNaturalCompareText;

  end;

implementation

{ TTestStrUtils }

type
  CaseRec = record
    a, b: string;
    expect: int8;
  end;

const
  Cases: array[0 .. 4] of CaseRec = (
      (a: '100000000000000000000'; b: '100000000000000000001'; expect: -1),
      (a: ' 10 hi';                b: '010 hi';                expect:  0),
      (a: 'score: 10';             b: 'score:010';             expect:  0),
      (a: '9';                     b: ' ';                     expect: -1),
      (a: 'A';                     b: '';                      expect: +1)
  );

procedure TTestStrUtils.TestNaturalCompareText;

var
  somethingFailed: boolean = false;

  function RandomString: string;

  const
    TextChars = 'abAB ';
    NumberChars = '012';
  var
    iComp, iSym: SizeInt;
  begin
    result := '';
    for iComp := 0 to random(5) do
      case random(2) of
        0:
          for iSym := 0 to random(3) do
            result += TextChars[1 + random(length(TextChars))];
        else
          for iSym := 0 to random(3) do
            result += NumberChars[1 + random(length(NumberChars))];
      end;
  end;
const
  NFuzzStrings = 200;

var
  cs: CaseRec;
  got, ab: integer;
  desc : string;
  fuzz: array of string;
  i, iA, iB, iC: SizeInt;
  comparisons: array[0 .. NFuzzStrings - 1, 0 .. NFuzzStrings - 1] of int8;

begin
  for cs in Cases do
  begin
    got := NaturalCompareText(cs.a, cs.b);
    Desc:=Format('a = ''%s'', b = ''%s'' ',[cs.a, cs.b]);
    AssertEquals(Desc,cs.expect,got);
  end;
  SetLength(fuzz, NFuzzStrings);
  fuzz[0] := '';
  fuzz[1] := ' ';
  for i := 2 to High(fuzz) do
    fuzz[i] := RandomString;

  for iA := 0 to High(fuzz) do
    for iB := iA to High(fuzz) do
    begin
      comparisons[iA, iB] := NaturalCompareText(fuzz[iA], fuzz[iB]);
      comparisons[iB, iA] := NaturalCompareText(fuzz[iB], fuzz[iA]);
      Desc:=Format('Antisymmetry: a= ''%s'', b= ''%s'' ',[fuzz[iA],fuzz[iB]]);
      AssertEquals('Expect '+Desc, -comparisons[iB, iA], comparisons[iA, iB]);
    end;

  for iA := 0 to High(fuzz) do
    for iB := iA to High(fuzz) do
    begin
      ab := comparisons[iA, iB];
      for iC := 0 to High(fuzz) do
        begin
          Desc:=Format('Transitivity: a= ''%s'', b= ''%s'' , c= ''%s'' ',[fuzz[iA], fuzz[iB], fuzz[iC]]);
          AssertFalse(Desc,(comparisons[iB, iC] = ab) and (comparisons[iA, iC] <> ab));
        end;
    end;
end;

initialization
  RegisterTest(TTestStrUtils);
end.

