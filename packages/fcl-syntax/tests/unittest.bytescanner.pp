{
  Tests for the cursor helpers At and Ch of TByteSyntaxHighlighter.
}
unit unittest.bytescanner;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.table, syntax.c;

type

  { TByteScannerProbe }

  TByteScannerProbe = class(TCSyntaxHighlighter)
  public
    // Start scanning aSource with the cursor at aCursor
    procedure Scan(const aSource: string; aCursor: SizeInt);
    // Call the protected At
    function ProbeAt(const aValue: string; aOffset: SizeInt = 0): boolean;
    // Call the protected Ch
    function ProbeCh(aOffset: SizeInt = 0): char;
  end;

  { TTestByteScanner }

  TTestByteScanner = class(TTestCase)
  private
    FProbe: TByteScannerProbe;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAtMatches;
    procedure TestAtMismatches;
    procedure TestAtBounds;
    procedure TestAtOffsets;
    procedure TestCh;
  end;

implementation

procedure TByteScannerProbe.Scan(const aSource: string; aCursor: SizeInt);

begin
  Start(aSource);
  FCursor := aCursor;
end;


function TByteScannerProbe.ProbeAt(const aValue: string; aOffset: SizeInt): boolean;

begin
  Result := At(aValue, aOffset);
end;


function TByteScannerProbe.ProbeCh(aOffset: SizeInt): char;

begin
  Result := Ch(aOffset);
end;


procedure TTestByteScanner.SetUp;

begin
  FProbe := TByteScannerProbe.Create;
end;


procedure TTestByteScanner.TearDown;

begin
  FreeAndNil(FProbe);
end;


procedure TTestByteScanner.TestAtMatches;

begin
  FProbe.Scan('abc/*def', 4);
  AssertTrue('two characters', FProbe.ProbeAt('/*'));
  AssertTrue('one character', FProbe.ProbeAt('/'));
  AssertTrue('up to the end', FProbe.ProbeAt('/*def'));
  FProbe.Scan('abc', 1);
  AssertTrue('whole text', FProbe.ProbeAt('abc'));
end;


procedure TTestByteScanner.TestAtMismatches;

begin
  FProbe.Scan('abc/*def', 4);
  AssertFalse('first character differs', FProbe.ProbeAt('*/'));
  AssertFalse('second character differs', FProbe.ProbeAt('//'));
  AssertFalse('last character differs', FProbe.ProbeAt('/*deg'));
  AssertFalse('empty value', FProbe.ProbeAt(''));
end;


procedure TTestByteScanner.TestAtBounds;

begin
  FProbe.Scan('abc', 3);
  AssertTrue('last character', FProbe.ProbeAt('c'));
  AssertFalse('value runs past the end', FProbe.ProbeAt('cd'));
  FProbe.Scan('abc', 4);
  AssertFalse('cursor past the end', FProbe.ProbeAt('c'));
  FProbe.Scan('', 1);
  AssertFalse('empty text', FProbe.ProbeAt('a'));
end;


procedure TTestByteScanner.TestAtOffsets;

begin
  FProbe.Scan('abc', 2);
  AssertTrue('negative offset', FProbe.ProbeAt('ab', -1));
  AssertTrue('positive offset', FProbe.ProbeAt('c', 1));
  AssertFalse('offset before the text', FProbe.ProbeAt('a', -2));
  AssertFalse('offset past the end', FProbe.ProbeAt('c', 2));
end;


procedure TTestByteScanner.TestCh;

begin
  FProbe.Scan('abc', 2);
  AssertEquals('character at the cursor', 'b', FProbe.ProbeCh);
  AssertEquals('previous character', 'a', FProbe.ProbeCh(-1));
  AssertEquals('next character', 'c', FProbe.ProbeCh(1));
  AssertEquals('before the text', #0, FProbe.ProbeCh(-2));
  AssertEquals('past the end', #0, FProbe.ProbeCh(2));
end;

initialization
  RegisterTest(TTestByteScanner);
end.
