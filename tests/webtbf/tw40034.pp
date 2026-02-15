{ Test for bug #40034: generics.collections uses slow AnsiCompareStr
  instead of binary comparison for string operations.

  Before the fix, the default AnsiString comparer used AnsiCompareStr(),
  which performs locale-aware Unicode comparison 
  (slow and not Delphi-compatible, as per bug report). 
  After the fix, it uses a byte-per-byte for fast binary (ordinal) comparison.

  This test verifies:
  1. Binary comparison semantics (not locale-aware)
  2. Correct ordering of strings by byte value
  3. That TDictionary and TList work correctly with string keys/values 

  Test can be compiled with FPC and delphi  
}

program tw40034;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

uses
{$ifdef fpc}
  SysUtils, Generics.Collections, Generics.Defaults;
{$else}
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;
{$endif}

var
  Errors: Integer = 0;

procedure Check(const ATest: string; ACondition: Boolean);
begin
  if ACondition then
    WriteLn('OK: ', ATest)
  else begin
    WriteLn('FAIL: ', ATest);
    Inc(Errors);
  end;
end;

procedure TestBinaryOrdering;
{ Binary comparison means 'Z' (90) < 'a' (97).
  Locale-aware comparison would sort 'a' before 'Z'. }
var
  Cmp: IComparer<string>;
begin
  WriteLn('--- Binary ordering ---');
  Cmp := TComparer<string>.Default;
  Check('Z < a in binary order', Cmp.Compare('Z', 'a') < 0);
  Check('a > Z in binary order', Cmp.Compare('a', 'Z') > 0);
  Check('equal strings', Cmp.Compare('hello', 'hello') = 0);
  Check('empty = empty', Cmp.Compare('', '') = 0);
  Check('empty < non-empty', Cmp.Compare('', 'a') < 0);
  Check('non-empty > empty', Cmp.Compare('a', '') > 0);
  Check('shorter prefix < longer', Cmp.Compare('abc', 'abcd') < 0);
end;

procedure TestAnsiStringBinaryOrdering;
var
  Cmp: IComparer<AnsiString>;
begin
  WriteLn('--- AnsiString binary ordering ---');
  Cmp := TComparer<AnsiString>.Default;
  Check('AnsiString: Z < a', Cmp.Compare('Z', 'a') < 0);
  Check('AnsiString: equal', Cmp.Compare('test', 'test') = 0);
  Check('AnsiString: abc < abd', Cmp.Compare('abc', 'abd') < 0);
end;

procedure TestDictionaryWithStringKeys;
var
  Dict: TDictionary<string, Integer>;
begin
  WriteLn('--- TDictionary<string,Integer> ---');
  Dict := TDictionary<string, Integer>.Create;
  try
    Dict.Add('alpha', 1);
    Dict.Add('beta', 2);
    Dict.Add('gamma', 3);
    Check('Dict contains alpha', Dict.ContainsKey('alpha'));
    Check('Dict contains beta', Dict.ContainsKey('beta'));
    Check('Dict does not contain delta', not Dict.ContainsKey('delta'));
    Check('Dict[alpha] = 1', Dict['alpha'] = 1);
    Check('Dict[gamma] = 3', Dict['gamma'] = 3);
  finally
    Dict.Free;
  end;
end;

procedure TestListIndexOf;
var
  List: TList<string>;
begin
  WriteLn('--- TList<string>.IndexOf ---');
  List := TList<string>.Create;
  try
    List.Add('first');
    List.Add('second');
    List.Add('third');
    Check('IndexOf first = 0', List.IndexOf('first') = 0);
    Check('IndexOf third = 2', List.IndexOf('third') = 2);
    Check('IndexOf missing = -1', List.IndexOf('missing') = -1);
  finally
    List.Free;
  end;
end;

procedure TestContainsValue;
var
  Dict: TDictionary<Integer, string>;
begin
  WriteLn('--- TDictionary<Integer,string>.ContainsValue ---');
  Dict := TDictionary<Integer, string>.Create;
  try
    Dict.Add(1, 'one');
    Dict.Add(2, 'two');
    Dict.Add(3, 'three');
    Check('ContainsValue one', Dict.ContainsValue('one'));
    Check('ContainsValue three', Dict.ContainsValue('three'));
    Check('Not ContainsValue four', not Dict.ContainsValue('four'));
  finally
    Dict.Free;
  end;
end;

begin
  try
    TestBinaryOrdering;
    TestAnsiStringBinaryOrdering;
    TestDictionaryWithStringKeys;
    TestListIndexOf;
    TestContainsValue;
    if Errors = 0 then
      WriteLn('All tests passed.')
    else
      WriteLn(Errors, ' test(s) FAILED.');
  except
    on E: Exception do
      WriteLn('FAIL with exception: ', E.ClassName, ': ', E.Message);
  end;
  Halt(Errors);
end.
