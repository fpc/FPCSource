program texception16;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  AllocCount: Integer = 0;

function MakeStr(const S: String): String;
begin
  { Force a dynamic string allocation so finalization matters }
  Result := S + IntToStr(Random(1000));
  Inc(AllocCount);
end;

procedure Test(aArg: array of String);
begin
  WriteLn('  Inside Test, High(aArg) = ', High(aArg));
  WriteLn('  aArg[0] = ', aArg[0]);
  if High(aArg) > 0 then
    WriteLn('  aArg[1] = ', aArg[1]);
  raise Exception.Create('Boom');
end;

procedure TestMany(A, B, C, D, E: Integer; aArg: array of String);
{ Extra params push the high bound further into the frame,
  making a garbage SP read more likely to be obviously wrong }
begin
  WriteLn('  Inside TestMany, High(aArg) = ', High(aArg),
          ', A=', A, ' B=', B, ' C=', C, ' D=', D, ' E=', E);
  raise Exception.Create('Boom from TestMany');
end;

{ Test with a large array to make fpc_finalize_array iterate many times
  if the count is garbage }
procedure TestLargeArray(aArg: array of String);
var
  I: Integer;
begin
  WriteLn('  Inside TestLargeArray, High(aArg) = ', High(aArg));
  for I := 0 to High(aArg) do
    Write('.');
  WriteLn;
  raise Exception.Create('Boom from TestLargeArray');
end;

begin
  WriteLn('=== Test 1: Basic open array with dynamic strings ===');
  try
    Test([MakeStr('hello'), MakeStr('world')]);
  except
    on E: Exception do
      WriteLn('  Caught: ', E.Message);
  end;

  WriteLn;
  WriteLn('=== Test 2: Open array with many preceding params ===');
  try
    TestMany(1, 2, 3, 4, 5, [MakeStr('aaa'), MakeStr('bbb'), MakeStr('ccc')]);
  except
    on E: Exception do
      WriteLn('  Caught: ', E.Message);
  end;

  WriteLn;
  WriteLn('=== Test 3: Large open array ===');
  try
    TestLargeArray([MakeStr('s0'), MakeStr('s1'), MakeStr('s2'), MakeStr('s3'),
                    MakeStr('s4'), MakeStr('s5'), MakeStr('s6'), MakeStr('s7'),
                    MakeStr('s8'), MakeStr('s9')]);
  except
    on E: Exception do
      WriteLn('  Caught: ', E.Message);
  end;

  WriteLn;
  WriteLn('AllocCount = ', AllocCount);
  WriteLn('All tests passed without crash.');
end.
