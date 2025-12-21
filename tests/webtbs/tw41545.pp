program GetVKTest;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  VK_0 = $30;
  VK_A = $41;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

function GetVK(Character: Char): Integer;
begin
  Result := 0;
  case Character of
    '0'..'9': Result := VK_0 + Ord(Character) - Ord('0');
    'a'..'z': Result := VK_A + Ord(Character) - Ord('a');
    'A'..'Z': Result := VK_A + Ord(Character) - Ord('A');
  end;
end;

procedure Check(const TestName: String; Condition: Boolean);
begin
  if Condition then
  begin
    WriteLn('[PASS] ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', TestName);
    Inc(TestsFailed);
  end;
end;

procedure CheckEqual(const TestName: String; Expected, Actual: Integer);
begin
  if Expected = Actual then
  begin
    WriteLn('[PASS] ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn(Format('[FAIL] %s: expected $%02X, got $%02X', [TestName, Expected, Actual]));
    Inc(TestsFailed);
  end;
end;

var
  C: Char;
begin
  WriteLn('Testing GetVK Function');
  WriteLn('======================');
  WriteLn;

  { Test digits }
  WriteLn('-- Digit tests --');
  CheckEqual('GetVK(''0'') = VK_0', VK_0, GetVK('0'));
  CheckEqual('GetVK(''5'') = VK_0 + 5', VK_0 + 5, GetVK('5'));
  CheckEqual('GetVK(''9'') = VK_0 + 9', VK_0 + 9, GetVK('9'));

  { Verify all digits map correctly }
  for C := '0' to '9' do
    Check(Format('Digit %s maps to $%02X', [C, VK_0 + Ord(C) - Ord('0')]),
          GetVK(C) = VK_0 + Ord(C) - Ord('0'));
  WriteLn;

  { Test uppercase letters }
  WriteLn('-- Uppercase letter tests --');
  CheckEqual('GetVK(''A'') = VK_A', VK_A, GetVK('A'));
  CheckEqual('GetVK(''M'') = VK_A + 12', VK_A + 12, GetVK('M'));
  CheckEqual('GetVK(''Z'') = VK_A + 25', VK_A + 25, GetVK('Z'));

  { Verify all uppercase letters map correctly }
  for C := 'A' to 'Z' do
    Check(Format('Uppercase %s maps to $%02X', [C, VK_A + Ord(C) - Ord('A')]),
          GetVK(C) = VK_A + Ord(C) - Ord('A'));
  WriteLn;

  { Test lowercase letters }
  WriteLn('-- Lowercase letter tests --');
  CheckEqual('GetVK(''a'') = VK_A', VK_A, GetVK('a'));
  CheckEqual('GetVK(''m'') = VK_A + 12', VK_A + 12, GetVK('m'));
  CheckEqual('GetVK(''z'') = VK_A + 25', VK_A + 25, GetVK('z'));

  { Verify all lowercase letters map correctly }
  for C := 'a' to 'z' do
    Check(Format('Lowercase %s maps to $%02X', [C, VK_A + Ord(C) - Ord('a')]),
          GetVK(C) = VK_A + Ord(C) - Ord('a'));
  WriteLn;

  { Test that lowercase and uppercase produce same VK code }
  WriteLn('-- Case insensitivity tests --');
  Check('GetVK(''a'') = GetVK(''A'')', GetVK('a') = GetVK('A'));
  Check('GetVK(''z'') = GetVK(''Z'')', GetVK('z') = GetVK('Z'));
  for C := 'A' to 'Z' do
    Check(Format('%s and %s produce same VK', [C, Chr(Ord(C) + 32)]),
          GetVK(C) = GetVK(Chr(Ord(C) + 32)));
  WriteLn;

  { Test invalid characters return 0 }
  WriteLn('-- Invalid character tests --');
  CheckEqual('GetVK('' '') = 0 (space)', 0, GetVK(' '));
  CheckEqual('GetVK(''!'') = 0', 0, GetVK('!'));
  CheckEqual('GetVK(''@'') = 0', 0, GetVK('@'));
  CheckEqual('GetVK(''['') = 0', 0, GetVK('['));
  CheckEqual('GetVK(#0) = 0 (null)', 0, GetVK(#0));
  WriteLn;

  { Summary }
  WriteLn('======================');
  WriteLn(Format('Tests passed: %d', [TestsPassed]));
  WriteLn(Format('Tests failed: %d', [TestsFailed]));
  WriteLn;

  if TestsFailed = 0 then
    WriteLn('All tests PASSED!')
  else
  begin
    WriteLn('Some tests FAILED!');
    Halt(1);
  end;
end.
