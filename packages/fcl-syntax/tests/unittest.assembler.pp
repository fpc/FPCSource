{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    pascal embedded assembler highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.assembler;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.pascal;

type
  TTestAsmHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    flags: Byte;
  published
    procedure TestBasicAsmBlock;
    procedure TestAsmWithInstructions;
    procedure TestAsmWithRegisters;
    procedure TestAsmWithComments;
    procedure TestAsmWithDirectives;
    procedure TestAsmMultiline;
    procedure TestEmptyAsm;
  end;

implementation

procedure TTestAsmHighlighter.SetUp;
begin
  flags := 0;
end;

procedure TTestAsmHighlighter.TearDown;
begin
  // Nothing to do
end;

procedure TTestAsmHighlighter.TestBasicAsmBlock;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting( 'asm end');
  AssertTrue('Should have at least 2 tokens', Length(tokens) >= 2);
  AssertEquals('First token should be ASM', 'asm', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
  AssertEquals('Last token should be END', 'END', tokens[High(tokens)].Text);
  AssertEquals('Last token should be keyword', Ord(shKeyword), Ord(tokens[High(tokens)].Kind));
end;

procedure TTestAsmHighlighter.TestAsmWithInstructions;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundAsmToken: Boolean;
begin
  tokens := DoPascalHighlighting( 'asm mov eax, ebx end');
  AssertTrue('Should have multiple tokens', Length(tokens) >= 3);

  // First token should be ASM keyword
  AssertEquals('First token should be ASM', 'asm', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

  // Should have assembler content between asm and end
  foundAsmToken := False;
  for i := 1 to Length(tokens) - 2 do
    begin
    if tokens[i].Kind = shAssembler then
      begin
      foundAsmToken := True;
      break;
      end;
    end;
  AssertTrue('Should contain assembler tokens', foundAsmToken);

  // Last token should be END keyword
  AssertEquals('Last token should be END', 'END', tokens[High(tokens)].Text);
  AssertEquals('Last token should be keyword', Ord(shKeyword), Ord(tokens[High(tokens)].Kind));
end;

procedure TTestAsmHighlighter.TestAsmWithRegisters;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundAsmToken: Boolean;
begin
  tokens := DoPascalHighlighting( 'asm push eax; pop ebx; end');
  AssertTrue('Should have multiple tokens', Length(tokens) >= 3);

  // Should have assembler content
  foundAsmToken := False;
  for i := 1 to Length(tokens) - 2 do
    begin
    if tokens[i].Kind = shAssembler then
      begin
      foundAsmToken := True;
      break;
      end;
    end;
  AssertTrue('Should contain assembler tokens with registers', foundAsmToken);
end;

procedure TTestAsmHighlighter.TestAsmWithComments;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundComment: Boolean;
begin
  tokens := DoPascalHighlighting( 'asm { comment } mov eax, ebx end');
  AssertTrue('Should have multiple tokens', Length(tokens) >= 3);

  // Should have a comment token
  foundComment := False;
  for i := 0 to High(tokens) do
    begin
    if tokens[i].Kind = shComment then
      begin
      foundComment := True;
      AssertEquals('Comment should include braces', '{ comment }', tokens[i].Text);
      break;
      end;
    end;
  AssertTrue('Should contain comment token', foundComment);
end;

procedure TTestAsmHighlighter.TestAsmWithDirectives;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  foundDirective: Boolean;
begin
  tokens := DoPascalHighlighting( 'asm {$ASMMODE INTEL} mov eax, ebx end');
  AssertTrue('Should have multiple tokens', Length(tokens) >= 3);

  // Should have a directive token
  foundDirective := False;
  for i := 0 to High(tokens) do
    begin
    if tokens[i].Kind = shDirective then
      begin
      foundDirective := True;
      AssertEquals('Directive should include braces', '{$ASMMODE INTEL}', tokens[i].Text);
      break;
      end;
    end;
  AssertTrue('Should contain directive token', foundDirective);
end;

procedure TTestAsmHighlighter.TestAsmMultiline;
var
  tokens: TSyntaxTokenArray;
  source: String;
  i: Integer;
  foundAsmToken: Boolean;
begin
  source := 'asm' + #13#10 + '  mov eax, ebx' + #13#10 + '  add eax, 1' + #13#10 + 'end';
  tokens := DoPascalHighlighting( source);
  AssertTrue('Should have multiple tokens', Length(tokens) >= 3);

  // First token should be ASM keyword
  AssertEquals('First token should be ASM', 'asm', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

  // Should have assembler content
  foundAsmToken := False;
  for i := 1 to Length(tokens) - 2 do
    begin
    if tokens[i].Kind = shAssembler then
      begin
      foundAsmToken := True;
      break;
      end;
    end;
  AssertTrue('Should contain assembler tokens in multiline', foundAsmToken);

  // Last token should be END keyword
  AssertEquals('Last token should be END', 'END', tokens[High(tokens)].Text);
  AssertEquals('Last token should be keyword', Ord(shKeyword), Ord(tokens[High(tokens)].Kind));
end;

procedure TTestAsmHighlighter.TestEmptyAsm;
var
  tokens: TSyntaxTokenArray;
begin
  tokens := DoPascalHighlighting( 'asm'#13#10'end');
  AssertTrue('Should have at least 3 tokens', Length(tokens) >= 3);

  // First token should be ASM keyword
  AssertEquals('First token should be ASM', 'asm', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

  // Should have assembler token (whitespace is treated as assembler inside asm block)
  AssertEquals('Second token should be whitespace', #13#10, tokens[1].Text);
  AssertEquals('Second token should be assembler', Ord(shAssembler), Ord(tokens[1].Kind));

  // Last token should be END keyword
  AssertEquals('Last token should be END', 'END', tokens[High(tokens)].Text);
  AssertEquals('Last token should be keyword', Ord(shKeyword), Ord(tokens[High(tokens)].Kind));
end;

initialization
  RegisterTest(TTestAsmHighlighter);
end.
