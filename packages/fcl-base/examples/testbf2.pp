{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2023 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  Blowfish 2 test.
  Based on http://pccipher.free.fr/blowfish2/blowfish2.txt
}
program testbf2;

uses blowfish2, blowfish, sysutils;

Procedure Test1;

var
  L,R : QWord;
  ctx : TBlowfish2Context;

begin
 (* Plaintext 128-bit block :$00000000000000010000000000000002 *)

 L := QWord($0000000000000001);
 R := QWord($0000000000000002); // 64 bits L + 64 bits R = 128-bit block


 Writeln(format('Plaintext 128-bit block: %x %x',[L, R]));
 Writeln('Key: TESTKEY');

 Blowfish2_Init (@ctx, 'TESTKEY', 7);
 Blowfish2_Encrypt(@ctx, L, R);

 Writeln('Ciphertext 128-bit block: ');
 Writeln(format('%X %X',[L, R]));

 if (L = QWord($7B2B9DE71D1B1C62)) and (R = QWord($91C230351177BEE8)) then
   Writeln('Test encryption OK.')
 else
   Writeln('Test encryption failed');
 Blowfish2_Decrypt(@ctx, L, R);
 if (L = 1) and (R = 2) then
   Writeln('Test decryption OK.')
 else
   Writeln('Test decryption failed.');
end;

Procedure Test2;

var
  L,R : QWord;
  ctx : TBlowfish2Context;

begin

  (* Plaintext 128-bit block :$01020304050607080910111213141516 *)

  L:=QWord($0102030405060708);
  R:=QWord($0910111213141516);


  Writeln(format('Plaintext 128-bit block : %x %x',[L, R]));

  Writeln('Key: A');

  Blowfish2_Init (@ctx, 'A', 1);
  Blowfish2_Encrypt(@ctx, L, R);

  Writeln('Ciphertext 128-bit block: ');
  Writeln(Format('%x %x', [L, R]));

  if (L = QWord($CA38165603F9915C)) and (R = QWord($61F0776A0F55E807)) then
    Writeln('Test encryption OK.')
  else
    Writeln('Test encryption failed.');
  Blowfish2_Decrypt(@ctx, L,R);
  if (L=QWord($0102030405060708)) and (R = QWord($0910111213141516)) then
    Writeln('Test decryption OK.')
  else
    Writeln('Test decryption failed.');
end;

Procedure Test3;

var
  L,R : QWord;
  ctx : TBlowfish2Context;

begin

(* Plaintext 128-bit block :$01020304050607080910111213141516 *)

 L:=QWord($0102030405060708);
 R:=QWord($0910111213141516);


 Writeln(Format('Plaintext 128-bit block: %x %x',[L, R]));

 Writeln('Key: B');

 Blowfish2_Init(@ctx, 'B', 1);
 Blowfish2_Encrypt(@ctx, L, R);

 Writeln('Ciphertext 128-bit block: ');
 Writeln(format('%x %x', [L, R]));

 if (L = QWord($D07690A78B109983)) and (R = QWord($8DDF85826F2366C2)) then
   Writeln('Test encryption OK.')
 else
   Writeln('Test encryption failed.');
 Blowfish2_Decrypt(@ctx, L, R);
 if (L = QWord($0102030405060708)) and (R = QWord($0910111213141516)) then
   Writeln('Test decryption OK.')
 else
   Writeln('Test decryption failed.');
end;


begin
  Test1;
  Test2;
  Test3;
end.

