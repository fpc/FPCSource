{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2014 by the Free Pascal development team

    Tests MD2, MD4 and MD5 hashes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program mdtest;

{$mode objfpc}{$h+}

uses
  SysUtils, md5;

const
  Suite: array[1..7] of string = (
    '',
    'a',
    'abc',
    'message digest',
    'abcdefghijklmnopqrstuvwxyz',
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
    '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
    );

  Results: array[TMDVersion, Low(Suite)..High(Suite)] of string = (
    // MD_VERSION_2
    ('8350e5a3e24c153df2275c9f80692773',
     '32ec01ec4a6dac72c0ab96fb34c0b5d1',
     'da853b0d3f88d99b30283a69e6ded6bb',
     'ab4f496bfb2a530b219ff33031fe06b0',
     '4e8ddff3650292ab5a4108c3aa47940b',
     'da33def2a42df13975352846c30338cd',
     'd5976f79d83d3a0dc9806c3c66f3efd8'),

    // MD_VERSION_4
    ('31d6cfe0d16ae931b73c59d7e0c089c0',
     'bde52cb31de33e46245e05fbdbd6fb24',
     'a448017aaf21d8525fc10ae87aa6729d',
     'd9130a8164549fe818874806e1c7014b',
     'd79e1c308aa5bbcdeea8ed63df412da9',
     '043f8582f241db351ce627e153e7f0e4',
     'e33b4ddc9c38f2199c3e7b164fcc0536'),

    // MD_VERSION_5
    ('d41d8cd98f00b204e9800998ecf8427e',
     '0cc175b9c0f1b6a831c399e269772661',
     '900150983cd24fb0d6963f7d28e17f72',
     'f96b697d7cb7938d525a2f31aaf161d0',
     'c3fcd3d76192e4007dfb496cca67e13b',
     'd174ab98d277d9f5a5611c2c9f419d9f',
     '57edf4a22be3c955ac49da2e2107b67a')
  );

function performTest(const Ver: TMDVersion; const Verbose: boolean): boolean;
// Runs test and returns success or failure
var
  I: Integer;
  S: String;
begin
  result := false;
  for I := Low(Suite) to High(Suite) do
  begin
    S := LowerCase(MDPrint(MDString(Suite[I], Ver)));
    if S = Results[Ver, I] then
      result := true;
    if Verbose then WriteLn('  "', Suite[I], '" = ', S);
  end;
end;

var
  i: integer;
begin
  i:=0;
  Writeln('Executing RFC 1319 test suite ...');
  if performTest(MD_VERSION_2,true) then
    Write('RFC 1319 test suite passed  ')
  else
  begin
    Write('RFC 1319 test suite failed  ');
    i:=i or 1;
  end;
  Writeln;
  Writeln;

  Writeln('Executing RFC 1320 test suite ...');
  if performTest(MD_VERSION_4,true) then
    Write('RFC 1320 test suite passed  ')
  else
  begin
    Write('RFC 1320 test suite failed  ');
    i:=i or 2;
  end;
  Writeln;
  Writeln;

  Writeln('Executing RFC 1321 test suite ...');
  if performTest(MD_VERSION_5,true) then
    Write('RFC 1321 test suite passed  ')
  else
  begin
    Write('RFC 1321 test suite failed  ');
    i:=i or 4;
  end;
  Writeln;
  Writeln;
  halt(i); //halt with error code 0 if everything ok
end.
