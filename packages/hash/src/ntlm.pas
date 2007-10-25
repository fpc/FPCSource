{
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2006 by the Free Pascal development team

    Implements a NTLM password hash algorithm.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ntlm;

{$mode objfpc}

interface

uses
  Math, Strings, md5;


function LMGenerate(const Password: PChar): TMDDigest;
function NTGenerate(const Password: PChar): TMDDigest;

implementation

const
  perm1: array[0..55] of Byte = (
    57, 49, 41, 33, 25, 17,  9,
     1, 58, 50, 42, 34, 26, 18,
    10,  2, 59, 51, 43, 35, 27,
    19, 11,  3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
     7, 62, 54, 46, 38, 30, 22,
    14,  6, 61, 53, 45, 37, 29,
    21, 13,  5, 28, 20, 12,  4);

  perm2: array[0..47] of Byte = (
    14, 17, 11, 24,  1,  5,
     3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8,
    16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32);

  perm3: array[0..63] of Byte = (
    58, 50, 42, 34, 26, 18, 10,  2,
    60, 52, 44, 36, 28, 20, 12,  4,
    62, 54, 46, 38, 30, 22, 14,  6,
    64, 56, 48, 40, 32, 24, 16,  8,
    57, 49, 41, 33, 25, 17,  9,  1,
    59, 51, 43, 35, 27, 19, 11,  3,
    61, 53, 45, 37, 29, 21, 13,  5,
    63, 55, 47, 39, 31, 23, 15,  7);

  perm4: array[0..47] of Byte = (
    32,  1,  2,  3,  4,  5,
     4,  5,  6,  7,  8,  9,
     8,  9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32,  1);

  perm5: array[0..31] of Byte = (
    16,  7, 20, 21,
    29, 12, 28, 17,
     1, 15, 23, 26,
     5, 18, 31, 10,
     2,  8, 24, 14,
    32, 27,  3,  9,
    19, 13, 30,  6,
    22, 11,  4, 25);

  perm6: array[0..63] of Byte = (
    40,  8, 48, 16, 56, 24, 64, 32,
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25);

  sc: array[0..15] of Byte = (1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1);

  sbox: array[0..7, 0..3, 0..15] of Byte = (
    ((14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7),
     (0,  15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8),
     (4,   1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0),
     (15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13)),

    ((15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10),
     (3,  13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5),
     (0,  14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15),
     (13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9)),

    ((10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8),
     (13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1),
     (13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7),
     (1,  10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12)),

    ((7,  13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15),
     (13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9),
     (10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4),
     (3,  15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14)),

    ((2,  12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9),
     (14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6),
     (4,   2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14),
     (11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3)),

    ((12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11),
     (10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8),
     (9,  14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6),
     (4,   3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13)),

    ((4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1),
     (13, 0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6),
     (1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2),
     (6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12)),

    ((13, 2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7),
     (1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2),
     (7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8),
     (2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11)));


procedure permute({out} const _out: PByte; {in} const _in: PByte; {in} const p: PByte; {in} const n: Integer);
var
  i: Integer;
begin
  for i := 0 to n-1 do
    _out[i] := _in[p[i]-1];
end;


procedure lshift({in/out} const d: PByte; {in} const count: Integer; {in} const n: Integer);
var
  _out  : array[0..63] of Byte;
  i     : Integer;
begin
  for i := 0 to n-1 do
    _out[i] := d[(i+count) mod n];
  for i := 0 to n-1 do
    d[i] := _out[i];
end;


procedure concat({out} const _out: PByte; {in} const _in1, _in2: PByte; {in} const l1, l2: Integer);
var
  i: Integer;
begin
  for i := 0 to l1-1 do
    _out[i] := _in1[i];
  for i := 0 to l2-1 do
    _out[i+l1] := _in2[i];
end;


procedure mxor({out} const _out: PByte; {in} const _in1, _in2: PByte; {in} const n: Integer);
var
  i: Integer;
begin
  for i := 0 to n-1 do
    _out[i] := _in1[i] xor _in2[i];
end;


procedure dohash({out} const _out: PByte; {in} const _in: PByte; {in} const key: PByte; {in} const forw: Boolean);
var
  i     : Integer;
  j     : Integer;
  k     : Integer;
  pk1   : array[0..55] of Byte;
  c     : array[0..27] of Byte;
  d     : array[0..27] of Byte;
  cd    : array[0..55] of Byte;
  ki    : array[0..15,0..47] of Byte;
  pd1   : array[0..63] of Byte;
  l     : array[0..31] of Byte;
  r     : array[0..31] of Byte;
  rl    : array[0..63] of Byte;

  er    : array[0..47] of Byte;
  erk   : array[0..47] of Byte;
  b     : array[0..7,0..5] of Byte;
  cb    : array[0..31] of Byte;
  pcb   : array[0..31] of Byte;
  r2    : array[0..31] of Byte;

  m     : Integer;
  n     : Integer;
begin
  permute(@pk1[0], key, @perm1[0], 56);

  for i := 0 to 27 do
  begin
    c[i] := pk1[i];
    d[i] := pk1[i+28];
  end;

  for i := 0 to 15 do
  begin
    lshift(@c[0], sc[i], 28);
    lshift(@d[0], sc[i], 28);

    concat(@cd[0], @c[0], @d[0], 28, 28);
    permute(@ki[i][0], @cd[0], @perm2[0], 48);
  end;

  permute(@pd1[0], _in, @perm3[0], 64);

  for i := 0 to 31 do
  begin
    l[i] := pd1[i];
    r[i] := pd1[i+32];
  end;

  for i := 0 to 15 do
  begin
    permute(@er[0], @r[0], @perm4[0], 48);

    if forw then
      mxor(@erk[0], @er[0], @ki[i][0], 48) else
      mxor(@erk[0], @er[0], @ki[15-i][0], 48);

    for j := 0 to 7 do
      for k := 0 to 5 do
        b[j][k] := erk[j*6 + k];

    for j := 0 to 7 do
    begin
      m := (b[j][0] shl 1) or b[j][5];

      n := (b[j][1] shl 3) or (b[j][2] shl 2) or (b[j][3] shl 1) or (b[j][4]);

      for k := 0 to 3 do
        b[j][k] := min(sbox[j][m][n] and (1 shl (3-k)), 1);  // store binary
    end;

    for j := 0 to 7 do
      for k := 0 to 3 do
        cb[j*4+k] := b[j][k];

    permute(@pcb[0], @cb[0], @perm5[0], 32);

    mxor(@r2[0], @l[0], @pcb[0], 32);

    for j := 0 to 31 do
    begin
      l[j] := r[j];
      r[j] := r2[j];
    end;
  end;

  concat(@rl[0], @r[0], @l[0], 32, 32);

  permute(_out, @rl[0], @perm6[0], 64);
end;


procedure str_to_key({in} const str: PByte; {out} const key: PByte);
var
  i: Integer;
begin
  key[0] := str[0] shr 1;
  key[1] := ((str[0] and $01) shl 6) or (str[1] shr 2);
  key[2] := ((str[1] and $03) shl 5) or (str[2] shr 3);
  key[3] := ((str[2] and $07) shl 4) or (str[3] shr 4);
  key[4] := ((str[3] and $0F) shl 3) or (str[4] shr 5);
  key[5] := ((str[4] and $1F) shl 2) or (str[5] shr 6);
  key[6] := ((str[5] and $3F) shl 1) or (str[6] shr 7);
  key[7] := str[6] and $7F;
  for i := 0 to 7 do
    key[i] := key[i] shl 1;
end;


procedure smbhash({out} const _out: PByte; {in} const _in: PByte; {in} const key: PByte; {in} const forw: Boolean);
var
  i     : Integer;
  outb  : array[0..63] of Byte;
  inb   : array[0..63] of Byte;
  keyb  : array[0..63] of Byte;
  key2  : array[0..7] of Byte;
begin
  str_to_key(key, @key2[0]);

  for i := 0 to 63 do
  begin
    inb[i]  := min( _in[i div 8] and (1 shl (7-(i mod 8))), 1); // store binary
    keyb[i] := min(key2[i div 8] and (1 shl (7-(i mod 8))), 1); // store binary
    outb[i] := 0;
  end;

  dohash(@outb[0], @inb[0], @keyb[0], forw);

  for i := 0 to 7 do
    _out[I] := 0;

  for i := 0 to 63 do
  begin
    if outb[i] <> 0 then
      _out[i div 8] := _out[i div 8] or (1 shl (7-(i mod 8)));
  end;
end;


procedure E_P16({in} const p14: PByte; {out} const p16: PByte);
const
  sp8: array[0..7] of Byte = ($4b, $47, $53, $21, $40, $23, $24, $25);
begin
  smbhash(@p16[0], @sp8[0], @p14[0], True);
  smbhash(@p16[8], @sp8[0], @p14[7], True);
end;


(*procedure E_P24({in} const p21: PByte; {in} const c8: PByte; {out} const p24: PByte);
begin
  smbhash(@p24[0],  c8, @p21[0],  True);
  smbhash(@p24[8],  c8, @p21[7],  True);
  smbhash(@p24[16], c8, @p21[14], True);
end;*)


function LMGenerate(const Password: PChar): TMDDigest;
var
  dospwd: array[0..14] of Byte;
begin
  if not Assigned(Password) then
    Exit;

  FillChar(dospwd, Sizeof(dospwd), 0);

  (* Password must be converted to DOS charset - null terminated, uppercase *)
  StrLCopy(PChar(@dospwd[0]), PChar(@Password[0]), SizeOf(dospwd)-1);
  StrUpper(PChar(@dospwd[0]));

  (* Only the first 14 chars are considered, password need not be null terminated *)
  E_P16(@dospwd[0], @Result);

  FillChar(dospwd, Sizeof(dospwd), 0);
end;


function NTGenerate(const Password: PChar): TMDDigest;
var
  pos: Integer;
  wpwd: array[0..127] of WideChar;
begin
  if not Assigned(Password) then
    Exit;

  pos := 0;
  while (pos < 128) and (Password[pos] <> #0) do
  begin
    wpwd[pos] := Password[pos];
    inc(pos);
  end;

  Result := MDBuffer(wpwd, 2*pos, MD_VERSION_4);
  FillChar(wpwd, Sizeof(wpwd), 0);
end;

end.
