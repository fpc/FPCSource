{
  Translation of the SAMBA md4 code for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
  Ported from SAMBA/source/lib/md4.c:
}

{
  Unix SMB/CIFS implementation.
  a implementation of MD4 designed for use in the SMB authentication protocol
  Copyright (C) Andrew Tridgell 1997-1998.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit md4;

{$mode objfpc}

interface

type
  TMD4Hash = packed array[0..15] of byte;

// NOTE: _out is expected to be a 16 bytes array
procedure mdfour({out} var _out: TMD4Hash; {in} _in: PByte; {in} n: Integer); overload;
procedure mdfour({out} const _out: PByte; {in} _in: PByte; {in} n: Integer); overload;

implementation

procedure mdfour({out} var _out: TMD4Hash; {in} _in: PByte; {in} n: Integer);
begin
  mdfour(@_out[0], _in, n);
end;


(*
 * produce a md4 message digest from data of length n bytes
 *)

procedure mdfour({out} const _out: PByte; {in} _in: PByte; {in} n: Integer);
var
  A, B, C, D: Longword;


        (* this applies md4 to 64 byte chunks *)
        procedure mdfour64({in} const M: PLongword);

                function F(X, Y, Z: Longword): Longword; inline;
                begin
                  Result := (X and Y) or ((not X) and Z)
                end;

                function G(X, Y, Z: Longword): Longword; inline;
                begin
                  Result := (X and Y) or (X and Z) or (Y and Z);
                end;

                function H(X, Y, Z: Longword): Longword; inline;
                begin
                  Result := X xor Y xor Z;
                end;

                function lshift(X: Longword; S: Integer): Longword; inline;
                begin
                  x := x and $FFFFFFFF;
                  Result := ((x shl s) and $FFFFFFFF) or (x shr (32-s));
                end;

                procedure ROUND1(var a: Longword; b, c, d, k: Longword; s: Integer); inline;
                begin
                  a := lshift(a + F(b,c,d) + M[k], s);
                end;

                procedure ROUND2(var a: Longword; b, c, d, k: Longword; s: Integer); inline;
                begin
                  a := lshift(a + G(b,c,d) + M[k] + $5A827999, s);
                end;

                procedure ROUND3(var a: Longword; b, c, d, k: Longword; s: Integer); inline;
                begin
                  a := lshift(a + H(b,c,d) + M[k] + $6ED9EBA1, s);
                end;

        var
          AA    : Longword;
          BB    : Longword;
          CC    : Longword;
          DD    : Longword;
        begin
          AA := A;
          BB := B;
          CC := C;
          DD := D;

          ROUND1(A,B,C,D,  0,  3);  ROUND1(D,A,B,C,  1,  7);
          ROUND1(C,D,A,B,  2, 11);  ROUND1(B,C,D,A,  3, 19);
          ROUND1(A,B,C,D,  4,  3);  ROUND1(D,A,B,C,  5,  7);
          ROUND1(C,D,A,B,  6, 11);  ROUND1(B,C,D,A,  7, 19);
          ROUND1(A,B,C,D,  8,  3);  ROUND1(D,A,B,C,  9,  7);
          ROUND1(C,D,A,B, 10, 11);  ROUND1(B,C,D,A, 11, 19);
          ROUND1(A,B,C,D, 12,  3);  ROUND1(D,A,B,C, 13,  7);
          ROUND1(C,D,A,B, 14, 11);  ROUND1(B,C,D,A, 15, 19);

          ROUND2(A,B,C,D,  0,  3);  ROUND2(D,A,B,C,  4,  5);
          ROUND2(C,D,A,B,  8,  9);  ROUND2(B,C,D,A, 12, 13);
          ROUND2(A,B,C,D,  1,  3);  ROUND2(D,A,B,C,  5,  5);
          ROUND2(C,D,A,B,  9,  9);  ROUND2(B,C,D,A, 13, 13);
          ROUND2(A,B,C,D,  2,  3);  ROUND2(D,A,B,C,  6,  5);
          ROUND2(C,D,A,B, 10,  9);  ROUND2(B,C,D,A, 14, 13);
          ROUND2(A,B,C,D,  3,  3);  ROUND2(D,A,B,C,  7,  5);
          ROUND2(C,D,A,B, 11,  9);  ROUND2(B,C,D,A, 15, 13);

          ROUND3(A,B,C,D,  0,  3);  ROUND3(D,A,B,C,  8,  9);
          ROUND3(C,D,A,B,  4, 11);  ROUND3(B,C,D,A, 12, 15);
          ROUND3(A,B,C,D,  2,  3);  ROUND3(D,A,B,C, 10,  9);
          ROUND3(C,D,A,B,  6, 11);  ROUND3(B,C,D,A, 14, 15);
          ROUND3(A,B,C,D,  1,  3);  ROUND3(D,A,B,C,  9,  9);
          ROUND3(C,D,A,B,  5, 11);  ROUND3(B,C,D,A, 13, 15);
          ROUND3(A,B,C,D,  3,  3);  ROUND3(D,A,B,C, 11,  9);
          ROUND3(C,D,A,B,  7, 11);  ROUND3(B,C,D,A, 15, 15);

          A := A + AA;
          B := B + BB;
          C := C + CC;
          D := D + DD;
        end;

        procedure copy64({out} const M: PLongword; {in} const _in: PByte);
        var
          i: Integer;
        begin
          for i := 0 to 15 do
            M[i] := (_in[i*4+3] shl 24) or (_in[i*4+2] shl 16) or (_in[i*4+1] shl 8) or (_in[i*4+0] shl 0);
        end;

        procedure copy4({out} const _out: PByte; {in} const x: Longword);
        begin
          _out[0] := x and $FF;
          _out[1] := (x shr 8) and $FF;
          _out[2] := (x shr 16) and $FF;
          _out[3] := (x shr 24) and $FF;
        end;

var
  buf   : array[0..127] of Byte;
  M     : array[0..15] of Longword;
  bb    : Longword;
begin
  bb := n * 8;

  A := $67452301;
  B := $efcdab89;
  C := $98badcfe;
  D := $10325476;

  while n > 64 do
  begin
    copy64(@M[0], _in);
    mdfour64(@M[0]);
    Inc(_in, 64);
    Dec(n, 64);
  end;

  FillChar(buf, sizeof(buf), 0);
  Move(_in[0], buf, n);
  buf[n] := $80;

  if n <= 55 then
  begin
    copy4(@buf[56], bb);
    copy64(@M[0], @buf[0]);
    mdfour64(@M[0]);
  end else begin
    copy4(@buf[120], bb);
    copy64(@M[0], @buf[0]);
    mdfour64(@M[0]);
    copy64(@M[0], @buf[64]);
    mdfour64(@M[0]);
  end;

  FillChar(buf, sizeof(buf), 0);
  copy64(@M[0], @buf[0]);

  copy4(@_out[0], A);
  copy4(@_out[4], B);
  copy4(@_out[8], C);
  copy4(@_out[12], D);
end;

end.
