{ This file is part of the Free Component Library.

  Elliptic curve crypto hashing
  
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$mode delphi}
unit fpECC;

interface

const
  // ASN.1 OID: 1.2.840.10045.3.1.7 <- secp256r1
  secp256r1 = 32;
  ECC_CURVE = secp256r1; // 32
  ECC_BYTES = ECC_CURVE; // 32
  NUM_ECC_DIGITS = ECC_BYTES div 8; // =4
  MAX_TRIES = 16;

type
  UInt32 = Cardinal;
  {$IFNDEF POSIX}
    Int32 = LongInt;
  {$ENDIF}
  UInt64 = QWord;

  TByteBuffer = array[0..MaxInt-1] of Byte;
  PByteBuffer = ^TByteBuffer;

  TEccPublicKey = array[0..ECC_BYTES] of Byte;
  TEccPrivateKey = array[0..ECC_BYTES-1] of Byte;
  PEccPrivateKey = ^TEccPrivateKey;
  TEccHash = array[0..ECC_BYTES-1] of Byte;
  PEccHash = ^TEccHash;
  TEccSecretKey = array[0..ECC_BYTES-1] of Byte;
  TEccSignature = array[0..2*ECC_BYTES-1] of Byte;

  TVLI = array[0..NUM_ECC_DIGITS-1] of UInt64;
  PVLI = ^TVLI;
  TVLI2 = array[0..(2*NUM_ECC_DIGITS)-1] of UInt64;

  TEccGetRandomNumber = function(out VLI: TVLI): Boolean;

  TPublicKey = TECCPublicKey;
  TPrivateKey = TECCPrivateKey;
  TSignature = TEccSignature;
  PPrivateKey = PEccPrivateKey;


// Util function to work with ASN.1  
procedure EccPublicKeyFromPrivateKey(out PublicKey: TEccPublicKey; const PrivateKey: TEccPrivateKey);
function EccImportX963(Input: PByteBuffer; InputLen: Integer; out PublicKey: TEccPublicKey): Boolean;
function EccImportX963FromXY(InputX: PByteBuffer; InputXLen: Integer; InputY: PByteBuffer; InputYLen: Integer; out PublicKey: TEccPublicKey): Boolean;
function EccExportX963(const PublicKey: TEccPublicKey; Output: PByteBuffer; OutputLen: Integer): Boolean; 
procedure EccPublicKeyFromHexa(out PublicKey: TEccPublicKey; const Hexa: String); overload;
Function EccPublicKeyFromHexa(const Hexa: String) : TEccPublicKey; overload;

// Major functions
function EccMakeKey(out PublicKey: TEccPublicKey; out PrivateKey: TEccPrivateKey): Boolean;
function EcdhSharedSecret(const PublicKey: TEccPublicKey; const PrivateKey: TEccPrivateKey; out Secret: TEccSecretKey): Boolean;
function EcdsaSign(const PrivateKey: TEccPrivateKey; const Hash: TEccHash; out Signature: TSignature): Boolean;
function EcdsaVerify(const PublicKey: TEccPublicKey; const Hash: TEccHash; const Signature: TSignature): Boolean;

var
  EccGetRandomNumber: TEccGetRandomNumber;

implementation

uses sysutils, fphashutils;

type
  UInt128 = record
    m_low: UInt64;
    m_high: UInt64;
  end;
  TEccPoint = record
    x: TVLI;
    y: TVLI;
  end;
  PEccPoint = ^TEccPoint;

const
  Curve_P_32: TVLI = (UInt64($FFFFFFFFFFFFFFFF), $00000000FFFFFFFF, $0000000000000000, UInt64($FFFFFFFF00000001));
  Curve_B_32: TVLI = ($3BCE3C3E27D2604B, $651D06B0CC53B0F6, UInt64($B3EBBD55769886BC), $5AC635D8AA3A93E7);
  Curve_G_32: TEccPoint = (x: (UInt64($F4A13945D898C296), UInt64($77037D812DEB33A0), UInt64($F8BCE6E563A440F2), UInt64($6B17D1F2E12C4247));
                           y: (UInt64($CBB6406837BF51F5), UInt64($2BCE33576B315ECE), UInt64($8EE7EB4A7C0F9E16), UInt64($4FE342E2FE1A7F9B)));
  Curve_N_32: TVLI = (UInt64($F3B9CAC2FC632551), UInt64($BCE6FAADA7179E84), UInt64($FFFFFFFFFFFFFFFF), UInt64($FFFFFFFF00000000));

procedure vli_clear(out VLI: TVLI);
var
  I: Integer;
begin
  for I := 0 to NUM_ECC_DIGITS-1 do
    VLI[I] := 0;
end;

function vli_isZero(const VLI: TVLI): Boolean;
var
  I: Integer;
begin
  for I := 0 to NUM_ECC_DIGITS-1 do
  begin
    if VLI[I] <> 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function vli_testBit(const VLI: TVLI; const Bit: UInt32): Boolean;
begin
  // TODO: Replace with BitsetGet
  Result := VLI[Bit shr 6] and (Int64(1) shl (Bit and 63)) <> 0;
end;

// Counts the number of 64-bit "digits" in VLI
function vli_numDigits(const VLI: TVLI): Integer;
var
  I: Integer;
begin
  for I := NUM_ECC_DIGITS-1 downto 0 do
  begin
    if VLI[I] <> 0 then
    begin
      Result := I+1;
      Exit;
    end;
  end;
  Result := 0;
end;

// Counts the number of bits required for VLI
function vli_numBits(const VLI: TVLI): Integer;
var
  I: Integer;
  Digit: UInt64;
  NumDigits: Integer;
begin
  NumDigits := vli_numDigits(VLI);
  if NumDigits = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Digit := VLI[NumDigits-1];
  I := 0;
  while Digit > 0 do
  begin
    Inc(I);
    Digit := Digit shr 1;
  end;
  Result := (NumDigits-1)*64 + I    
end;

// Sets Dest = Src
procedure vli_set(out Dest: TVLI; const Src: TVLI); overload;
var
  I: Integer;
begin
  for I := 0 to NUM_ECC_DIGITS-1 do
    Dest[I] := Src[I];
end;

procedure vli_set(out Dest: TVLI; const Src: TVLI2); overload;
var
  I: Integer;
begin
  for I := 0 to NUM_ECC_DIGITS-1 do
    Dest[I] := Src[I];
end;

// Returns sign of Left - Right
function vli_cmp(const Left, Right: TVLI): Integer;
var
  I: Integer;
begin
  for I := NUM_ECC_DIGITS-1 downto 0 do
  begin
    if Left[I] > Right[I] then
    begin
      Result := 1;
      Exit;
    end else if Left[I] < Right[I] then
    begin
      Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

// Computes Output = Input shl Shift, returning carry. Can modify in place (if Output == Input). 0 < Shift < 64.
function vli_lshift(var Output: TVLI; const Input: TVLI; const Shift: Integer): UInt64;
var
  I: Integer;
  Temp: UInt64;
begin
  Result := 0;
  for I := 0 to NUM_ECC_DIGITS-1 do
  begin
    Temp := Input[I];
    Output[I] := (Temp shl Shift) or Result;
    Result := Temp shr (64 - Shift);
  end;
end;

// Computes p_vli = p_vli shr 1 
procedure vli_rshift1(var VLI: TVLI);
var
  Carry: UInt64;
  I: Integer;
  Temp: UInt64;
begin
  Carry := 0;
  for I := NUM_ECC_DIGITS-1 downto 0 do
  begin
    Temp := VLI[I];
    VLI[I] := (Temp shr 1) or Carry;
    Carry := Temp shl 63;
  end;
end;

// Computes Output = Left + Right, returning carry. Can modify in place
function vli_add(var Output: TVLI; const Left, Right: TVLI): Int32;
var
  Carry: UInt64;
  I: Integer;
  Sum: UInt64;
begin
  Carry := 0;
  for I := 0 to NUM_ECC_DIGITS-1 do
  begin
    Sum := Left[I] + Right[I] + Carry;
    if Sum <> Left[I] then
    begin
      if Sum < Left[I] then
        Carry := 1
      else
        carry := 0;
    end;
    Output[I] := Sum;
  end;
  Result := Carry;
end;

// Computes Output = Left + Right, returning borrow. Can modify in place.
function vli_sub(out Output: TVLI; const Left, Right: TVLI): Int32;
var
  Borrow: UInt64;
  Diff: UInt64;
  I: Integer;
begin
  Borrow := 0;
  for I := 0 to NUM_ECC_DIGITS-1 do
  begin
    Diff := Left[I] - Right[I] - Borrow;
    if Diff <> Left[I] then
    begin
      if Diff > Left[I] then
        Borrow := 1
      else
        Borrow := 0;
    end;
    Output[I] := Diff;
  end;
  Result := Borrow;
end;

procedure mul_64_64(const Left, Right: UInt64; out Result: UInt128);
var
  a0, a1, b0, b1, m0, m1, m2, m3: UInt64;
begin
  a0 := Left and $FFFFFFFF;
  a1 := Left shr 32;
  b0 := Right and $FFFFFFFF;
  b1 := Right shr 32;
  m0 := a0 * b0;
  m1 := a0 * b1;
  m2 := a1 * b0;
  m3 := a1 * b1;
    
  m2 := m2 + (m0 shr 32);
  m2 := m2+m1;
  if m2 < m1 then
    m3 := m3+UInt64($100000000); // overflow

  Result.m_low := (m0 and $FFFFFFFF) or (m2 shl 32);
  Result.m_high := m3 + (m2 shr 32);
end;

function add_128_128(var a, b: UInt128): UInt128;
begin
  Result.m_low := a.m_low + b.m_low;
  Result.m_high := a.m_high + b.m_high;
  if Result.m_low < a.m_low then
    Inc(Result.m_high);
end;

procedure vli_mult(out Output: TVLI2; const Left, Right: TVLI);
var
  I, K, l_min: Integer;
  Product, r01: UInt128;
  r2: UInt64;
begin
  r01.m_low := 0;
  r01.m_high := 0;
  r2 := 0;
  // Compute each digit of Output in sequence, maintaining the carries.
  for k := 0 to 2*NUM_ECC_DIGITS-2 do
  begin
    if k < NUM_ECC_DIGITS then
      l_min := 0
    else
      l_min := (k + 1) - NUM_ECC_DIGITS;
    for I := l_min to K do
    begin
      if i >= NUM_ECC_DIGITS then
        Break;
      mul_64_64(Left[I], Right[K-I], Product);
      r01 := add_128_128(r01, Product);
      if r01.m_high < Product.m_high then
        Inc(r2);
    end;
    Output[K] := r01.m_low;
    r01.m_low := r01.m_high;
    r01.m_high := r2;
    r2 := 0;
  end;
  Output[NUM_ECC_DIGITS*2-1] := r01.m_low;
end;

procedure vli_square(out Output: TVLI2; const Left: TVLI);
var
  I, K, l_min: Integer;
  Product, r01: UInt128;
  r2: UInt64;
begin
  r01.m_low := 0;
  r01.m_high := 0;
  r2 := 0;
  for K := 0 to 2*NUM_ECC_DIGITS-2 do
  begin
    if K < NUM_ECC_DIGITS then
      l_min := 0
    else
      l_min := (K+1) - NUM_ECC_DIGITS;
    for I := l_min to K do
    begin
      if I > K-I then
        Break;
      mul_64_64(Left[I], Left[K-I], Product);
      if I < K-I then
      begin
        Inc(r2, Product.m_high shr 63);
        Product.m_high := (Product.m_high shl 1) or (Product.m_low shr 63);
        Product.m_low := Product.m_low shl 1;
      end;
      r01 := add_128_128(r01, Product);
      if r01.m_high < Product.m_high then
        Inc(r2);
    end;
    Output[K] := r01.m_low;
    r01.m_low := r01.m_high;
    r01.m_high := r2;
    r2 := 0;
  end;
  Output[NUM_ECC_DIGITS*2-1] := r01.m_low;
end;

// Computes Result = (Left + Right) mod Modulo
//   Assumes that p_left < p_mod and p_right < p_mod, p_result != p_mod.
procedure vli_modAdd(var Output: TVLI; const Left, Right, Modulo: TVLI);
var
  Carry: UInt64;
begin
  Carry := vli_add(Output, Left, Right);
  if (Carry <> 0) or (vli_cmp(Output, Modulo) >= 0) then
  begin // Result > Modulo (Result = Modulo + Remainder), so subtract Modulo to get remainder.
    vli_sub(Output, Output, Modulo);
  end;
end;

// Computes Result = (Left - Right) mod Modulo.
// Assumes that Left < Modulo and Right < Modulo, Result != Modulo
procedure vli_modSub(out Output: TVLI; const Left, Right, Modulo: TVLI);
var
  Borrow: UInt64;
begin
  Borrow := vli_sub(Output, Left, Right);
  if Borrow > 0 then
  begin
    // In this case, Output == -diff == (max int) - diff.
    // Since -x mod d == d - x, we can get the correct result from Output + Modulo (with overflow)
    vli_add(Output, Output, Modulo);
  end;
end;

// Computes Result = Product mod Curve
// from http://www.nsa.gov/ia/_files/nist-routines.pdf 
procedure vli_mmod_fast(out Output: TVLI; var p_product: TVLI2);
var
  Carry: Int32;
  l_tmp: TVLI;
begin
  // t
  vli_set(Output, p_product);
    
  // s1
  l_tmp[0] := 0;
  l_tmp[1] := p_product[5] and $FFFFFFFF00000000;
  l_tmp[2] := p_product[6];
  l_tmp[3] := p_product[7];
  Carry := vli_lshift(l_tmp, l_tmp, 1);
  Carry := Carry + vli_add(Output, Output, l_tmp);
    
  // s2
  l_tmp[1] := p_product[6] shl 32;
  l_tmp[2] := (p_product[6] shr 32) or (p_product[7] shl 32);
  l_tmp[3] := p_product[7] shr 32;
  Carry := Carry + Int32(vli_lshift(l_tmp, l_tmp, 1));
  Carry := Carry + vli_add(Output, Output, l_tmp);
    
  // s3
  l_tmp[0] := p_product[4];
  l_tmp[1] := p_product[5] and $FFFFFFFF;
  l_tmp[2] := 0;
  l_tmp[3] := p_product[7];
  Carry := Carry + vli_add(Output, Output, l_tmp);
    
  // s4
  l_tmp[0] := (p_product[4] shr 32) or (p_product[5] shl 32);
  l_tmp[1] := (p_product[5] shr 32) or (p_product[6] and $FFFFFFFF00000000);
  l_tmp[2] := p_product[7];
  l_tmp[3] := (p_product[6] shr 32) or (p_product[4] shl 32);
  Carry := Carry + vli_add(Output, Output, l_tmp);

  // d1 
  l_tmp[0] := (p_product[5] shr 32) or (p_product[6] shl 32);
  l_tmp[1] := (p_product[6] shr 32);
  l_tmp[2] := 0;
  l_tmp[3] := (p_product[4] and $FFFFFFFF) or (p_product[5] shl 32);
  Carry := Carry - vli_sub(Output, Output, l_tmp);

  // d2
  l_tmp[0] := p_product[6];
  l_tmp[1] := p_product[7];
  l_tmp[2] := 0;
  l_tmp[3] := (p_product[4] shr 32) or (p_product[5] and $FFFFFFFF00000000);
  Carry := Carry - vli_sub(Output, Output, l_tmp);

  // d3
  l_tmp[0] := (p_product[6] shr 32) or (p_product[7] shl 32);
  l_tmp[1] := (p_product[7] shr 32) or (p_product[4] shl 32);
  l_tmp[2] := (p_product[4] shr 32) or (p_product[5] shl 32);
  l_tmp[3] := (p_product[6] shl 32);
  Carry := Carry - vli_sub(Output, Output, l_tmp);
    
  // d4 
  l_tmp[0] := p_product[7];
  l_tmp[1] := p_product[4] and $FFFFFFFF00000000;
  l_tmp[2] := p_product[5];
  l_tmp[3] := p_product[6] and $FFFFFFFF00000000;
  Carry := Carry - vli_sub(Output, Output, l_tmp);
    
  if Carry < 0 then
  begin
    repeat
      Carry := Carry + vli_add(Output, Output, Curve_P_32);
    until not (Carry < 0);
  end else
  begin
    while (Carry <> 0) or (vli_cmp(Curve_P_32, Output) <> 1) do
      Carry := Carry - vli_sub(Output, Output, Curve_P_32);
  end;
end;

// Computes Result = (Left * Right) mod Curve
procedure vli_modMult_fast(out Output: TVLI; const Left, Right: TVLI);
var
  Product: TVLI2;
begin
  vli_mult(Product, Left, Right);
  vli_mmod_fast(Output, Product);
end;

// Computes Result = Left^2 mod Curve
procedure vli_modSquare_fast(out Output: TVLI; const Left: TVLI);
var
  Product: TVLI2;
begin
  vli_square(Product, Left);
  vli_mmod_fast(Output, Product);
end;

// define EVEN(vli) (!(vli[0] & 1))
// Computes Result = (1 / p_input) mod Modulo. All VLIs are the same size.
// See "From Euclid's GCD to Montgomery Multiplication to the Great Divide"
// https://labs.oracle.com/techrep/2001/smli_tr-2001-95.pdf */
procedure vli_modInv(out Output: TVLI; const Input, Modulo: TVLI);
var
  a, b, u, v: TVLI;
  Carry: UInt64;
  Cmp: Integer;
begin
  if vli_isZero(Input) then
  begin
    vli_clear(Output);
    Exit;
  end;
  vli_set(a, Input);
  vli_set(b, Modulo);
  vli_clear(u);
  u[0] := 1;
  vli_clear(v);
  while True do
  begin
    Cmp := vli_cmp(a, b);
    if Cmp = 0 then
      Break;
    Carry := 0;
    if (a[0] and 1) = 0 then
    begin
      vli_rshift1(a);
      if (u[0] and 1) = 1 then 
        Carry := vli_add(u, u, Modulo);
      vli_rshift1(u);
      if Carry <> 0 then
        u[NUM_ECC_DIGITS-1] := u[NUM_ECC_DIGITS-1] or $8000000000000000;
    end else if (b[0] and 1) = 0 then
    begin
      vli_rshift1(b);
      if (v[0] and 1) = 1 then
        Carry := vli_add(v, v, Modulo);
      vli_rshift1(v);
      if Carry <> 0 then
        v[NUM_ECC_DIGITS-1] := v[NUM_ECC_DIGITS-1] or $8000000000000000;
    end else if Cmp > 0 then
    begin
      vli_sub(a, a, b);
      vli_rshift1(a);
      if vli_cmp(u, v) < 0 then
        vli_add(u, u, Modulo);
      vli_sub(u, u, v);
      if (u[0] and 1) = 1 then 
        Carry := vli_add(u, u, Modulo);
      vli_rshift1(u);
      if Carry <> 0 then
        u[NUM_ECC_DIGITS-1] := u[NUM_ECC_DIGITS-1] or $8000000000000000;
    end else
    begin
      vli_sub(b, b, a);
      vli_rshift1(b);
      if vli_cmp(v, u) < 0 then
        vli_add(v, v, Modulo);
      vli_sub(v, v, u);
      if (v[0] and 1) = 1 then
        Carry := vli_add(v, v, Modulo);
      vli_rshift1(v);
      if Carry > 0 then
        v[NUM_ECC_DIGITS-1] := v[NUM_ECC_DIGITS-1] or $8000000000000000;
    end;
  end;
  vli_set(Output, u);
end;

// ------ Point operations ------ 

// Returns 1 if p_point is the point at infinity, 0 otherwise.
function EccPointIsZero(var Point: TEccPoint): Boolean;
begin
  Result := vli_isZero(Point.x) and vli_isZero(Point.y);
end;

// Point multiplication algorithm using Montgomery's ladder with co-Z coordinates.
// From http://eprint.iacr.org/2011/338.pdf

// Double in place
procedure EccPointDoubleJacobian(var X1, Y1, Z1: TVLI);
var
  Carry: UInt64;
  t4, t5: TVLI;
begin
  // t1 = X, t2 = Y, t3 = Z
  if vli_isZero(Z1) then
    Exit;

  vli_modSquare_fast(t4, Y1);   // t4 = y1^2
  vli_modMult_fast(t5, X1, t4); // t5 = x1*y1^2 = A
  vli_modSquare_fast(t4, t4);   // t4 = y1^4
  vli_modMult_fast(Y1, Y1, Z1); // t2 = y1*z1 = z3

  vli_modSquare_fast(Z1, Z1);   // t3 = z1^2

  vli_modAdd(X1, X1, Z1, Curve_P_32); // t1 = x1 + z1^2
  vli_modAdd(Z1, Z1, Z1, Curve_P_32); // t3 = 2*z1^2
  vli_modSub(Z1, X1, Z1, Curve_P_32); // t3 = x1 - z1^2
  vli_modMult_fast(X1, X1, Z1);    // t1 = x1^2 - z1^4

  vli_modAdd(Z1, X1, X1, Curve_P_32); // t3 = 2*(x1^2 - z1^4)
  vli_modAdd(X1, X1, Z1, Curve_P_32); // t1 = 3*(x1^2 - z1^4)
  if vli_testBit(X1, 0) then
  begin
    Carry := vli_add(X1, X1, Curve_P_32);
    vli_rshift1(X1);
    X1[NUM_ECC_DIGITS-1] := X1[NUM_ECC_DIGITS-1] or (Carry shl 63);
  end else
    vli_rshift1(X1);
  // t1 = 3/2*(x1^2 - z1^4) = B
  vli_modSquare_fast(Z1, X1);         // t3 = B^2
  vli_modSub(Z1, Z1, t5, Curve_P_32); // t3 = B^2 - A
  vli_modSub(Z1, Z1, t5, Curve_P_32); // t3 = B^2 - 2A = x3
  vli_modSub(t5, t5, Z1, Curve_P_32); // t5 = A - x3
  vli_modMult_fast(X1, X1, t5);       // t1 = B * (A - x3) 
  vli_modSub(t4, X1, t4, Curve_P_32); // t4 = B * (A - x3) - y1^4 = y3 
    
  vli_set(X1, Z1);
  vli_set(Z1, Y1);
  vli_set(Y1, t4);
end;

// Modify (x1, y1) => (x1 * z^2, y1 * z^3) 
procedure apply_z(var X1, Y1, Z: TVLI);
var
  t1: TVLI;
begin
  vli_modSquare_fast(t1, Z);    // z^2
  vli_modMult_fast(X1, X1, t1); // x1 * z^2
  vli_modMult_fast(t1, t1, Z);  // z^3
  vli_modMult_fast(Y1, Y1, t1); // y1 * z^3
end;

// P = (x1, y1) => 2P, (x2, y2) => P'
procedure XYcZ_initial_double(var X1, Y1, X2, Y2: TVLI; InitialZ: PVLI);
var
  z: TVLI;
begin
  vli_set(X2, X1);
  vli_set(Y2, Y1);
  vli_clear(z);
  z[0] := 1;
  if InitialZ <> nil then
    vli_set(z, InitialZ^);
  apply_z(X1, Y1, z);
  EccPointDoubleJacobian(X1, Y1, z);
  apply_z(X2, Y2, z);
end;

// Input P = (x1, y1, Z), Q = (x2, y2, Z)
// Output P' = (x1', y1', Z3), P + Q = (x3, y3, Z3)
// or P => P', Q => P + Q
procedure XYcZ_add(var X1, Y1, X2, Y2: TVLI);
var
  t5: TVLI;
begin
  // t1 = X1, t2 = Y1, t3 = X2, t4 = Y2
  vli_modSub(t5, X2, X1, Curve_P_32); // t5 = x2 - x1
  vli_modSquare_fast(t5, t5);         // t5 = (x2 - x1)^2 = A
  vli_modMult_fast(X1, X1, t5);       // t1 = x1*A = B
  vli_modMult_fast(X2, X2, t5);       // t3 = x2*A = C
  vli_modSub(Y2, Y2, Y1, Curve_P_32); // t4 = y2 - y1
  vli_modSquare_fast(t5, Y2);         // t5 = (y2 - y1)^2 = D

  vli_modSub(t5, t5, X1, Curve_P_32); // t5 = D - B
  vli_modSub(t5, t5, X2, Curve_P_32); // t5 = D - B - C = x3
  vli_modSub(X2, X2, X1, Curve_P_32); // t3 = C - B
  vli_modMult_fast(Y1, Y1, X2);       // t2 = y1*(C - B)
  vli_modSub(X2, X1, t5, Curve_P_32); // t3 = B - x3
  vli_modMult_fast(Y2, Y2, X2);       // t4 = (y2 - y1)*(B - x3)
  vli_modSub(Y2, Y2, Y1, Curve_P_32); // t4 = y3
    
  vli_set(X2, t5);
end;

// Input P = (x1, y1, Z), Q = (x2, y2, Z)
//  Output P + Q = (x3, y3, Z3), P - Q = (x3', y3', Z3)
//  or P => P - Q, Q => P + Q
procedure XYcZ_addC(var X1, Y1, X2, Y2: TVLI);
var
  t5, t6, t7: TVLI;
begin
  // t1 = X1, t2 = Y1, t3 = X2, t4 = Y2
  vli_modSub(t5, X2, X1, Curve_P_32); // t5 = x2 - x1
  vli_modSquare_fast(t5, t5);         // t5 = (x2 - x1)^2 = A
  vli_modMult_fast(X1, X1, t5);       // t1 = x1*A = B
  vli_modMult_fast(X2, X2, t5);       // t3 = x2*A = C
  vli_modAdd(t5, Y2, Y1, Curve_P_32); // t4 = y2 + y1
  vli_modSub(Y2, Y2, Y1, Curve_P_32); // t4 = y2 - y1

  vli_modSub(t6, X2, X1, Curve_P_32); // t6 = C - B
  vli_modMult_fast(Y1, Y1, t6);       // t2 = y1 * (C - B)
  vli_modAdd(t6, X1, X2, Curve_P_32); // t6 = B + C
  vli_modSquare_fast(X2, Y2);         // t3 = (y2 - y1)^2
  vli_modSub(X2, X2, t6, Curve_P_32); // t3 = x3

  vli_modSub(t7, X1, X2, Curve_P_32); // t7 = B - x3
  vli_modMult_fast(Y2, Y2, t7);       // t4 = (y2 - y1)*(B - x3)
  vli_modSub(Y2, Y2, Y1, Curve_P_32); // t4 = y3

  vli_modSquare_fast(t7, t5);         // t7 = (y2 + y1)^2 = F
  vli_modSub(t7, t7, t6, Curve_P_32); // t7 = x3'
  vli_modSub(t6, t7, X1, Curve_P_32); // t6 = x3' - B
  vli_modMult_fast(t6, t6, t5);       // t6 = (y2 + y1)*(x3' - B)
  vli_modSub(Y1, t6, Y1, Curve_P_32); // t2 = y3' 

  vli_set(X1, t7);
end;

procedure EccPointMult(out Output: TEccPoint; const Point: TEccPoint; Scalar: TVLI; InitialZ: PVLI);
var
  Rx, Ry: array[0..1] of TVLI;
  z: TVLI;
  i, nb: Integer;
begin
  // R0 and R1
  vli_set(Rx[1], Point.x);
  vli_set(Ry[1], Point.y);
  XYcZ_initial_double(Rx[1], Ry[1], Rx[0], Ry[0], InitialZ);
  for I := vli_numBits(Scalar)-2 downto 1 do
  begin
    if vli_testBit(Scalar, i) then
      nb := 0
    else
      nb := 1;
    XYcZ_addC(Rx[1-nb], Ry[1-nb], Rx[nb], Ry[nb]);
    XYcZ_add(Rx[nb], Ry[nb], Rx[1-nb], Ry[1-nb]);
  end;
  if vli_testBit(Scalar, 0) then
    nb := 0
  else
    nb := 1;
  XYcZ_addC(Rx[1-nb], Ry[1-nb], Rx[nb], Ry[nb]);
  // Find final 1/Z value. 
  vli_modSub(z, Rx[1], Rx[0], Curve_P_32);  // X1 - X0
  vli_modMult_fast(z, z, Ry[1-nb]);         // Yb * (X1 - X0)
  vli_modMult_fast(z, z, Point.x);          // xP * Yb * (X1 - X0)
  vli_modInv(z, z, Curve_P_32);             // 1 / (xP * Yb * (X1 - X0))
  vli_modMult_fast(z, z, Point.y);          // yP / (xP * Yb * (X1 - X0))
  vli_modMult_fast(z, z, Rx[1-nb]);         // Xb * yP / (xP * Yb * (X1 - X0))
  // End 1/Z calculation 
  XYcZ_add(Rx[nb], Ry[nb], Rx[1-nb], Ry[1-nb]);
  apply_z(Rx[0], Ry[0], z);
  vli_set(Output.x, Rx[0]);
  vli_set(Output.y, Ry[0]);
end;

procedure EccBytes2Native(out Native: TVLI; Bytes: PByteBuffer);
var
  I: Integer;
  Digits: PByteBuffer;
begin
  for I := 0 to NUM_ECC_DIGITS-1 do
  begin
    Digits := @Bytes[8*(NUM_ECC_DIGITS-1-I)];
    Native[I] := (UInt64(Digits^[0]) shl 56) or (UInt64(Digits^[1]) shl 48) or (UInt64(Digits^[2]) shl 40) or (UInt64(Digits^[3]) shl 32)
            or (UInt64(Digits^[4]) shl 24) or (UInt64(Digits^[5]) shl 16) or (UInt64(Digits^[6]) shl 8) or UInt64(Digits^[7]);
  end;
end;

procedure EccNative2Bytes(Bytes: PByteBuffer; const Native: TVLI);
var
  I: Integer;
  Digits: PByteBuffer;
  V: UInt64;
begin
  for I := 0 to NUM_ECC_DIGITS-1 do
  begin
    V := Native[I];
    Digits := @Bytes[8*(NUM_ECC_DIGITS-1-I)];
    Digits^[0] := V shr 56;
    Digits^[1] := V shr 48;
    Digits^[2] := V shr 40;
    Digits^[3] := V shr 32;
    Digits^[4] := V shr 24;
    Digits^[5] := V shr 16;
    Digits^[6] := V shr 8;
    Digits^[7] := V;
  end;
end;

// Compute a = sqrt(a) (mod curve_p). 
procedure ModSqrt(var a: TVLI);
var
  I: Integer;
  p1, Result: TVLI;
begin
  vli_clear(p1);
  p1[0] := 1;
  vli_clear(Result);
  Result[0] := 1;
  // Since curve_p == 3 (mod 4) for all supported curves, we can compute sqrt(a) = a^((curve_p + 1) / 4) (mod curve_p).
  vli_add(p1, Curve_P_32, p1); // p1 = curve_p + 1
  for I := vli_numBits(p1)-1 downto 2 do
  begin
    vli_modSquare_fast(Result, Result);
    if vli_testBit(p1, i) then
      vli_modMult_fast(Result, Result, a);
  end;
  vli_set(a, Result);
end;

// 04
// b4042c80e6fba6fd065d3b62b12e5c9e012ff601550d1dbedf4d406189c4842
// 27495873cd6bd62ba2ebdc43d77bfccab3bc64bd025cfad1264d086dea7e9
// -> 03b4042c80e6fba6fd065d3b62b12e5c9e012ff601550d1dbedf4d406189c4842
procedure EccPointCompress(const PublicKey: PByteBuffer; Compressed: PByteBuffer);
var
  I: Integer;
begin
  for I := 0 to ECC_BYTES-1 do
    Compressed^[I+1] := PublicKey^[I];
  Compressed^[0] := 2 + (PublicKey^[ECC_BYTES*2-1] and $01); // 2 + (9 and 1) -> 2 + 1 -> 3
end;

procedure EccPointDecompress(out Point: TEccPoint; Compressed: PByteBuffer);
var
  _3: TVLI;
begin
  vli_clear(_3);
  _3[0] := 3; // -a = 3
  EccBytes2Native(Point.x, @Compressed[1]);
  vli_modSquare_fast(Point.y, Point.x);           // y = x^2
  vli_modSub(Point.y, Point.y, _3, Curve_P_32);          // y = x^2 - 3
  vli_modMult_fast(Point.y, Point.y, Point.x);           // y = x^3 - 3x
  vli_modAdd(Point.y, Point.y, Curve_B_32, Curve_P_32);  // y = x^3 - 3x + b
  ModSqrt(Point.y);
  if (Point.y[0] and $01) <> (Compressed^[0] and $01) then
    vli_sub(Point.y, Curve_P_32, Point.y);
end;

function EccImportX963(Input: PByteBuffer; InputLen: Integer; out PublicKey: TEccPublicKey): Boolean; //TLS_1_2
begin
  Result := False;
  if Input = nil then
    Exit;
  if InputLen < 1 + 2*ECC_BYTES then
    Exit;
  if (InputLen and 1) = 0 then // Check Length (Must be Odd)
    Exit;
  if Input^[0] <> 4 then // Check for 4 (Uncompressed)
    Exit;
  EccPointCompress(@Input[1], @PublicKey);
  Result := True;
end;

function EccImportX963FromXY(InputX: PByteBuffer; InputXLen: Integer; InputY: PByteBuffer; InputYLen: Integer; out PublicKey: TEccPublicKey): Boolean;
var
  Compressed: PByteBuffer;
  I: Integer;
begin
  Compressed := @PublicKey;
  for I := 0 to ECC_BYTES-1 do
    Compressed^[I+1] := InputX^[I];
  Compressed^[0] := 2 + (InputY^[ECC_BYTES-1] and $01);
  Result := True;
end;

// @Output[65] in case secp256r1
function EccExportX963(const PublicKey: TEccPublicKey; Output: PByteBuffer; OutputLen: Integer): Boolean; //TLS_1_2
var
  PublicPoint: TEccPoint;
begin
  Result := False;
  if Output = nil then
    Exit;
  if OutputLen < 1 + 2*ECC_BYTES then
    Exit;
  EccPointDecompress(PublicPoint, @PublicKey);
  EccNative2Bytes(@Output[1], PublicPoint.x);
  EccNative2Bytes(@Output[1 + SizeOf(PublicPoint.x)], PublicPoint.y);
  Output^[0] := $04; // Mark as Uncompressed
  Result := True;
end;

procedure EccPublicKeyFromPrivateKey(out PublicKey: TEccPublicKey; const PrivateKey: TEccPrivateKey);

var
  PrivateK: TVLI;
  PublicPoint: TEccPoint;

begin
  EccBytes2Native(PrivateK, @PrivateKey);
  EccPointMult(PublicPoint, Curve_G_32, PrivateK, nil);
  EccNative2Bytes(@PublicKey[1], PublicPoint.x);
  PublicKey[0] := 2 + (PublicPoint.y[0] and $01);
end;

function EccMakeKey(out PublicKey: TEccPublicKey; out PrivateKey: TEccPrivateKey): Boolean;

var
  PrivateK: TVLI;
  PublicPoint: TEccPoint;
  Tries: Integer;

begin
  Result := False;
  Tries := 0;
  repeat
    Inc(Tries);
    if not EccGetRandomNumber(PrivateK) or (Tries >= MAX_TRIES) then
      Exit;
    if vli_isZero(PrivateK) then
      Continue;
    // Make sure the private key is in the range [1, n-1]
    // For the supported curves, n is always large enough that we only need to subtract once at most
    if vli_cmp(Curve_N_32, PrivateK) <> 1 then
      vli_sub(PrivateK, PrivateK, Curve_N_32);
    EccPointMult(PublicPoint, Curve_G_32, PrivateK, nil);
  until not EccPointIsZero(PublicPoint);
  EccNative2Bytes(@PrivateKey, PrivateK);
  EccNative2Bytes(@PublicKey[1], PublicPoint.x);
  PublicKey[0] := 2 + (PublicPoint.y[0] and $01);
  Result := True;
end;

function EcdhSharedSecret(const PublicKey: TEccPublicKey; const PrivateKey: TEccPrivateKey; out Secret: TEccSecretKey): Boolean;

var
  PublicPoint: TEccPoint;
  PrivateK: TVLI;
  Product: TEccPoint;
  RandomKey: TVLI;

begin
  Result := False;
  if not EccGetRandomNumber(RandomKey) then
    Exit;
  EccPointDecompress(PublicPoint, @PublicKey);
  EccBytes2Native(PrivateK, @PrivateKey);
  EccPointMult(Product, PublicPoint, PrivateK, @RandomKey);
  EccNative2Bytes(@Secret, Product.x);
  Result := not EccPointIsZero(Product);
end;

// -------- ECDSA code -------- 

// Computes Result = (Left * Right) mod Modulo
procedure vli_modMult(out Output: TVLI; const Left, Right, Modulo: TVLI);

var
  Carry: UInt64;
  Cmp: Integer; 
  ModMultiple, Product: TVLI2;
  ModMultipleVLI_Lo, ModMultipleVLI_Hi, ProductVLI_Lo, ProductVLI_Hi: PVLI;
  DigitShift, BitShift, ProductBits, ModBits: Integer;
  VLI: PVLI;

begin
  ModBits := vli_numBits(Modulo);
  vli_mult(Product, Left, Right);
  ProductVLI_Hi := @Product[NUM_ECC_DIGITS];
  ProductVLI_Lo := @Product;
  ProductBits := vli_numBits(ProductVLI_Hi^);
  if ProductBits > 0 then
    ProductBits := ProductBits + NUM_ECC_DIGITS*64
  else
    ProductBits := vli_numBits(ProductVLI_Lo^);
  if ProductBits < ModBits then
  begin // l_product < p_mod.
    vli_set(Output, Product);
    Exit;
  end;
  // Shift p_mod by (LeftBits - ModBits). This multiplies p_mod by the largest power of two possible while still resulting in a number less than p_left.
  ModMultipleVLI_Lo := @ModMultiple;
  ModMultipleVLI_Hi := @ModMultiple[NUM_ECC_DIGITS];
  vli_clear(ModMultipleVLI_Lo^);
  vli_clear(ModMultipleVLI_Hi^);
  DigitShift := (ProductBits-ModBits) div 64;
  BitShift := (ProductBits-ModBits) mod 64;
  if BitShift > 0 then
  begin
    VLI := @ModMultiple[DigitShift];
    ModMultiple[DigitShift+NUM_ECC_DIGITS] := vli_lshift(VLI^, Modulo, BitShift)
  end else
  begin
    VLI := @ModMultiple[DigitShift];
    vli_set(VLI^, Modulo);
  end;
  // Subtract all multiples of Modulo to get the remainder
  vli_clear(Output);
  Output[0] := 1; // Use Output as a temp var to store 1 (for subtraction)
  while (ProductBits > NUM_ECC_DIGITS*64) or (vli_cmp(ModMultipleVLI_Lo^, Modulo) >= 0) do
  begin
    Cmp := vli_cmp(ModMultipleVLI_Hi^, ProductVLI_Hi^);
    if (Cmp < 0) or ((Cmp = 0) and (vli_cmp(ModMultipleVLI_Lo^, ProductVLI_Lo^) <= 0)) then
    begin
      if vli_sub(ProductVLI_Lo^, ProductVLI_Lo^, ModMultipleVLI_Lo^) > 0 then
        vli_sub(ProductVLI_Hi^, ProductVLI_Hi^, Output); // borrow
      vli_sub(ProductVLI_Hi^, ProductVLI_Hi^, ModMultipleVLI_Hi^);
    end;
    Carry := (ModMultiple[NUM_ECC_DIGITS] and $01) shl 63;
    vli_rshift1(ModMultipleVLI_Hi^);
    vli_rshift1(ModMultipleVLI_Lo^);
    if Carry > 0 then
      ModMultiple[NUM_ECC_DIGITS-1] := ModMultiple[NUM_ECC_DIGITS-1] or Carry;
    Dec(ProductBits);
  end;
  vli_set(Output, Product);
end;

function Max(const A, B: UInt32): UInt32; inline;

begin
  if A > b then
    Result := A
  else                                         
    Result := B;
end;

function EcdsaSign(const PrivateKey: TEccPrivateKey; const Hash: TEccHash; out Signature: TSignature): Boolean;

var
  K, Temp, S: TVLI;
  P: TEccPoint;
  Tries: Integer;

begin
  Result := False;
  Tries := 0;
  repeat
    Inc(Tries);
    if not EccGetRandomNumber(k) or (Tries >= MAX_TRIES) then
      Exit;
    if vli_isZero(k) then
      Continue;
    if vli_cmp(Curve_N_32, k) <> 1 then
      vli_sub(k, k, Curve_N_32);
    // Temp = k * G
    EccPointMult(P, Curve_G_32, k, nil);
    // r = x1 (mod n) 
    if vli_cmp(Curve_N_32, P.x) <> 1 then
      vli_sub(P.x, P.x, Curve_N_32);
  until not vli_isZero(p.x);
  EccNative2Bytes(@Signature, P.x);
  EccBytes2Native(Temp, @PrivateKey);
  vli_modMult(S, P.x, Temp, Curve_N_32); // s = r*d
  EccBytes2Native(Temp, @Hash);
  vli_modAdd(S, Temp, S, Curve_N_32); // s = e + r*d
  vli_modInv(k, k, Curve_N_32); // k = 1 / k 
  vli_modMult(S, S, k, Curve_N_32); // s = (e + r*d) / k
  EccNative2Bytes(@Signature[ECC_BYTES], S);
  Result := True;
end;

type
  TEccPoints4 = array[0..3] of PEccPoint;

function EcdsaVerify(const PublicKey: TEccPublicKey; const Hash: TEccHash; const Signature: TSignature): Boolean;

var
  I, Index, NumBits: Integer;
  PublicPoint, SumPoint: TEccPoint;
  Point: PEccPoint;
  Points: TEccPoints4;
  rx, ry, tx, ty, tz, l_r, l_s, u1, u2, z: TVLI;

begin
  Result := False;
  EccPointDecompress(PublicPoint, @PublicKey);
  EccBytes2Native(l_r, @Signature);
  EccBytes2Native(l_s, @Signature[ECC_BYTES]);
  if vli_isZero(l_r) or vli_isZero(l_s) then
    Exit; // r, s must not be 0
  if (vli_cmp(Curve_N_32, l_r) <> 1) or (vli_cmp(Curve_N_32, l_s) <> 1) then
    Exit; // r, s must be < n
  // Calculate u1 and u2
  vli_modInv(z, l_s, Curve_N_32); // Z = s^-1 
  EccBytes2Native(u1, @Hash);
  vli_modMult(u1, u1, z, Curve_N_32); // u1 = e/s
  vli_modMult(u2, l_r, z, Curve_N_32); // u2 = r/s
  // Calculate l_sum = G + Q. 
  vli_set(SumPoint.x, PublicPoint.x);
  vli_set(SumPoint.y, PublicPoint.y);
  vli_set(tx, Curve_G_32.x);
  vli_set(ty, Curve_G_32.y);
  vli_modSub(z, SumPoint.x, tx, Curve_P_32); // Z = x2 - x1
  XYcZ_add(tx, ty, SumPoint.x, SumPoint.y);
  vli_modInv(z, z, Curve_P_32); // Z = 1/Z 
  apply_z(SumPoint.x, SumPoint.y, z);
  // Use Shamir's trick to calculate u1*G + u2*Q 
  Points[0] := nil;
  Points[1] := @Curve_G_32;
  Points[2] := @PublicPoint;
  Points[3] := @SumPoint;
  NumBits := Max(vli_numBits(u1), vli_numBits(u2));
  if vli_testBit(u1, NumBits-1) then
    Index := 1
  else
    Index := 0;
  if vli_testBit(u2, NumBits-1) then
    Inc(Index, 2);
  Point := Points[Index];
  vli_set(rx, Point^.x);
  vli_set(ry, Point^.y);
  vli_clear(z);
  z[0] := 1;
  for I := NumBits-2 downto 0 do
  begin
    EccPointDoubleJacobian(rx, ry, z);
    if vli_testBit(u1, I) then
      Index := 1
    else
      Index := 0;
    if vli_testBit(u2, i) then
      Inc(Index, 2);
    Point := Points[Index];
    if Point <> nil then
    begin
      vli_set(tx, Point^.x);
      vli_set(ty, Point^.y);
      apply_z(tx, ty, z);
      vli_modSub(tz, rx, tx, Curve_P_32); // Z = x2 - x1
      XYcZ_add(tx, ty, rx, ry);
      vli_modMult_fast(z, z, tz);
    end;
  end;
  vli_modInv(z, z, Curve_P_32); // Z = 1/Z
  apply_z(rx, ry, z);
  // v = x1 (mod n)
  if vli_cmp(Curve_N_32, rx) <> 1 then
    vli_sub(rx, rx, Curve_N_32);
  Result := vli_cmp(rx, l_r) = 0; // Accept only if v == r
end;

function IntEccGetRandomNumber(out VLI: TVLI): Boolean;

Var
  P : PByte;
  i : Integer;

begin
  if (Pointer(@GetRandomBytes)=Nil) or not GetRandomBytes(@VLI,Sizeof(VLI)) then
    begin
    P:=@VLI;
    For I:=0 to SizeOf(TVLI)-1 do
      begin
      P^:=Random(256);
      Inc(P);
      end;
    Result:=True;
    end;
end;

Function EccPublicKeyFromHexa(const Hexa: String) : TEccPublicKey;

begin
  EccPublicKeyFromHexa(Result,Hexa);
end;

procedure EccPublicKeyFromHexa(out PublicKey: TEccPublicKey; const Hexa: String);

var
  B: Byte;
  HLength: Integer;
  SHexa: String;
  Buf : TBytes;

begin
  hLength:=Length(Hexa);
  if hLength= 66 then
  begin
    // _XString(S, @PublicKey, SizeOf(PublicKey));
    Buf:=HexStrToBytes(Hexa);
    if Length(Buf)<SizeOf(PublicKey) then
      raise Exception.CreateFmt('EccPublicKeyFromHexa - Invalid length: %d', [Length(buf)]);
    Move(Buf[0],PublicKey,Sizeof(PublicKey)) // Compressed public key
  end else
  begin // uncompressed public key as stored in PEM ASN.1
    if hLength = 2+2*(2*ECC_BYTES) then
      SHexa:=Copy(Hexa, 3, ECC_BYTES*2) // skip the leading $04 for uncompressed publickey
    else if hLength <> 2*(2*ECC_BYTES) then
      raise Exception.CreateFmt('EccPublicKeyFromHexa - Invalid length: %d', [hLength])
    else
      SHexa:=Hexa;
    Buf:=HexStrToBytes(SHexa);
    B:=StrToInt('$'+Copy(Hexa,hLength-1,2));
    PublicKey[0] := 2 + (B and $01);
    Move(Buf[0],PublicKey[1],SizeOf(TEccPublicKey)-1);
  end;
end;


initialization
  EccGetRandomNumber:=@IntEccGetRandomNumber;

finalization

end.


