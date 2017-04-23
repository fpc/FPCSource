{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    This is the most basic unit from NumLib.
    The most important items this unit defines are matrix types and machine
    constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
In the FPC revision, instead of picking a certain floating point type,
 a new type "ArbFloat" is defined, which is used as floating point type
 throughout the entire library. If you change the floating point type, you
 should only have to change ArbFloat, and the machineconstants belonging to
 the type you want.
 However for IEEE Double (64bit) and Extended(80bit) these constants are
 already defined, and autoselected by the library. (the library tests the
 size of the float type in bytes for 8 and 10 and picks the appropiate
 constants

Also some stuff had to be added to get ipf running (vector object and
complex.inp and scale methods)
 }

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit typ;

{$I DIRECT.INC}                 {Contains "global" compilerswitches which
                                  are imported into every unit of the library }

{$DEFINE ArbExtended}

interface

uses
  Math;


CONST numlib_version=2;         {used to detect version conflicts between
                                  header unit and dll}
      highestelement=20000;     {Maximal n x m dimensions of matrix.
                                 +/- highestelement*SIZEOF(arbfloat) is
                                  minimal size of matrix.}
type {Definition of base types}
{$IFDEF ArbExtended}
      ArbFloat    = extended;
{$ELSE}
     ArbFloat    = double;
{$ENDIF}
     ArbInt      = LONGINT;
     ArbString   = AnsiString;

     Float8Arb  =ARRAY[0..7] OF BYTE;
     Float10Arb =ARRAY[0..9] OF BYTE;

CONST {Some constants for the variables below, in binary formats.}
{$IFNDEF ArbExtended}
        {First for REAL/Double}
    TC1 :  Float8Arb  = ($00,$00,$00,$00,$00,$00,$B0,$3C);
    TC2 :  Float8Arb  = ($FF,$FF,$FF,$FF,$FF,$FF,$EF,$7F);
    TC3 :  Float8Arb  = ($00,$00,$00,$00,$01,$00,$10,$00);
    TC5 :  Float8Arb  = ($EF,$39,$FA,$FE,$42,$2E,$86,$40);
    TC6 :  Float8Arb  = ($D6,$BC,$FA,$BC,$2B,$23,$86,$C0);
{$ENDIF}

     {For Extended}
{$IFDEF ArbExtended}
    TC1 : Float10Arb = (0,0,$00,$00,$00,$00,0,128,192,63);         {Eps}
    TC2 : Float10Arb = ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$D6,$FE,127);  {9.99188560553925115E+4931}
    TC3 : Float10Arb = (1,0,0,0,0,0,0,0,0,0);                      {3.64519953188247460E-4951}
    TC5 : Float10Arb = (18,25,219,91,61,101,113,177,12,64);        {1.13563488668777920E+0004}
    TC6 : Float10Arb = (108,115,3,170,182,56,27,178,12,192);       {-1.13988053843083006E+0004}
{$ENDIF}
  { numdig  is the number of useful (safe) decimal places of an "ArbFloat"
            for display.
    minform is the number of decimal places shown by the rtls
            write(x:ArbFloat)
    maxform is the maximal number of decimal positions
    }

    numdig    = 25;
    minform   = 10;
    maxform   = 26;

var
    macheps  : ArbFloat absolute TC1;  { macheps = r - 1,  with r
                                        the smallest ArbFloat > 1}
    giant    : ArbFloat absolute TC2;  { the largest ArbFloat}
    midget   : ArbFloat absolute TC3;  { the smallest positive ArbFloat}
    LnGiant  : ArbFloat absolute TC5;  {ln of giant}
    LnMidget : ArbFloat absolute TC6;  {ln of midget}

{Copied from Det. Needs ArbExtended conditional}
const               {  og = 8^-maxexp, ogý>=midget,
                       bg = 8^maxexp,  bgý<=giant

                       midget and giant are defined in typ.pas}

{$IFDEF ArbExtended}
    ogx: Float10Arb = (51,158,223,249,51,243,4,181,224,31);
    bgx: Float10Arb = (108,119,117,92,70,38,155,234,254,95);
    maxexpx : ArbInt = 2740;
{$ELSE}
    ogx: Float8Arb= (84, 254, 32, 128, 32, 0, 0, 32);
    bgx: Float8Arb= (149, 255, 255, 255, 255, 255, 239, 95);
    maxexpx : ArbInt = 170;
{$ENDIF}

  var
    og          : ArbFloat absolute ogx;
    bg          : ArbFloat absolute bgx;
    MaxExp      : ArbInt   absolute maxexpx;


{Like standard EXP(), but for very small values (near lowest possible
      ArbFloat this version returns 0}
Function exp(x: ArbFloat): ArbFloat;

type
     Complex  = object
       { Crude complex record. For me an example of
         useless OOP, specially if you have operator overloading
       }
                   xreal, imag : ArbFloat;
                   procedure Init (r, i: ArbFloat);
                   procedure Add  (c: complex);
                   procedure Sub  (c: complex);
                   function  Inp(z:complex):ArbFloat;
                   procedure Conjugate;
                   procedure Scale(s: ArbFloat);
                   Function  Norm  : ArbFloat;
                   Function  Size  : ArbFloat;
                   Function  Re    : ArbFloat;
                   procedure Unary;
                   Function  Im    : ArbFloat;
                   Function  Arg   : ArbFloat;
                   procedure MinC(c: complex);
                   procedure MaxC(c: complex);
                   Procedure TransF(var t: complex);

               end;

    vector =  object
               i, j, k: ArbFloat;
               procedure Init (vii, vjj, vkk: ArbFloat);
               procedure Unary;
               procedure Add  (c: vector);
               procedure Sub  (c: vector);
               function  Vi : ArbFloat;
               function  Vj : ArbFloat;
               function  Vk : ArbFloat;
               function  Norm  : ArbFloat;
               Function  Norm8 : ArbFloat;
               function  Size  : ArbFloat;
               function  InProd(c: vector): ArbFloat;
               procedure Uitprod(c: vector; var e: vector);
               procedure Scale(s: ArbFloat);
               procedure DScale(s: ArbFloat);
               procedure Normalize;
               procedure Rotate(calfa, salfa: ArbFloat; axe: vector);
               procedure Show(p,q: ArbInt);
            end;

     transformorg  = record offset: complex; ss, sc: real end;
     transform = record
                       offsetx, offsety, scalex, scaley: ArbFloat
                 end;



     {Standard Functions used in NumLib}
     rfunc1r    = Function(x : ArbFloat): ArbFloat;
     rfunc1rn   = Function(x : ArbFloat): ArbFloat is nested;
     rfunc2r    = Function(x, y : ArbFloat): ArbFloat;

     {Complex version}
     rfunc1z    = Function(z: complex): ArbFloat;

     {Special Functions}
     oderk1n    = procedure(x: ArbFloat; var y, f: ArbFloat);
     roofnrfunc = procedure(var x, fx: ArbFloat; var deff: boolean);

     {Definition of matrix types in NumLib. First some vectors.
      The high boundery is a maximal number only. Vectors can be smaller, but
      not bigger. The difference is the starting number}
     arfloat0   = array[0..highestelement] of ArbFloat;
     arfloat1   = array[1..highestelement] of ArbFloat;
     arfloat2   = array[2..highestelement] of ArbFloat;
     arfloat_1  = array[-1..highestelement] of ArbFloat;

     {A matrix is an array of floats}
     ar2dr      = array[0..highestelement] of ^arfloat0;
     ar2dr1     = array[1..highestelement] of ^arfloat1;

     {Matrices can get big, so we mosttimes allocate them on the heap.}
     par2dr1    = ^ar2dr1;

     {Integer vectors}
     arint0     = array[0..highestelement] of ArbInt;
     arint1     = array[1..highestelement] of ArbInt;

     {Boolean (true/false) vectors}
     arbool1    = array[1..highestelement] of boolean;

     {Complex vectors}
     arcomp0    = array[0..highestelement] of complex;
     arcomp1    = array[1..highestelement] of complex;
     arvect0    = array[0..highestelement] of vector;
     vectors    = array[1..highestelement] of vector;

     parcomp    = ^arcomp1;

{(de) Allocate mxn matrix to A}
procedure AllocateAr2dr(m, n: integer; var a: par2dr1);
procedure DeAllocateAr2dr(m, n: integer; var a: par2dr1);

{(de) allocate below-left triangle matrix for (de)convolution
(a 3x3 matrix looks like this

  x
  x x
  x x x)
}
procedure AllocateL2dr(n: integer; var a: par2dr1);
procedure DeAllocateL2dr(n: integer; var a: par2dr1);

{Get the Re and Im parts of a complex type}
Function Re(z: complex): ArbFloat;
Function Im(z: complex): ArbFloat;

{ Creates a string from a floatingpoint value}
Function R2S(x: ArbFloat; p, q: integer): string;

{Calculate inproduct of V1 and V2, which are vectors with N elements;
I1 and I2 are the SIZEOF the datatypes of V1 and V2
MvdV: Change this to "V1,V2:array of ArbFloat and forget the i1 and i2
parameters?}

Function Inprod(var V1, V2; n, i1, i2: ArbInt): ArbFloat;

{Return certain special machine constants.(macheps=1, Nan=7)}
Function MachCnst(n: ArbInt): ArbFloat;

function dllversion:LONGINT;

implementation

Function MachCnst(n: ArbInt): ArbFloat;
begin
    case n of
    1: MachCnst := macheps;
    2: MachCnst := giant;
    3: MachCnst := midget;
    4: MachCnst := infinity;
    5: MachCnst := LnGiant;
    6: MachCnst := LnMidget;
    7: MachCnst := Nan;
    end
end;

{ Are used in many of the example programs}
Function Re(z: complex): ArbFloat;
begin
  Re := z.xreal
end;

Function Im(z: complex): ArbFloat;
begin
  Im := z.imag
end;

{Kind of Sysutils.TrimRight and TrimLeft called after eachother}
procedure Compress(var s: string);
var i, j: LONGINT;
begin
     j := length(s);
     while (j>0) and (s[j]=' ') do dec(j);
     i := 1;
     while (i<=j) and (s[i]=' ') do Inc(i);
     s := copy(s, i, j+1-i)
end;

Function R2S(x: ArbFloat; p, q: integer): string;
var s: string;
    i, j, k: integer;
begin
   if q=-1 then
    begin
        Str(x:p, s);
        i := Pos('E', s)-1; k := i+1;
        j := i+3; while (j<length(s)) and (s[j]='0') do inc(j);
        while s[i]='0' do dec(i); if s[i]='.' then dec(i);
        if s[j]='0' then s := copy(s,1,i) else
        if s[k]='-' then
         s := copy(s, 1, i)+'E-'+Copy(s, j, length(s)+1-j)
        else
         s := copy(s, 1, i)+'E'+Copy(s, j, length(s)+1-j)
    end
   else
    Str(x:p:q, s);
   Compress(s);
   R2S := s
end;

procedure AllocateAr2dr(m, n: integer; var a: par2dr1);
var i: integer;
begin
    GetMem(a, m*SizeOf(pointer));
    for i:=1 to m do GetMem(a^[i], n*SizeOf(ArbFloat))
end;

procedure DeAllocateAr2dr(m, n: integer; var a: par2dr1);
var i: integer;
begin
    for i:=m downto 1 do FreeMem(a^[i], n*SizeOf(ArbFloat));
    FreeMem(a, m*SizeOf(pointer));
    a := Nil
end;

procedure AllocateL2dr(n: integer; var a: par2dr1);
var i: integer;
begin
    GetMem(a, n*SizeOf(pointer));
    for i:=1 to n do GetMem(a^[i], i*SizeOf(ArbFloat))
end;

procedure DeAllocateL2dr(n: integer; var a: par2dr1);
var i: integer;
begin
    for i:=n downto 1 do FreeMem(a^[i], i*SizeOf(ArbFloat));
    FreeMem(a, n*SizeOf(pointer));
    a := Nil
end;

procedure Complex.Init(r, i: ArbFloat);
begin
      xreal:= r;
      imag := i
end;

procedure Complex.Conjugate;
begin
    imag := -imag
end;

function Complex.Inp(z:complex):ArbFloat;
begin
     Inp := xreal*z.xreal + imag*z.imag
end;

procedure Complex.MinC(c: complex);
begin if c.xreal<xreal then xreal := c.xreal;
      if c.imag<imag then imag := c.imag
end;

procedure Complex.Maxc(c: complex);
begin if c.xreal>xreal then xreal := c.xreal;
      if c.imag>imag then imag := c.imag
end;

procedure Complex.Add(c: complex);
begin
    xreal := xreal + c.xreal; imag := imag + c.imag
end;

procedure Complex.Sub(c: complex);
begin
    xreal := xreal - c.xreal; imag := imag - c.imag
end;

Function Complex.Norm: ArbFloat;
begin
    Norm := Sqr(xreal) + Sqr(imag)
end;

Function Complex.Size: ArbFloat;
begin
    Size := Sqrt(Norm)
end;

Function Complex.Re: ArbFloat;
begin
    Re := xreal;
end;

Function Complex.Im: ArbFloat;
begin
    Im := imag
end;

Procedure Complex.TransF(var t: complex);
var w: complex;
    tt: transformorg absolute t;
begin
   w := Self; Conjugate;
   with tt do
    begin
     w.scale(ss);
     scale(sc);
     Add(offset)
    end;
   Add(w)
end;


procedure Complex.Unary;
begin
 xreal := -xreal;
 imag := -imag
end;

procedure Complex.Scale(s:ArbFloat);
begin
    xreal := xreal*s; imag := imag*s
end;

Function Complex.Arg: ArbFloat;
begin
    if xreal=0 then
    if imag>0 then Arg := 0.5*pi else
    if imag=0 then Arg := 0 else Arg := -0.5*pi else
    if xReal>0 then Arg := ArcTan(imag/xReal)
    else if imag>=0 then Arg := ArcTan(imag/xReal) + pi
                    else Arg := ArcTan(imag/xReal) - pi
end;

Function exp(x: ArbFloat): ArbFloat;
begin
    if x<LnMidget then exp := 0 else exp := system.exp(x)
end;

{ procedure berekent: v1 = v1 + r*v2 i1 en i2 geven de
  increments in bytes voor v1 en v2 }

Function Inprod(var V1, V2; n, i1, i2: ArbInt): ArbFloat;

VAR i: LONGINT;
    p1, p2: ^ArbFloat;
    s: ArbFloat;
begin
  IF I1 <>SIZEOF(ArbFloat) THEN
   BEGIN
    WRITELN('1 Something went probably wrong while porting!');
    HALT;
   END;
   p1 := @v1; p2 := @v2; s := 0;
   for i:=1 to n do
    begin
     s := s + p1^*p2^;
     Inc(ptrint(p1), i1);
     Inc(ptrint(p2), i2)
    end;
    Inprod := s
end;

procedure Vector.Init(vii, vjj, vkk: ArbFloat);
begin
    i := vii; j := vjj; k := vkk
end;

procedure Vector.Unary;
begin i := -i; j := -j; k := -k end;

procedure Vector.Add(c: vector);
begin
    i := i + c.i; j := j + c.j; k := k + c.k
end;

procedure Vector.Sub(c: vector);
begin
    i := i - c.i; j := j - c.j; k := k - c.k
end;

function Vector.Vi : ArbFloat; begin Vi := i end;

function Vector.Vj : ArbFloat; begin Vj := j end;

function Vector.Vk : ArbFloat; begin Vk := k end;

function Vector.Norm:ArbFloat;
begin
    Norm := Sqr(i) + Sqr(j) + Sqr(k)
end;

function Vector.Norm8:ArbFloat;
var r: ArbFloat;
begin
    r := abs(i);
    if abs(j)>r then r := abs(j);
    if abs(k)>r then r := abs(k);
    Norm8 := r
end;

function Vector.Size: ArbFloat;
begin
    Size := Sqrt(Norm)
end;

function Vector.InProd(c: vector): ArbFloat;
begin
     InProd := i*c.i + j*c.j + k*c.k
end;

procedure Vector.Uitprod(c: vector; var e: vector);
begin
      e.i := j*c.k - k*c.j;
      e.j := k*c.i - i*c.k;
      e.k := i*c.j - j*c.i
end;

procedure Vector.Scale(s: ArbFloat);
begin
    i := i*s; j := j*s; k := k*s
end;

procedure Vector.DScale(s: ArbFloat);
begin
    i := i/s; j := j/s; k := k/s
end;

procedure Vector.Normalize;
begin
    DScale(Size)
end;

procedure Vector.Show(p,q:ArbInt);
begin writeln(i:p:q, 'I', j:p:q, 'J', k:p:q, 'K') end;

procedure Vector.Rotate(calfa, salfa: arbfloat; axe: vector);
var qv : vector;
begin
    Uitprod(axe, qv); qv.scale(salfa);
    axe.scale((1-calfa)*Inprod(axe));
    scale(calfa); sub(qv);  add(axe)
end;

function dllversion:LONGINT;

BEGIN
 dllversion:=numlib_version;
END;


END.
