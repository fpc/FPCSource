{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    This unit contains some basic matrix operations.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit omv;
{$I direct.inc}

interface

uses typ;

{Calculates inproduct of vectors a and b which have N elements. The first
element is passed in a and b}
Function omvinp(Var a, b: ArbFloat; n: ArbInt): ArbFloat;

{Multiplication of two matrices C=AxB }
Procedure omvmmm(Var a: ArbFloat; m, n, rwa: ArbInt;
                 Var b: ArbFloat; k, rwb: ArbInt;
                 Var c: ArbFloat; rwc: ArbInt);

{Multiplication of a matrix(A) with a vector(B), C=A x B}
Procedure omvmmv(Var a: ArbFloat; m, n, rwidth: ArbInt; Var b, c: ArbFloat);

{Calculate 1-Norm of matrix A}
Function omvn1m(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;

{Calculate 1-Norm of vector A}
Function omvn1v(Var a: ArbFloat; n: ArbInt): ArbFloat;

{Calculate 2-Norm of vector A}
Function omvn2v(Var a: ArbFloat; n: ArbInt): ArbFloat;

{Calculate Frobenius-Norm of mxn matrix A}
Function omvnfm(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;

{Calculates maximum (infinite) norm of mxn matrix a}
Function omvnmm(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;

{Calculates maximum (infinite) norm of n-Vector }
Function omvnmv(Var a: ArbFloat; n: ArbInt): ArbFloat;

{Transponate mxn matrix A  (which was declared rwa bytes wide), put
it to C (rwc was declared elements wide)}
Procedure omvtrm(Var a: ArbFloat; m, n, rwa: ArbInt; Var c: ArbFloat;
                        rwc: ArbInt);

IMPLEMENTATION

Function omvinp(Var a, b: ArbFloat; n: ArbInt): ArbFloat;

Var        pa, pb : ^arfloat1;
                i : ArbInt;
                s : ArbFloat;
Begin
  If n<1 Then
    exit(0);
  pa := @a;
 pb := @b;
 s := 0;
  For i:=1 To n Do
    Begin
      s := s+pa^[i]*pb^[i]
    End; {i}
  omvinp := s
End; {omvinp}

Procedure omvmmm(Var a: ArbFloat; m, n, rwa: ArbInt;
                 Var b: ArbFloat; k, rwb: ArbInt;
                 Var c: ArbFloat; rwc: ArbInt);

Var           pa, pb, pc : ^arfloat1;
     i, j, l, inda, indc : ArbInt;
                       s : ArbFloat;
Begin
  If (m<1) Or (n<1) Or (k<1) Then
   exit;
  pa := @a;
 pb := @b;
 pc := @c;
  For i:=1 To m Do
    Begin
      inda := (i-1)*rwa;
      indc := (i-1)*rwc;
      For j:=1 To k Do
        Begin
          s := 0;
          For l:=1 To n Do
            s := s+pa^[inda+l]*pb^[(l-1)*rwb+j];
          pc^[indc+j] := s
        End {j}
    End; {i}
End; {omvmmm}

Procedure omvmmv(Var a: ArbFloat; m, n, rwidth: ArbInt; Var b, c: ArbFloat);

Var     pa, pb, pc : ^arfloat1;
            i, ind : ArbInt;
Begin
  If (m<1) Or (n<1) Then
    exit;
  pa := @a;
 pb := @b;
 pc := @c;
 ind := 0;
  For i:=1 To m Do
    Begin
      pc^[i] := omvinp(pa^[ind+1], pb^[1], n);
      ind := ind+rwidth
    End; {i}
End; {omvmmv}

Function omvn1m(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;

Var           pa : ^arfloat1;
            i, j : ArbInt;
     norm, normc : ArbFloat;

Begin
  If (m<1) Or (n<1) Then
    exit;
  pa := @a;
 norm := 0;
  For j:=1 To n Do
    Begin
      normc := 0;
      For i:=1 To m Do
        normc := normc+abs(pa^[j+(i-1)*rwidth]);
      If norm<normc Then
        norm := normc
    End;
  omvn1m := norm
End {omvn1m};

Function omvn1v(Var a: ArbFloat; n: ArbInt): ArbFloat;

Var   pa : ^arfloat1;
       i : ArbInt;
    norm : ArbFloat;

Begin
  If n<1 Then
    exit;
  pa := @a;
  norm := 0;
  For i:=1 To n Do
    norm := norm+abs(pa^[i]);
  omvn1v := norm
End {omvn1v};

Function omvn2v(Var a: ArbFloat; n: ArbInt): ArbFloat;

Var   pa : ^arfloat1;
       i : ArbInt;
    norm : ArbFloat;

Begin
  If n<1 Then
    exit;
  pa := @a;
  norm := 0;
  For i:=1 To n Do
    norm := norm+sqr(pa^[i]);
  omvn2v := sqrt(norm)
End {omvn2v};

Function omvnfm(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;

Var      pa : ^arfloat1;
    i, j, k : ArbInt;
       norm : ArbFloat;

Begin
  If (m<1) Or (n<1) Then
    exit;
  pa := @a;
 norm := 0;
 k := 0;
  For i:=1 To m Do
    Begin
      For j:=1 To n Do
        norm := norm+sqr(pa^[j+k]);
      k := k+rwidth
    End;
  omvnfm := sqrt(norm)
End {omvnfm};

Function omvnmm(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;

Var          pa : ^arfloat1;
           i, k : ArbInt;
    normr, norm : ArbFloat;

Begin
  If (m<1) Or (n<1) Then
    exit;
  pa := @a;
 norm := 0;
 k := 0;
  For i:=1 To m Do
    Begin
      normr := omvn1v(pa^[1+k], n);
      If norm<normr Then
        norm := normr;
      k := k+rwidth
    End;
  omvnmm := norm
End {omvnmm};

Function omvnmv(Var a: ArbFloat; n: ArbInt): ArbFloat;

Var       pa : ^arfloat1;
           i : ArbInt;
    norm, aa : ArbFloat;

Begin
  If (n<1) Then
    exit;
  pa := @a;
  norm := 0;
  For i:=1 To n Do
    Begin
      aa := abs(pa^[i]);
      If aa>norm Then
        norm := aa
    End;
  omvnmv := norm
End {omvnmv};

Procedure omvtrm(Var a: ArbFloat; m, n, rwa: ArbInt;
                 Var c: ArbFloat; rwc: ArbInt);

Var        pa, pc : ^arfloat1;
           ind, i, j : ArbInt;

Begin
  If (m<1) Or (n<1) Then
    exit;
  pa := @a;
 pc := @c;
 ind := 0;
  For i:=1 To m Do
    Begin
      For j:=1 To n Do
        pc^[(j-1)*rwc+i] := pa^[ind+j];
      ind := ind+rwa
    End; {i}
End; {omvtrm}

End.
