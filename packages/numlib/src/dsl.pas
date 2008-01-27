{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Unknown unit. There doesn't exist any documentation for it, it isn't
    commented, and I don't recognize the algortism directly.
    I added some comments, since suffixes of the procedures seem to indicate
    some features of the matrixtype (from unit SLE)
    So probably Some pivot matrix?

    This code was probably internal in older libs, and only exported
    in later versions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit dsl;

interface
{$I DIRECT.INC}


uses typ;

{Gen=generic, matrix without special or unknown ordering}
Procedure dslgen(n, rwidth: ArbInt; Var alu: ArbFloat; Var p: ArbInt;
                 Var b, x: ArbFloat; Var term: ArbInt);

{"tridiagonal matrix"}
Procedure dslgtr(n: ArbInt; Var l1, d1, u1, u2: ArbFloat;
                 Var p: boolean; Var b, x: ArbFloat; Var term: ArbInt);

{Symmetrical matrix}
Procedure dslgsy(n, rwidth: ArbInt; Var alt: ArbFloat; Var p: ArbInt;
                 Var q: boolean; Var b, x: ArbFloat; Var term: ArbInt);

{Symmetrical positive definitive matrix}
Procedure dslgpd(n, rwidth: ArbInt; Var al, b, x: ArbFloat;
                 Var term: ArbInt);

{Generic "band" matrix}
Procedure dslgba(n, lb, rb, rwa: ArbInt; Var au: ArbFloat; rwl: ArbInt;
                 Var l: ArbFloat; Var p: ArbInt; Var b, x: ArbFloat;
                 Var term: ArbInt);

{Positive definite bandmatrix}
Procedure dslgpb(n, lb, rwidth: ArbInt; Var al, b, x: ArbFloat;
                 Var term: ArbInt);

{Special tridiagonal matrix}
Procedure dsldtr(n:ArbInt; Var l, d, u, b, x: ArbFloat; Var term: ArbInt);

implementation

Procedure dslgen(n, rwidth: ArbInt; Var alu: ArbFloat; Var p: ArbInt;
                 Var b, x: ArbFloat; Var term: ArbInt);

Var
                          success : boolean;
    indk, j, k, indexpivot, kmin1 : ArbInt;
                      h, pivot, s : ArbFloat;
                               pp : ^arint1;
                     palu, pb, px : ^arfloat1;

Begin
  If (n<1) Or (rwidth<1) Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  pp := @p;
 palu := @alu;
 pb := @b;
 px := @x;
  move(pb^, px^, n*sizeof(ArbFloat));
  For k:=1 To n Do
    Begin
      indexpivot := pp^[k];
      If indexpivot  <> k Then
        Begin
          h := px^[k];
         px^[k] := px^[indexpivot];
          px^[indexpivot] := h
        End {indexpivot <> k}
    End; {k}
  For k:=2 To n Do
    Begin
      s := px^[k];
     kmin1 := k-1;
      For j:=1 To kmin1 Do
        s := s-palu^[(k-1)*rwidth+j]*px^[j];
      px^[k] := s
    End; {k}
  success := true;
 k := n+1;
  while (k>1) and success Do
    Begin
      k := k-1;
     indk := (k-1)*rwidth;
      pivot := palu^[indk+k];
      If pivot=0 Then
        success := false
      Else
        Begin
          s := px^[k];
          For j:=k+1 To n Do
            s := s-palu^[indk+j]*px^[j];
          px^[k] := s/pivot
        End {pivot <> 0}
    End; {k}
  If success Then
    term := 1
  Else
    term := 2
End; {dslgen}

Procedure dslgtr(n: ArbInt; Var l1, d1, u1, u2: ArbFloat;
                 Var p: boolean; Var b, x: ArbFloat; Var term: ArbInt);

Var
                    i, j, nmin1 : ArbInt;
                          h, di : ArbFloat;
                        success : boolean;
          pd1, pu1, pu2, pb, px : ^arfloat1;
                            pl1 : ^arfloat2;
                             pp : ^arbool1;
Begin
  If n<1 Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  pl1 := @l1;  pd1 := @d1;  pu1 := @u1; pu2 := @u2; pb := @b;  px := @x;
 pp := @p;
  move(pb^, px^, n*sizeof(ArbFloat));
  success := true;
 i := 0;
  while (i<>n) and success Do
    Begin
      i := i+1;
     success := pd1^[i]<>0
    End; {i}
  If success Then
    Begin
      nmin1 := n-1;
     j := 1;
      while j <> n Do
        Begin
          i := j;
         j := j+1;
          If pp^[i] Then
            Begin
              h := px^[i];
             px^[i] := px^[j];
             px^[j] := h-pl1^[j]*px^[i]
            End {pp^[i]}
          Else
            px^[j] := px^[j]-pl1^[j]*px^[i]
        End;  {j}
      di := pd1^[n];
      px^[n] := px^[n]/di;
      If n > 1 Then
        Begin
          di := pd1^[nmin1];
          px^[nmin1] := (px^[nmin1]-pu1^[nmin1]*px^[n])/di
        End; {n > 1}
      For i:=n-2 Downto 1 Do
        Begin
          di := pd1^[i];
          px^[i] := (px^[i]-pu1^[i]*px^[i+1]-pu2^[i]*px^[i+2])/di
        End {i}
    End; {success}
  If success Then
    term := 1
  Else
    term := 2
End; {dslgtr}

Procedure dslgsy(n, rwidth: ArbInt; Var alt: ArbFloat; Var p: ArbInt;
                 Var q: boolean; Var b, x: ArbFloat; Var term: ArbInt);

Var
    i, indexpivot, imin1, j, jmin1, iplus1, imin2, ns, ii  : ArbInt;
                                                   success : boolean;
                                                     h, di : ArbFloat;
                               palt, pb, px, y, l, d, u, v : ^arfloat1;
                                                        pp : ^arint1;
                                                        pq : ^arbool1;

Begin
  If (n<1) Or (rwidth<1) Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  palt := @alt;
 pp := @p;
 pq := @q;
 pb := @b;
 px := @x;
  ns := n*sizeof(ArbFloat);
  getmem(l, ns);
  getmem(d, ns);
  getmem(u, ns);
  getmem(v, ns);
  getmem(y, ns);
  move(pb^, y^, ns);
  success := true;
 i := 0;
 ii := 1;
  while (i<>n) and success Do
    Begin
      i := i+1;
     success := palt^[ii]<>0;
     ii := ii+rwidth+1
    End; {i}
  If success Then
    Begin
      For i:=1 To n Do
        Begin
          indexpivot := pp^[i];
          If indexpivot <> i Then
            Begin
              h := y^[i];
             y^[i] := y^[indexpivot];
              y^[indexpivot] := h
            End {indexpivot <> i}
        End; {i}
      i := 0;
      while i<n Do
        Begin
          imin1 := i;
         i := i+1;
         j := 1;
         h := y^[i];
          while j<imin1 Do
            Begin
              jmin1 := j;
             j := j+1;
              h := h-palt^[(i-1)*rwidth+jmin1]*y^[j]
            End; {j}
          y^[i] := h
        End; {i}
      d^[1] := palt^[1];
     di := d^[1];
      If n>1 Then
        Begin
          l^[1] := palt^[rwidth+1];
         d^[2] := palt^[rwidth+2];
          di := d^[2];
          u^[1] := palt^[2]
        End; {n>1}
      imin1 := 1;
     i := 2;
      while i<n Do
        Begin
          imin2 := imin1;
         imin1 := i;
         i := i+1;
          ii := (i-1)*rwidth;
          l^[imin1] := palt^[ii+imin1];
         d^[i] := palt^[ii+i];
         di := d^[i];
          u^[imin1] := palt^[ii-rwidth+i];
         v^[imin2] := palt^[ii-2*rwidth+i]
        End; {i}
      dslgtr(n, l^[1], d^[1], u^[1], v^[1], pq^[1], y^[1], px^[1], term);
      i := n+1;
     imin1 := n;
      while i>2 Do
        Begin
          iplus1 := i;
         i := imin1;
         imin1 := imin1-1;
         h := px^[i];
          For j:=iplus1 To n Do
            h := h-palt^[(j-1)*rwidth+imin1]*px^[j];
          px^[i] := h
        End; {i}
      For i:=n Downto 1 Do
        Begin
          indexpivot := pp^[i];
          If indexpivot <> i Then
            Begin
              h := px^[i];
             px^[i] := px^[indexpivot];
              px^[indexpivot] := h
            End {indexpivot <> i}
        End {i}
   End; {success}
  If success Then
    term := 1
  Else
    term := 2;
  freemem(l, ns);
  freemem(d, ns);
  freemem(u, ns);
  freemem(v, ns);
  freemem(y, ns)
End; {dslgsy}

Procedure dslgpd(n, rwidth: ArbInt; Var al, b, x: ArbFloat;
                 Var term: ArbInt);

Var
       ii, imin1, i, j : ArbInt;
                h, lii : ArbFloat;
               success : boolean;
           pal, pb, px : ^arfloat1;
Begin
  If (n<1) Or (rwidth<1) Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  pal := @al;
 pb := @b;
 px := @x;
  move(pb^, px^, n*sizeof(ArbFloat));
  success := true;
 i := 0;
 ii := 1;
  while (i<>n) and success Do
    Begin
      i := i+1;
     success := pal^[ii]<>0;
     ii := ii+rwidth+1
    End; {i}
  If success Then
    Begin
      For i:=1 To n Do
        Begin
          ii := (i-1)*rwidth;
          h := px^[i];
         imin1 := i-1;
          For j:=1 To imin1 Do
            h := h-pal^[ii+j]*px^[j];
          lii := pal^[ii+i];
          px^[i] := h/lii
        End; {i}
      For i:=n Downto 1 Do
        Begin
          h := px^[i];
          For j:=i+1 To n Do
            h := h-pal^[(j-1)*rwidth+i]*px^[j];
          px^[i] := h/pal^[(i-1)*rwidth+i]
        End {i}
    End; {success}
  If success Then
    term := 1
  Else
    term := 2
End;  {dslgpd}

Procedure dslgba(n, lb, rb, rwa: ArbInt; Var au: ArbFloat; rwl: ArbInt;
                 Var l: ArbFloat; Var p: ArbInt; Var b, x: ArbFloat;
                 Var term: ArbInt);

Var
   i, j, k, ipivot, ubi, ubj : ArbInt;
   h, pivot                  : ArbFloat;
   pau, pl, px, pb           : ^arfloat1;
   pp                        : ^arint1;

Begin
  If (n<1) Or (lb<0) Or (rb<0) Or (lb>n-1)
        Or (rb>n-1) Or (rwa<1) Or (rwl<0) Then
    Begin
      term := 3;
     exit
    End; {term=3}
  pau := @au;
 pl := @l;
 pb := @b;
 px := @x;
 pp := @p;
  move(pb^, px^, n*sizeof(ArbFloat));
  ubi := lb;
  For k:=1 To n Do
    Begin
      ipivot := pp^[k];
      If ipivot <> k Then
        Begin
          h := px^[k];
         px^[k] := px^[ipivot];
          px^[ipivot] := h
        End; {ipivot <> k}
      If ubi<n Then
        ubi := ubi+1;
      For i:=k+1 To ubi Do
        px^[i] := px^[i]-px^[k]*pl^[(k-1)*rwl+i-k]
    End; {k}
  ubj := 0;
 i := n;
 term := 1;
  while (i >= 1) and (term=1) Do
    Begin
      If ubj<rb+lb+1 Then
        ubj := ubj+1;
      h := px^[i];
      For j:=2 To ubj Do
        h := h-pau^[(i-1)*rwa+j]*px^[i+j-1];
      pivot := pau^[(i-1)*rwa+1];
      If pivot=0 Then
        term := 2
      Else
        px^[i] := h/pivot;
      i := i-1
    End {i}
End; {dslgba}

Procedure dslgpb(n, lb, rwidth: ArbInt; Var al, b, x: ArbFloat;
                 Var term: ArbInt);

Var
   ll, ii, llmin1, p, i, q, k : ArbInt;
                      h, alim : ArbFloat;
                  pal, pb, px : ^arfloat1;
Begin
  If (lb<0) Or (lb>n-1) Or (n<1) Or (rwidth<1) Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  pal := @al;
 pb := @b;
 px := @x;
  move(pb^, px^, n*sizeof(ArbFloat));
  ll := lb+1;
  llmin1 := ll-1;
 p := ll+1;
 term := 1;
 i := 1;
  while (i <= n) and (term=1) Do
    Begin
      ii := (i-1)*rwidth;
      If p>1 Then
        p := p-1;
      h := px^[i];
     q := i;
      For k:=llmin1 Downto p Do
        Begin
          q := q-1;
         h := h-pal^[ii+k]*px^[q]
        End; {k}
      alim := pal^[ii+ll];
      If alim=0 Then
        term := 2
      Else
        px^[i] := h/alim;
      i := i+1
    End; {i}
  If term=1 Then
    Begin
      p := ll+1;
      For i:=n Downto 1 Do
        Begin
          If p>1 Then
            p := p-1;
          q := i;
         h := px^[i];
          For k:=llmin1 Downto p Do
            Begin
              q := q+1;
             h := h-pal^[(q-1)*rwidth+k]*px^[q]
            End; {k}
          px^[i] := h/pal^[(i-1)*rwidth+ll]
        End {i}
    End {term=1}
End; {dslgpb}

Procedure dsldtr(n:ArbInt; Var l, d, u, b, x: ArbFloat; Var term: ArbInt);

Var
                   i, j : ArbInt;
                     di : ArbFloat;
         pd, pu, pb, px : ^arfloat1;
                     pl : ^arfloat2;
Begin
  If n<1 Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  pl := @l;
 pd := @d;
 pu := @u;
 pb := @b;
 px := @x;
  move(pb^, px^, n*sizeof(ArbFloat));
  j := 1;
  while j <> n Do
    Begin
      i := j;
     j := j+1;
     px^[j] := px^[j]-pl^[j]*px^[i]
    End;
  di := pd^[n];
  If di=0 Then
    term := 2
  Else
    term := 1;
  If term=1 Then
    px^[n] := px^[n]/di;
  i := n-1;
  while (i >= 1) and (term=1) Do
    Begin
      di := pd^[i];
      If di=0 Then
        term := 2
      Else
        px^[i] := (px^[i]-pu^[i]*px^[i+1])/di;
      i := i-1
    End; {i}
End; {dsldtr}

End.
