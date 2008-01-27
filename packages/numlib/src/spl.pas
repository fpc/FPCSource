{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Undocumented unit. B- and other Splines. Not imported by the other units
    afaik.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit spl;
{$I direct.inc}

interface

uses typ, sle;

function  spl1bspv(q: ArbInt; var kmin1, c1: ArbFloat; x: ArbFloat; var term: ArbInt): ArbFloat;
function  spl2bspv(qx, qy: ArbInt; var kxmin1, kymin1, c11: ArbFloat; x, y: ArbFloat; var term: ArbInt): ArbFloat;
procedure spl1bspf(M, Q: ArbInt; var XYW1: ArbFloat;
                 var Kmin1, C1, residu: ArbFloat;
                 var term: ArbInt);
procedure spl2bspf(M, Qx, Qy: ArbInt; var XYZW1: ArbFloat;
                 var Kxmin1, Kymin1, C11, residu: ArbFloat;
                 var term: ArbInt);
procedure spl1nati(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt);
procedure spl1naki(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt);
procedure spl1cmpi(n: ArbInt; var xyc1: ArbFloat; dy1, dyn: ArbFloat;
                 var term: ArbInt);
procedure spl1peri(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt);
function  spl1pprv(n: ArbInt; var xyc1: ArbFloat; t: ArbFloat; var term: ArbInt): ArbFloat;

procedure spl1nalf(n: ArbInt; var xyw1: ArbFloat; lambda:ArbFloat;
                     var xac1, residu: ArbFloat; var term: ArbInt);
function spl2natv(n: ArbInt; var xyg0: ArbFloat; u, v: ArbFloat): ArbFloat;
procedure spl2nalf(n: ArbInt; var xyzw1: ArbFloat; lambda:ArbFloat;
                     var xyg0, residu: ArbFloat; var term: ArbInt);
      { term = 1: succes,
        term = 2: set linear equations is not "PD"
        term = 4: Approx. number of points? On a line.
        term = 3: wrong input n<3 or a weight turned out to be <=0 }
implementation
{$goto on}

type
    Krec = record K1, K2, K3, K4, K5, K6 : ArbFloat end;

function spl1bspv(q: ArbInt; var kmin1, c1: ArbFloat; x: ArbFloat; var term: ArbInt): ArbFloat;
var c    : arfloat1 absolute c1;
    k    : arfloat_1 absolute kmin1;
    D1, D2, D3,
    E2, E3, E4, E5: ArbFloat;
    pk   : ^Krec;
    l, r, m : ArbInt;
begin
    spl1bspv := NaN;
    term := 3;                           { q >=4 !     }
    if q<4 then exit;                    { at least 1 interval   }
    if (x<k[2]) or (x>k[q-1]) then exit; { x inside the interval }
    term := 1;                           { Let's hope the params are good :-)}
    l := 2; r := q-1;
    while l+1<r do                       { after this loop goes: }
     begin                                { k[l]<=x<=k[l+1] with  }
      m := (l+r) div 2;                {   k[l] < k[l+1]       }
      if x>=k[m] then l := m else r := m
     end;
    pk := @k[l-2];                       { the (de) Boor algoritm ..  }
    with pk^ do
     begin
      E2 := X - K2; E3 := X - K3; E4 := K4 - X; E5 := K5 - X;
      D2 := C[l]; D3 := C[l+1];
      D1 := ((X-K1)*D2+E4*C[l-1])/(K4-K1);
      D2 := (E2*D3+E5*D2)/(K5-K2);
      D3 := (E3*C[l+2]+(K6-X)*D3)/(K6-K3);
      D1 := (E2*D2+E4*D1)/(K4-K2);
      D2 := (E3*D3+E5*D2)/(K5-K3);
      spl1bspv := (E3*D2+E4*D1)/(K4-K3)
    end;
end;

function  spl2bspv(qx, qy: ArbInt; var kxmin1, kymin1, c11: ArbFloat; x, y: ArbFloat; var term: ArbInt): ArbFloat;
var  pd: ^arfloat1;
  i, iy: ArbInt;
      c: arfloat1 absolute c11;
begin
    GetMem(pd, qx*SizeOf(ArbFloat));
    i := 0;
    iy := 1;
    repeat
        i := i + 1;
        pd^[i] := spl1bspv(qy, kymin1, c[iy], y, term);
        Inc(iy, qy)
    until (i=qx) or (term<>1);
    if term=1
    then spl2bspv := spl1bspv(qx, kxmin1, pd^[1], x, term)
    else spl2bspv := NaN;
    FreeMem(pd, qx*SizeOf(ArbFloat));
end;

(*  Bron: NAG LIBRARY SUBROUTINE  E02BAF *)

function Imin(x, y: ArbInt): ArbInt;
begin if x<y then Imin := x else Imin := y end;

type ar4 = array[1..$ffe0 div (4*SizeOf(ArbFloat)),1..4] of ArbFloat;
     ar3 = array[1..$ffe0 div (3*SizeOf(ArbFloat)),1..3] of ArbFloat;
     r_3 = record x, y, w: ArbFloat end;
     r3Ar= array[1..$ffe0 div SizeOf(r_3)] of r_3;
     r_4 = record x, y, z, w: ArbFloat end;
     r4Ar= array[1..$ffe0 div SizeOf(r_4)] of r_4;
     r4  = array[1..4] of ArbFloat;
     r2  = array[1..2] of ArbFloat;

     r4x  = record xy: R2; alfa, d: ArbFloat end;
     r4xAr= array[1..$ffe0 div SizeOf(r4x)] of r4x;
     nsp2rec = array[0..$ff80 div (3*SizeOf(ArbFloat))] of
               record xy: R2; gamma: ArbFloat end;

procedure spl1bspf(M, Q: ArbInt; var XYW1: ArbFloat;
                 var Kmin1, C1, residu: ArbFloat;
                 var term: ArbInt);
var work1: ^arfloat1;
    work2: ^ar4;
    c    : arfloat1 absolute c1;
    k    : arfloat_1 absolute kmin1;
    xyw  : r3Ar absolute XYW1;
    r, j, jmax, l, lplus1, i, iplusj, jold, jrev,
    jplusl, iu, lplusu : ArbInt;
    s, k0, k4, sigma,
    d, d4, d5, d6, d7, d8, d9,
    e2, e3, e4, e5,
    n1, n2, n3,
    relemt, dprime, cosine, sine,
    acol, arow, crow, ccol, ss     : ArbFloat;
    pk   : ^Krec;

label einde;
(*
      DOUBLE PRECISION  C(NCAP7), K(NCAP7), W(M), WORK1(M),
     *                  WORK2(4,NCAP7), X(M), Y(M)
     .. Local Scalars ..
      DOUBLE PRECISION  ACOL, AROW, CCOL, COSINE, CROW, D, D4, D5, D6,
     *                  D7, D8, D9, DPRIME, E2, E3, E4, E5, K0, K1, K2,
     *                  K3, K4, K5, K6, N1, N2, N3, RELEMT, S, SIGMA,
     *                  SINE, WI, XI
      INTEGER           I, IERROR, IPLUSJ, IU, J, JOLD, JPLUSL, JREV, L,
     *                  L4, LPLUS1, LPLUSU, NCAP, NCAP3, NCAPM1, R
*)
begin
    term := 3;
    if q<4 then exit;
    if m<q then exit;
(*
     CHECK THAT THE VALUES OF  M  AND  NCAP7  ARE REASONABLE
      IF (NCAP7.LT.8 .OR. M.LT.NCAP7-4) GO TO 420
      NCAP = NCAP7 - 7
      NCAPM1 = NCAP - 1
      NCAP3 = NCAP + 3

     IN ORDER TO DEFINE THE FULL B-SPLINE BASIS, AUGMENT THE
     PRESCRIBED INTERIOR KNOTS BY KNOTS OF MULTIPLICITY FOUR
     AT EACH END OF THE DATA RANGE.

*)
    for j:=-1 to 2 do k[j] := xyw[1].x;
    for j:=q-1 to q+2 do k[j] := xyw[m].x;

    if (k[3]<=xyw[1].x) or (k[q-2]>=xyw[m].x) then exit;
(*
     CHECK THAT THE KNOTS ARE ORDERED AND ARE INTERIOR
     TO THE DATA INTERVAL.
*)
    j := 3; while (k[j]<=k[j+1]) and (j<q-2) do Inc(j);
    if j<q-2 then exit;
(*
     CHECK THAT THE WEIGHTS ARE STRICTLY POSITIVE.
*)
    j := 1;
    while (xyw[j].w>0) and (j<m) do Inc(j);
    if xyw[j].w<=0 then exit;
(*
     CHECK THAT THE DATA ABSCISSAE ARE ORDERED, THEN FORM THE
     ARRAY  WORK1  FROM THE ARRAY  X.  THE ARRAY  WORK1  CONTAINS
     THE
     SET OF DISTINCT DATA ABSCISSAE.
*)
    GetMem(Work1, m*SizeOf(ArbFloat));
    GetMem(Work2, q*4*SizeOf(ArbFloat));
    r := 1; work1^[1] := xyw[1].x;
    j := 1;
    while (j<m) do
    begin
       Inc(j);
       if xyw[j].x>work1^[r]
       then begin Inc(r); work1^[r] := xyw[j].x end
       else if xyw[j].x<work1^[r] then goto einde;
    end;
    if r<q then goto einde;

(*
     CHECK THE FIRST  S  AND THE LAST  S  SCHOENBERG-WHITNEY
     CONDITIONS ( S = MIN(NCAP - 1, 4) ).
*)
    jmax := Imin(q-4,4);
    j := 1;
    while (j<=jmax) do
    begin
      if (work1^[j]>=k[j+2]) or (k[q-j-1]>=work1^[r-j+1]) then goto einde;
      Inc(j)
    end;
(*
     CHECK ALL THE REMAINING SCHOENBERG-WHITNEY CONDITIONS.
*)
    Dec(r, 4); i := 4; j := 5;
    while j<=q-4 do
    begin
       K0 := K[j+2]; K4 := K[J-2];
       repeat Inc(i) until (Work1^[i]>k4);
       if (I>R) or (WORK1^[I]>=K0) then goto einde;
       Inc(j)
    end;

(*
     INITIALISE A BAND TRIANGULAR SYSTEM (I.E. A
     MATRIX AND A RIGHT HAND SIDE) TO ZERO. THE
     PROCESSING OF EACH DATA POINT IN TURN RESULTS
     IN AN UPDATING OF THIS SYSTEM. THE SUBSEQUENT
     SOLUTION OF THE RESULTING BAND TRIANGULAR SYSTEM
     YIELDS THE COEFFICIENTS OF THE B-SPLINES.
*)
    FillChar(Work2^, q*4*SizeOf(ArbFloat), 0);
    FillChar(c, q*SizeOf(ArbFloat), 0);

    SIGMA := 0; j := 0; jold := 0;
    for i:=1 to m do
    with xyw[i] do
    begin
(*
        FOR THE DATA POINT  (X(I), Y(I))  DETERMINE AN INTERVAL
        K(J + 3) .LE. X .LT. K(J + 4)  CONTAINING  X(I).  (IN THE
        CASE  J + 4 .EQ. NCAP  THE SECOND EQUALITY IS RELAXED TO
        INCLUDE
        EQUALITY).
*)
       while (x>=k[j+2]) and (j<=q-4) do Inc(j);
       if j<>jold then
       begin
         pk := @k[j-1];
         with pk^ do
         begin
             D4 := 1/(K4-K1); D5 := 1/(K5-K2); D6 := 1/(K6-K3);
             D7 := 1/(K4-K2); D8 := 1/(K5-K3); D9 := 1/(K4-K3)
         end;
         JOLD := J;
       end;
(*
        COMPUTE AND STORE IN  WORK1(L) (L = 1, 2, 3, 4)  THE VALUES
        OF
        THE FOUR NORMALIZED CUBIC B-SPLINES WHICH ARE NON-ZERO AT
        X=X(I).
*)     with pk^ do
       begin
           E5 := k5 - X;
           E4 := K4 - X;
           E3 := X - K3;
           E2 := X - K2;
           N1 := W*D9;
           N2 := E3*N1*D8;
           N1 := E4*N1*D7;
           N3 := E3*N2*D6;
           N2 := (E2*N1+E5*N2)*D5;
           N1 := E4*N1*D4;
           WORK1^[4] := E3*N3;
           WORK1^[3] := E2*N2 + (K6-X)*N3;
           WORK1^[2] := (X-K1)*N1 + E5*N2;
           WORK1^[1] := E4*N1;
           CROW := Y*W;
       end;
(*
        ROTATE THIS ROW INTO THE BAND TRIANGULAR SYSTEM USING PLANE
        ROTATIONS.
*)
       for lplus1:=1 to 4 do
       begin L := LPLUS1 - 1;
          RELEMT := WORK1^[LPLUS1];
          if relemt<>0 then
          begin JPLUSL := J + L;
            D := WORK2^[JPLUSL,1];
            IF (ABS(RELEMT)>=D)
            then DPRIME := ABS(RELEMT)*SQRT(1+sqr(D/RELEMT))
            else DPRIME := D*SQRT(1+sqr(RELEMT/D));
            WORK2^[JPLUSL,1] := DPRIME;
            COSINE := D/DPRIME; SINE := RELEMT/DPRIME;
            for iu :=2 to 4-l do
            begin
               LPLUSU := L + IU;
               ACOL := WORK2^[JPLUSL,iu];
               AROW := WORK1^[LPLUSU];
               WORK2^[JPLUSL,iu] := COSINE*ACOL + SINE*AROW;
               WORK1^[LPLUSU] := COSINE*AROW - SINE*ACOL
            end;

            CCOL := C[JPLUSL];
            C[JPLUSL] := COSINE*CCOL + SINE*CROW;
            CROW := COSINE*CROW - SINE*CCOL
          end;
       end;
       SIGMA := SIGMA + sqr(CROW)
   end;

   residu := SIGMA;
(*
     SOLVE THE BAND TRIANGULAR SYSTEM FOR THE B-SPLINE
     COEFFICIENTS. IF A DIAGONAL ELEMENT IS ZERO, AND HENCE
     THE TRIANGULAR SYSTEM IS SINGULAR, THE IMPLICATION IS
     THAT THE SCHOENBERG-WHITNEY CONDITIONS ARE ONLY JUST
     SATISFIED. THUS IT IS APPROPRIATE TO EXIT IN THIS
     CASE WITH THE SAME VALUE  (IFAIL=5)  OF THE ERROR
     INDICATOR.
*)
    term := 2;
    L := -1;
    for jrev:=1 to q do
    begin
       J := q - JREV + 1; D := WORK2^[J,1];
       if d=0 then goto einde;
       IF l<3 then L := L + 1;
       S := C[j];
       for i:=1 to l do
       begin
         IPLUSJ := I + J;
         S := S - WORK2^[j,i+1]*C[IPLUSJ];
       end;
       C[J] := S/D
    end;

    term:=1;
einde:
    FreeMem(Work2, q*4*SizeOf(ArbFloat));
    FreeMem(Work1, m*SizeOf(ArbFloat))

end;

procedure spl2bspf(M, Qx, Qy: ArbInt; var XYZW1: ArbFloat;
                 var Kxmin1, Kymin1, C11, residu: ArbFloat;
                 var term: ArbInt);

(* !!!!!!!! Test input !!!!!!!!!! *)

(*
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c part 1: determination of the number of knots and their position.     c
c ****************************************************************     c
c given a set of knots we compute the least-squares spline sinf(x,y),  c
c and the corresponding weighted sum of squared residuals fp=f(p=inf). c
c if iopt=-1  sinf(x,y) is the requested approximation.                c
c if iopt=0 or iopt=1 we check whether we can accept the knots:        c
c   if fp <=s we will continue with the current set of knots.          c
c   if fp > s we will increase the number of knots and compute the     c
c      corresponding least-squares spline until finally  fp<=s.        c
c the initial choice of knots depends on the value of s and iopt.      c
c   if iopt=0 we first compute the least-squares polynomial of degree  c
c     3 in x and 3 in y; nx=nminx=2*3+2 and ny=nminy=2*3+2.            c
c     fp0=f(0) denotes the corresponding weighted sum of squared       c
c     residuals                                                        c
c   if iopt=1 we start with the knots found at the last call of the    c
c     routine, except for the case that s>=fp0; then we can compute    c
c     the least-squares polynomial directly.                           c
c eventually the independent variables x and y (and the corresponding  c
c parameters) will be switched if this can reduce the bandwidth of the c
c system to be solved.                                                 c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc *)

function Min(a, b:ArbInt): ArbInt;
begin if a<b then Min := a else Min := b end;

procedure WisselR(var x, y: ArbFloat);
var h: ArbFloat; begin h := x; x := y; y := h end;

procedure Wisseli(var x, y: ArbInt);
var h: ArbInt; begin h := x; x := y; y := h end;

procedure fprota(var cos1, sin1, a, b: ArbFloat);
var store: ArbFloat;
begin
    store := b; b := cos1*b+sin1*a; a := cos1*a-sin1*store
end;

procedure fpgivs(var piv, ww, cos1, sin1: ArbFloat);
var store, dd: ArbFloat;
begin
   store := abs(piv);
   if store>=ww
   then dd := store*sqrt(1+sqr(ww/piv))
   else dd := ww*sqrt(1+sqr(piv/ww));
   cos1 := ww/dd; sin1 := piv/dd; ww := dd
end;

procedure fpback(var a11, z1: ArbFloat; n, k: ArbInt; var c1: ArbFloat);
(*
   subroutine fpback calculates the solution of the system of
   equations a*c = z with a a n x n upper triangular matrix
   of bandwidth k.
   ArbFloat a(.,k)
*)
var a: arfloat1 absolute a11;
    z: arfloat1 absolute z1;
    c: arfloat1 absolute c1;
    i, l: ArbInt;
    store : ArbFloat;
begin
    for i:=n downto 1 do
    begin
       store := z[i];
       for l:=min(n+1-i,k)-1 downto 1 do store := store-c[i+l]*a[(i-1)*k+l+1];
       c[i] := store/a[(i-1)*k+1]
    end;
end;

procedure fpbspl(var kmin1: ArbFloat; x: ArbFloat; l: ArbInt; var h: r4);
(*
   subroutine fpbspl evaluates the 4 non-zero b-splines of
   degree 3 at t(l) <= x < t(l+1) using the stable recurrence
   relation of de boor and cox.
*)
var k : arfloat_1 absolute kmin1;
    f : ArbFloat;
    hh: array[1..3] of ArbFloat;
    i, j, li, lj : ArbInt;
begin
    h[1] := 1;
    for j:=1 to 3 do
    begin
       for i:=1 to j do hh[i] := h[i];
       h[1] := 0;
       for i:=1 to j do
       begin
          li := l+i; lj := li-j;
          f := hh[i]/(k[li]-k[lj]);
          h[i] := h[i]+f*(k[li]-x);
          h[i+1] := f*(x-k[lj])
       end;
    end;
end;

procedure fporde(m, qx, qy: ArbInt; var xyzw1, kxmin1, kymin1: ArbFloat;
                 var nummer1, index1: ArbInt);
var xi, yi : ArbFloat;
    i, im, num,
    k, l   : ArbInt;
    xyzw   : r4Ar absolute xyzw1;
    kx     : arfloat_1 absolute kxmin1;
    ky     : arfloat_1 absolute kymin1;
    nummer : arint1 absolute nummer1;
    index  : arint1 absolute index1;
begin
   for i:=1 to (qx-3)*(qy-3) do index[i] := 0;
   for im:=1 to m do
   with xyzw[im] do
   begin
     l := 2; while (x>=kx[l+1]) and (l<qx-2) do Inc(l);
     k := 2; while (y>=ky[k+1]) and (k<qy-2) do Inc(k);
     num := (l-2)*(qy-3)+k-1;
     nummer[im] := index[num]; index[num] := im
   end;
end;

label einde;

var x0, x1, y0, y1, eps, cos1, sin1, dmax, sigma,
    wi, zi, hxi, piv    : ArbFloat;
    i, j, l, l1, l2, lx, ly, nreg, ncof, jrot,
    inpanel, i1, j1,
    iband, num, irot    : ArbInt;
    xyzw                : r4Ar absolute xyzw1;
    kx, ky              : ^arfloat_1;
    a, f, h             : ^arfloat1;
    c                   : arfloat1 absolute c11;
    nummer, index       : ^arint1;
    hx, hy              : r4;
    ichang, fullrank    : boolean;
begin

    eps := 10*macheps;
(*  find the position of the additional knots which are needed for the
  b-spline representation of s(x,y) *)
    iband := 1+min(3*qy+3,3*qx+3);
    if qy>qx then
    begin
       ichang := true;
       kx := @kymin1; ky := @kxmin1;
       for i:=1 to m do with xyzw[i] do Wisselr(x, y);
       WisselI(qx, qy)
    end else
    begin
       ichang := false;
       kx := @kxmin1; ky := @kymin1;
    end;
    with xyzw[1] do begin x0 := x; x1 := x; y0 := y; y1 := y end;
    for i:=2 to m do with xyzw[i] do
    begin if x<x0 then x0 := x; if x>x1 then x1 := x;
          if y<y0 then y0 := y; if y>y1 then y1 := y
    end;
    for i:=-1 to 2 do kx^[i] := x0;
    for i:=-1 to 2 do ky^[i] := y0;
    for i:=qx-1 to qx+2 do kx^[i] := x1;
    for i:=qy-1 to qy+2 do ky^[i] := y1;
(*  arrange the data points according to the panel they belong to *)
    nreg := (qx-3)*(qy-3);
    ncof := qx*qy;
    GetMem(nummer, m*SizeOf(ArbInt));
    GetMem(index, nreg*SizeOf(ArbInt));
    GetMem(h, iband*SizeOf(ArbFloat));
    GetMem(a, iband*ncof*SizeOf(ArbFloat));
    GetMem(f, ncof*SizeOf(ArbFloat));
    fporde(m, qx, qy, xyzw1, kx^[-1], ky^[-1], nummer^[1], index^[1]);
    for i:=1 to ncof do f^[i] := 0;
    for j:=1 to ncof*iband do a^[j] := 0;
    residu := 0;
(*  fetch the data points in the new order. main loop for the different panels *)
    for num:=1 to nreg do
    begin
       lx := (num-1) div (qy-3); l1 := lx+2;
       ly := (num-1) mod (qy-3); l2 := ly+2;
       jrot := lx*qy+ly;
       inpanel := index^[num];
       while inpanel<>0 do
       with xyzw[inpanel] do
       begin
          wi := w; zi := z*wi;
          fpbspl(kx^[-1], x, l1, hx);
          fpbspl(ky^[-1], y, l2, hy);
          for i:=1 to iband do h^[i] := 0;
          i1 := 0;
          for i:=1 to 4 do
          begin
            hxi := hx[i]; j1 := i1;
            for j:=1 to 4 do begin Inc(j1); h^[j1] := hxi*hy[j]*wi end;
            Inc(i1, qy)
          end;
          irot := jrot;
          for i:=1 to iband do
          begin
            Inc(irot); piv := h^[i];
            if piv<>0 then
            begin
              fpgivs(piv, a^[(irot-1)*iband+1], cos1, sin1);
              fprota(cos1, sin1, zi, f^[irot]);
              for j:=i+1 to iband do
                fprota(cos1, sin1, h^[j], a^[(irot-1)*iband+j-i+1])
            end;
          end;
          residu := residu+sqr(zi);
          inpanel := nummer^[inpanel]
      end;
   end;

   dmax := 0;
   i := 1;
   while i<ncof*iband do
   begin
      if dmax<a^[i] then dmax:=a^[i]; Inc(i, iband)
   end;

   sigma := eps*dmax;
   i := 1; fullrank := true;
   while fullrank and (i<ncof*iband) do
   begin
      fullrank := a^[i]>sigma; Inc(i, iband)
   end;

   term := 2; if not fullrank then goto einde;
   term := 1;

   fpback(a^[1], f^[1], ncof, iband, c11);
   if ichang then
   begin
      l1 := 1;
      for i:=1 to qx do
      begin
        l2 := i;
        for j:=1 to qy do
        begin
          f^[l2] := c[l1]; Inc(l1); Inc(l2, qx)
        end;
      end;
      for i:=1 to ncof do c[i] := f^[i]
   end;

einde:
   if ichang then for i:=1 to m do with xyzw[i] do Wisselr(x, y);
   FreeMem(f, ncof*SizeOf(ArbFloat));
   FreeMem(a, iband*ncof*SizeOf(ArbFloat));
   FreeMem(h, iband*SizeOf(ArbFloat));
   FreeMem(index, nreg*SizeOf(ArbInt));
   FreeMem(nummer, m*SizeOf(ArbInt))
end;


procedure spl1nati(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt);
var
    xyc           : r3Ar absolute XYC1;
    l, b, d, u, c : ^arfloat1;
    h2, h3, s2, s3: ArbFloat;
    i, m          : ArbInt;       { afmeting van op te lossen stelsel }
begin
    term:=3;
    if n < 2 then exit;
    for i:=2 to n do if xyc[i-1].x>=xyc[i].x then exit;
    term:=1;
    xyc[1].w := 0; xyc[n].w := 0;  { c1=cn=0 }
    m := n-2;
    if m=0 then exit;

    getmem(u, n*SizeOf(ArbFloat));
    getmem(l, n*Sizeof(ArbFloat));
    getmem(d, n*SizeOf(ArbFloat));
    getmem(c, n*SizeOf(ArbFloat));
    getmem(b, n*SizeOf(ArbFloat));
    h3:=xyc[2].x-xyc[1].x;
    s3:=(xyc[2].y-xyc[1].y)/h3;

    for i:=2 to n-1 do
    begin
      h2:=h3; h3:=xyc[i+1].x-xyc[i].x;
      s2:=s3; s3:=(xyc[i+1].y-xyc[i].y)/h3;
      l^[i]:=h2/6;
      d^[i]:=(h2+h3)/3;
      u^[i]:=h3/6;
      b^[i]:=s3-s2
    end;
    sledtr(m, l^[3], d^[2], u^[2], b^[2], c^[2], term);
    for i:=2 to n-1 do xyc[i].w := c^[i];
    Freemem(b, n*SizeOf(ArbFloat));
    Freemem(c, n*SizeOf(ArbFloat));
    Freemem(d, n*SizeOf(ArbFloat));
    Freemem(l, n*Sizeof(ArbFloat));
    Freemem(u, n*SizeOf(ArbFloat));
end; {spl1nati}

procedure spl1naki(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt);
var
    xyc           : r3Ar absolute XYC1;
    l, b, d, u, c : ^arfloat1;
    h2, h3, s2, s3: ArbFloat;
    i, m          : ArbInt;       { Dimensions of set lin eqs to solve}
begin
    term:=3;
    if n < 4 then exit;
    for i:=2 to n do if xyc[i-1].x>=xyc[i].x then exit;
    term:=1;
    m := n-2;
    getmem(u, n*SizeOf(ArbFloat));
    getmem(l, n*Sizeof(ArbFloat));
    getmem(d, n*SizeOf(ArbFloat));
    getmem(c, n*SizeOf(ArbFloat));
    getmem(b, n*SizeOf(ArbFloat));
    h3:=xyc[2].x-xyc[1].x;
    s3:=(xyc[2].y-xyc[1].y)/h3;
    for i:=2 to n-1 do
    begin
      h2:=h3; h3:=xyc[i+1].x-xyc[i].x;
      s2:=s3; s3:=(xyc[i+1].y-xyc[i].y)/h3;
      l^[i]:=h2/6;
      d^[i]:=(h2+h3)/3;
      u^[i]:=h3/6;
      b^[i]:=s3-s2
    end;
    d^[n-1]:=d^[n-1]+h3/6*(1+h3/h2); l^[n-1]:=l^[n-1]-sqr(h3)/(6*h2);
    h2:=xyc[2].x-xyc[1].x; h3:=xyc[3].x-xyc[2].x;
    d^[2]:=d^[2]+h2/6*(1+h2/h3); u^[2]:=u^[2]-sqr(h2)/(6*h3);

    sledtr(m, l^[3], d^[2], u^[2], b^[2], c^[2], term);
    for i:=2 to n-1 do xyc[i].w := c^[i];
    xyc[1].w := xyc[2].w + (h2/h3)*(xyc[2].w-xyc[3].w);
    h2:=xyc[n-1].x-xyc[n-2].x; h3:=xyc[n].x-xyc[n-1].x;
    xyc[n].w := xyc[n-1].w + (h3/h2)*(xyc[n-1].w-xyc[n-2].w);
    Freemem(b, n*SizeOf(ArbFloat));
    Freemem(c, n*SizeOf(ArbFloat));
    Freemem(d, n*SizeOf(ArbFloat));
    Freemem(l, n*Sizeof(ArbFloat));
    Freemem(u, n*SizeOf(ArbFloat));
end; {spl1naki}

procedure spl1cmpi(n: ArbInt; var xyc1: ArbFloat; dy1, dyn: ArbFloat;
                 var term: ArbInt);
var
    xyc           : r3Ar absolute XYC1;
    l, b, d, u, c : ^arfloat1;
    h2, h3, s2, s3: ArbFloat;
    i             : ArbInt;     { Dimensions of set lin eqs to solve}
begin
    term:=3;
    if n < 2 then exit;
    for i:=2 to n do if xyc[i-1].x>=xyc[i].x then exit;
    term:=1;
    getmem(u, n*SizeOf(ArbFloat));
    getmem(l, n*Sizeof(ArbFloat));
    getmem(d, n*SizeOf(ArbFloat));
    getmem(c, n*SizeOf(ArbFloat));
    getmem(b, n*SizeOf(ArbFloat));
    h3:=xyc[2].x-xyc[1].x;
    s3:=(xyc[2].y-xyc[1].y)/h3;
    d^[1] := h3/3; u^[1] := h3/6; b^[1] := -dy1+s3;
    for i:=2 to n-1 do
    begin
      h2:=h3; h3:=xyc[i+1].x-xyc[i].x;
      s2:=s3; s3:=(xyc[i+1].y-xyc[i].y)/h3;
      l^[i]:=h2/6;
      d^[i]:=(h2+h3)/3;
      u^[i]:=h3/6;
      b^[i]:=s3-s2
    end;
    d^[n] := h3/3; l^[n] := h3/6; b^[n] := dyn-s3;

    sledtr(n, l^[2], d^[1], u^[1], b^[1], c^[1], term);
    for i:=1 to n do xyc[i].w := c^[i];
    Freemem(b, n*SizeOf(ArbFloat));
    Freemem(c, n*SizeOf(ArbFloat));
    Freemem(d, n*SizeOf(ArbFloat));
    Freemem(l, n*Sizeof(ArbFloat));
    Freemem(u, n*SizeOf(ArbFloat));
end; {spl1cmpi}

procedure spl1peri(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt);
var
    xyc           : r3Ar absolute XYC1;
    l, b, d, u, c, k : ^arfloat1;
    k2, kn1, dy1, cn,
    h2, h3, s2, s3: ArbFloat;
    i, m          : ArbInt;             { Dimensions of set lin eqs to solve}
begin
    term:=3;
    if n < 2 then exit;
    if xyc[1].y<>xyc[n].y then exit;
    for i:=2 to n do if xyc[i-1].x>=xyc[i].x then exit;
    term:=1;
    m := n-2;
    xyc[1].w := 0; xyc[n].w := 0;  { c1=cn=0 }
    if m=0 then exit;
    if m=1 then
    begin
       h2:=xyc[2].x-xyc[1].x;
       s2:=(xyc[2].y-xyc[1].y)/h2;
       h3:=xyc[3].x-xyc[2].x;
       s3:=(xyc[3].y-xyc[2].y)/h3;
       xyc[2].w := 6*(s3-s2)/(h2+h3);
       xyc[3].w := -xyc[2].w;
       xyc[1].w := xyc[3].w;
       exit
    end;

    getmem(u, n*SizeOf(ArbFloat));
    getmem(l, n*Sizeof(ArbFloat));
    getmem(k, n*SizeOf(ArbFloat));
    getmem(d, n*SizeOf(ArbFloat));
    getmem(c, n*SizeOf(ArbFloat));
    getmem(b, n*SizeOf(ArbFloat));
    h3:=xyc[2].x-xyc[1].x;
    s3:=(xyc[2].y-xyc[1].y)/h3;
    k2 := h3/6; dy1 := s3;
    for i:=2 to n-1 do
    begin
      h2:=h3; h3:=xyc[i+1].x-xyc[i].x;
      s2:=s3; s3:=(xyc[i+1].y-xyc[i].y)/h3;
      l^[i]:=h2/6;
      d^[i]:=(h2+h3)/3;
      u^[i]:=h3/6;
      b^[i]:=s3-s2;
      k^[i]:=0
    end;
    kn1 := h3/6; k^[2] := k2; k^[n-1] := kn1;
    sledtr(m, l^[3], d^[2], u^[2], k^[2], k^[2], term);
    sledtr(m, l^[3], d^[2], u^[2], b^[2], c^[2], term);
    cn := (dy1-s3-k2*c^[2]-kn1*c^[n-1])/(2*(k2+kn1)-k2*k^[2]-kn1*k^[n-1]);
    for i:=2 to n-1 do xyc[i].w := c^[i] - cn*k^[i];
    xyc[1].w := cn; xyc[n].w := cn;
    Freemem(b, n*SizeOf(ArbFloat));
    Freemem(c, n*SizeOf(ArbFloat));
    Freemem(d, n*SizeOf(ArbFloat));
    Freemem(l, n*Sizeof(ArbFloat));
    Freemem(k, n*SizeOf(ArbFloat));
    Freemem(u, n*SizeOf(ArbFloat));
end; {spl1peri}

function spl1pprv(n: ArbInt; var xyc1: ArbFloat; t: ArbFloat; var term: ArbInt): ArbFloat;
var
   xyc          : r3Ar absolute XYC1;
   i, j, m      : ArbInt;
   d, d3, h, dy : ArbFloat;
begin                          { Assumption : x[i]<x[i+1] i=1..n-1 }
  spl1pprv := NaN;
  term:=3; if n<2 then exit;
  if (t<xyc[1].x) or (t>xyc[n].x) then exit;
  term:=1;
  i:=1; j:=n;
  while j <> i+1 do
  begin
      m:=(i+j) div 2;
      if t>=xyc[m].x then i:=m else j:=m
  end;   { x[i]<= t <=x[i+1] }
  h     := xyc[i+1].x-xyc[i].x;
  d     := t-xyc[i].x;
  d3    :=(xyc[i+1].w-xyc[i].w)/h;
  dy    :=(xyc[i+1].y-xyc[i].y)/h-h*(2*xyc[i].w+xyc[i+1].w)/6;
  spl1pprv:= xyc[i].y+d*(dy+d*(xyc[i].w/2+d*d3/6))

end; {spl1pprv}

procedure spl1nalf(n: ArbInt; var xyw1: ArbFloat; lambda:ArbFloat;
                     var xac1, residu: ArbFloat; var term: ArbInt);
var
   xyw        : r3Ar absolute xyw1;
   xac        : r3Ar absolute xac1;
   i, j, ncd  : ArbInt;
   ca, crow   : ArbFloat;
   h, qty     : ^arfloat1;
   ch         : ^arfloat0;
   qtdq       : ^arfloat1;
begin
   term := 3;                   { testing input}
   if n<2 then exit;
   for i:=2 to n do if xyw[i-1].x>=xyw[i].x then exit;
   for i:=1 to n do if xyw[i].w<=0 then exit;
   if lambda<0 then exit;
   term := 1;
   Move(xyw, xac, n*SizeOf(r_3));
   if n=2 then begin xac[1].w := 0; xac[2].w := 0; exit end;

   Getmem(ch, (n+2)*SizeOf(ArbFloat)); FillChar(ch^, (n+2)*SizeOf(ArbFloat), 0);
   Getmem(h, n*SizeOf(ArbFloat));
   Getmem(qty, n*SizeOf(ArbFloat));
   ncd := n-3; if ncd>2 then ncd := 2;
   Getmem(qtdq, ((n-2)*(ncd+1)-(ncd*(ncd+1)) div 2)*SizeOf(ArbFloat));
   for i:=2 to n do h^[i] := 1/(xyw[i].x-xyw[i-1].x); h^[1] := 0;
   for i:=1 to n-2
   do qty^[i] := (h^[i+1]*xyw[i].y -
                  (h^[i+1]+h^[i+2])*xyw[i+1].y +
                  h^[i+2]*xyw[i+2].y);
   j := 1; i := 1;
   qtdq^[j] := sqr(h^[i+1])/xyw[i].w +
               sqr(h^[i+1]+h^[i+2])/xyw[i+1].w +
               sqr(h^[i+2])/xyw[i+2].w +
               lambda*(1/h^[i+1]+1/h^[i+2])/3;
   Inc(j);
   if ncd>0 then
   begin i := 2;
      qtdq^[j] := -h^[i+1]*(h^[i]+h^[i+1])/xyw[i].w
                  -h^[i+1]*(h^[i+1]+h^[i+2])/xyw[i+1].w +
                   lambda/h^[i+1]/6;
      Inc(j);
      qtdq^[j] := sqr(h^[i+1])/xyw[i].w +
                  sqr(h^[i+1]+h^[i+2])/xyw[i+1].w +
                  sqr(h^[i+2])/xyw[i+2].w +
                  lambda*(1/h^[i+1]+1/h^[i+2])/3;
      Inc(j)
   end;
   for i:=3 to n-2
   do begin
      qtdq^[j] := h^[i]*h^[i+1]/xyw[i].w;
      Inc(j);
      qtdq^[j] := -h^[i+1]*(h^[i]+h^[i+1])/xyw[i].w
                  -h^[i+1]*(h^[i+1]+h^[i+2])/xyw[i+1].w +
                   lambda/h^[i+1]/6;
      Inc(j);
      qtdq^[j] := sqr(h^[i+1])/xyw[i].w +
                  sqr(h^[i+1]+h^[i+2])/xyw[i+1].w +
                  sqr(h^[i+2])/xyw[i+2].w +
                  lambda*(1/h^[i+1]+1/h^[i+2])/3;
      Inc(j)
   end;
   { Solving for c/lambda }
   Slegpb(n-2, ncd, qtdq^[1], qty^[1], ch^[2], ca, term);
   if term=1 then
   begin
       residu := 0;
       for i:=1 to n do
       begin
         crow := (h^[i]*ch^[i-1] - (h^[i]+h^[i+1])*ch^[i]+h^[i+1]*ch^[i+1])
                 /xyw[i].w;
         xac[i].y := xyw[i].y - crow;
         residu := residu + sqr(crow)*xyw[i].w
       end;
       xac[1].w := 0;
       for i:=2 to n-1 do xac[i].w := lambda*ch^[i];
       xac[n].w := 0;
   end;
   Freemem(qtdq, ((n-2)*(ncd+1)-(ncd*(ncd+1)) div 2)*SizeOf(ArbFloat));
   Freemem(qty, n*SizeOf(ArbFloat));
   Freemem(h, n*SizeOf(ArbFloat));
   Freemem(ch, (n+2)*SizeOf(ArbFloat));
end;


procedure spl2nalf(n: ArbInt; var xyzw1: ArbFloat; lambda:ArbFloat;
                   var xyg0, residu: ArbFloat; var term: ArbInt);
type  R3 = array[1..3] of ArbFloat;
      R33= array[1..3] of R3;
      Rn3= array[1..$ffe0 div SizeOf(R3)] of R3;

var b,e21t,ht   :^Rn3;
    pfac        :par2dr1;
    e22         :R33;
    i,j,l,i1,i2,n3 :ArbInt;
    s,s1,px,py,hr,ca,
    x,absdet,x1,x2,
    absdetmax   :ArbFloat;
    vr          :R4x;
    wr          :R2;
    w,u         :R3;
    a_alfa_d    :R4xAr absolute xyzw1;
    a_gamma     :nsp2rec absolute xyg0;
    gamma       :^arfloat1;


  function e(var x,y:R2):ArbFloat;
  const c1:ArbFloat=1/(16*pi);
    var s:ArbFloat;
    begin s:=sqr(x[1]-y[1]) +sqr(x[2]-y[2]);
      if s=0 then e:=0 else e:=c1*s*ln(s)
    end {e};

   procedure pfxpfy(var a,b,c:R2;var f:r3; var pfx,pfy:ArbFloat);
    var det:ArbFloat;
    begin det:=(b[1]-a[1])*(c[2]-a[2]) - (b[2]-a[2])*(c[1]-a[1]);
      pfx:=((f[2]-f[1])*(c[2]-a[2]) - (f[3]-f[1])*(b[2]-a[2]))/det;
      pfy:=(-(f[2]-f[1])*(c[1]-a[1]) + (f[3]-f[1])*(b[1]-a[1]))/det
    end {pfxpfy};

  procedure pxpy(var a,b,c:R2; var px,py:ArbFloat);
    var det : ArbFloat;
    begin det:=(b[1]-a[1])*(c[2]-a[2]) - (b[2]-a[2])*(c[1]-a[1]);
      px:=(b[2]-c[2])/det; py:=(c[1]-b[1])/det
    end {pxpy};

  function p(var x,a:R2; var px,py:ArbFloat):ArbFloat;
    begin p:=1 + (x[1]-a[1])*px +(x[2]-a[2])*py end {p};

  procedure slegpdlown(n: ArbInt; var a1; var bx1: ArbFloat;
                    var term: ArbInt);
   var i, j, k, kmin1 : ArbInt;
       h, lkk : ArbFloat;
       a  : ar2dr1 absolute a1;
       x  : arfloat1 absolute bx1;
   begin
     k:=0; term := 2;
     while (k<n) do
       begin
         kmin1:=k; k:=k+1; lkk:=a[k]^[k];
         for j:=1 to kmin1 do lkk:=lkk-sqr(a[k]^[j]);
         if lkk<=0 then exit else
           begin
             a[k]^[k]:=sqrt(lkk); lkk:=a[k]^[k];
             for i:=k+1 to n do
               begin
                 h:=a[i]^[k];
                 for j:=1 to kmin1 do h:=h-a[k]^[j]*a[i]^[j];
                 a[i]^[k]:=h/lkk
               end; {i}
             h:=x[k];
             for j:=1 to kmin1 do h:=h-a[k]^[j]*x[j];
             x[k]:=h/lkk
           end {lkk > 0}
       end; {k}
           for i:=n downto 1 do
             begin
               h:=x[i];
               for j:=i+1 to n do h:=h-a[j]^[i]*x[j];
               x[i]:=h/a[i]^[i];
             end; {i}
      term := 1
   end;

begin
    term := 3; if n<3 then exit;
    n3 := n - 3;
    i1:=1; x1:=a_alfa_d[1].xy[1]; i2:=1; x2:=x1;
    for i:= 2 to n do
    begin hr:=a_alfa_d[i].xy[1];
      if hr < x1 then begin i1:=i; x1:=hr end else
      if hr > x2 then begin i2:=i; x2:=hr end;
    end;
    vr:=a_alfa_d[n-2]; a_alfa_d[n-2]:=a_alfa_d[i1]; a_alfa_d[i1]:=vr;
    vr:=a_alfa_d[n-1]; a_alfa_d[n-1]:=a_alfa_d[i2]; a_alfa_d[i2]:=vr;

    for i:=1 to 2 do vr.xy[i]:=a_alfa_d[n-2].xy[i]-a_alfa_d[n-1].xy[i];
    absdetmax:=-1; i1:=0;
    for i:=1 to n do
    begin for j:=1 to 2 do wr[j]:=a_alfa_d[i].xy[j]-a_alfa_d[n-2].xy[j];
      if a_alfa_d[i].d<=0 then exit;
      absdet:=abs(wr[1]*vr.xy[2]-wr[2]*vr.xy[1]);
      if absdet > absdetmax then begin i1:=i; absdetmax:=absdet end;
    end;
    term := 4;
    if absdetmax<=macheps*abs(x2-x1) then exit;
    term := 1;
    vr:=a_alfa_d[n]; a_alfa_d[n]:=a_alfa_d[i1]; a_alfa_d[i1]:=vr;
    GetMem(e21t, n3*SizeOf(r3));
    GetMem(b, n3*SizeOf(r3));
    GetMem(gamma, n*SizeOf(ArbFloat));

    pxpy(a_alfa_d[n-2].xy,a_alfa_d[n-1].xy,a_alfa_d[n].xy,px,py);
    for i:=1 to n3 do b^[i][1]:=p(a_alfa_d[i].xy,a_alfa_d[n-2].xy,px,py);
    pxpy(a_alfa_d[n-1].xy,a_alfa_d[n].xy,a_alfa_d[n-2].xy,px,py);
    for i:=1 to n3 do b^[i][2]:=p(a_alfa_d[i].xy,a_alfa_d[n-1].xy,px,py);
    pxpy(a_alfa_d[n].xy,a_alfa_d[n-2].xy,a_alfa_d[n-1].xy,px,py);
    for i:=1 to n3 do b^[i][3]:=p(a_alfa_d[i].xy,a_alfa_d[n].xy,px,py);
    e22[1,1]:=0; e22[2,2]:=0; e22[3,3]:=0;
    e22[2,1]:=e(a_alfa_d[n-1].xy,a_alfa_d[n-2].xy); e22[1,2]:=e22[2,1];
    e22[3,1]:=e(a_alfa_d[n].xy,a_alfa_d[n-2].xy); e22[1,3]:=e22[3,1];
    e22[3,2]:=e(a_alfa_d[n].xy,a_alfa_d[n-1].xy); e22[2,3]:=e22[3,2];
    for i:=1 to 3 do
    for j:=1 to n3 do e21t^[j,i]:=e(a_alfa_d[n3+i].xy,a_alfa_d[j].xy);

    GetMem(ht, n3*SizeOf(r3));
    for i:=1 to 3 do
    for j:=1 to n3 do
    begin s:=0;
      for l:= 1 to 3 do s:=s+e22[i,l]*b^[j][l]; ht^[j][i]:=s
    end;
    AllocateL2dr(n3,pfac);
    for i:= 1 to n3 do
    for j:= 1 to i do
    begin if j=i then s1:=0 else s1:=e(a_alfa_d[i].xy,a_alfa_d[j].xy);
      for l:= 1 to 3 do s1:=s1+b^[i][l]*(ht^[j][l]-e21t^[j][l])-e21t^[i][l]*b^[j][l];
      if j=i then s:=1/a_alfa_d[i].d else s:=0;
      for l:= 1 to 3 do s:=s+b^[i][l]*b^[j][l]/a_alfa_d[n3+l].d;
      pfac^[i]^[j] := s1+s/lambda
    end;
    for i:= 1 to n3 do
      gamma^[i]:=a_alfa_d[i].alfa-b^[i][1]*a_alfa_d[n-2].alfa-b^[i][2]*a_alfa_d[n-1].alfa-b^[i][3]*a_alfa_d[n].alfa;
    slegpdlown(n3,pfac^[1],gamma^[1],term);
    DeAllocateL2dr(n3,pfac);
    FreeMem(ht, n3*SizeOf(r3));

    if term=1 then
     begin
      for i:= 1 to 3 do
      begin s:= 0;
        for j:= 1 to n3 do
         s:=s+b^[j][i]*gamma^[j]; w[i]:=s;
        gamma^[n3+i]:=-w[i]
     end;{w=btgamma}
      for i:=1 to 3 do
      begin s:=0;
        for l:=1 to n3 do s:=s+e21t^[l][i]*gamma^[l];
        s1:=0;
        for l:=1 to 3 do s1:=s1+e22[i,l]*w[l];
        u[i]:=a_alfa_d[n3+i].alfa+w[i]/(lambda*a_alfa_d[n3+i].d)+s1-s
      end;
      with a_gamma[0] do
      pfxpfy(a_alfa_d[n-2].xy,a_alfa_d[n-1].xy,a_alfa_d[n].xy,u,xy[1],xy[2]);
      residu:=0;for i:=1 to n3 do residu:=residu+sqr(gamma^[i])/a_alfa_d[i].d;
      for i:= 1 to 3 do residu:=residu+sqr(w[i])/a_alfa_d[n3+i].d;
      residu:=residu/sqr(lambda);
      a_gamma[0].gamma := u[1];
      for i:=1 to n do
      begin
       a_gamma[i].xy := a_alfa_d[i].xy;
       a_gamma[i].gamma := gamma^[i]
      end;
    end;
    FreeMem(gamma, n*SizeOf(ArbFloat));
    FreeMem(b, n3*SizeOf(r3));
    FreeMem(e21t, n3*SizeOf(r3))
  end;

function spl2natv(n: ArbInt; var xyg0: ArbFloat; u, v: ArbFloat): ArbFloat;

const c1: ArbFloat=1/(16*pi);

  var i         : ArbInt;
      s         : ArbFloat;
      a_gamma   : nsp2rec absolute xyg0;
      z         : R2;

  function e(var x,y:R2):ArbFloat;
    var s:ArbFloat;
    begin
      s:=sqr(x[1]-y[1]) + sqr(x[2]-y[2]);
      if s=0 then
       e:= 0
      else
       e:=s*ln(s)
    end {e};

  function pf(var x,a:R2;fa,pfx,pfy:ArbFloat):ArbFloat;
    begin
     pf:=fa + (x[1]-a[1])*pfx + (x[2]-a[2])*pfy
    end {pf};

  begin
    s:=0;
    z[1] := u;
    z[2] := v;
    for i:=1 to n do
     s:=s+a_gamma[i].gamma*e(z, a_gamma[i].xy);
    with a_gamma[0] do
     spl2natv :=s*c1+pf(z,a_gamma[n-2].xy, gamma, xy[1], xy[2])
  end;

begin

end.
