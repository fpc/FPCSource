{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Unit to find roots of (various kinds of) equations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit roo;
{$i direct.inc}

interface

uses typ, spe;

{Find the all roots of the binomial eq. x^n=a, with "a" a complex number}

Procedure roobin(n: ArbInt; a: complex; Var z: complex; Var term: ArbInt);

{Find root point of f(x)=0 with f(x) a continuous function on domain [a,b]
 If f(a)*f(b)<=0 then there must be (at least) one rootpoint}

Procedure roof1r(f: rfunc1r; a, b, ae, re: ArbFloat; Var x: ArbFloat;
                 Var term: ArbInt);

{Determine all zeropoints for a given n'th degree polynomal with real
coefficients}

Procedure roopol(Var a: ArbFloat; n: ArbInt; Var z: complex;
                 Var k, term: ArbInt);

{Find roots for a simple 2th degree eq  x^2+px+q=0 with p and q real}

Procedure rooqua(p, q: ArbFloat; Var z1, z2: complex);

{Roofnr is undocumented, but verry big}

Procedure roofnr(f: roofnrfunc; n: ArbInt; Var x, residu: ArbFloat; re: ArbFloat;
                 Var term: ArbInt);

{ term : 1     succesful termination
         2     Couldn't reach the specified precision
               Value X is the best one which could be found.
         3     Wrong input
         4     Too many functionvalues calculated, try to recalc with the
                calculated X
         5     Not enough progress. Possibly there is no solution, or the
               solution is too close to 0. Try to choose a different
               initial startingvalue
         6     Process wants to calculate a function value outside the by
               "deff" defined area.
}

implementation

Procedure roobin(n: ArbInt; a: complex; Var z: complex; Var term: ArbInt);
{ This procedure solves the binomial equation z**n = a, with a complex}

Var         i, j, k : ArbInt;
    w, fie, dfie, r : ArbFloat;
                 pz : ^arcomp1;
Begin
  If n<1 Then
   Begin
      term := 2;
    exit
   End;
  term := 1;
 pz := @z;
 dfie := 2*pi/n;
 k := 1;
  If a.im=0 Then
   Begin
      If a.re>0 Then
       Begin
          r := spepow(a.re, 1/n);
        pz^[1].Init(r, 0);
          k := k+1;
        i := (n-1) Div 2;
          If Not odd(n) Then
           Begin
              pz^[k].Init(-r, 0);
            k := k+1
           End;
          For j:=1 To i Do
           Begin
              w := j*dfie;
              pz^[k].Init(r*cos(w), r*sin(w));
              pz^[k+1] := pz^[k];
            pz^[k+1].Conjugate;
              k := k+2
           End
       End
    Else
      Begin
          fie := pi/n;
       r := spepow(-a.re, 1/n);
       i := n Div 2-1;
          If odd(n) Then
           Begin
              pz^[k].Init(-r, 0);
            k := k+1
           End;
          For j:=0 To i Do
           Begin
              w := fie+j*dfie;
              pz^[k].Init(r*cos(w), r*sin(w));
              pz^[k+1] := pz^[k];
            pz^[k+1].Conjugate;
              k := k+2
           End
      End
   End
 Else
  Begin
      If abs(a.re)>=abs(a.im) Then
       r := spepow(abs(a.re)*sqrt(1+sqr(a.im/a.re)), 1/n)
      Else r := spepow(abs(a.im)*sqrt(1+sqr(a.re/a.im)), 1/n);
      fie := a.arg/n;
   i := n Div 2;
      For j:=0 To n-1 Do
       Begin
          w := fie+(j-i)*dfie;
          pz^[j+1].Init(r*cos(w), r*sin(w))
       End
   End
End {roobin};

Procedure roof1r(f: rfunc1r; a, b, ae, re: ArbFloat; Var x: ArbFloat;
                 Var term: ArbInt);

Var fa, fb, c, fc, m, tol, w1, w2 : ArbFloat;
                                k : ArbInt;
                             stop : boolean;

Begin
  fa := f(a);
 fb := f(b);
  If (spesgn(fa)*spesgn(fb)=1) Or (ae<0) Or (re<0)
   Then  {wrong input}
    Begin
      term := 3;
     exit
    End;
  If abs(fb)>abs(fa) Then
    Begin
      c := b;
     fc := fb;
     x := a;
     b := a;
     fb := fa;
     a := c;
     fa := fc
    End
 Else
    Begin
      c := a;
     fc := fa;
     x := b
    End;
  k := 0;
  tol := ae+re*spemax(abs(a), abs(b));
  w1 := abs(b-a);
 stop := false;
  while (abs(b-a)>tol) and (fb<>0) and (Not stop) Do
    Begin
      m := (a+b)/2;
      If (k>=2) Or (fb=fc) Then x := m
     Else
        Begin
          x := (b*fc-c*fb)/(fc-fb);
          If abs(b-x)<tol Then x := b-tol*spesgn(b-a);
          If spesgn(x-m)=spesgn(x-b) Then x := m
        End;
      c := b;
     fc := fb;
     b := x;
     fb := f(x);
      If spesgn(fa)*spesgn(fb)>0 Then
        Begin
          a := c;
         fa := fc;
         k := 0
        End
     Else k := k+1;
      If abs(fb)>=abs(fa) Then
        Begin
          c := b;
         fc := fb;
         x := a;
         b := a;
         fb := fa;
         a := c;
         fa := fc;
         k := 0
        End;
      tol := ae+re*spemax(abs(a), abs(b));
      w2 := abs(b-a);
      If w2>=w1 Then
        Begin
          stop := true;
         term := 2
        End;
      w1 := w2
    End;
  If Not stop Then term := 1
End {roof1r};

Procedure roopol(Var a: ArbFloat; n: ArbInt; Var z: complex;
                 Var k, term: ArbInt);

Const max = 50;

Type  rnep2 = array[-2..$ffe0 div SizeOf(ArbFloat)] Of ArbFloat;

Var rk, i, j, l, m, length, term1                             : ArbInt;
    p, q, r, s, f, df, delp, delq, delr, telp, telq, sn, sn1,
    sn2, noise, noise1, noise2, g, absr, maxcoef, coef, d, t,
    maxx, fac, meps                                           : ArbFloat;
    convergent, linear, quadratic                             : boolean;
    u, v                                                      : complex;
    pa                                                        : ^arfloat1;
    pb, pc, ph                                                : ^rnep2;
    pz                                                        : ^arcomp1;

Function gcd(n, m: ArbInt): ArbInt;
{ This function computes the greatest common divisor of m and n}

Var r : ArbInt;
Begin
    r := n Mod m;
    while r>0 Do
    Begin
        n := m;
     m := r;
     r := n Mod m
    End;
    gcd := m
End {gcd};
Begin
    If n<1 Then
     Begin
        term := 3;
      exit
     End;
    length := (n+3)*sizeof(ArbFloat);
    getmem(pb, length);
 getmem(pc, length);
 getmem(ph, length);
    meps := macheps;
    pa := @a;
 pz := @z;
    pb^[-2] := 0;
 pb^[-1] := 0;
 pc^[-2] := 0;
 pc^[-1] := 0;
 ph^[-1] := 0;
 ph^[0] := 1;
    For i:=1 To n Do
     ph^[i] := pa^[i];
    k := 0;
    while (n>0) and (ph^[n]=0) Do
    Begin
        k := k+1;
     pz^[k].Init(0, 0);
     n := n-1
    End;
    If n>0 Then
     Begin
        l := n;
      i := 1;
        while (l>1) and (i<n) Do
        Begin
            If ph^[i] <> 0 Then l := gcd(l, n-i);
         i := i+1
        End;
        If l>1 Then
         Begin
            n := n Div l;
            For i:=1 To n Do
             ph^[i] := ph^[l*i]
         End
     End;
    convergent := true ;
    while (n>0) and convergent Do
    Begin
        linear := false;
     quadratic := false ;
        If n=1 Then
         Begin
            r := -ph^[1]/ph^[0];
          linear := true
         End;
        If n=2 Then
         Begin
            p := ph^[1]/ph^[0];
          q := ph^[2]/ph^[0];
          quadratic := true
         End;
        If n>2 Then
         Begin
            If (ph^[n-1]=0) Or (ph^[n-2]=0) Then
             Begin
                maxcoef := abs(ph^[n-1]/ph^[n]);
                For i:=2 To n Do
                 Begin
                    coef := spepow(abs(ph^[n-i]/ph^[n]),1/i);
                    If maxcoef<coef Then maxcoef := coef
                 End;
                maxcoef := 2*maxcoef
             End;
            If ph^[n-1]=0 Then r := -spesgn(ph^[0])*spesgn(ph^[n])/maxcoef
            Else r := -ph^[n]/ph^[n-1];
            If ph^[n-2]=0 Then
             Begin
                p := 0;
              q := -1/sqr(maxcoef)
             End
          Else
            Begin
                q := ph^[n]/ph^[n-2];
             p := (ph^[n-1]-q*ph^[n-3])/ph^[n-2]
            End;
            m := 0;
            while (m<max) and (Not linear) and (Not quadratic) Do
            Begin
                m := m+1;
                For j:=0 To n Do
                 pb^[j] := ph^[j]-p*pb^[j-1]-q*pb^[j-2];
                For j:=0 To n-2 Do
                 pc^[j] := pb^[j]-p*pc^[j-1]-q*pc^[j-2];
                pc^[n-1] := -p*pc^[n-2]-q*pc^[n-3];
                s := sqr(pc^[n-2])-pc^[n-1]*pc^[n-3];
                telp := pb^[n-1]*pc^[n-2]-pb^[n]*pc^[n-3];
                telq := pb^[n]*pc^[n-2]-pb^[n-1]*pc^[n-1];
                If s=0 Then
                 Begin
                    delp := telp;
                  delq := telq
                 End
             Else
                Begin
                    delp := telp/s;
                 delq := telq/s
                End;
                noise1 := 0;
             sn1 := 0;
             sn := 1;
                noise2 := 4*abs(pb^[n])+3*abs(p*pb^[n-1]);
                For j:=n-1 Downto 0 Do
                 Begin
                    g := 4*abs(pb^[j])+3*abs(p*pb^[j-1]);
                    noise1 := noise1+g*abs(sn);
                    sn2 := sn1;
                  sn1 := sn;
                  sn := -p*sn1-q*sn2;
                    noise2 := noise2+g*abs(sn)
                 End;
                d := p*p-4*q;
                absr := abs(r);
             f := ph^[0];
             df := 0;
             noise := abs(f)/2;
                For j:=1 To n Do
                 Begin
                    df := f+r*df;
                  f := ph^[j]+r*f;
                  noise := abs(f)+absr*noise
                 End;
                If df=0 Then delr := f
             Else delr := f/df;
                If (abs(telp)<=meps*(noise1*abs(pc^[n-2])+
                               noise2*abs(pc^[n-3])))
                   And
                   (abs(telq)<=meps*(noise1* abs(pc^[n-1])+
                             noise2*abs(pc^[n-2])))
                 Then quadratic := true
                Else
                 Begin
                    p := p+delp;
                  q := q+delq
                 End;
                If abs(f)<=2*meps*noise Then linear := true
             Else r := r-delr
            End
         End;
        convergent := linear Or quadratic;
        If linear Then
         Begin
            If l=1 Then
             Begin
                k := k+1;
              pz^[k].xreal := r;
              pz^[k].imag := 0
             End
          Else
            Begin
                u.init(r, 0);
             roobin(l, u, pz^[k+1], term1);
             k := k+l
            End;
            maxx := 0;
          rk := 0;
          fac := 1;
            For j:=n Downto 0 Do
             Begin
                s := abs(ph^[j]*fac);
              fac := fac*r;
                If s>maxx Then
                 Begin
                    maxx := s;
                  rk := j-1
                 End
             End;
            For j:=1 To rk Do
             ph^[j] := ph^[j]+r*ph^[j-1];
            If rk<n-1 Then
             Begin
                s := ph^[n-1];
              ph^[n-1] := -ph^[n]/r;
                For j:=n-2 Downto rk+1 Do
                 Begin
                    t := ph^[j];
                  ph^[j] := (ph^[j+1]-s)/r;
                  s := t
                 End
             End;
            n := n-1;
         End
     Else
        If quadratic Then
         Begin
            If l=1 Then
             Begin
                rooqua(p,q,pz^[k+1],pz^[k+2]);
              k := k+2
             End
          Else
            Begin
                rooqua(p,q,u,v);
             roobin(l,u,pz^[k+1],term1);
                roobin(l,v,pz^[k+l+1],term1);
             k := k+2*l
            End;
            n := n-2;
            For j:=1 To n Do
             ph^[j] := ph^[j]-p*ph^[j-1]-q*ph^[j-2]
         End
  End;
  If k<n Then term := 2
 Else term := 1;
  freemem(pb, length);
 freemem(pc, length);
 freemem(ph, length);
End {roopol};

Procedure rooqua(p, q: ArbFloat; Var z1, z2: complex);

Var s, d : ArbFloat;
Begin
    p := -p/2;
 d := sqr(p)-q;
    If d<0 Then
     Begin
        z1.Init(p, sqrt(-d));
      z2 := z1;
      z2.conjugate
     End
 Else
    Begin
        If p>0 Then s := p+sqrt(d)
     Else s := p-sqrt(d);
        If s=0 Then
         Begin
            z1.Init(0, 0);
          z2 := z1
         End
     Else
        Begin
            z1.Init(s, 0);
         z2.Init(q/s, 0)
        End
    End
End {rooqua};

Procedure roo001(uplo, trans, diag: char; n: ArbInt; Var ap1, x1: ArbFloat;
                 incx: ArbInt);

Var
    ap   : arfloat1 absolute ap1;
    x    : arfloat1 absolute x1;
    temp : ArbFloat;
    info, ix, j, jx, k, kk, kx: ArbInt;
    nounit: boolean;
Begin
    info := 0;
 uplo := upcase(uplo);
 trans := upcase(trans);
 diag := upcase(diag);
    If n=0 Then exit;
    nounit := diag='N';
    If incx<=0 Then kx := 1-(n-1)*incx
 Else kx := 1;
    If trans='N' Then
     Begin
        If uplo='U' Then
         Begin
            kk := 1;
          jx := kx;
            For j:=1 To n Do
             Begin
                If x[jx]<>0 Then
                 Begin
                    temp := x[jx];
                  ix := kx;
                    For k:=kk To kk+j-2 Do
                     Begin
                        x[ix] := x[ix]+temp*ap[k];
                        inc(ix, incx)
                     End;
                    If nounit Then x[jx] := x[jx]*ap[kk+j-1]
                 End;
                inc(jx, incx);
              inc(kk, j)
             End
         End
      Else
        Begin
            kk := n*(n+1) Div 2;
         inc(kx, (n-1)*incx);
         jx := kx;
            For j:=n Downto 1 Do
             Begin
               If x[jx]<>0 Then
                Begin
                   temp := x[jx];
                 ix := kx;
                   For k:=kk Downto kk-(n-(j+1)) Do
                    Begin
                       x[ix] := x[ix]+temp*ap[k];
                     dec(ix, incx)
                    End;
                   If nounit Then x[jx] := x[jx]*ap[kk-n+j]
                End;
               dec(jx, incx);
              dec(kk, n-j+1)
             End
        End
     End
 Else
    Begin
        If uplo='U' Then
         Begin
            kk := n*(n+1) Div 2;
          jx := kx+(n-1)*incx;
            For j:= n Downto 1 Do
             Begin
               temp := x[jx];
              ix := jx;
               If nounit Then temp := temp*ap[kk];
               For k:= kk-1 Downto kk-j+1 Do
                Begin
                   dec(ix, incx);
                 temp := temp+ap[k]*x[ix]
                End;
               x[jx] := temp;
              dec(jx, incx);
              dec(kk, j)
             End
         End
     Else
        Begin
            kk := 1;
         jx := kx;
            For j:=1 To n Do
             Begin
                temp := x[jx];
              ix := jx;
                If nounit Then temp := temp*ap[kk];
                For k:=kk+1 To kk+n-j Do
                 Begin
                    inc(ix, incx);
                  temp := temp+ap[k]*x[ix]
                 End;
                x[jx] := temp;
              inc(jx, incx);
              inc(kk, n-j+1)
             End
        End
    End
End;

Procedure roo002(uplo, trans, diag: char; n: ArbInt;
                  Var ap1, x1: ArbFloat; incx: ArbInt );

Var ap : arfloat1 absolute ap1;
    x  : arfloat1 absolute x1;
    temp : ArbFloat;
    info, ix, j, jx, k, kk, kx: ArbInt;
    nounit: boolean;
Begin
    info := 0;
 uplo := upcase(uplo);
 trans := upcase(trans);
 diag := upcase(diag);
    If n=0 Then exit;
    nounit := diag='N';
    If incx<=0 Then kx := 1-(n-1)*incx
 Else kx := 1;
    If trans='N' Then
     Begin
        If uplo='U' Then
         Begin
            kk := n*(n+1) Div 2;
          jx := kx+(n-1)*incx;
            For j:=n Downto 1 Do
             Begin
                If x[jx]<>0 Then
                 Begin
                    If nounit Then x[jx] := x[jx]/ap[kk];
                    temp := x[jx];
                  ix := jx;
                    For k:=kk-1 Downto kk-j+1 Do
                     Begin
                        dec(ix, incx);
                      x[ix] := x[ix]-temp*ap[k];
                     End
                 End;
                dec(jx, incx);
              dec(kk, j)
             End
         End
      Else
        Begin
            kk := 1;
         jx := kx;
            For j:=1 To n Do
             Begin
                If x[jx]<>0 Then
                 Begin
                    If nounit Then x[jx] := x[jx]/ap[kk];
                    temp := x[jx];
                  ix := jx;
                    For k:= kk+1 To kk+n-j Do
                     Begin
                        inc(ix, incx);
                      x[ix] := x[ix]-temp*ap[k]
                     End;
                 End;
                inc(jx, incx);
              inc(kk, n-j+1)
             End
         End
     End
 Else
     Begin
         If uplo='U' Then
          Begin
             kk := 1;
           jx := kx;
             For j:= 1 To n Do
              Begin
                 temp := x[jx];
               ix := kx;
                 For k:= kk To kk+j-2 Do
                  Begin
                     temp := temp-ap[k]*x[ix];
                     inc(ix, incx);
                  End;
                 If nounit Then temp := temp/ap[kk+j-1];
                 x[jx] := temp;
               inc(jx, incx);
               inc(kk, j)
              End
          End
      Else
          Begin
              kk := n*(n+1) Div 2;
           kx := kx+(n-1)*incx;
           jx := kx;
              For j:=n Downto 1 Do
               Begin
                  temp := x[jx];
                ix := kx;
                  For k:= kk Downto kk-(n-(j+1)) Do
                   Begin
                      temp := temp-ap[k]*x[ix];
                    dec(ix, incx)
                   End;
                  If nounit Then temp := temp/ap[kk-n+j];
                  x[jx] := temp;
                dec(jx, incx);
                dec(kk, n-j+1)
               End
          End
     End
End;

Procedure roo003( n: ArbInt; Var x1: ArbFloat; incx: ArbInt;
                  Var scale, sumsq: ArbFloat );

Var absxi : ArbFloat;
    i, ix : ArbInt;
    x     : arfloat1 absolute x1;
Begin
    ix := 1;
    If n>0 Then
     For i:=1 To n Do
      Begin
        If x[ix]<>0 Then
         Begin
            absxi := abs(x[ix]);
            If (scale<absxi) Then
             Begin
                sumsq := 1+sumsq*sqr(scale/absxi);
              scale := absxi
             End
          Else sumsq := sumsq + sqr(absxi/scale)
         End;
        inc(ix, incx)
      End
End;

Function norm2( n: ArbInt; Var x1: ArbFloat; incx: ArbInt): ArbFloat;

Var  scale, ssq : ArbFloat;
     sqt: ArbFloat;
Begin
    If n<1 Then norm2 := 0
 Else
    If n=1 Then norm2 := abs(x1)
 Else
    Begin
        scale := 0;
     ssq := 1;
        roo003(n, x1, incx, scale, ssq );
        sqt := sqrt( ssq );
        If scale<(giant/sqt) Then norm2 := scale*sqt
     Else norm2 := giant
    End
End;

Procedure roo004(n: ArbInt; Var r1, diag1, qtb1: ArbFloat;
                 delta: ArbFloat; Var x1: ArbFloat);

Var
   r     : arfloat1 absolute r1;
   diag  : arfloat1 absolute diag1;
   qtb   : arfloat1 absolute qtb1;
   x     : arfloat1 absolute x1;
   wa1, wa2     : ^arfloat1;
   alpha, bnorm, gnorm, qnorm, sgnorm, temp: ArbFloat;
   i, j, jj, l  : ArbInt;
Begin
    getmem(wa1, n*sizeof(ArbFloat));
 getmem(wa2, n*sizeof(ArbFloat));
    jj := 1;
    For j:=1 To n Do
     Begin
        wa1^[j] := r[jj];
        If r[jj]=0 Then
         Begin
            temp := 0;
          l := j;
            For i:=1 To j-1 Do
             Begin
               If abs(r[l])>temp Then temp := abs(r[l]);
               inc(l, n-i)
             End;
            If temp=0 Then r[jj] := macheps
          Else r[jj] := macheps*temp
         End;
        inc(jj, n-j+1)
     End;
    move(qtb, x, n*sizeof(ArbFloat));
    roo002('l','t','n', n, r1, x1, 1);
    jj := 1;
    For j:=1 To n Do
     Begin
        r[jj] := wa1^[j];
        inc(jj, n - j + 1)
     End;
    For j:=1 To n Do
     wa2^[j] := diag[j]*x[j];
    qnorm := norm2(n, wa2^[1], 1);
    If qnorm>delta Then
     Begin
        move(qtb, wa1^, n*sizeof(ArbFloat));
        roo001('l','n','n', n, r1, wa1^[1], 1);
        For i:=1 To n Do
         wa1^[i] := wa1^[i]/diag[i];
        gnorm := norm2(n, wa1^[1], 1);
        sgnorm := 0;
      alpha := delta/qnorm;
        If gnorm<>0 Then
         Begin
            For j:=1 To n Do
             wa1^[j] := (wa1^[j]/gnorm)/diag[j];
            move(wa1^, wa2^, n*sizeof(ArbFloat));
            roo001('l','t','n',n,r1,wa2^[1],1);
            temp := norm2(n, wa2^[1],1);
            sgnorm := (gnorm/temp)/temp;
            alpha := 0;
            If sgnorm<delta Then
             Begin
                bnorm := norm2(n, qtb1, 1);
                temp := (bnorm/gnorm)*(bnorm/qnorm)*(sgnorm/delta);
                temp := temp-(delta/qnorm)*sqr(sgnorm/delta) +
                        sqrt(sqr(temp-delta/qnorm) +
                         (1-sqr(delta/qnorm))*(1-sqr(sgnorm/delta)));
                alpha := ((delta/qnorm)*(1-sqr(sgnorm/delta)))/temp
             End
         End;
        If sgnorm<delta Then temp := (1-alpha)*sgnorm
                        Else temp := (1-alpha)*delta;
        For j:=1 To n Do
         x[j] := temp*wa1^[j] + alpha*x[j]
     End;
    freemem(wa2, n*sizeof(ArbFloat));
 freemem(wa1, n*sizeof(ArbFloat));
End;

Procedure roo005(fcn: roofnrfunc; n: ArbInt; Var x1, fvec1, fjac1: ArbFloat;
                 ldfjac: ArbInt; Var iflag: ArbInt; ml, mu: ArbInt;
                 epsfcn: ArbFloat; Var wa1, wa2: arfloat1);

Var   eps, h, temp: ArbFloat;
     i, j, k, msum: ArbInt;
     x     : arfloat1 absolute x1;
     fvec  : arfloat1 absolute fvec1;
     fjac  : arfloat1 absolute fjac1;
     deff  : boolean;
Begin
    If epsfcn>macheps Then eps := sqrt(epsfcn)
 Else eps := sqrt(macheps);
    msum := ml+mu+1;
    If msum>=n Then
     Begin
        For j:=1 To n Do
         Begin
           temp := x[j];
          h := eps*abs(temp);
          If h=0 Then h := eps;
          x[j] := temp+h;
           deff := true;
          fcn(x1, wa1[1], deff);
          If Not deff Then iflag := -1;
           If iflag<0 Then exit;
           x[j] := temp;
           For i:= 1 To n Do
            fjac[j+(i-1)*ldfjac] := (wa1[i]-fvec[i])/h
         End
     End
 Else
    Begin
        For k:=1  To msum Do
         Begin
            j := k;
            while j <= n Do
                      Begin
                       wa2[j] := x[j];
                       h := eps*abs(wa2[j]);
                       If h=0 Then h := eps;
                       x[j] := wa2[j]+h;
                       inc(j, msum)
                      End;
            deff := true;
          fcn(x1, wa1[1], deff);
          If Not deff Then iflag := -1;
            If iflag<0 Then exit;
            j := k;
            while j<= n Do
                      Begin
                       x[j] := wa2[j];
                       h := eps*abs(wa2[j]);
                       If h=0 Then h := eps;
                       For i:=1 To n Do
                        Begin
                         fjac[j+(i-1)*ldfjac] := 0;
                         If (i>=(j-mu)) And (i<=(j+ml))
                          Then fjac[j+(i-1)*ldfjac] := (wa1[i]-fvec[i])/h
                        End;
                       inc(j, msum)
                      End
         End
    End
End;

Procedure roo006(trans: char; m, n: ArbInt; alpha: ArbFloat; Var a1: ArbFloat;
                 lda: ArbInt; Var x1: ArbFloat; incx : ArbInt; beta: ArbFloat;
                 Var y1: ArbFloat; incy : ArbInt);

Var  temp : ArbFloat;
     i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny: ArbInt;
     x     : arfloat1 absolute x1;
     y     : arfloat1 absolute y1;
     a     : arfloat1 absolute a1;
Begin
    info := 0;
 trans := upcase(trans);
    If (m=0) Or (n=0) Or ((alpha=0) And (beta=1)) Then exit;
    If trans='N' Then
     Begin
        lenx := n;
      leny := m
     End
 Else
    Begin
        lenx := m;
     leny := n
    End;
    If incx>0 Then kx := 1
 Else kx := 1-(lenx-1)*incx;
    If incy>0 Then ky := 1
 Else ky := 1-(leny-1)*incy;
    If (beta<>1) Then
     Begin
        iy := ky;
        If beta=0 Then
         For i:=1 To leny Do
          Begin
            y[iy] := 0;
           inc(iy, incy)
          End
          Else
           For i:=1 To leny Do
            Begin
             y[iy] := beta*y[iy];
             inc(iy, incy)
            End;
     End;
   If alpha=0 Then exit;
   If trans='N' Then
    Begin
       jx := kx;
       For j:=1 To n Do
        Begin
           If x[jx]<>0 Then
            Begin
               temp := alpha*x[jx];
             iy := ky;
               For i:=1 To m Do
                Begin
                  y[iy] := y[iy]+temp*a[j+(i-1)*lda];
                 inc(iy, incy)
                End
            End;
           inc(jx, incx)
        End
    End
 Else
   Begin
       jy := ky;
       For j:=1 To n Do
        Begin
           temp := 0;
         ix := kx;
           For i:=1 To m Do
            Begin
               temp := temp+a[j+(i-1)*lda]*x[ix];
               inc(ix, incx)
            End;
           y[jy] := y[jy]+alpha*temp;
           inc(jy, incy)
        End
   End
End;

Procedure roo007(m, n: ArbInt; alpha: ArbFloat; Var x1: ArbFloat; incx: ArbInt;
                  Var y1: ArbFloat; incy: ArbInt; Var a1: ArbFloat; lda: ArbInt);

Var                    temp: ArbFloat;
     i, info, ix, j, jy, kx: ArbInt;
     x     : arfloat1 absolute x1;
     y     : arfloat1 absolute y1;
     a     : arfloat1 absolute a1;
Begin
    info := 0;
    If (m=0) Or (n=0) Or (alpha=0) Then exit;
    If incy>0 Then jy := 1
 Else jy := 1-(n-1)*incy;
    If incx>0 Then kx := 1
 Else kx := 1-(m-1)*incx;
    For j:=1 To n Do
     Begin
        If y[jy]<>0 Then
         Begin
            temp := alpha*y[jy];
            ix  := kx;
            For i:=1 To m Do
             Begin
               a[j +(i-1)*lda] := a[j + (i-1)*lda] + x[ix]*temp;
               inc(ix, incx)
             End
         End;
        inc(jy, incy)
     End
End;

Procedure roo008(n: ArbInt; Var q1: ArbFloat; ldq: ArbInt; Var wa: arfloat1);

Var       q: arfloat1 absolute q1;
    i, j, k: ArbInt;
Begin
     For j:=2 To n Do
      For i:=1 To j-1 Do
       q[j+(i-1)*ldq] := 0;
     For k:=n Downto 1 Do
      Begin
         If (q[k+(k-1)*ldq]<>0) And (k<>n) Then
          Begin
            roo006('t', n-k+1, n-k, 1, q[k+1+(k-1)*ldq], ldq,
                   q[k +(k-1)*ldq], ldq, 0, wa[k+1], 1);
            roo007(n-k+1, n-k, -1/q[k+(k-1)*ldq], q[k+(k-1)*ldq], ldq,
                   wa[k+1], 1, q[k+1+(k-1)*ldq], ldq)
          End;
         For i:=k + 1 To n Do
          q[k+(i-1)*ldq] := -q[k+(i-1)*ldq];
         q[k+(k-1)*ldq] := 1-q[k+(k-1)*ldq]
      End;
End;

Procedure roo009(n: ArbInt; Var a1: ArbFloat; lda: ArbInt;
                 Var rdiag1, acnorm1: ArbFloat);

Var  a       : arfloat1 absolute a1;
     rdiag   : arfloat1 absolute rdiag1;
     acnorm  : arfloat1 absolute acnorm1;
     ajnorm  : ArbFloat;
     i, j    : ArbInt;
Begin
    For j:=1 To n Do
     acnorm[j] := norm2(n, a[j], lda);
    For j:=1 To n Do
     Begin
        ajnorm := norm2(n-j+1, a[j+(j-1)*lda], lda);
        If ajnorm<>0 Then
         Begin
            If a[j+(j-1)*lda]<0 Then ajnorm := -ajnorm;
            For i:=j To n Do
             a[j+(i-1)*lda] := a[j+(i-1)*lda]/ajnorm;
            a[j+(j-1)*lda] := a[j+(j-1)*lda]+1;
            If j<>n Then
             Begin
               roo006('t', n-j+1, n-j, 1, a[j+1+(j-1)*lda], lda,
                      a[j+(j-1)*lda], lda, 0, rdiag[j+1], 1);
               roo007(n-j+1, n-j, -1/a[j+(j-1)*lda], a[j+(j-1)*lda], lda,
                      rdiag[j+1], 1, a[j+1+(j-1)*lda], lda)
             End
         End;
         rdiag[j] := -ajnorm
     End
End;

Procedure roo010(n: ArbInt; Var x1: ArbFloat; incx: ArbInt;
                  Var y1: ArbFloat; incy: ArbInt; c, s:ArbFloat );

Var temp1: ArbFloat;
    x : arfloat1 absolute x1;
    y : arfloat1 absolute y1;
    i, ix, iy: ArbInt;
Begin
   If incy>=0 Then iy := 1
 Else iy := 1-(n-1)*incy;
   If incx>=0 Then ix := 1
 Else ix := 1-(n-1)*incx;
   For i:=1 To n Do
    Begin
      temp1 := x[ix];
     x[ix] := s*y[iy]+c*temp1;
     y[iy] := c*y[iy]-s*temp1;
      inc(ix, incx);
     inc(iy, incy)
    End
End;

Procedure roo011(m, n: ArbInt; Var a1: ArbFloat; lda: ArbInt; Var v1, w1: ArbFloat);

Var a: arfloat1 absolute a1;
    v: arfloat1 absolute v1;
    w: arfloat1 absolute w1;
    sine, cosine: ArbFloat;
    j, nm1, nmj: ArbInt;
Begin
    nm1 := n-1;
    For nmj:=1 To nm1 Do
     Begin
        j := n-nmj;
        If (abs(v[j])>1) Then
         Begin
            cosine := 1/v[j];
          sine := sqrt(1-sqr(cosine))
         End
      Else
        Begin
            sine := v[j];
         cosine := sqrt(1-sqr(sine))
        End;
        roo010(m, a[n], lda, a[j], lda, cosine, sine)
     End;
   For j:=1 To nm1 Do
    Begin
       If (abs(w[j])>1) Then
        Begin
           cosine := 1/w[j];
         sine := sqrt(1-sqr(cosine))
        End
     Else
       Begin
           sine := w[j];
        cosine := sqrt(1-sqr(sine))
       End;
       roo010(m, a[j], lda, a[n], lda, cosine, sine)
    End
End;

Procedure roo012(m, n: ArbInt; Var s1: ArbFloat; ls: ArbInt;
                 Var u1, v1, w1: ArbFloat; Var sing: boolean);

Const   one = 1.0;
 p5 = 0.5;
 p25 = 0.25;
 zero = 0.0;

Var    cosine, cotan, sine, tangnt, tau: ArbFloat;
                  i, j, jj, nm1, nmj: ArbInt;
    s : arfloat1 absolute s1;
    u : arfloat1 absolute u1;
    v : arfloat1 absolute v1;
    w : arfloat1 absolute w1;
Begin
    jj := (n*(2*m-n+1)) Div 2 - (m-n);
    If m>=n Then move(s[jj], w[n], (m-n+1)*sizeof(ArbFloat));
    nm1 := n-1;
    For nmj:=1 To nm1 Do
     Begin
       j := n-nmj;
      jj := jj-(m-j+1);
      w[j] := zero;
       If (v[j]<>zero) Then
        Begin
           If (abs(v[n])<abs(v[j])) Then
            Begin
               cotan := v[n]/v[j];
                sine := p5/sqrt(p25+p25*sqr(cotan));
               cosine := sine*cotan;
               If (abs(cosine)*giant)>one
                Then tau := one/cosine
             Else tau := one
            End
         Else
           Begin
               tangnt := v[j]/v[n];
               cosine := p5/sqrt(p25+p25*sqr(tangnt));
               sine := cosine*tangnt;
               tau := sine;
           End;
           v[n] := sine*v[j]+cosine*v[n];
           v[j] := tau;
           roo010(m-j+1, w[j], 1, s[jj], 1, cosine, sine)
        End
     End;
   For i:=1 To m Do
    w[i] := w[i]+v[n]*u[i];
   sing := false;
   For j:=1 To nm1 Do
    Begin
       If w[j]<>zero Then
        Begin
           If abs(s[jj])<abs(w[j]) Then
            Begin
               cotan := s[jj]/w[j];
             sine := p5/sqrt(p25+p25*sqr(cotan));
               cosine := sine*cotan;
               If (abs(cosine)*giant)>one Then tau := one/cosine
             Else tau := one
            End
         Else
            Begin
                tangnt := w[j]/s[jj];
             cosine := p5/sqrt(p25+p25*sqr(tangnt));
                sine := cosine*tangnt;
             tau := sine
            End;
            roo010(m-j+1, s[jj], 1, w[j], 1, cosine, sine);
            w[j] := tau
        End;
       If (s[jj]=zero) Then sing := true;
     inc(jj, m-j+1)
    End;
   If m>=n Then move(w[n], s[jj], (m-n+1)*sizeof(ArbFloat));
   If s[jj]=zero Then sing := true
End;

Procedure roo013(fcn: roofnrfunc; n: ArbInt; Var x1, fvec1: ArbFloat;
                 xtol: ArbFloat; maxfev, ml, mu: ArbInt; epsfcn: ArbFloat;
                 Var diag1: ArbFloat; factor: ArbFloat; Var info: ArbInt;
                 Var fjac1: ArbFloat; ldfjac: ArbInt;
                 Var r1: ArbFloat; lr: ArbInt; Var qtf1: ArbFloat);

Const p1 = 0.1;
 p5 = 0.5;
 p001 = 0.001;
 p0001 = 0.0001;

Var  diag : arfloat1 absolute diag1;
     fjac : arfloat1 absolute fjac1;
     fvec : arfloat1 absolute fvec1;
     qtf  : arfloat1 absolute qtf1;
     r    : arfloat1 absolute r1;
     wa1, wa2, wa3, wa4: ^arfloat1;
     x    : arfloat1 absolute x1;
     actred, delta, fnorm, fnorm1, pnorm,
     prered, ratio, sum, temp, xnorm : ArbFloat;
     i, iflag, iter, j, jm1, l, msum, ncfail, ncsuc, nfev,
     nslow1, nslow2, ns : ArbInt;
     jeval, sing, deff: boolean;
Begin
    info := 1;
 iflag := 0;
 nfev := 0;
 ns := n*sizeof(ArbFloat);
    For j:=1 To n Do
     If diag[j]<=0 Then exit;
    iflag := 1;
 deff := true;
 fcn(x1, fvec1, deff);
    If Not deff Then iflag := -1;
 nfev := 1;
    If iflag<0 Then
     Begin
        info := iflag;
      exit
     End;
    fnorm := norm2(n, fvec1, 1);
    msum := ml+mu+1;
 If msum>n Then msum := n;
    getmem(wa1, ns);
 getmem(wa2, ns);
 getmem(wa3, ns);
 getmem(wa4, ns);
    iter := 1;
 ncsuc := 0;
 ncfail := 0;
 nslow1 := 0;
 nslow2 := 0;
    while (info=1) and (iflag>=0) Do
    Begin
        jeval := true;
     iflag := 2;
        roo005(fcn, n, x1, fvec1, fjac1, ldfjac, iflag, ml, mu, epsfcn,
               wa1^, wa2^);
        inc(nfev, msum);
        If iflag>=0 Then
         Begin
            roo009(n, fjac1, ldfjac, wa1^[1], wa2^[1]);
            If iter=1 Then
             Begin
                For j:=1 To n Do
                 wa3^[j] := diag[j]*x[j];
                xnorm := norm2(n, wa3^[1], 1);
                delta := factor*xnorm;
                If delta=0 Then delta := factor;
             End;
             For i:=1 To n Do
              qtf[i] := fvec[i];
             For j:=1 To n Do
              If fjac[j+(j-1)*ldfjac]<>0 Then
               Begin
                sum := 0;
                For i:=j To n Do
                 sum := sum+fjac[j+(i-1)*ldfjac]*qtf[i];
                temp := -sum/fjac[j+(j-1)*ldfjac];
                For i:=j To n Do
                 qtf[i] := qtf[i]+fjac[j+(i-1)*ldfjac]*temp
               End;
             sing := false;
             For j:=1 To n Do
              Begin
                l := j;
               jm1 := j-1;
                For i:=1 To jm1 Do
                 Begin
                   r[l] := fjac[j+(i-1)*ldfjac];
                  inc(l, n-i)
                 End;
                r[l] := wa1^[j];
                If wa1^[j]=0 Then sing := true
              End;
             roo008(n, fjac1, ldfjac, wa1^);
             Repeat
                roo004(n, r1, diag1, qtf1, delta, wa1^[1]);
                For j:=1 To n Do
                 Begin
                   wa1^[j] := -wa1^[j];
                  wa2^[j] := x[j]+wa1^[j];
                   wa3^[j] := diag[j]*wa1^[j]
                 End;
                pnorm := norm2(n, wa3^[1], 1);
                If iter=1 Then If pnorm<delta Then delta := pnorm;
                iflag := 1;
                deff := true;
                fcn(wa2^[1], wa4^[1], deff);
                If Not deff Then iflag := -1;
                inc(nfev);
                If iflag>0 Then
                 Begin
                   fnorm1 := norm2(n, wa4^[1], 1);
                   If fnorm1<fnorm Then actred := 1-sqr(fnorm1/fnorm)
                   Else actred := -1;
                   move(wa1^, wa3^, n*sizeof(ArbFloat));
                   roo001('l','t','n', n, r1, wa3^[1], 1);
                   For i:=1 To n Do
                    wa3^[i] := wa3^[i] + qtf[i];
                   temp := norm2(n, wa3^[1], 1);
                   If temp<fnorm
                    Then prered := 1 - sqr(temp/fnorm)
                   Else prered := 1;
                   If prered>0 Then ratio := actred/prered
                  Else ratio := 0;
                   If ratio<p1 Then
                    Begin
                      ncsuc := 0;
                     inc(ncfail);
                     delta := p5*delta
                    End
                  Else
                   Begin
                      ncfail := 0;
                    inc(ncsuc);
                      If (ratio>=p5) Or (ncsuc>1)
                       Then If delta<pnorm/p5 Then delta := pnorm/p5;
                      If abs(ratio-1)<=p1 Then delta := pnorm/p5
                   End;
                   If ratio>=p0001 Then
                    Begin
                      For j:=1 To n Do
                       Begin
                          x[j] := wa2^[j];
                        wa2^[j] := diag[j]*x[j];
                          fvec[j] := wa4^[j]
                       End;
                      xnorm := norm2(n, wa2^[1], 1);
                     fnorm := fnorm1;
                     inc(iter)
                    End;
                   inc(nslow1);
                   If actred>=p001 Then nslow1 := 0;
                   If jeval Then inc(nslow2);
                   If actred>=p1 Then nslow2 := 0;
                   If (delta<=xtol*xnorm) Or
                      (fnorm=0) Or (pnorm=0) Then info := 0
                   Else If nfev>=maxfev Then info := 2
                        Else If delta<=macheps*xnorm Then info := 3
                             Else If nslow2=5 Then info := 4
                                  Else If nslow1=10 Then info := 5;
                   If (info=1) And (ncfail<>2) Then
                    Begin
                      roo006('t', n, n, 1, fjac1, ldfjac, wa4^[1], 1, 0,
                              wa2^[1], 1);
                      If ratio>=p0001 Then move(wa2^, qtf, ns);
                      For j:=1 To n Do
                       Begin
                         wa2^[j] := (wa2^[j]-wa3^[j])/pnorm;
                         wa1^[j] := diag[j]*((diag[j]*wa1^[j])/pnorm)
                       End;
                      roo012(n, n, r1, lr, wa1^[1], wa2^[1], wa3^[1], sing);
                      roo011(n, n, fjac1, ldfjac, wa2^[1], wa3^[1]);
                      roo011(1, n, qtf1, 1, wa2^[1], wa3^[1]);
                      jeval := false
                    End
                 End
             Until (iflag<0) Or (ncfail=2) Or (info<>1)
         End
      End;
   freemem(wa4, ns);
 freemem(wa3, ns);
 freemem(wa2, ns);
 freemem(wa1, ns);
   If iflag<0 Then info := iflag;
End;

Procedure roofnr(f: roofnrfunc; n: ArbInt; Var x, residu: ArbFloat; re: ArbFloat;
                 Var term: ArbInt);

Var       j, lr, ns          : ArbInt;
      wa1, wa2, wa3, wa4, fx : ^arfloat1;
Begin
    ns := n*sizeof(ArbFloat);
    If n<=0 Then term := 3
 Else
    Begin
        If re<0 Then term := 3
     Else
        Begin
            lr := (n*(n+1)) Div 2;
            getmem(wa1, ns);
         getmem(wa2, ns);
         getmem(wa3, lr*sizeof(ArbFloat));
            getmem(wa4, n*ns);
         getmem(fx, ns);
            For j:=1 To n Do
             wa1^[j] := 1;
            roo013(f, n, x, fx^[1], re, 200*(n+1), n-1, n-1, 0, wa1^[1],
                   100.0, term, wa4^[1], n, wa3^[1], lr, wa2^[1]);
            residu := Norm2(n, fx^[1], 1);
            freemem(fx, ns);
         freemem(wa4, n*ns);
            freemem(wa3, lr*sizeof(ArbFloat));
         freemem(wa2, ns);
         freemem(wa1, ns);
            If term<0 Then term := 6
         Else
            Case term Of
             0: term := 1;
             2: term := 4;
             3: term := 2;
             4, 5: term := 5;
            End
        End
    End
End;
End.
