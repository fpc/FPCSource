{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Solve first order starting value differential eqs, and
    sets of first order starting value differential eqs,

    Both versions are not suited for stiff differential equations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit ode;
{$I DIRECT.INC}


interface

uses typ;

{Solve first order, starting value, differential eqs,
Calc y(b) for dy/dx=f(x,y) and y(a)=ae}

Procedure odeiv1(f: rfunc2r; a, ya: ArbFloat; Var b, yb: ArbFloat;
                 ae: ArbFloat; Var term: ArbInt);

{ The same as above, for a set of equations. ya and yb are vectors}
Procedure odeiv2(f: oderk1n; a: ArbFloat; Var ya, b, yb: ArbFloat;
                 n: ArbInt; ae: ArbFloat; Var term: ArbInt);

implementation

Procedure odeiv1(f: rfunc2r; a, ya: ArbFloat; Var b, yb: ArbFloat;
                 ae: ArbFloat; Var term: ArbInt);

Var last, first, reject, goon         : boolean;
    x, y, d, h, xl, yl, int, hmin,
    absh,k0, k1, k2, k3, k4, k5,
    discr, tol, mu, mu1, fh, hl       : ArbFloat;
Begin
    x := a;
 y := ya;
 d := b-a;
 yb := y;
 term := 1;
    If ae <= 0 Then
     Begin
        term := 3;
      exit
     End;
    If d <> 0 Then
     Begin
        xl := x;
      yl := y;
      h := d/4;
      absh := abs(h);
        int := abs(d);
      hmin := int*1e-6;
        ae := ae/int;
      first := true;
      goon := true;
        while goon Do
        Begin
            absh := abs(h);
            If absh < hmin Then
             Begin
                If h>0 Then h := hmin
              Else h := -hmin;
                absh := hmin
             End;
            If (h >= b-xl) = (h >= 0) Then
             Begin
                last := true;
              h := b-xl;
              absh := abs(h)
             End
         Else last := false;
            x := xl;
         y := yl;
         k0 := f(x,y)*h;
            x := xl+h*2/9;
         y := yl+k0*2/9;
         k1 := f(x,y)*h;
            x := xl+h/3;
         y := yl+(k0+k1*3)/12;
         k2 := f(x,y)*h;
            x := xl+h/2;
         y := yl+(k0+k2*3)/8;
         k3 := f(x,y)*h;
            x := xl+h*0.8;
         y := yl+(k0*53-k1*135+k2*126+k3*56)/125;
         k4 := f(x,y)*h;
            If last Then x := b
         Else x := xl+h;
            y := yl+(k0*133-k1*378+k2*276+k3*112+k4*25)/168;
         k5 := f(x,y)*h;
            discr := abs(21*(k0-k3)-162*(k2-k3)-125*(k4-k3)+42*(k5-k3))/14;
            tol := absh*ae;
            mu := 1/(1+discr/tol)+0.45;
            reject := discr > tol;
            If reject Then
             Begin
                If absh <= hmin Then
                 Begin
                    b := xl;
                  yb := yl;
                  term := 2;
                  exit
                 End;
                h := mu*h
             End
         Else
            Begin
                If first Then
                 Begin
                    first := false;
                  hl := h;
                  h := mu*h
                 End
             Else
                Begin
                    fh := mu*h/hl+mu-mu1;
                 hl := h;
                 h := fh*h
                End;
                mu1 := mu;
                y := yl+(-k0*63+k1*189-k2*36-k3*112+k4*50)/28;
             k5 := f(x,y)*hl;
                y := yl+(k0*35+k2*162+k4*125+k5*14)/336;
                If b <> x Then
                 Begin
                    xl := x;
                  yl := y
                 End
             Else
                Begin
                    yb := y;
                 goon := false
                End
            End {not reject}
        End; {while}
     End {d<>0}
End; {odeiv1}

Procedure odeiv2(f: oderk1n; a: ArbFloat; Var ya, b, yb: ArbFloat;
                 n: ArbInt; ae: ArbFloat; Var term: ArbInt);

Var pya, pyb, yl, k0, k1, k2, k3, k4, k5, y : ^arfloat1;
    i, jj, ns                               : ArbInt;
    last, first, reject, goon               : boolean;
    x, xl, hmin, int, hl, absh, fhm,
    discr, tol, mu, mu1, fh, d, h           : ArbFloat;
Begin
    If (ae <= 0) Or (n < 1) Then
     Begin
        term := 3;
      exit
     End;
    ns := n*sizeof(ArbFloat);
    pya := @ya;
 pyb := @yb;
 move(pya^[1], pyb^[1], ns);
 term := 1;
    getmem(yl, ns);
 getmem(k0, ns);
 getmem(k1, ns);
 getmem(k2, ns);
    getmem(k3, ns);
 getmem(k4, ns);
 getmem(k5, ns);
 getmem(y, ns);
    x := a;
 d := b-a;
 move(pya^[1], y^[1], ns);
    If d <> 0 Then
     Begin
        xl := x;
      move(y^[1], yl^[1], ns);
      h := d/4;
      absh := abs(h);
        int := abs(d);
      hmin := int*1e-6;
      hl := ae;
      ae := ae/int;
        first := true;
      goon := true;
        while goon Do
        Begin
            absh := abs(h);
            If absh < hmin Then
             Begin
                If h > 0 Then h := hmin
              Else h := -hmin;
                absh := hmin
             End;
            If (h >= b-xl) = (h >= 0) Then
             Begin
                last := true;
              h := b-xl;
              absh := abs(h)
             End
         Else last := false;
            x := xl;
         move(yl^[1], y^[1], ns);
            f(x, y^[1], k0^[1]);
            For i:=1 To n Do
             k0^[i] := k0^[i]*h;
            x := xl+h*2/9;
            For jj:=1 To n Do
             y^[jj] := yl^[jj]+k0^[jj]*2/9;
            f(x, y^[1], k1^[1]);
            For i:=1 To n Do
             k1^[i] := k1^[i]*h;
            x := xl+h/3;
            For jj:=1 To n Do
             y^[jj] := yl^[jj]+(k0^[jj]+k1^[jj]*3)/12;
            f(x, y^[1], k2^[1]);
            For i:=1 To n Do
             k2^[i] := k2^[i]*h;
            x := xl+h/2;
            For jj:=1 To n Do
             y^[jj] := yl^[jj]+(k0^[jj]+k2^[jj]*3)/8;
            f(x, y^[1], k3^[1]);
            For i:=1 To n Do
             k3^[i] := k3^[i]*h;
            x := xl+h*0.8;
            For jj:=1 To n Do
             y^[jj] := yl^[jj]+
                       (k0^[jj]*53-k1^[jj]*135+k2^[jj]*126+k3^[jj]*56)/125;
            f(x, y^[1], k4^[1]);
            For i:=1 To n Do
             k4^[i] := k4^[i]*h;
            If last Then x := b
         Else x := xl+h;
            For jj:=1 To n Do
             y^[jj] := yl^[jj]+(k0^[jj]*133-k1^[jj]*378+k2^[jj]*276+
                               k3^[jj]*112+k4^[jj]*25)/168;
            f(x, y^[1], k5^[1]);
            For i:=1 To n Do
             k5^[i] := k5^[i]*h;
            reject := false;
         fhm := 0;
         tol := absh*ae;
            For jj:=1 To n Do
             Begin
                discr := abs((k0^[jj]-k3^[jj])*21-(k2^[jj]-k3^[jj])*162-
                         (k4^[jj]-k3^[jj])*125+(k5^[jj]-k3^[jj])*42)/14;
                reject := (discr > tol) Or  reject;
              fh := discr/tol;
                If fh > fhm Then fhm := fh
             End; {jj}
            mu := 1/(1+fhm)+0.45;
            If reject Then
             Begin
                If absh <= hmin Then
                 Begin
                    b := xl;
                  move(yl^[1], pyb^[1], ns);
                  term := 2;
                    freemem(yl, ns);
                  freemem(k0, ns);
                    freemem(k1, ns);
                  freemem(k2, ns);
                    freemem(k3, ns);
                  freemem(k4, ns);
                    freemem(k5, ns);
                  freemem(y, ns);
                  exit
                 End;
                h := mu*h
             End
         Else
            Begin
                If first Then
                 Begin
                    first := false;
                  hl := h;
                  h := mu*h
                 End
             Else
                Begin
                    fh := mu*h/hl+mu-mu1;
                 hl := h;
                 h := fh*h
                End;
                mu1 := mu;
                For jj:=1 To n Do
                 y^[jj] := yl^[jj]+(-k0^[jj]*63+k1^[jj]*189
                                  -k2^[jj]*36-k3^[jj]*112+k4^[jj]*50)/28;
                f(x, y^[1], k5^[1]);
                For i:=1 To n Do
                 k5^[i] := k5^[i]*hl;
                For jj:=1 To n Do
                 y^[jj] := yl^[jj]+(k0^[jj]*35+k2^[jj]*162+k4^[jj]*125
                           +k5^[jj]*14)/336;
                If b <> x Then
                 Begin
                    xl := x;
                  move(y^[1], yl^[1], ns)
                 End
             Else
                Begin
                    move(y^[1], pyb^[1], ns);
                 goon := false
                End
            End {not reject}
       End {while}
     End; {d<>0}
  freemem(yl, ns);
 freemem(k0, ns);
 freemem(k1, ns);
 freemem(k2, ns);
  freemem(k3, ns);
 freemem(k4, ns);
 freemem(k5, ns);
 freemem(y, ns)
End; {odeiv2}

End.
