{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Integration. This routine is fit for smooth "integrand" so no singularities,
    sharp edges, or quickly oscillating behaviour.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit int;
{$I DIRECT.INC}

interface

uses typ;

Var
    limit    : ArbInt;
    epsrel   : ArbFloat;

{calc int(x,a,b,f(x)) for a function with a nice behaviour in the
interval [A,B]}

Procedure int1fr(f: rfunc1r; a, b, ae: ArbFloat; Var integral, err: ArbFloat;
                 Var term: ArbInt);

implementation

Function amin1(x, y: ArbFloat): ArbFloat;
Begin
    If x<y Then amin1 := x
 Else amin1 := y
End;

Function amax1(x, y: ArbFloat): ArbFloat;
Begin
    If x>y Then amax1 := x
 Else amax1 := y
End;

Procedure qk21(f: rfunc1r; a, b: ArbFloat;
               Var result, abserr, resabs, resasc: ArbFloat);

Const

 xgk: array[1..11] Of ArbFloat =
                                ( 0.9956571630258081, 0.9739065285171717,
                                  0.9301574913557082, 0.8650633666889845,
                                  0.7808177265864169, 0.6794095682990244,
                                  0.5627571346686047, 0.4333953941292472,
                                  0.2943928627014602, 0.1488743389816312, 0);

 wgk: array[1..11] Of ArbFloat =
                                ( 0.1169463886737187e-1, 0.3255816230796473e-1,
                                  0.5475589657435200e-1, 0.7503967481091995e-1,
                                  0.9312545458369761e-1, 0.1093871588022976,
                                  0.1234919762620659,    0.1347092173114733,
                                  0.1427759385770601,    0.1477391049013385,
                                  0.1494455540029169);

 wg: array[1..5] Of ArbFloat =
                              ( 0.6667134430868814e-1, 0.1494513491505806,
                                0.2190863625159820,    0.2692667193099964,
                                0.2955242247147529);

Var  absc, centr, dhlgth, fc, fsum, fval1, fval2,
     hlgth, resg, resk, reskh: ArbFloat;
     j, jtw, jtwm1: ArbInt;
          fv1, fv2: ^arfloat1;
Begin
   getmem(fv1, 10*sizeof(ArbFloat));
 getmem(fv2, 10*sizeof(ArbFloat));
   centr := (a+b)/2;
 hlgth := (b-a)/2;
 dhlgth := abs(hlgth);
 resg := 0;
   fc := f(centr);
 resk := wgk[11]*fc;
 resabs := abs(resk);
   For j:=1 To 5 Do
    Begin
       jtw := 2*j;
     absc := hlgth*xgk[jtw];
       fval1 := f(centr-absc);
     fval2 := f(centr+absc);
       fv1^[jtw] := fval1;
     fv2^[jtw] := fval2;
     fsum := fval1+fval2;
       resg := resg+wg[j]*fsum;
     resk := resk+wgk[jtw]*fsum;
       resabs := resabs+wgk[jtw]*(abs(fval1)+abs(fval2))
    End;
   For j:=1 To 5 Do
    Begin
       jtwm1 := 2*j-1;
     absc := hlgth*xgk[jtwm1];
       fval1 := f(centr-absc);
     fval2 := f(centr+absc);
       fv1^[jtwm1] := fval1;
     fv2^[jtwm1] := fval2;
     fsum := fval1+fval2;
       resk := resk+wgk[jtwm1]*fsum;
       resabs := resabs+wgk[jtwm1]*(abs(fval1)+abs(fval2))
    End;
   reskh := resk/2;
 resasc := wgk[11]*abs(fc-reskh);
   For j:=1 To 10 Do
     resasc := resasc+wgk[j]*(abs(fv1^[j]-reskh)+abs(fv2^[j]-reskh));
   result := resk*hlgth;
 resabs := resabs*dhlgth;
 resasc := resasc*dhlgth;
   abserr := abs((resk-resg)*hlgth);
   If (resasc <> 0) And (abserr <> 0)
    Then abserr := resasc*amin1(1,exp(1.5*ln(200*abserr/resasc)));
   If resabs > midget/(50*macheps)
    Then abserr := amax1((50*macheps)*resabs, abserr);
   freemem(fv1, 10*sizeof(ArbFloat));
 freemem(fv2, 10*sizeof(ArbFloat));
End;

Procedure qpsrt(limit: ArbInt;
                Var last, maxerr: ArbInt;
                Var ermax, elist1: ArbFloat;
                Var iord1, nrmax: ArbInt);

Var errmax, errmin: ArbFloat;
    i, ibeg, ido, isucc,
    j, jbnd, jupbn, k : ArbInt;
    continue : boolean;
    elist : arfloat1 absolute elist1;
    iord  : arint1 absolute iord1;
Begin
      If (last<=2)
       Then
       Begin
          iord[1] := 1;
          iord[2] := 2;
          maxerr := iord[nrmax];
          ermax := elist[maxerr];
          exit
       End;

      errmax := elist[maxerr];
      ido := nrmax-1;
      i := 0;
      If ido>0 Then
       Repeat
          Inc(i);
          isucc := iord[nrmax-1];
          If errmax>elist[isucc]
           Then
           Begin
               iord[nrmax] := isucc;
               nrmax := nrmax-1
           End
        Else i := ido
       Until (i=ido);

      jupbn := last;
      If (last>(limit Div 2+2)) Then jupbn := limit+3-last;
      errmin := elist[last];
      jbnd := jupbn-1;
      ibeg := nrmax+1;

      If (ibeg>jbnd)
       Then
       Begin
         iord[jbnd] := maxerr;
         iord[jupbn] := last;
         maxerr := iord[nrmax];
         ermax := elist[maxerr];
         exit
       End;

      i := ibeg-1;
      continue := true;
      while (i<jbnd) and continue Do
      Begin
        Inc(i);
        isucc := iord[i];
        If (errmax<elist[isucc])
         Then iord[i-1] := isucc
        Else continue := false
      End;
      If continue
       Then
       Begin
          iord[jbnd] := maxerr;
          iord[jupbn] := last
       End
 Else
      Begin
          iord[i-1] := maxerr;
          k := jbnd;
          continue := true;
          j := i-1;
          while (j<jbnd) and continue Do
          Begin
             Inc(j);
             isucc := iord[k];
             If errmin<elist[isucc]
              Then continue := false
             Else
              Begin
                 iord[k+1] := isucc;
                 Dec(k)
              End
          End;
          If continue Then iord[i] := last
                      Else iord[k+1] := last
      End;

      maxerr := iord[nrmax];
      ermax := elist[maxerr]

End;

Type
     stock = array[1..52] Of ArbFloat;
     hulpar = array[1..3] Of ArbFloat;

Procedure qelg(Var n: ArbInt;
               Var epstab: stock;
               Var result, abserr: ArbFloat;
               Var res3la: hulpar;
               Var nres: ArbInt);

Var
     delta1, delta2, delta3,
     epsinf, error, err1, err2, err3,
     e0, e1, e2, e3, e0abs, e1abs, e2abs, e3abs,
     res, ss, tol1, tol2, tol3: ArbFloat;
     i, ib, ib2, k1, k2, k3,
     limexp, num, newelm:  ArbInt;
     continue: boolean;
Begin
      Inc(nres);
      abserr := giant;
      result := epstab[n];

      If (n<3) Then exit;

      limexp := 50;
      epstab[n+2] := epstab[n];
      epstab[n] := giant;
      num := n;
      k1 := n;
      continue := true;
      i := 1;
      newelm := (n-1) Div 2;
      while (i<=newelm) and continue Do
      Begin
        k2 := k1-1;
        k3 := k1-2;
        res := epstab[k1+2];
        e0 := epstab[k3];
        e1 := epstab[k2];
        e2 := res;
        e0abs := abs(e0);
        e1abs := abs(e1);
        e2abs := abs(e2);
        delta2 := e2-e1;
        err2 := abs(delta2);

        If e1abs>e2abs
         Then tol2 := e1abs*macheps
        Else tol2 := e2abs*macheps;

        delta3 := e1-e0;
        err3 := abs(delta3);
        If e1abs>e0abs
         Then tol3 := e1abs*macheps
        Else tol3 := e0abs*macheps;

        If (err2<=tol2) And (err3<=tol3)
         Then
         Begin
           result := res;
           abserr := err2+err3;
           If abserr<5*macheps*abs(result)
            Then abserr := 5*macheps*abs(result);
           exit
         End;

        e3 := epstab[k1];
        epstab[k1] := e1;
        delta1 := e1-e3;
        err1 := abs(delta1);
        e3abs := abs(e3);

        If e1abs<e3abs
         Then tol1 := e3abs*macheps
        Else tol1 := e1abs*macheps;

        continue := false;

        If (err1<=tol1) Or (err2<=tol2) Or (err3<=tol3)
         Then n := 2*i-1
        Else
         Begin
           ss := 1/delta1 + 1/delta2 - 1/delta3;
           epsinf := abs(ss*e1);
           If (epsinf>1e-4)
            Then
            Begin
              continue := true;
              res := e1+1/ss;
              epstab[k1] := res;
              k1 := k1-2;
              error := err2+abs(res-e2)+err3;
              If (error<=abserr)
               Then
               Begin
                 abserr := error;
                 result := res
               End
            End
          Else n := 2*i-1
         End;
        Inc(i)

      End;

      If n=limexp Then n := 2*(limexp Div 2)-1;

      If Odd(Num) Then ib := 1
 Else ib := 2;

      For i:=1 To newelm+1 Do
       Begin
         ib2 := ib+2;
         epstab[ib] := epstab[ib2];
         ib := ib2
       End;

      Move(epstab[num-n+1], epstab[1], n*SizeOf(ArbFloat));

      If (nres<4)
       Then
       Begin
         res3la[nres] := result;
         abserr := giant
       End
 Else
      Begin
         abserr := abs(result-res3la[3]) +
                   abs(result-res3la[2]) +
                   abs(result-res3la[1]);
         res3la[1] := res3la[2];
         res3la[2] := res3la[3];
         res3la[3] := result;
         If abserr<5*macheps*abs(result)
          Then abserr := 5*macheps*abs(result)
      End
End;

Procedure qagse(f: rfunc1r; a, b, epsabs, epsrel: ArbFloat;
                limit: ArbInt; Var result, abserr: ArbFloat;
                Var neval, ier, last: ArbInt);

Var abseps, area, area1, area12, area2, a1, a2, b1, b2, correc, defabs,
    defab1, defab2, dres, erlarg, erlast, errbnd, errmax,
    error1, error2, erro12, errsum, ertest, resabs, reseps, small: ArbFloat;
    id, ierro, iroff1, iroff2, iroff3, jupbnd, k, ksgn,
    ktmin, maxerr, nres, nrmax, numrl2, sr, lsr: ArbInt;
    extrap, noext, go_on, jump, smallers, p0, p1, p2, p3: boolean;
    alist, blist, elist, rlist: ^arfloat1;
    res3la: hulpar;
    rlist2: stock;
    iord: ^arint1;
Begin
  sr := sizeof(ArbFloat);
 lsr := limit*sr;
  getmem(alist, lsr);
  getmem(blist, lsr);
  getmem(elist, lsr);
  getmem(iord, limit*sizeof(ArbInt));
  getmem(rlist, lsr);
  ier := 0;
 neval := 0;
 last := 0;
 result := 0;
 abserr := 0;
  alist^[1] := a;
 blist^[1] := b;
 rlist^[1] := 0;
 elist^[1] := 0;
  If (epsabs <= 0) And (epsrel < amax1(0.5e+02*macheps, 0.5e-14)) Then
   Begin
      ier := 6;
      freemem(rlist, lsr);
      freemem(iord, limit*sizeof(ArbInt));
      freemem(elist, lsr);
      freemem(blist, lsr);
      freemem(alist, lsr);
      exit
   End;
  ierro := 0;
  qk21(f, a, b, result, abserr, defabs, resabs);
 dres := abs(result);
  errbnd := amax1(epsabs, epsrel*dres);
  last := 1;
 rlist^[1] := result;
 elist^[1] := abserr;
  iord^[1] := 1;
  If (abserr <= 100*macheps*defabs) And (abserr>errbnd) Then ier := 2;
  If limit=1 Then ier := 1;
  If (ier <> 0) Or ((abserr <= errbnd) And (abserr <> resabs)) Or (abserr=0)
   Then
   Begin
      neval := 21;
      freemem(rlist, lsr);
      freemem(iord, limit*sizeof(ArbInt));
      freemem(elist, lsr);
      freemem(blist, lsr);
      freemem(alist, lsr);
      exit
   End;
  rlist2[1] := result;
 errmax := abserr;
 maxerr := 1;
 area := result;
  errsum := abserr;
 abserr := giant;
 nrmax := 1;
 nres := 0;
 numrl2 := 2;
 ktmin := 0;
  extrap := false;
 noext := false;
 iroff1 := 0;
 iroff2 := 0;
 iroff3 := 0;
 ksgn := -1;
  If dres >= (1-50*macheps)*defabs Then ksgn := 1;
  go_on := limit > 1;
 smallers := false;
  while go_on Do
    Begin
      inc(last);
     a1 := alist^[maxerr];
      b1 := (alist^[maxerr]+blist^[maxerr])/2;
     a2 := b1;
     b2 := blist^[maxerr];
      erlast := errmax;
      qk21(f, a1, b1, area1, error1, resabs, defab1);
      qk21(f, a2, b2, area2, error2, resabs, defab2);
      area12 := area1+area2;
     erro12 := error1+error2;
      errsum := errsum+erro12-errmax;
     area := area+area12-rlist^[maxerr];
      If (defab1 <> error1) And (defab2 <> error2) Then
        Begin
          If (abs(rlist^[maxerr]-area12) <= 1e-5*abs(area12))
              And (erro12 >= 0.99*errmax) Then
           Begin
            If extrap Then inc(iroff2)
            Else inc(iroff1)
           End;
          If (last > 10) And (erro12 > errmax) Then inc(iroff3)
        End;
      rlist^[maxerr] := area1;
     rlist^[last] := area2;
      errbnd := amax1(epsabs, epsrel*abs(area));
      If (iroff1+iroff2 >= 10) Or (iroff3>=20) Then ier := 2;
      If iroff2>=5 Then ierro := 3;
     If last=limit Then ier := 1;
      If amax1(abs(a1),abs(b2)) <= (1+100*macheps)*(abs(a2)+1000*midget)
       Then ier := 4;
      If error2 <= error1 Then
        Begin
          alist^[last] := a2;
         blist^[maxerr] := b1;
         blist^[last] := b2;
          elist^[maxerr] := error1;
         elist^[last] := error2
        End
     Else
        Begin
          alist^[maxerr] := a2;
         alist^[last] := a1;
         blist^[last] := b1;
          rlist^[maxerr] := area2;
         rlist^[last] := area1;
          elist^[maxerr] := error2;
         elist^[last] := error1
        End;
      qpsrt(limit, last, maxerr, errmax, elist^[1], iord^[1], nrmax);
      If errsum <= errbnd Then
        Begin
          smallers := true;
         go_on := false
        End
     Else
        Begin
          If ier <> 0 Then go_on := false
         Else
            Begin
              If (last=2) Or (Not noext) Then
                Begin
                  If last <> 2 Then
                    Begin
                      erlarg := erlarg-erlast;
                      If abs(b1-a1) > small Then erlarg := erlarg+erro12;
                      If extrap Or
                         (abs(blist^[maxerr]-alist^[maxerr]) <= small) Then
                        Begin
                          If Not extrap Then nrmax := 2;
                         extrap := true;
                          jump := false;
                          If (ierro <> 3) And (erlarg>=ertest) Then
                            Begin
                              id := nrmax;
                             jupbnd := last;
                              If last > 2+limit/2 Then jupbnd := limit+3-last;
                              k := id;
                              while (k <= jupbnd) and (Not jump) Do
                                Begin
                                  maxerr := iord^[nrmax];
                                  errmax := elist^[maxerr];
                                  If abs(blist^[maxerr]-alist^[maxerr]) > small
                                   Then jump := true
                                  Else
                                    Begin
                                      nrmax := nrmax+1;
                                     k := k+1
                                    End
                                End;
                            End; {(ierro <> 3) and (erlarg>=ertest)}
                          If Not jump Then
                            Begin
                              numrl2 := numrl2+1;
                             rlist2[numrl2] := area;
                              qelg(numrl2, rlist2, reseps, abseps,
                                   res3la, nres);
                              ktmin := ktmin+1;
                              If (ktmin > 5) And (abserr < 1e-3*errsum)
                               Then ier := 5;
                              If abseps < abserr Then
                                Begin
                                  ktmin := 0;
                                 abserr := abseps;
                                 result := reseps;
                                  correc := erlarg;
                                  ertest := amax1(epsabs,epsrel*abs(reseps));
                                  If abserr <= ertest Then go_on := false
                                End;
                              If go_on Then
                                Begin
                                  If numrl2=1 Then noext := true;
                                  If ier=5 Then go_on := false
                                 Else
                                    Begin
                                      maxerr := iord^[1];
                                     errmax := elist^[maxerr];
                                      nrmax := 1;
                                     extrap := false;
                                     small := small/2;
                                      erlarg := errsum
                                    End; {ier <> 5}
                                End; {go_on}
                            End; {not jump}
                        End;  { abs(blist^[maxerr]-alist^[maxerr]) <= small }
                    End
                 Else {last=2}
                      Begin
                        small := abs(b-a)*0.375;
                       erlarg := errsum;
                        ertest := errbnd;
                       rlist2[2] := area
                      End
                End; {last=2 or not noext}
            End; {ier <> 0}
        End; {errsum <= errbnd}
      If go_on Then go_on := last < limit
    End; {while go_on}
  p0 := false;
 p1 := false;
 p2 := false;
 p3 := false;
  If (abserr=giant) Or smallers Then p0 := true
 Else
  If ier+ierro=0 Then p1 := true;
  If Not (p0 Or p1) Then
    Begin
      If ierro=3 Then abserr := abserr+correc;
      If ier=0 Then ier := 3;
      If (result <> 0) And (area <> 0) Then p2 := true
     Else
      If abserr > errsum Then p0 := true
     Else
      If area=0 Then p3 := true
     Else p1 := true
    End;
  If p2 Then
    Begin
      If abserr/abs(result) > errsum/abs(area) Then p0 := true
     Else p1 := true
    End;
  If p1 Then
    Begin
      If (ksgn=-1) And (amax1(abs(result),abs(area)) <= defabs*0.01)
       Then p3 := true
     Else
      If (0.01 > result/area) Or (result/area > 100) Or (errsum>abs(area))
       Then ier := 6;
      p3 := true
    End;
  If p0 Then
    Begin
      result := 0;
      For k:=1 To last Do
       result := result+rlist^[k]
    End;
  If Not p3 Then abserr := errsum;
  If ier>2 Then ier := ier-1;
  neval := 42*last-21;
  freemem(alist, lsr);
 freemem(blist, lsr);
 freemem(elist, lsr);
  freemem(rlist, lsr);
 freemem(iord, limit*sizeof(ArbInt));
End;


{    single-precision machine constants
   r1mach(1) = b**(emin-1), the midget positive magnitude..
   r1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
   r1mach(3) = b**(-t), the midget relative spacing.
   r1mach(4) = b**(1-t), the largest relative spacing.
   r1mach(5) = log10(b)
}

Procedure qk15i(f: rfunc1r; boun: ArbFloat;
                inf: ArbInt;
                a, b: ArbFloat;
                Var result, abserr, resabs, resasc: ArbFloat);

Const  xgk : array[1..8] Of ArbFloat = (
                                        0.9914553711208126, 0.9491079123427585,
                                        0.8648644233597691, 0.7415311855993944,
                                        0.5860872354676911, 0.4058451513773972,
                                        0.2077849550078985, 0.0000000000000000);
      wgk : array[1..8] Of ArbFloat = (
                                       0.02293532201052922,0.06309209262997855,
                                       0.1047900103222502, 0.1406532597155259,
                                       0.1690047266392679, 0.1903505780647854,
                                       0.2044329400752989, 0.2094821410847278);
      wg : array[1..8] Of ArbFloat = (
                                      0, 0.1294849661688697,
                                      0, 0.2797053914892767,
                                      0, 0.3818300505051189,
                                      0, 0.4179591836734694);

Var  absc, absc1, absc2, centr,
     dinf, fc, fsum, fval1, fval2,
     hlgth, resg, resk, reskh,
     tabsc1, tabsc2: ArbFloat;

     fv1, fv2: array[1..7] Of ArbFloat;

     j: ArbInt;
Begin
      If inf<1 Then dinf := inf
 Else dinf := 1;
      centr := 0.5*(a+b);
      hlgth := 0.5*(b-a);
      tabsc1 := boun+dinf*(1-centr)/centr;
      fval1 := f(tabsc1);
      If (inf=2) Then fval1 := fval1+f(-tabsc1);
      fc := (fval1/centr)/centr;
      resg := wg[8]*fc;
      resk := wgk[8]*fc;
      resabs := abs(resk);
      For j:=1 To 7 Do
       Begin
        absc := hlgth*xgk[j];
        absc1 := centr-absc;
        absc2 := centr+absc;
        tabsc1 := boun+dinf*(1-absc1)/absc1;
        tabsc2 := boun+dinf*(1-absc2)/absc2;
        fval1 := f(tabsc1);
        fval2 := f(tabsc2);
        If (inf=2) Then fval1 := fval1+f(-tabsc1);
        If (inf=2) Then fval2 := fval2+f(-tabsc2);
        fval1 := (fval1/absc1)/absc1;
        fval2 := (fval2/absc2)/absc2;
        fv1[j] := fval1;
        fv2[j] := fval2;
        fsum := fval1+fval2;
        resg := resg+wg[j]*fsum;
        resk := resk+wgk[j]*fsum;
        resabs := resabs+wgk[j]*(abs(fval1)+abs(fval2))
       End;

      reskh := resk*0.5;
      resasc := wgk[8]*abs(fc-reskh);

      For j:=1 To 7
       Do
       resasc := resasc+wgk[j]*(abs(fv1[j]-reskh)+abs(fv2[j]-reskh));

      result := resk*hlgth;
      resasc := resasc*hlgth;
      resabs := resabs*hlgth;
      abserr := abs((resk-resg)*hlgth);

      If (resasc<>0) And (abserr<>0)
       Then
       Begin
           reskh := 200*abserr/resasc;
           If reskh<1
            Then abserr := resasc*reskh*sqrt(reskh)
           Else abserr := resasc
       End;

      If (resabs>midget/(50*macheps))
       Then
       Begin
           reskh := macheps*50*resabs;
           If abserr<reskh Then abserr := reskh
       End
End;



Procedure qagie(f: rfunc1r;
                bound: ArbFloat;
                inf: ArbInt;
                epsabs, epsrel: ArbFloat;
                Var result, abserr: ArbFloat;
                Var ier: ArbInt);

{ procedure qagie is vertaald vanuit de PD-quadpack-Fortran-routine QAGIE
  naar Turbo Pascal, waarbij de volgende parameters uit de parameterlijst
  verdwenen zijn:
      limit , zoiets als 'maximale recursie diepte' vervangen door globale
              variabele limit, initieel op 500 gezet
      last  , actuele 'recursie diepte'
      workarrays: alist, blist, rlist, elist en iord ,
                  vervangen door dynamische locale arrays
      neval , het aantal functie-evaluaties
}

Var  abseps, area, area1, area12, area2,
     a1, a2, b1,b2, correc,
     defabs, defab1, defab2, dres,
     erlarg, erlast, errbnd, h,
     errmax, error1, error2, erro12, errsum, ertest, resabs,
     reseps, small: ArbFloat;
     res3la : hulpar;

     rlist, alist, blist, elist: ^arfloat1;
     iord: ^arint1;
     rlist2 : stock;
     id, ierro, iroff1, iroff2, iroff3, jupbnd,
     k, ksgn, ktmin, last, maxerr, nres, nrmax, numrl2: ArbInt;
     continue, break, extrap, noext : boolean;
Begin
      ier := 6;
      h := 50*macheps;
      If h<0.5e-14 Then h := 0.5e-14;
      If (epsabs<=0) And (epsrel<h) Then exit;

      If (inf=2) Then bound := 0;

      qk15i(f, bound, inf, 0, 1, result, abserr, defabs, resabs);

      dres := abs(result);

      errbnd := epsrel*dres;
      If epsabs>errbnd Then errbnd := epsabs;

      ier := 2;
      If (abserr<=100*macheps*defabs) And (abserr>errbnd) Then exit;
      ier := 0;
      If ((abserr<=errbnd) And (abserr<>resabs)) Or (abserr=0) Then exit;

      GetMem(rlist, limit*SizeOf(ArbFloat));
      GetMem(alist, limit*SizeOf(ArbFloat));
      GetMem(blist, limit*SizeOf(ArbFloat));
      GetMem(elist, limit*SizeOf(ArbFloat));
      GetMem(iord, limit*SizeOf(ArbInt));

      alist^[1] := 0;
      blist^[1] := 1;
      rlist^[1] := result;
      elist^[1] := abserr;
      iord^[1]  := 1;
      rlist2[1] := result;
      errmax    := abserr;
      maxerr    := 1;
      area      := result;
      errsum    := abserr;
      abserr    := giant;
      nrmax     := 1;
      nres      := 0;
      ktmin     := 0;
      numrl2    := 2;
      extrap    := false;
      noext     := false;
      ierro     := 0;
      iroff1    := 0;
      iroff2    := 0;
      iroff3    := 0;

      If dres>=(1-50*macheps)*defabs Then ksgn := 1
 Else ksgn := -1;

      last := 1;
      continue := true;
      while (last<limit) and (ier=0) and continue Do
      Begin
        Inc(last);
        a1 := alist^[maxerr];
        b1 := 0.5*(alist^[maxerr]+blist^[maxerr]);
        a2 := b1;
        b2 := blist^[maxerr];
        erlast := errmax;
        qk15i(f, bound, inf, a1, b1, area1, error1, resabs, defab1);
        qk15i(f, bound, inf, a2, b2, area2, error2, resabs, defab2);
        area12 := area1+area2;
        erro12 := error1+error2;
        errsum := errsum+erro12-errmax;
        area := area+area12-rlist^[maxerr];
        If (defab1<>error1) And (defab2<>error2)
         Then
         Begin
           If (abs(rlist^[maxerr]-area12)<=1e-5*abs(area12)) And
              (erro12>=0.99*errmax)
            Then If extrap Then Inc(iroff2)
          Else Inc(iroff1);
           If (last>10) And (erro12>errmax) Then Inc(iroff3)
         End;
        rlist^[maxerr] := area1;
        rlist^[last] := area2;

        errbnd := epsrel*abs(area);
        If errbnd<epsabs Then errbnd := epsabs;

        If (iroff1+iroff2>=10) Or (iroff3>=20) Then ier := 2;
        If (iroff2>=5) Then ierro := 3;
        If (last=limit) Then ier := 1;
        h := abs(a1);
       If h<abs(b2) Then h := abs(b2);
        If h<=(1+100*macheps)*(abs(a2)+1000*midget) Then ier := 3;
        If (error2<=error1) Then
         Begin
           alist^[last] := a2;
           blist^[maxerr] := b1;
           blist^[last] := b2;
           elist^[maxerr] := error1;
           elist^[last] := error2
         End
       Else
        Begin
           alist^[maxerr] := a2;
           alist^[last] := a1;
           blist^[last] := b1;
           rlist^[maxerr] := area2;
           rlist^[last] := area1;
           elist^[maxerr] := error2;
           elist^[last] := error1
        End;
        qpsrt(limit, last, maxerr, errmax, elist^[1], iord^[1], nrmax);

        If (errsum<=errbnd) Then continue := false;

        If (ier=0) And continue Then
         If last=2 Then
          Begin
            small := 0.375;
            erlarg := errsum;
            ertest := errbnd;
            rlist2[2] := area
          End
       Else
        If Not noext Then
         Begin
           erlarg := erlarg-erlast;
           If (abs(b1-a1)>small) Then erlarg := erlarg+erro12;
           break := false;
           If Not extrap Then
            If (abs(blist^[maxerr]-alist^[maxerr])>small)
             Then break := true
           Else
            Begin
                extrap :=  true;
                nrmax := 2
            End;
           If Not break And (ierro<>3) And (erlarg>ertest) Then
            Begin
              id := nrmax;
              jupbnd := last;
              If (last>(2+limit Div 2)) Then jupbnd := limit+3-last;
              k := id-1;
              while (k<jupbnd) and not break
              Do
             Begin
                 Inc(k);
                 maxerr := iord^[nrmax];
                 errmax := elist^[maxerr];
                 If (abs(blist^[maxerr]-alist^[maxerr])>small)
                  Then break := true
                 Else Inc(nrmax)
              End
            End;
           If Not break Then
            Begin
              Inc(numrl2);
              rlist2[numrl2] := area;
              qelg(numrl2, rlist2, reseps, abseps, res3la, nres);
              Inc(ktmin);

              If (ktmin>5) And (abserr<1e-3*errsum) Then ier := 4;

              If (abseps<abserr)
               Then
               Begin
                  ktmin := 0;
                  abserr := abseps;
                  result := reseps;
                  correc := erlarg;
                  ertest := epsrel*abs(reseps);
                  If epsabs>ertest Then ertest := epsabs;
                  If (abserr<=ertest) Then continue := false
               End;
            End;
           If continue And Not break Then
            Begin
              If (numrl2=1) Then noext := true;
              If ier<>4 Then
               Begin
                 maxerr := iord^[1];
                 errmax := elist^[maxerr];
                 nrmax := 1;
                 extrap :=  false;
                 small := small*0.5;
                 erlarg := errsum
               End
            End
         End
      End;

      h := 0;
 For k := 1 To last Do
  h := h+rlist^[k];
      FreeMem(rlist, limit*SizeOf(ArbFloat));
      FreeMem(alist, limit*SizeOf(ArbFloat));
      FreeMem(blist, limit*SizeOf(ArbFloat));
      FreeMem(elist, limit*SizeOf(ArbFloat));
      FreeMem(iord, limit*SizeOf(ArbInt));

      If (errsum<=errbnd) Or (abserr=giant) Then
       Begin
        result := h;
            abserr := errsum;
            exit
       End;

      If (ier+ierro)=0 Then
       Begin
           h := abs(result);
           If h<abs(area) Then h := abs(area);
           If (ksgn<>-1) Or (h>defabs*0.01) Then
            If (0.01>result/area) Or (result/area>100) Or (errsum>abs(area))
             Then ier := 5;
           exit
       End;

      If ierro=3 Then abserr := abserr+correc;
      If ier=0 Then ier := 2;

      If (result<>0) And (area<>0) Then
       If abserr/abs(result)>errsum/abs(area)
        Then
        Begin
           result := h;
           abserr := errsum;
           exit
        End
      Else
       Begin
           h := abs(result);
           If h<abs(area) Then h := abs(area);
           If (ksgn<>-1) Or (h>defabs*0.01) Then
            If (0.01>result/area) Or (result/area>100) Or (errsum>abs(area))
             Then ier := 5;
           exit
       End;

      If abserr>errsum Then
       Begin
        result := h;
            abserr := errsum;
            exit
       End;

      If area<>0
       Then
       Begin
           h := abs(result);
           If h<abs(area) Then h := abs(area);
           If (ksgn<>-1) Or (h>defabs*0.01) Then
            If (0.01>result/area) Or (result/area>100) Or (errsum>abs(area))
             Then ier := 5
       End
End;

Procedure int1fr(f: rfunc1r; a, b, ae: ArbFloat; Var integral, err: ArbFloat;
                 Var term: ArbInt);

Var neval, ier, last: ArbInt;
Begin
     term := 3;
 integral := NaN;
     If abs(a)=infinity
      Then If abs(b)=infinity
            Then If (a=b)
                  Then exit
               Else
                Begin
                    qagie(f, 0, 2, ae, epsrel, integral, err, ier);
                    If a=infinity Then integral := -integral
                End
          Else If a=-infinity
                Then qagie(f, b, -1, ae, epsrel, integral, err, ier)
               Else
                Begin
                    qagie(f, b, 1, ae, epsrel, integral, err, ier);
                    integral := -integral
                End
     Else If abs(b)=infinity
           Then If b=-infinity
                 Then
                 Begin
                    qagie(f, a, -1, ae, epsrel, integral, err, ier);
                    integral := -integral
                 End
 Else qagie(f, a, 1, ae, epsrel, integral, err, ier)
          Else qagse(f, a, b, ae, epsrel, limit, integral, err, neval, ier, last);
     term := 4;
     If ier=6 Then term := 3;
     If ier=0 Then term := 1;
     If (ier=2) Or (ier=4) Then term := 2
End;

Begin
    limit    := 500;
    epsrel   := 0;
End.
