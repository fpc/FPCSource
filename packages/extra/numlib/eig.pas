{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit eig;
{$I DIRECT.INC}

interface

uses typ;

const versie = 'augustus 1993';

procedure eiggs1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);

procedure eiggs2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt;
                 var lam: ArbFloat; var term: ArbInt);

procedure eiggs3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat;
                 rwidthx: ArbInt; var term: ArbInt);

procedure eiggs4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat;
                 rwidthx: ArbInt; var m2, term: ArbInt);

procedure eigts1(var d, cd: ArbFloat; n: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);

procedure eigts2(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);

procedure eigts3(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat;
                 rwidth: ArbInt; var term: ArbInt);

procedure eigts4(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam, x: ArbFloat;
                 rwidth: ArbInt; var m2, term: ArbInt);

procedure eigbs1(var a: ArbFloat; n, l: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);

procedure eigbs2(var a: ArbFloat; n, l, k1, k2: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);

procedure eigbs3(var a: ArbFloat; n, l: ArbInt; var lam, x: ArbFloat;
                 rwidthx: ArbInt; var term: ArbInt);

procedure eigbs4(var a: ArbFloat; n, l, k1, k2: ArbInt;
                 var lam, x: ArbFloat;  rwidthx: ArbInt;
                 var m2, term: ArbInt);

procedure eigge1(var a: ArbFloat; n, rwidth: ArbInt; var lam: complex;
                 var term: ArbInt);

procedure eigge3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: complex;
                 rwidthx: ArbInt; var term: ArbInt);

procedure eiggg1(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt);

procedure eiggg2(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt);

procedure eiggg3(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;
                 var term: ArbInt);

procedure eiggg4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;
                 var m2, term: ArbInt);

procedure eigsv1(var a: ArbFloat; m, n, rwidth: ArbInt; var sig: ArbFloat;
                 var term: ArbInt);

procedure eigsv3(var a: ArbFloat; m, n, rwidtha: ArbInt; var sig, u: ArbFloat;
                 rwidthu: ArbInt; var v: ArbFloat; rwidthv: ArbInt;
                 var term: ArbInt);

implementation

uses eigh1, eigh2;

procedure eiggs1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);
var            i, sr, nsr : ArbInt;
    d, cd, dh, cdh, u, pa : ^arfloat1;
begin
  if n<1 then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  sr:=sizeof(ArbFloat); nsr:=n*sr;
  getmem(d, nsr); getmem(cd, nsr); getmem(dh, nsr); getmem(cdh, nsr);
  getmem(u, n*nsr);
  for i:=1 to n do move(pa^[(i-1)*rwidth+1], u^[(i-1)*n+1], i*sr);
  tred1(u^[1], n, n, d^[1], cd^[1], term);
  move(d^[1], dh^[1], nsr); move(cd^[1], cdh^[1], nsr);
  tql1(d^[1], cd^[1], n, lam, term);
  if term=2 then bisect(dh^[1], cdh^[1], n, 1, n, 0, lam, term);
  freemem(d, nsr); freemem(cd, nsr); freemem(dh, nsr); freemem(cdh, nsr);
  freemem(u, n*nsr);
end; {eiggs1}

procedure eiggs2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt;
                 var lam: ArbFloat; var term: ArbInt);
var          i, sr, nsr : ArbInt;
           d, cd, u, pa : ^arfloat1;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  sr:=sizeof(ArbFloat); nsr:=n*sr;
  getmem(d, nsr); getmem(cd, nsr); getmem(u, n*nsr);
  for i:=1 to n do move(pa^[(i-1)*rwidth+1], u^[(i-1)*n+1], i*sr);
  tred1(u^[1], n, n, d^[1], cd^[1], term);
  bisect(d^[1], cd^[1], n, k1, k2, 0, lam, term);
  freemem(d, nsr); freemem(cd, nsr); freemem(u, n*nsr);
end; {eiggs2}

procedure eiggs3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat;
                 rwidthx: ArbInt; var term: ArbInt);
var   nsr : ArbInt;
    d, cd : ^arfloat1;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  nsr:=n*sizeof(ArbFloat);
  getmem(d, nsr); getmem(cd, nsr);
  tred2(a, n, rwidtha, d^[1], cd^[1], x, rwidthx, term);
  tql2(d^[1], cd^[1], n, lam, x, rwidthx, term);
  freemem(d, nsr); freemem(cd, nsr)
end;  {eiggs3}

procedure eiggs4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat;
                 rwidthx: ArbInt; var m2, term: ArbInt);
var      i, sr, nsr : ArbInt;
       pa, d, cd, u : ^arfloat1;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  sr:=sizeof(ArbFloat); nsr:=n*sr;
  getmem(d, nsr); getmem(cd, nsr); getmem(u, n*nsr);
  for i:=1 to n do move(pa^[(i-1)*rwidtha+1], u^[(i-1)*n+1], i*sr);
  tred1(u^[1], n, n, d^[1], cd^[1], term);
  trsturm1(d^[1], cd^[1], n, k1, k2, lam, x, rwidthx, m2, term);
  trbak1(u^[1], n, n, cd^[1], k1, k2, x, rwidthx);
  freemem(d, nsr); freemem(cd, nsr); freemem(u, n*nsr) { toegevoegd 3 apr 92 }
end; {eiggs4}

procedure eigts1(var d, cd: ArbFloat; n: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);
var               sr, nsr : ArbInt;
         pd, pcd, dh, cdh : ^arfloat1;
begin
  if n<1 then
    begin
      term:=3; exit
    end; {wrong input}
  sr:=sizeof(ArbFloat); nsr:=n*sr;
  pd:=@d; pcd:=@cd; getmem(dh, nsr); getmem(cdh, nsr);
  move(pd^[1], dh^[1], nsr); move(pcd^[1], cdh^[2], (n-1)*sr);
  tql1(dh^[1], cdh^[1], n, lam, term);
  if term=2 then
    begin
      move(pd^[1], dh^[1], nsr); move(pcd^[1], cdh^[2], (n-1)*sr);
      bisect(dh^[1], cdh^[1], n, 1, n, 0, lam, term)
    end;
  freemem(dh, nsr); freemem(cdh, nsr);
end; {eigts1}

procedure eigts2(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);
var               sr, nsr : ArbInt;
                 pcd, cdh : ^arfloat1;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end; {wrong input}
  pcd:=@cd;
  term:=1; sr:=sizeof(ArbFloat); nsr:=n*sr; getmem(cdh, nsr);
  move(pcd^[1], cdh^[2], (n-1)*sr);
  bisect(d, cdh^[1], n, k1, k2, 0, lam, term);
  freemem(cdh, nsr)
end; {eigts2}

procedure eigts3(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat;
                 rwidth: ArbInt; var term: ArbInt);
var             i, sr, nsr : ArbInt;
              px, pcd, cdh : ^arfloat1;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  px:=@x; pcd:=@cd;
  sr:=sizeof(ArbFloat); nsr:=n*sr;
  getmem(cdh, nsr);
  move(pcd^[1], cdh^[2], (n-1)*sr);
  for i:=1 to n do fillchar(px^[(i-1)*rwidth+1], nsr, 0);
  for i:=1 to n do px^[(i-1)*rwidth+i]:=1;
  tql2(d, cdh^[1], n, lam, px^[1], rwidth, term);
  freemem(cdh, nsr);
end;  {eigts3}

procedure eigts4(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam, x: ArbFloat;
                 rwidth: ArbInt; var m2, term: ArbInt);
var                    sr : ArbInt;
                 pcd, cdh : ^arfloat1;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end; {wrong input}
  term:=1;
  pcd:=@cd; sr:=sizeof(ArbFloat);
  getmem(cdh, n*sr);
  move(pcd^[1], cdh^[2], (n-1)*sr);
  trsturm1(d, cdh^[1], n, k1, k2, lam, x, rwidth, m2, term);
  freemem(cdh, n*sr)
end; {eigts4}

procedure eigbs1(var a: ArbFloat; n, l: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);
var             u, d, cd : ^arfloat1;
      uwidth, sr, nsr : ArbInt;
begin
  if (n<1) or (l<0) or (l>n-1) then
    begin
      term:=3; exit
    end; {wrong input}
  sr:=sizeof(ArbFloat); nsr:=n*sr; uwidth:=l+1;
  getmem(u, uwidth*nsr); getmem(d, nsr); getmem(cd, nsr);
  transf(a, n, l, u^[1], uwidth);
  bandrd1(u^[1], n, l, uwidth, d^[1], cd^[1]);
  eigts1(d^[1], cd^[2], n, lam, term);
  freemem(u, uwidth*nsr); freemem(d, nsr); freemem(cd, nsr);
end; {eigbs1}

procedure eigbs2(var a: ArbFloat; n, l, k1, k2: ArbInt; var lam: ArbFloat;
                 var term: ArbInt);
var                  u, d, cd : ^arfloat1;
            sr, nsr, uwidth : ArbInt;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) or (l<0) or (l>n-1) then
    begin
      term:=3; exit
    end; {wrong input}
  sr:=sizeof(ArbFloat); nsr:=n*sr; uwidth:=l+1;
  getmem(u, uwidth*nsr); getmem(d, nsr); getmem(cd, nsr);
  transf(a, n, l, u^[1], uwidth);
  bandrd1(u^[1], n, l, uwidth, d^[1], cd^[1]);
  eigts2(d^[1], cd^[2], n, k1, k2, lam, term);
  freemem(u, uwidth*nsr); freemem(d, nsr); freemem(cd, nsr)
end; {eigbs2}

procedure eigbs3(var a: ArbFloat; n, l: ArbInt; var lam, x: ArbFloat;
                 rwidthx: ArbInt; var term: ArbInt);
var                  u, d, cd : ^arfloat1;
           sr, nsr, uwidth : ArbInt;
begin
  if (n<1) or (l<0) or (l>n-1) then
    begin
      term:=3; exit
    end; {wrong input}
  sr:=sizeof(ArbFloat); nsr:=n*sr; uwidth:=l+1;
  getmem(u, uwidth*nsr); getmem(d, nsr); getmem(cd, nsr);
  transf(a, n, l, u^[1], uwidth);
  bandrd2(u^[1], n, l, uwidth, d^[1], cd^[1], x, rwidthx);
  tql2(d^[1], cd^[1], n, lam, x, rwidthx, term);
  freemem(u, uwidth*nsr); freemem(d, nsr); freemem(cd, nsr)
end; {eigbs3}

procedure eigbs4(var a: ArbFloat; n, l, k1, k2: ArbInt;
                 var lam, x: ArbFloat;  rwidthx: ArbInt;
                 var m2, term: ArbInt);
var  i, j, k, m, uwidth : ArbInt;
     plam, px, pa, v, u : ^arfloat1;
                s, norm : ArbFloat;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) or (l<0) or (l>n-1) then
    begin
      term:=3; exit
    end; {wrong input}
  plam:=@lam; px:=@x; pa:=@a; getmem(v, n*sizeof(ArbFloat));
  uwidth:=l+1; getmem(u, n*uwidth*sizeof(ArbFloat));
  eigbs2(a, n, l, k1, k2, plam^[1], term);
  { kijk of norm(A-lambda.I)=0 }
  { zo ja, lever dan de eenheidsvectoren e(k1) t/m e(k2) af }
  norm:=0; j:=1;
  for i:=1 to n do
  begin
      if i<=l then m:=i else m:=l+1; s:=0;
      for k:=j to j+m-1 do
      if k=j+m-1 then s:=s+abs(pa^[k]-plam^[1]) else s:=s+abs(pa^[k]);
      if s>norm then norm:=s;
      j:=j+m
  end;
  if norm=0 then
  begin
      for i:=k1 to k2 do for j:=1 to n do
      if j=i then px^[(j-1)*rwidthx+i-k1+1]:=1
      else px^[(j-1)*rwidthx+i-k1+1]:=0;
      freemem(v, n*sizeof(ArbFloat)); freemem(u, n*uwidth*sizeof(ArbFloat));
      m2:=k2; term:=1; exit
  end;
  transf(a, n, l, u^[1], uwidth);
  i:=k1; m2:=k1-1;
  while (i <= k2) and (term=1) do
    begin
      bandev(u^[1], n, l, uwidth, plam^[i-k1+1], v^[1], term);
      if term=1 then
        begin
          m2:=i; for j:=1 to n do px^[(j-1)*rwidthx+i-k1+1]:=v^[j]
        end;
      i:=i+1
    end; {i}
  freemem(v, n*sizeof(ArbFloat));
  freemem(u, n*uwidth*sizeof(ArbFloat));
end; {eigbs4}

procedure eigge1(var a: ArbFloat; n, rwidth: ArbInt; var lam: complex;
                 var term: ArbInt);
var pa, h, dummy : ^arfloat1;
           i, ns : ArbInt;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  ns:=n*sizeof(ArbFloat); pa:=@a;
  getmem(dummy, ns); getmem(h, n*ns);
  for i:=1 to n do move(pa^[(i-1)*rwidth+1], h^[(i-1)*n+1], ns);
  orthes(h^[1], n, n, dummy^[1]);
  hessva(h^[1], n, n, lam, term);
  freemem(dummy, ns); freemem(h, n*ns);
end;  {eigge1}

procedure eigge3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: complex;
                 rwidthx: ArbInt; var term: ArbInt);
var     pa, pd, u, v: ^arfloat1;
    m1, m2, i, ns: ArbInt;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  ns:=n*sizeof(ArbFloat); getmem(pd, ns); getmem(u, n*ns); getmem(v, n*ns);
  pa:=@a; for i:=1 to n do move(pa^[(i-1)*rwidtha+1], u^[(i-1)*n+1], ns);
  balance(u^[1], n, n, m1, m2, pd^[1]);
  orttrans(u^[1], n, n, v^[1], n);
  hessvec(u^[1], n, n, lam, v^[1], n, term);
  if term=1 then
    begin
      balback(pd^[1], n, m1, m2, 1, n, v^[1], n);
      normeer(lam, n, v^[1], n);
      transx(v^[1], n, n, lam, x, rwidthx)
    end;
  freemem(pd, ns); freemem(u, n*ns); freemem(v, n*ns);
end;  {eigge3}

procedure eiggg1(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt);
var u, v, pa, pb : ^arfloat1;
        i, ns : ArbInt;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  pa:=@a; pb:=@b; ns:=n*sizeof(ArbFloat); getmem(u, n*ns); getmem(v, n*ns);
  for i:=1 to n do move(pa^[(i-1)*rwidtha+1], u^[(i-1)*n+1], ns);
  for i:=1 to n do move(pb^[(i-1)*rwidthb+1], v^[(i-1)*n+1], ns);
  reduc1(u^[1], n, n, v^[1], n, term);
  if term=1 then eiggs1(u^[1], n, n, lam, term);
  freemem(u, n*ns); freemem(v, n*ns);
end; {eiggg1}

procedure eiggg2(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt);
var u, v, pa, pb : ^arfloat1;
        i, ns : ArbInt;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end;
  pa:=@a; pb:=@b; ns:=n*sizeof(ArbFloat); getmem(u, n*ns); getmem(v, n*ns);
  for i:=1 to n do move(pa^[(i-1)*rwidtha+1], u^[(i-1)*n+1], ns);
  for i:=1 to n do move(pb^[(i-1)*rwidthb+1], v^[(i-1)*n+1], ns);
  reduc1(u^[1], n, n, v^[1], n, term);
  if term=1 then eiggs2(u^[1], n, n, k1, k2, lam, term);
  freemem(u, n*ns); freemem(v, n*ns)
end; {eiggg2}

procedure eiggg3(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;
                 var term: ArbInt);
var u, v, pa, pb : ^arfloat1;
        i, ns : ArbInt;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  pa:=@a; pb:=@b;
  ns:=n*sizeof(ArbFloat);
  getmem(u, n*ns); getmem(v, n*ns);
  for i:=1 to n do move(pa^[(i-1)*rwidtha+1], u^[(i-1)*n+1], ns);
  for i:=1 to n do move(pb^[(i-1)*rwidthb+1], v^[(i-1)*n+1], ns);
  reduc1(u^[1], n, n, v^[1], n, term);
  if term=1 then
    begin
      eiggs3(u^[1], n, n, lam, x, rwidthx, term);
      if term=1 then rebaka(v^[1], n, n, 1, n, x, rwidthx, term)
    end;
  freemem(u, n*ns); freemem(v, n*ns)
end; {eiggg3}

procedure eiggg4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;
                 var m2, term: ArbInt);

var u, v, pa, pb : ^arfloat1;
     i, ns, t : ArbInt;
begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end;
  pa:=@a; pb:=@b; ns:=n*sizeof(ArbFloat); getmem(u, n*ns); getmem(v, n*ns);
  for i:=1 to n do move(pa^[(i-1)*rwidtha+1], u^[(i-1)*n+1], ns);
  for i:=1 to n do move(pb^[(i-1)*rwidthb+1], v^[(i-1)*n+1], ns);
  reduc1(u^[1], n, n, v^[1], n, term);
  if term=1 then
    begin
      eiggs4(u^[1], n, n, k1, k2, lam, x, rwidthx, m2, term);
      if m2 < k2 then term:=4;
      if m2 > k1-1 then
        begin
          rebaka(v^[1], n, n, k1, m2, x, rwidthx, t);
          if t=2 then
            begin
              term:=4; m2:=k1-1
            end
        end
    end;
  freemem(u, n*ns); freemem(v, n*ns)
end; {eiggg4}

procedure eigsv1(var a: ArbFloat; m, n, rwidth: ArbInt; var sig: ArbFloat;
                 var term: ArbInt);

var                     pa, pq, u, e : ^arfloat1;
          i, j, k, l, ns, ii, jj, kk : ArbInt;
 c, f, g, h, p, s, x, y, z, eps, tol : ArbFloat;
                  conv, goon, cancel : boolean;
begin
  if (n<1) or (m<n) then
    begin
      term:=3; exit
    end;
  pa:=@a; pq:=@sig; term:=1;
  ns:=n*sizeof(ArbFloat); getmem(e, ns); getmem(u, m*ns);
  for i:=1 to m do move(pa^[(i-1)*rwidth+1], u^[(i-1)*n+1], ns);
  g:=0; x:=0; tol:=midget/macheps;
  for i:=1 to n do
    begin
      ii:=(i-1)*n; e^[i]:=g;
      s:=0; for j:=i to m do s:=s+sqr(u^[(j-1)*n+i]);
      if s<tol then g:=0 else
        begin
          f:=u^[ii+i]; if f<0 then g:=sqrt(s) else g:=-sqrt(s);
          h:=f*g-s; u^[ii+i]:=f-g;
          for j:=i+1 to n do
            begin
              s:=0;
              for k:=i to m do
                begin
                  kk:=(k-1)*n; s:=s+u^[kk+i]*u^[kk+j]
                end; {k}
              f:=s/h;
              for k:=i to m do
                begin
                  kk:=(k-1)*n; u^[kk+j]:=u^[kk+j]+f*u^[kk+i]
                end {k}
            end {j}
        end; {s}
      pq^[i]:=g; s:=0;
      for j:=i+1 to n do s:=s+sqr(u^[ii+j]);
      if s < tol then g:=0 else
        begin
          f:=u^[ii+i+1]; if f < 0 then g:=sqrt(s) else g:=-sqrt(s);
          h:=f*g-s; u^[ii+i+1]:=f-g;
          for j:=i+1 to n do e^[j]:=u^[ii+j]/h;
          for j:=i+1 to m do
            begin
              s:=0; jj:=(j-1)*n;
              for k:=i+1 to n do s:=s+u^[jj+k]*u^[ii+k];
              for k:=i+1 to n do u^[jj+k]:=u^[jj+k]+s*e^[k]
            end {j}
        end; {s}
      y:=abs(pq^[i])+abs(e^[i]); if y > x then x:=y
    end; {i}
  eps:=macheps*x;
  for k:=n downto 1 do
    begin
      conv:=false;
      repeat
        l:=k; goon:=true;
        while goon do
          begin
            if abs(e^[l]) <= eps then
              begin
                goon:=false; cancel:=false
              end else
            if abs(pq^[l-1]) <= eps then
              begin
                goon:=false; cancel:=true
              end
            else l:=l-1
          end; {goon}
        if cancel then
          begin
            c:=0; s:=1;
            i:=l; goon:=true;
            while goon do
              begin
                f:=s*e^[i]; e^[i]:=c*e^[i]; goon:=abs(f) > eps;
                if goon then
                  begin
                    g:=pq^[i]; h:=sqrt(f*f+g*g); pq^[i]:=h;
                    c:=g/h; s:=-f/h;
                    i:=i+1; goon:=i <= k
                  end {goon}
              end {while goon}
          end; {cancel}
        z:=pq^[k];
        if k=l then conv:=true else
          begin
            x:=pq^[l]; y:=pq^[k-1]; g:=e^[k-1]; h:=e^[k];
            f:=((y-z)*(y+z)+(g-h)*(g+h))/(2*h*y); g:=sqrt(f*f+1);
            if f < 0 then s:=f-g else s:=f+g;
            f:=((x-z)*(x+z)+h*(y/s-h))/x;
            c:=1; s:=1;
            for i:=l+1 to k do
              begin
                g:=e^[i]; y:=pq^[i]; h:=s*g; g:=c*g;
                z:=sqrt(f*f+h*h); e^[i-1]:=z; c:=f/z; s:=h/z;
                f:=x*c+g*s; g:=-x*s+g*c; h:=y*s; y:=y*c;
                z:=sqrt(f*f+h*h); pq^[i-1]:=z; c:=f/z; s:=h/z;
                f:=c*g+s*y; x:=-s*g+c*y
              end; {i}
            e^[l]:=0; e^[k]:=f; pq^[k]:=x
          end {k <> l}
      until conv;
      if z < 0 then pq^[k]:=-z
    end; {k}
  for i:=1 to n do
    begin
      k:=i; p:=pq^[i];
      for j:=i+1 to n do
        if pq^[j] < p then
          begin
            k:=j; p:=pq^[j]
          end;
        if k <> i then
          begin
            pq^[k]:=pq^[i]; pq^[i]:=p
          end
    end; {i}
  freemem(e, ns); freemem(u, m*ns)
end; {eigsv1}

procedure eigsv3(var a: ArbFloat; m, n, rwidtha: ArbInt; var sig, u: ArbFloat;
                 rwidthu: ArbInt; var v: ArbFloat; rwidthv: ArbInt;
                 var term: ArbInt);

var                pa, pu, pq, pv, e : ^arfloat1;
          i, j, k, l, ns, ii, jj, kk : ArbInt;
 c, f, g, h, p, s, x, y, z, eps, tol : ArbFloat;
                  conv, goon, cancel : boolean;
begin
  if (n<1) or (m<n)
  then
    begin
      term:=3; exit
    end;
  pa:=@a; pu:=@u; pq:=@sig; pv:=@v; term:=1;
  ns:=n*sizeof(ArbFloat); getmem(e, ns);
  for i:=1 to m do move(pa^[(i-1)*rwidtha+1], pu^[(i-1)*rwidthu+1], ns);
  g:=0; x:=0; tol:=midget/macheps;
  for i:=1 to n do
    begin
      ii:=(i-1)*rwidthu;
      e^[i]:=g; s:=0;
      for j:=i to m do s:=s+sqr(pu^[(j-1)*rwidthu+i]);
      if s<tol then g:=0 else
        begin
          f:=pu^[ii+i]; if f<0 then g:=sqrt(s) else g:=-sqrt(s);
          h:=f*g-s; pu^[ii+i]:=f-g;
          for j:=i+1 to n do
            begin
              s:=0;
              for k:=i to m do
                begin
                  kk:=(k-1)*rwidthu; s:=s+pu^[kk+i]*pu^[kk+j]
                end; {k}
              f:=s/h;
              for k:=i to m do
                begin
                  kk:=(k-1)*rwidthu; pu^[kk+j]:=pu^[kk+j]+f*pu^[kk+i]
                end {k}
            end {j}
        end; {s}
      pq^[i]:=g; s:=0; for j:=i+1 to n do s:=s+sqr(pu^[ii+j]);
      if s < tol then g:=0 else
        begin
          f:=pu^[ii+i+1];
          if f < 0 then g:=sqrt(s) else g:=-sqrt(s);
          h:=f*g-s; pu^[ii+i+1]:=f-g;
          for j:=i+1 to n do e^[j]:=pu^[ii+j]/h;
          for j:=i+1 to m do
            begin
              s:=0; jj:=(j-1)*rwidthu;
              for k:=i+1 to n do s:=s+pu^[jj+k]*pu^[ii+k];
              for k:=i+1 to n do pu^[jj+k]:=pu^[jj+k]+s*e^[k]
            end {j}
        end; {s}
      y:=abs(pq^[i])+abs(e^[i]); if y > x then x:=y
    end; {i}
  for i:=n downto 1 do
    begin
      ii:=(i-1)*rwidthu;
      if g <> 0 then
        begin
          h:=pu^[ii+i+1]*g;
          for j:=i+1 to n do pv^[(j-1)*rwidthv+i]:=pu^[ii+j]/h;
          for j:=i+1 to n do
            begin
              s:=0;
              for k:=i+1 to n do s:=s+pu^[ii+k]*pv^[(k-1)*rwidthv+j];
              for k:=i+1 to n do
                begin
                  kk:=(k-1)*rwidthv; pv^[kk+j]:=pv^[kk+j]+s*pv^[kk+i]
                end {k}
            end {j}
        end; {g}
      ii:=(i-1)*rwidthv;
      for j:=i+1 to n do
        begin
          pv^[ii+j]:=0; pv^[(j-1)*rwidthv+i]:=0
        end; {j}
      pv^[ii+i]:=1; g:=e^[i]
    end; {i}
  for i:=n downto 1 do
    begin
      g:=pq^[i]; ii:=(i-1)*rwidthu;
      for j:=i+1 to n do pu^[ii+j]:=0;
      if g <> 0 then
        begin
          h:=pu^[ii+i]*g;
          for j:=i+1 to n do
            begin
              s:=0;
              for k:=i+1 to m do
                begin
                  kk:=(k-1)*rwidthu; s:=s+pu^[kk+i]*pu^[kk+j]
                end; {k}
              f:=s/h;
              for k:=i to m do
                begin
                  kk:=(k-1)*rwidthu;
                  pu^[kk+j]:=pu^[kk+j]+f*pu^[kk+i]
                end {k}
            end; {j}
          for j:=i to m do
            begin
              jj:=(j-1)*rwidthu+i; pu^[jj]:=pu^[jj]/g
            end {j}
        end {g}
      else
        for j:=i to m do pu^[(j-1)*rwidthu+i]:=0;
      pu^[ii+i]:=pu^[ii+i]+1
    end; {i}
  eps:=macheps*x;
  for k:=n downto 1 do
    begin
      conv:=false;
      repeat
        l:=k; goon:=true;
        while goon do
          begin
            if abs(e^[l]) <= eps then
              begin
                goon:=false; cancel:=false
              end else
            if abs(pq^[l-1]) <= eps then
              begin
                goon:=false; cancel:=true
              end else l:=l-1
          end; {goon}
        if cancel then
          begin
            c:=0; s:=1; i:=l; goon:=true;
            while goon do
              begin
                f:=s*e^[i]; e^[i]:=c*e^[i]; goon:=abs(f) > eps;
                if goon then
                  begin
                    g:=pq^[i]; h:=sqrt(f*f+g*g); pq^[i]:=h;
                    c:=g/h; s:=-f/h;
                    for j:=1 to m do
                      begin
                        jj:=(j-1)*rwidthu; y:=pu^[jj+l-1]; z:=pu^[jj+i];
                        pu^[jj+l-1]:=y*c+z*s; pu^[jj+i]:=-y*s+z*c
                      end; {j}
                    i:=i+1; goon:=i <= k
                  end {goon}
              end {while goon}
          end; {cancel}
        z:=pq^[k]; if k=l then conv:=true else
          begin
            x:=pq^[l]; y:=pq^[k-1]; g:=e^[k-1]; h:=e^[k];
            f:=((y-z)*(y+z)+(g-h)*(g+h))/(2*h*y); g:=sqrt(f*f+1);
            if f < 0 then s:=f-g else s:=f+g;
            f:=((x-z)*(x+z)+h*(y/s-h))/x;
            c:=1; s:=1;
            for i:=l+1 to k do
              begin
                g:=e^[i]; y:=pq^[i]; h:=s*g; g:=c*g;
                z:=sqrt(f*f+h*h); e^[i-1]:=z; c:=f/z; s:=h/z;
                f:=x*c+g*s; g:=-x*s+g*c; h:=y*s; y:=y*c;
                for j:=1 to n do
                  begin
                    jj:=(j-1)*rwidthv;
                    x:=pv^[jj+i-1]; z:=pv^[jj+i];
                    pv^[jj+i-1]:=x*c+z*s; pv^[jj+i]:=-x*s+z*c
                  end; {j}
                z:=sqrt(f*f+h*h); pq^[i-1]:=z; c:=f/z; s:=h/z;
                f:=c*g+s*y; x:=-s*g+c*y;
                for j:=1 to m do
                  begin
                    jj:=(j-1)*rwidthu;
                    y:=pu^[jj+i-1]; z:=pu^[jj+i];
                    pu^[jj+i-1]:=y*c+z*s; pu^[jj+i]:=-y*s+z*c
                  end {j}
              end; {i}
            e^[l]:=0; e^[k]:=f; pq^[k]:=x
          end {k <> l}
      until conv;
      if z < 0 then
        begin
          pq^[k]:=-z;
          for j:=1 to n do
            begin
              jj:=(j-1)*rwidthv+k; pv^[jj]:=-pv^[jj]
            end {j}
        end {z}
    end; {k}
  for i:=1 to n do
    begin
      k:=i; p:=pq^[i];
      for j:=i+1 to n do
        if pq^[j] < p then
          begin
            k:=j; p:=pq^[j]
          end;
        if k <> i then
          begin
            pq^[k]:=pq^[i]; pq^[i]:=p;
            for j:=1 to m do
              begin
                jj:=(j-1)*rwidthu;
                p:=pu^[jj+i]; pu^[jj+i]:=pu^[jj+k]; pu^[jj+k]:=p;
              end;
            for j:=1 to n do
              begin
                jj:=(j-1)*rwidthv;
                p:=pv^[jj+i]; pv^[jj+i]:=pv^[jj+k]; pv^[jj+k]:=p
              end { interchange in u and v column i with comlumn k }
          end
    end; {i}
  freemem(e, ns)
end; {eigsv3}
end.
