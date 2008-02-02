{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Determinants for different kinds of matrices (different with respect
                 to symmetry)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit det;

interface
{$I DIRECT.INC}

uses typ;

{Generic determinant}
procedure detgen(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

{determinant symmetrical matrix}
procedure detgsy(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

{determinant of a symmetrical positive definitive matrix}
procedure detgpd(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

{determinant of a generic bandmatrix}
procedure detgba(n, l, r: ArbInt; var a, f: ArbFloat; var k, term:ArbInt);

{determinant of a symmetrical positive definitive bandmatrix}
procedure detgpb(n, l: ArbInt; var a, f: ArbFloat; var k, term:ArbInt);

{determinant of a tridiagonal matrix}
procedure detgtr(n: ArbInt; var l, d, u, f: ArbFloat; var k, term:ArbInt);

{ moved to the TYP unit because of a bug in FPC 1.0.x FK
var og          : ArbFloat absolute ogx;
    bg          : ArbFloat absolute bgx;
    MaxExp      : ArbInt   absolute maxexpx;
}

implementation

uses mdt;

procedure detgen(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

var
    kk, ind, ind1, ns, i        : ArbInt;
    u, ca                       : ArbFloat;
    pa, acopy                   : ^arfloat1;
    p                           : ^arint1;
begin
  if (n<1) or (rwidth<1) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  ns:=n*sizeof(ArbFloat);
  getmem(p, n*sizeof(ArbInt));
  getmem(acopy, n*ns);
  ind:=1; ind1:=1;
  for i:=1 to n do
    begin
      move(pa^[ind1], acopy^[ind], ns);
      ind1:=ind1+rwidth; ind:=ind+n
    end; {i}
  mdtgen(n, n, acopy^[1], p^[1], ca, term);
  if term=1 then
    begin
      f:=1; k:=0; kk:=1; ind:=1;
      while (kk<=n) and (f<>0) do
        begin
          u:=acopy^[ind];
          while (u<>0) and (abs(u)<og) do
            begin
              u:=u/og; k:=k-maxexp
            end; {underflow control}
          while abs(u)>bg do
            begin
              u:=u/bg; k:=k+maxexp
            end; {overflow control}
          f:=f*u;
          if p^[kk]<>kk then f:=-f;
          while (f<>0) and (abs(f)<og) do
            begin
              f:=f/og; k:=k-maxexp
            end; {underflow control}
          while abs(f)>bg do
            begin
              f:=f/bg; k:=k+maxexp
            end; {overflow control}
          kk:=kk+1; ind:=ind+n+1
        end; {kk}
    end {term=1}
  else {term=4}
    begin
      f:=0; k:=0; term:=1
    end;
  freemem(p, n*sizeof(ArbInt));
  freemem(acopy, n*ns)
end; {detgen}

procedure detgsy(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

var i, kk, ind, ind1, s : ArbInt;
    u, ca               : ArbFloat;
    pa, acopy           : ^arfloat1;
    p                   : ^arint1;
    q                   : ^arbool1;
begin
  if (n<1) or (rwidth<1) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  getmem(p, n*sizeof(ArbInt));
  getmem(q, n*sizeof(boolean));
  s:=sizeof(ArbFloat);
  getmem(acopy, n*n*s);
  ind:=1; ind1:=1;
  for i:=1 to n do
    begin
      move(pa^[ind1], acopy^[ind], i*s);
      ind1:=ind1+rwidth; ind:=ind+n
    end; {i}
  mdtgsy(n, n, acopy^[1], p^[1], q^[1], ca, term);
  if term=1 then
    begin
      f:=1; k:=0; kk:=1; ind:=1;
      while (kk<=n) and (f<>0) do
        begin
          u:=acopy^[ind];
          while (u<>0) and (abs(u)<og) do
            begin
              u:=u/og; k:=k-maxexp
            end; {underflow control}
          while abs(u)>bg do
            begin
              u:=u/bg; k:=k+maxexp
            end; {overflow control}
          f:=f*u;
          if q^[kk] then f:=-f;
          while (f<>0) and (abs(f)<og) do
            begin
              f:=f/og; k:=k-maxexp
            end; {underflow control}
          while abs(f)>bg do
            begin
              f:=f/bg; k:=k+maxexp
            end; {overflow control}
          kk:=kk+1; ind:=ind+n+1
        end; {kk}
    end {term=1}
  else {term=4}
    begin
      term:=1; f:=0; k:=0
    end;
  freemem(p, n*sizeof(ArbInt));
  freemem(q, n*sizeof(boolean));
  freemem(acopy, n*n*s)
end; {detgsy}

procedure detgpd(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

var
   i, kk, ind, ind1, s : ArbInt;
   u, ca               : ArbFloat;
   pa, acopy           : ^arfloat1;
begin
  if (n<1) or (rwidth<1) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  s:=sizeof(ArbFloat);
  getmem(acopy, n*n*s);
  ind:=1; ind1:=1;
  for i:=1 to n do
    begin
      move(pa^[ind1], acopy^[ind], i*s);
      ind1:=ind1+rwidth; ind:=ind+n
    end; {i}
  mdtgpd(n, n, acopy^[1], ca, term);
  if term=1 then
    begin
      f:=1; k:=0; kk:=1; ind:=1;
      while kk<=n do
        begin
          u:=sqr(acopy^[ind]);
          while u < og do
            begin
              u:=u/og; k:=k-maxexp
            end; {underflow control}
          while u > bg do
            begin
              u:=u/bg; k:=k+maxexp
            end; {overflow control}
          f:=f*u;
          while f < og do
            begin
              f:=f/og; k:=k-maxexp
            end; {underflow control}
          while f > bg do
            begin
              f:=f/bg; k:=k+maxexp
            end; {overflow control}
          kk:=kk+1; ind:=ind+n+1
        end; {kk}
    end; {term=1}
  freemem(acopy, n*n*s)
end; {detgpd}

procedure detgba(n, l, r: ArbInt; var a, f: ArbFloat;
                 var k, term:ArbInt);
var
    rwidth, s, ns, kk, ii, i, jj, ll : ArbInt;
    u, ca                            : ArbFloat;
    pa, l1, acopy                    : ^arfloat1;
    p                                : ^arint1;
begin
  if (n<1) or (l<0) or (r<0) or (l>n-1) or (r>n-1) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  s:=sizeof(ArbFloat); ns:=n*s;
  ll:=l+r+1;
  getmem(acopy, ll*ns);
  getmem(l1, l*ns);
  getmem(p, n*sizeof(ArbInt));
  jj:=1; ii:=1;
  for i:=1 to n do
    begin
      if i <= l+1 then
        begin
          if i <= (n-r) then rwidth:=r+i else rwidth:=n
        end else
          if i <= (n-r) then rwidth:=ll else rwidth:=n-i+l+1;
      if i > l then kk:=ii else kk:=ii+l-i+1;
      move(pa^[jj], acopy^[kk], rwidth*s);
      jj:=jj+rwidth; ii:=ii+ll;
    end; {i}
  mdtgba(n, l, r, ll, acopy^[1], l, l1^[1], p^[1], ca, term);
  if term=1 then
    begin
      f:=1; k:=0; kk:=1; ii:=1;
      while (kk<=n) and (f<>0) do
        begin
          u:=acopy^[ii];
          while (u<>0) and (abs(u)<og) do
            begin
              u:=u/og; k:=k-maxexp
            end; {underflow control}
          while abs(u)>bg do
            begin
              u:=u/bg; k:=k+maxexp
            end; {overflow control}
          f:=f*u;
          if p^[kk]<>kk then f:=-f;
          while (f<>0) and (abs(f)<og) do
            begin
              f:=f/og; k:=k-maxexp
            end; {underflow control}
          while abs(f)>bg do
            begin
              f:=f/bg; k:=k+maxexp
            end; {overflow control}
          kk:=kk+1; ii:=ii+ll
        end; {kk}
    end {term=1}
  else {term=4}
    begin
      term:=1; f:=0; k:=0
    end;
  freemem(acopy, ll*ns);
  freemem(l1, l*ns);
  freemem(p, n*sizeof(ArbInt))
end; {detgba}

procedure detgpb(n, l: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);

var
  rwidth, kk, ii, ns, ll, jj, i, s  : ArbInt;
          u, ca                     : ArbFloat;
          pa, acopy                 : ^arfloat1;
begin
  if (n<1) or (l<0) or (l>n-1) then
    begin
      term:=3; exit
    end; {wrong input}
  pa:=@a;
  ll:=l+1;
  s:=sizeof(ArbFloat); ns:=s*n;
  getmem(acopy, ll*ns);
  jj:=1; ii:=1;
  for i:=1 to n do
    begin
      if i>l then rwidth:=ll else rwidth:=i;
      move(pa^[jj], acopy^[ii+ll-rwidth], rwidth*s);
      jj:=jj+rwidth; ii:=ii+ll
    end; {i}
  mdtgpb(n, l, ll, acopy^[1], ca, term);
  if term=1 then
    begin
      f:=1; k:=0; kk:=1; ii:=ll;
      while (kk<=n) do
        begin
          u:=sqr(acopy^[ii]);
          while u < og do
            begin
              u:=u/og; k:=k-maxexp
            end; {underflow control}
          while u > bg do
            begin
              u:=u/bg; k:=k+maxexp
            end; {overflow control}
          f:=f*u;
          while f < og do
            begin
              f:=f/og; k:=k-maxexp
            end; {underflow control}
          while f > bg do
            begin
              f:=f/bg; k:=k+maxexp
            end; {overflow control}
          kk:=kk+1; ii:=ii+ll
        end; {kk}
    end; {term=1}
  freemem(acopy, ll*ns);
end; {detgpb}

procedure detgtr(n: ArbInt; var l, d, u, f: ArbFloat; var k, term:ArbInt);

var
          ns, kk              : ArbInt;
          uu, ca              : ArbFloat;
  pl, pd, pu, l1, d1, u1, u2  : ^arfloat1;
  p                           : ^arbool1;
begin
  if n<1 then
    begin
      term:=3; exit
    end; {wrong input}
  pl:=@l; pd:=@d; pu:=@u;
  ns:=n*sizeof(ArbFloat);
  getmem(l1, ns);
  getmem(d1, ns);
  getmem(u1, ns);
  getmem(u2, ns);
  getmem(p, n*sizeof(boolean));
  mdtgtr(n, pl^[1], pd^[1], pu^[1], l1^[1], d1^[1], u1^[1], u2^[1],
         p^[1], ca, term);
  if term=1 then
    begin
      f:=1; k:=0; kk:=1;
      while (kk<=n) and (f<>0) do
        begin
          if p^[kk] then f:=-f;
          uu:=d1^[kk];
          while (uu<>0) and (abs(uu)<og) do
            begin
              uu:=uu/og; k:=k-maxexp
            end; {underflow control}
          while abs(uu)>bg do
            begin
              uu:=uu/bg; k:=k+maxexp
            end; {overflow control}
          f:=f*uu;
          while (f<>0) and (abs(f)<og) do
            begin
              f:=f/og; k:=k-maxexp
            end; {underflow control}
          while abs(f)>bg do
            begin
              f:=f/bg; k:=k+maxexp
            end; {overflow control}
          kk:=kk+1
        end; {kk}
    end {term=1}
  else {term=4}
    begin
      term:=1; f:=0; k:=0
    end;
  freemem(l1, ns);
  freemem(d1, ns);
  freemem(u1, ns);
  freemem(u2, ns);
  freemem(p, n*sizeof(boolean));
end; {detgtr}

end.
