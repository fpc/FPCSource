{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             Documentation by Michael van Canneyt (Michael@freepascal.org)

    This is a helper unit for the unit eig. These functions aren't documented,
    so if you find out what it does, please mail it to us.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit eigh2;
{$I DIRECT.INC}

interface

uses typ;

procedure orthes(var a: ArbFloat; n, rwidth: ArbInt; var u: ArbFloat);
procedure hessva(var h: ArbFloat; n, rwidth: ArbInt; var lam: complex;
                 var term: ArbInt);
procedure balance(var a: ArbFloat; n, rwidtha: ArbInt; var low, hi: ArbInt;
                  var d: ArbFloat);
procedure orttrans(var a: ArbFloat; n, rwidtha: ArbInt; var q: ArbFloat;
                   rwidthq: ArbInt);
procedure balback(var pd: ArbFloat; n, m1, m2, k1, k2: ArbInt; var pdx: ArbFloat;
                  rwidth: ArbInt);
procedure hessvec(var h: ArbFloat; n, rwidthh: ArbInt; var lam: complex;
                  var v: ArbFloat; rwidthv: ArbInt; var term: ArbInt);
procedure normeer(var lam: complex; n: ArbInt; var v: ArbFloat;
                  rwidthv: ArbInt);
procedure transx(var v: ArbFloat; n, rwidthv: ArbInt; var lam, x: complex;
                 rwidthx: ArbInt);
procedure reduc1(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var term: ArbInt);
procedure rebaka(var l: ArbFloat; n, rwidthl, k1, k2: ArbInt; var x: ArbFloat;
                 rwidthx: ArbInt; var term: ArbInt);

implementation

procedure orthes(var a: ArbFloat; n, rwidth: ArbInt; var u: ArbFloat);
var               pa, pu, d : ^arfloat1;
    sig, sig2, h, f, tol : ArbFloat;
                    k, i, j : ArbInt;
begin
  pa:=@a; pu:=@u; tol:=midget/macheps;
  getmem(d, n*sizeof(ArbFloat));
  for k:=1 to n-2 do
    begin
      sig2:=0;
      for i:=k+2 to n do
        begin
          d^[i]:=pa^[(i-1)*rwidth+k]; f:=d^[i]; sig2:=sig2+sqr(f)
        end; {i}
      if sig2<tol then
        begin
          pu^[k]:=0; for i:=k+2 to n do pa^[(i-1)*rwidth+k]:=0
        end else
        begin
          f:=pa^[k*rwidth+k]; sig2:=sig2+sqr(f);
          if f<0 then sig:=sqrt(sig2) else sig:=-sqrt(sig2);
          pa^[k*rwidth+k]:=sig;
          h:=sig2-f*sig; d^[k+1]:=f-sig; pu^[k]:=d^[k+1];
          for j:=k+1 to n do
          begin
            f:=0; for i:=k+1 to n do f:=f+d^[i]*pa^[(i-1)*rwidth+j]; f:=f/h;
           for i:=k+1 to n do pa^[(i-1)*rwidth+j]:=pa^[(i-1)*rwidth+j]-f*d^[i]
          end; {j}
          for i:=1 to n do
          begin
            f:=0; for j:=k+1 to n do f:=f+d^[j]*pa^[(i-1)*rwidth+j]; f:=f/h;
           for j:=k+1 to n do pa^[(i-1)*rwidth+j]:=pa^[(i-1)*rwidth+j]-f*d^[j]
          end {i}
        end
    end;  {k}
  freemem(d, n*sizeof(ArbFloat));
end  {orthes};

procedure hessva(var h: ArbFloat; n, rwidth: ArbInt; var lam: complex;
                 var term: ArbInt);
var   i, j, k, kk, k1, k2, k3, l, m, mr,
                ik, nn, na, n1, n2, its : ArbInt;
        meps, p, q, r, s, t, w, x, y, z : ArbFloat;
                          test, notlast : boolean;
                                     ph : ^arfloat1;
                                   plam : ^arcomp1;
begin
  ph:=@h; plam:=@lam;
  t:=0; term:=1; meps:=macheps; nn:=n;
  while (nn >= 1) and (term=1) do
    begin
      n1:=(nn-1)*rwidth; na:=nn-1; n2:=(na-1)*rwidth;
      its:=0;
      repeat
        l:=nn+1; test:=true;
        while test and (l>2) do
          begin
            l:=l-1;
            test:=abs(ph^[(l-1)*(rwidth+1)]) >
                  meps*(abs(ph^[(l-2)*rwidth+l-1])+abs(ph^[(l-1)*rwidth+l]))
          end;
        if (l=2) and  test then l:=l-1;
        if l<na then
          begin
            x:=ph^[n1+nn]; y:=ph^[n2+na]; w:=ph^[n1+na]*ph^[n2+nn];
            if (its=10) or (its=20) then
              begin
                {form exceptional shift}
                t:=t+x;
                for i:=1 to nn do ph^[(i-1)*rwidth+i]:=ph^[(i-1)*rwidth+i]-x;
                s:=abs(ph^[n1+na])+abs(ph^[n1+nn-2]);
                y:=0.75*s; x:=y; w:=-0.4375*sqr(s);
              end; {shift}
            {look for two consecutive small sub-diag elmts}
            m:=nn-1; test:= true ;
            repeat
              m:=m-1; mr:=m*rwidth;
              z:=ph^[mr-rwidth+m]; r:=x-z; s:=y-z;
              p:=(r*s-w)/ph^[mr+m]+ph^[mr-rwidth+m+1];
              q:=ph^[mr+m+1]-z-r-s; r:=ph^[mr+rwidth+m+1];
              s:=abs(p)+abs(q)+abs(r); p:=p/s; q:=q/s; r:=r/s;
              if m <> l then
                test:=abs(ph^[mr-rwidth+m-1])*(abs(q)+abs(r)) <=
                      meps*abs(p)*(abs(ph^[mr-2*rwidth+m-1])+abs(z)+
                                                    abs(ph^[mr+m+1]))
            until (m=l) or test;
            for i:=m+2 to nn do ph^[(i-1)*rwidth+i-2]:=0;
            for i:=m+3 to nn do ph^[(i-1)*rwidth+i-3]:=0;
            { double qp-step involving rows l to nn and columns m to nn}
            for k:=m to na do
              begin
                notlast:=k <> na;
                if k <> m then
                  begin
                    p:=ph^[(k-1)*(rwidth+1)]; q:=ph^[k*rwidth+k-1];
                    if notlast then r:=ph^[(k+1)*rwidth+k-1] else r:=0;
                    x:=abs(p)+abs(q)+abs(r);
                    if x>0 then
                      begin
                        p:=p/x; q:=q/x; r:=r/x
                      end
                  end else x:=1;
                if x>0 then
                begin
                  s:=sqrt(p*p+q*q+r*r); if p<0 then s:=-s;
                  if k <> m then ph^[(k-1)*(rwidth+1)]:=-s*x else
                  if l <> m then
                    begin
                      kk:=(k-1)*(rwidth+1); ph^[kk]:=-ph^[kk]
                    end;
                  p:=p+s; x:=p/s; y:=q/s; z:=r/s; q:=q/p; r:=r/p;
                  { row moxification}
                  for j:=k to nn do
                    begin
                      k1:=(k-1)*rwidth+j; k2:=k1+rwidth; k3:=k2+rwidth;
                      p:=ph^[k1]+q*ph^[k2];
                      if notlast then
                        begin
                          p:=p+r*ph^[k3]; ph^[k3]:=ph^[k3]-p*z;
                        end;
                      ph^[k2]:=ph^[k2]-p*y; ph^[k1]:=ph^[k1]-p*x;
                    end;  {j}
                  if k+3<nn then j:=k+3 else j:=nn;
                  { column modification}
                  for i:=l to j do
                    begin
                      ik:=(i-1)*rwidth+k;
                      p:=x*ph^[ik]+y*ph^[ik+1];
                      if notlast then
                        begin
                          p:=p+z*ph^[ik+2]; ph^[ik+2]:=ph^[ik+2]-p*r;
                        end;
                      ph^[ik+1]:=ph^[ik+1]-p*q; ph^[ik]:=ph^[ik]-p;
                    end  {i}
                end  {x <> 0}
              end  {k};
          end;  {l < na}
        its:=its+1
      until (l=na) or (l=nn) or (its=30);
      if l=nn then
        begin  { one root found}
          plam^[nn].Init(ph^[n1+nn]+t, 0); nn:=na
        end else
      if l=na then
        begin  { two roots found}
          x:=ph^[n1+nn]; y:=ph^[n2+na]; w:=ph^[n1+na]*ph^[n2+nn];
          p:=(y-x)/2; q:=p*p+w; y:=sqrt(abs(q)); x:=x+t;
          if q>0 then
            begin  {  ArbFloat pair}
              if p<0 then y:=-y; y:=p+y;
              plam^[na].Init(x+y, 0); plam^[nn].Init(x-w/y, 0)
            end else
            begin { complex pair}
              plam^[na].Init(x+p, y); plam^[nn].Init(x+p, -y)
            end;
          nn:=nn-2
        end else term:=2
    end {while }
end  {hessva};

procedure balance(var a: ArbFloat; n, rwidtha: ArbInt; var low, hi: ArbInt;
                  var d: ArbFloat);

const radix = 2;

var   i, j, k, l, ii, jj: ArbInt;
    b2, b, c, f, g, r, s: ArbFloat;
                  pa, pd: ^arfloat1;
           nonconv, cont: boolean;

  procedure exc(j, k: ArbInt);
  var i, ii, jj, kk: ArbInt;
                  h: ArbFloat;
  begin
    pd^[k]:=j;
    if j <> k then
      begin
        for i:=1 to n do
          begin
            ii:=(i-1)*rwidtha;
            h:=pa^[ii+j]; pa^[ii+j]:=pa^[ii+k]; pa^[ii+k]:=h
          end; {i}
        for i:=1 to n do
          begin
            jj:=(j-1)*rwidtha+i; kk:=(k-1)*rwidtha+i;
            h:=pa^[jj]; pa^[jj]:=pa^[kk]; pa^[kk]:=h
         end; {i}
     end {j <> k}
  end {exc};
begin
  pa:=@a; pd:=@d; b:=radix; b2:=b*b; l:=1; k:=n; cont:=true;
  while cont do
    begin
      j:=k+1;
      repeat
        j:=j-1; r:=0; jj:=(j-1)*rwidtha;
        for i:=1 to j-1 do r:=r+abs(pa^[jj+i]);
        for i:=j+1 to k do r:=r+abs(pa^[jj+i]);
      until (r=0) or (j=1);
      if r=0 then
        begin
          exc(j,k); k:=k-1
        end;
      cont:=(r=0) and (k >= 1);
    end;
  cont:= true ;
  while cont do
    begin
      j:=l-1;
      repeat
        j:=j+1; r:=0;
        for i:=l to j-1 do r:=r+abs(pa^[(i-1)*rwidtha+j]);
        for i:=j+1 to k do r:=r+abs(pa^[(i-1)*rwidtha+j])
      until (r=0) or (j=k);
      if r=0 then
        begin
          exc(j,l); l:=l+1
        end;
      cont:=(r=0) and (l <= k);
    end;
  for i:=l to k do pd^[i]:=1;
  low:=l; hi:=k; nonconv:=l <= k;
  while nonconv do
    begin
      for i:=l to k do
        begin
          c:=0; r:=0;
          for j:=l to i-1 do
            begin
              c:=c+abs(pa^[(j-1)*rwidtha+i]);
              r:=r+abs(pa^[(i-1)*rwidtha+j])
            end;
          for j:=i+1 to k do
            begin
              c:=c+abs(pa^[(j-1)*rwidtha+i]);
              r:=r+abs(pa^[(i-1)*rwidtha+j])
            end;
          g:=r/b; f:=1; s:=c+r;
          while c<g do
            begin
              f:=f*b; c:=c*b2
            end;
          g:=r*b;
          while c >= g do
            begin
              f:=f/b; c:=c/b2
            end;
          if (c+r)/f<0.95*s then
            begin
              g:=1/f; pd^[i]:=pd^[i]*f; ii:=(i-1)*rwidtha;
              for j:=l to n do pa^[ii+j]:=pa^[ii+j]*g;
              for j:=1 to k do pa^[(j-1)*rwidtha+i]:=pa^[(j-1)*rwidtha+i]*f;
            end else nonconv:=false
        end
     end {while}
end; {balance}

procedure orttrans(var a: ArbFloat; n, rwidtha: ArbInt; var q: ArbFloat;
                   rwidthq: ArbInt);

var                 i, j, k : ArbInt;
    sig, sig2, f, h, tol : ArbFloat;
                  pa, pq, d : ^arfloat1;

begin
  pa:=@a; pq:=@q; tol:=midget/macheps;
  getmem(d, n*sizeof(ArbFloat));
  for k:=1 to n-2 do
    begin
      sig2:=0;
      for i:=k+2 to n do
        begin
          d^[i]:=pa^[(i-1)*rwidtha+k]; f:=d^[i]; sig2:=sig2+sqr(f)
        end;
      if sig2<tol then
        begin
          d^[k+1]:=0; for i:=k+2 to n do pa^[(i-1)*rwidtha+k]:=0
        end else
        begin
          f:=pa^[k*rwidtha+k]; sig2:=sig2+sqr(f);
          if f<0 then sig:=sqrt(sig2) else sig:=-sqrt(sig2);
          pa^[k*rwidtha+k]:=sig; h:=sig2-f*sig; d^[k+1]:=f-sig;
          for j:=k+1 to n do
            begin
              f:=0; for i:=k+1 to n do f:=f+d^[i]*pa^[(i-1)*rwidtha+j];
              f:=f/h;
              for i:=k+1 to n do
                pa^[(i-1)*rwidtha+j]:=pa^[(i-1)*rwidtha+j]-f*d^[i];
            end;
          for i:=1 to n do
            begin
              f:=0; for j:=k+1 to n do f:=f+d^[j]*pa^[(i-1)*rwidtha+j];
              f:=f/h;
              for j:=k+1 to n do
                pa^[(i-1)*rwidtha+j]:=pa^[(i-1)*rwidtha+j]-f*d^[j];
            end
        end
    end; {k}
  for i:=1 to n do
    begin
      pq^[(i-1)*rwidthq+i]:=1;
      for j:=1 to i-1 do
        begin
          pq^[(i-1)*rwidthq+j]:=0; pq^[(j-1)*rwidthq+i]:=0
        end
    end;
  for k:=n-2 downto 1 do
    begin
      h:=pa^[k*rwidtha+k]*d^[k+1];
      if h <> 0
      then
        begin
          for i:=k+2 to n do d^[i]:=pa^[(i-1)*rwidtha+k];
          for i:=k+2 to n do pa^[(i-1)*rwidtha+k]:=0;
          for j:=k+1 to n do
            begin
              f:=0; for i:=k+1 to n do f:=f+d^[i]*pq^[(i-1)*rwidthq+j];
              f:=f/h;
              for i:=k+1 to n do
                pq^[(i-1)*rwidthq+j]:=pq^[(i-1)*rwidthq+j]+f*d^[i]
            end
        end
    end;
  freemem(d, n*sizeof(ArbFloat));
end; {orttrans}

procedure balback(var pd: ArbFloat; n, m1, m2, k1, k2: ArbInt; var pdx: ArbFloat;
                  rwidth: ArbInt);

var i, j, k, ii, kk: ArbInt;
                  s: ArbFloat;
          ppd, ppdx: ^arfloat1;

begin
  ppd:=@pd; ppdx:=@pdx;
  for i:=m1 to m2 do
    begin
      ii:=(i-1)*rwidth; s:=ppd^[i];
      for j:=k1 to k2 do ppdx^[ii+j]:=ppdx^[ii+j]*s;
    end;
  for i:=m1-1 downto 1 do
    begin
      k:=round(ppd^[i]); ii:=(i-1)*rwidth; kk:=(k-1)*rwidth;
      if k <> i then
        for j:=k1 to k2 do
          begin
            s:=ppdx^[ii+j]; ppdx^[ii+j]:=ppdx^[kk+j]; ppdx^[kk+j]:=s
          end
    end;
  for i:=m2+1 to n do
    begin
      k:=round(ppd^[i]); ii:=(i-1)*rwidth; kk:=(k-1)*rwidth;
      if k <> i then
        for j:=k1 to k2 do
          begin
            s:=ppdx^[ii+j]; ppdx^[ii+j]:=ppdx^[kk+j]; ppdx^[kk+j]:=s
          end
    end
end; {balback}

procedure cdiv(xr, xi, yr, yi: ArbFloat; var zr, zi: ArbFloat);
var h:ArbFloat;
begin
  if abs(yr)>abs(yi) then
    begin
      h:=yi/yr; yr:=h*yi+yr;
      zr:=(xr+h*xi)/yr; zi:=(xi-h*xr)/yr;
    end else
    begin
      h:=yr/yi; yi:=h*yr+yi;
      zr:=(h*xr+xi)/yi; zi:=(h*xi-xr)/yi
    end
end; {cdiv}

procedure hessvec(var h: ArbFloat; n, rwidthh: ArbInt; var lam: complex;
                  var v: ArbFloat; rwidthv: ArbInt; var term: ArbInt);

var                        iterate, stop, notlast, contin: boolean;
           i, j, k, l, m, na, its, en, n1, n2, ii, kk, ll,
                                   ik, i1, k0, k1, k2, mr: ArbInt;
    meps, p, q, r, s, t, w, x, y, z, ra, sa, vr, vi, norm: ArbFloat;
                                                   ph, pv: ^arfloat1;
                                                   plam  : ^arcomp1;
begin
  ph:=@h; pv:=@v; plam:=@lam;
  term:=1; en:=n; t:=0; meps:=macheps;
  while (term=1) and (en>=1) do
    begin
      its:=0; na:=en-1; iterate:=true;
      while iterate and (term=1) do
        begin
          l:=en; contin:=true;
          while (l>=2) and contin do
            begin
              ll:=(l-1)*rwidthh+l;
              if abs(ph^[ll-1])>meps*(abs(ph^[ll-rwidthh-1])+abs(ph^[ll]))
              then l:=l-1 else contin:=false
            end;
          n1:=(na-1)*rwidthh; n2:=(en-1)*rwidthh; x:=ph^[n2+en];
          if l=en then
            begin
              iterate:=false; plam^[en].Init(x+t, 0); ph^[n2+en]:=x+t;
              en:=en-1
            end else
            if l=en-1 then
              begin
                iterate:=false; y:=ph^[n1+na]; w:=ph^[n2+na]*ph^[n1+en];
                p:=(y-x)/2; q:=p*p+w; z:=sqrt(abs(q)); x:=x+t;
                ph^[n2+en]:=x; ph^[n1+na]:=y+t;
                if q>0 then
                  begin
                    if p<0 then z:=p-z else z:=p+z; plam^[na].Init(x+z, 0);
                    s:=x-w/z; plam^[en].Init(s, 0);
                    x:=ph^[n2+na]; r:=sqrt(x*x+z*z); p:=x/r; q:=z/r;
                    for j:=na to n do
                      begin
                        z:=ph^[n1+j]; ph^[n1+j]:=q*z+p*ph^[n2+j];
                        ph^[n2+j]:=q*ph^[n2+j]-p*z
                      end;
                    for i:=1 to en do
                      begin
                        ii:=(i-1)*rwidthh;
                        z:=ph^[ii+na]; ph^[ii+na]:=q*z+p*ph^[ii+en];
                        ph^[ii+en]:=q*ph^[ii+en]-p*z;
                      end;
                    for i:=1 to n do
                      begin
                        ii:=(i-1)*rwidthv;
                        z:=pv^[ii+na]; pv^[ii+na]:=q*z+p*pv^[ii+en];
                        pv^[ii+en]:=q*pv^[ii+en]-p*z;
                      end
                  end {q>0}
                else
                  begin
                    plam^[na].Init(x+p, z); plam^[en].Init(x+p, -z)
                  end;
                en:=en-2;
              end {l=en-1}
            else
              begin
                y:=ph^[n1+na]; w:=ph^[n1+en]*ph^[n2+na];
                if (its=10) or (its=20)
                then
                  begin
                    t:=t+x;
                    for i:=1 to en do
                      ph^[(i-1)*rwidthh+i]:=ph^[(i-1)*rwidthh+i]-x;
                    s:=abs(ph^[n2+na])+abs(ph^[n1+en-2]);
                    y:=0.75*s; x:=y; w:=-0.4375*s*s;
                  end;
                m:=en-1; stop:=false;
                repeat
                  m:=m-1; mr:=m*rwidthh;
                  z:=ph^[mr-rwidthh+m]; r:=x-z; s:=y-z;
                  p:=(r*s-w)/ph^[mr+m]+ph^[mr-rwidthh+m+1];
                  q:=ph^[mr+m+1]-z-r-s; r:=ph^[mr+rwidthh+m+1];
                  s:=abs(p)+abs(q)+abs(r); p:=p/s; q:=q/s; r:=r/s;
                  if m>l then
                    stop:=abs(ph^[mr-rwidthh+m-1])*(abs(q)+abs(r))<=
                          meps*abs(p)*(abs(ph^[mr-2*rwidthh+m-1])+
                                          abs(z)+abs(ph^[mr+m+1]))
                until stop or (m=l);
                for i:=m+2 to en do ph^[(i-1)*rwidthh+i-2]:=0;
                for i:=m+3 to en do ph^[(i-1)*rwidthh+i-3]:=0;
                for k:=m to na do
                  begin
                    k0:=(k-1)*rwidthh; k1:=k0+rwidthh; k2:=k1+rwidthh;
                    notlast:=k<na; contin:=true;
                    if k>m then
                      begin
                        p:=ph^[k0+k-1]; q:=ph^[k1+k-1];
                        if notlast then r:=ph^[k2+k-1] else r:=0;
                        x:=abs(p)+abs(q)+abs(r);
                        if x>0 then
                          begin
                            p:=p/x; q:=q/x; r:=r/x
                          end else contin:=false
                      end;
                    if contin then
                      begin
                        s:=sqrt(p*p+q*q+r*r);
                        if p<0 then s:=-s;
                        if k>m then ph^[k0+k-1]:=-s*x else
                        if l <> m then ph^[k0+k-1]:=-ph^[k0+k-1];
                        p:=p+s; x:=p/s; y:=q/s; z:=r/s; q:=q/p; r:=r/p;
                        for j:=k to n do
                          begin
                            p:=ph^[k0+j]+q*ph^[k1+j];
                            if notlast then
                              begin
                                p:=p+r*ph^[k2+j];
                                ph^[k2+j]:=ph^[k2+j]-p*z
                              end;
                            ph^[k1+j]:=ph^[k1+j]-p*y;
                            ph^[k0+j]:=ph^[k0+j]-p*x
                          end; {j}
                        if k+3<en then j:=k+3 else j:=en;
                        for i:=1 to j do
                          begin
                            ik:=(i-1)*rwidthh+k;
                            p:=x*ph^[ik]+y*ph^[ik+1];
                            if notlast then
                              begin
                                p:=p+z*ph^[ik+2]; ph^[ik+2]:=ph^[ik+2]-p*r
                              end;
                            ph^[ik+1]:=ph^[ik+1]-p*q; ph^[ik]:=ph^[ik]-p
                          end;  {i}
                        for i:=1 to n do
                          begin
                            ik:=(i-1)*rwidthv+k;
                            p:=x*pv^[ik]+y*pv^[ik+1];
                            if notlast then
                              begin
                                p:=p+z*pv^[ik+2]; pv^[ik+2]:=pv^[ik+2]-p*r
                              end;
                            pv^[ik+1]:=pv^[ik+1]-p*q; pv^[ik]:=pv^[ik]-p
                          end  {i}
                      end  {contin}
                  end;  {k}
                its:=its+1; if its >= 30 then term:=2
              end  {ifl}
        end  {iterate}
    end;  {term=1}
  if term=1 then
    begin
      norm:=0; k:=1;
      for i:=1 to n do
        begin
          for j:=k to n do norm:=norm+abs(ph^[(i-1)*rwidthh+j]);
          k:=i
        end;
      if norm=0 then
        begin
         { matrix is nulmatrix: eigenwaarden zijn alle 0 en aan de
           eigenvectoren worden de eenheidsvectoren toegekend }
          for i:=1 to n do plam^[i].Init(0, 0);
          for i:=1 to n do
            fillchar(pv^[(i-1)*rwidthv+1], n*sizeof(ArbFloat), 0);
          for i:=1 to n do pv^[(i-1)*rwidthv+i]:=1;
          exit
        end; {norm=0}
      for en:=n downto 1 do
        begin
          p:=plam^[en].re; q:=plam^[en].im; na:=en-1;
          n1:=(na-1)*rwidthh; n2:=(en-1)*rwidthh;
          if q=0 then
            begin
              m:=en; ph^[n2+en]:=1;
              for i:=na downto 1 do
                begin
                  ii:=(i-1)*rwidthh; i1:=ii+rwidthh;
                  w:=ph^[ii+i]-p; r:=ph^[ii+en];
                  for j:=m to na do r:=r+ph^[ii+j]*ph^[(j-1)*rwidthh+en];
                  if plam^[i].im < 0 then
                    begin
                      z:=w; s:=r
                    end else
                    begin
                      m:=i; if plam^[i].im=0 then
                      if w=0 then ph^[ii+en]:=-r/(meps*norm)
                      else ph^[ii+en]:=-r/w else
                        begin
                          x:=ph^[ii+i+1]; y:=ph^[i1+i];
                          q:=sqr(plam^[i].xreal-p)+sqr(plam^[i].imag);
                          ph^[ii+en]:=(x*s-z*r)/q; t:=ph^[ii+en];
                          if abs(x)>abs(z) then ph^[i1+en]:=(-r-w*t)/x
                          else ph^[i1+en]:=(-s-y*t)/z;
                        end  {plam^[i].imag > 0}
                    end  {plam^[i].imag >= 0}
                end  {i}
            end {q=0}
          else
            if q<0 then
              begin
                m:=na;
                if abs(ph^[n2+na]) > abs(ph^[n1+en]) then
                  begin
                    ph^[n1+na]:=-(ph^[n2+en]-p)/ph^[n2+na];
                    ph^[n1+en]:=-q/ph^[n2+na];
                  end else
                  cdiv(-ph^[n1+en], 0, ph^[n1+na]-p, q,
                        ph^[n1+na], ph^[n1+en]);
                ph^[n2+na]:=1; ph^[n2+en]:=0;
                for i:=na-1 downto 1 do
                  begin
                    ii:=(i-1)*rwidthh; i1:=ii+rwidthh;
                    w:=ph^[ii+i]-p; ra:=ph^[ii+en]; sa:=0;
                    for j:=m to na do
                      begin
                        ra:=ra+ph^[ii+j]*ph^[(j-1)*rwidthh+na];
                        sa:=sa+ph^[ii+j]*ph^[(j-1)*rwidthh+en]
                      end;
                    if plam^[i].imag < 0 then
                      begin
                        z:=w; r:=ra; s:=sa
                      end else
                      begin
                        m:=i;
                        if plam^[i].imag=0
                        then cdiv(-ra, -sa, w, q, ph^[ii+na], ph^[ii+en])
                        else
                          begin
                            x:=ph^[ii+i+1]; y:=ph^[i1+i];
                            vr:=sqr(plam^[i].xreal-p)+sqr(plam^[i].imag)-q*q;
                            vi:=(plam^[i].xreal-p)*q*2;
                            if (vr=0) and (vi=0)
                            then
                               vr:=meps*norm*(abs(w)+abs(q)+abs(x)+
                                                   abs(y)+abs(z));
                            cdiv(x*r-z*ra+q*sa, x*s-z*sa-q*ra, vr, vi,
                                 ph^[ii+na], ph^[ii+en]);
                            if abs(x)>abs(z)+abs(q)
                            then
                              begin
                                ph^[i1+na]:=(-ra-w*ph^[ii+na]+q*ph^[ii+en])/x;
                                ph^[i1+en]:=(-sa-w*ph^[ii+en]-q*ph^[ii+na])/x
                              end
                            else
                              cdiv(-r-y*ph^[ii+na], -s-y*ph^[ii+en],
                                   z, q, ph^[i1+na], ph^[i1+en])
                          end  {plam^[i].imag > 0}
                      end {plam^[i].imag >= 0}
                  end  {i}
              end
        end  {backsubst};
      for j:=n downto 1 do
        begin
          m:=j; l:=j-1;
          if plam^[j].imag < 0 then
            begin
              for i:=1 to n do
                begin
                  ii:=(i-1)*rwidthv; y:=0; z:=0;
                  for k:=1 to m do
                    begin
                      kk:=(k-1)*rwidthh;
                      y:=y+pv^[ii+k]*ph^[kk+l];
                      z:=z+pv^[ii+k]*ph^[kk+j]
                    end;
                  pv^[ii+l]:=y; pv^[ii+j]:=z
                end  {i}
            end else
            if plam^[j].imag=0 then
              for i:=1 to n do
                begin
                  z:=0;
                  ii:=(i-1)*rwidthv;
                  for k:=1 to m do z:=z+pv^[ii+k]*ph^[(k-1)*rwidthh+j];
                  pv^[ii+j]:=z;
                end  {i}
        end {j}
    end  {term=1}
end;  {hessvec}

procedure normeer(var lam: complex; n: ArbInt; var v: ArbFloat;
                  rwidthv: ArbInt);

var              i, j, k, ii, kk: ArbInt;
               max, s, t, vr, vi: ArbFloat;
                              pv: ^arfloat1;
                            plam: ^arcomp1;
begin
  plam:=@lam; pv:=@v; j:=1;
  while j<=n do
    if plam^[j].imag=0 then
      begin
        s:=0; for i:=1 to n do s:=s+sqr(pv^[(i-1)*rwidthv+j]); s:=sqrt(s);
        for i:=1 to n do pv^[(i-1)*rwidthv+j]:=pv^[(i-1)*rwidthv+j]/s;
        j:=j+1
      end else
      begin
        max:=0; s:=0;
        for i:=1 to n do
          begin
            ii:=(i-1)*rwidthv;
            t:=sqr(pv^[ii+j])+sqr(pv^[ii+j+1]); s:=s+t;
            if t>max then
              begin
                max:=t; k:=i
              end
          end;
        kk:=(k-1)*rwidthv;
        s:=sqrt(max/s); t:=pv^[kk+j+1]/s; s:=pv^[kk+j]/s;
        for i:=1 to n do
          begin
            ii:=(i-1)*rwidthv;
            vr:=pv^[ii+j]; vi:=pv^[ii+j+1];
            cdiv(vr, vi, s, t, pv^[ii+j], pv^[ii+j+1]);
          end;
        pv^[kk+j+1]:=0; j:=j+2;
      end
end; {normeer}

procedure transx(var v: ArbFloat; n, rwidthv: ArbInt; var lam, x: complex;
                 rwidthx: ArbInt);

var  i, j, ix, iv : ArbInt;
               pv : ^arfloat1;
         plam, px : ^arcomp1;
begin
  pv:=@v; plam:=@lam; px:=@x;
  for i:=1 to n do
    if plam^[i].imag > 0 then
      for j:=1 to n do
        begin
          iv:=(j-1)*rwidthv+i; ix:=(j-1)*rwidthx+i;
          px^[ix].xreal:=pv^[iv]; px^[ix].imag:=pv^[iv+1]
        end else
    if plam^[i].imag < 0 then
      for j:=1 to n do
        begin
          iv:=(j-1)*rwidthv+i; ix:=(j-1)*rwidthx+i;
          px^[ix].xreal:=pv^[iv-1]; px^[ix].imag:=-pv^[iv]
        end else
      for j:=1 to n do
        begin
          iv:=(j-1)*rwidthv+i; ix:=(j-1)*rwidthx+i;
          px^[ix].xreal:=pv^[iv]; px^[ix].imag:=0
        end
end; {transx}

procedure reduc1(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;
                 rwidthb: ArbInt; var term: ArbInt);

var  i, j, k, ia, ja, ib, jb : ArbInt;
                        x, y : ArbFloat;
                      pa, pb : ^arfloat1;
begin
  pa:=@a; pb:=@b;
  term:=1; i:=0;
  while (i<n) and (term=1) do
    begin
      i:=i+1; j:=i-1; jb:=(j-1)*rwidthb; ib:=(i-1)*rwidthb;
      while (j<n) and (term=1) do
        begin
          j:=j+1; jb:=jb+rwidthb; x:=pb^[jb+i];
          for k:=1 to i-1 do x:=x-pb^[ib+k]*pb^[jb+k];
            if i=j then
              begin
                if x<=0 then term:=2 else
                  begin
                    y:=sqrt(x); pb^[ib+i]:=y
                  end
              end else pb^[jb+i]:=x/y
        end  {j}
    end; {i}
  if term=1 then
    begin
      for i:=1 to n do
        begin
          ib:=(i-1)*rwidthb; y:=pb^[ib+i];
          for j:=i to n do
            begin
              ja:=(j-1)*rwidtha; x:=pa^[ja+i];
              for k:=i-1 downto 1 do x:=x-pb^[ib+k]*pa^[ja+k];
                pa^[ja+i]:=x/y;
            end {j}
        end; {i}
      for j:=1 to n do
        begin
          ja:=(j-1)*rwidtha;
          for i:=j to n do
            begin
              ia:=(i-1)*rwidtha; ib:=(i-1)*rwidthb; x:=pa^[ia+j];
              for k:=i-1 downto j do x:=x-pa^[(k-1)*rwidtha+j]*pb^[ib+k];
              for k:=j-1 downto 1 do x:=x-pa^[ja+k]*pb^[ib+k];
              pa^[ia+j]:=x/pb^[ib+i]
            end {i}
        end {j}
    end {term=1};
end; {reduc1}

procedure rebaka(var l: ArbFloat; n, rwidthl, k1, k2: ArbInt; var x: ArbFloat;
                 rwidthx: ArbInt; var term: ArbInt);

var         pl, px : ^arfloat1;
   i, j, k, il, ix : ArbInt;
                y : ArbFloat;
begin
  pl:=@l; px:=@x; term:=1; il:=1;
  for i:=1 to n do
    begin
      if pl^[il]=0 then
        begin
          term:=2; exit
        end;
      il:=il+rwidthl+1
    end; {i}
  for j:=1 to k2-k1+1 do
    for i:=n downto 1 do
      begin
        il:=(i-1)*rwidthl; ix:=(i-1)*rwidthx; y:=px^[ix+j];
        for k:=i+1 to n do y:=y-pl^[(k-1)*rwidthl+i]*px^[(k-1)*rwidthx+j];
        px^[ix+j]:=y/pl^[il+i]
      end
end; {rebaka}

end.
