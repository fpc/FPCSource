{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             Documentation by Michael van Canneyt (Michael@freepascal.org)

    This is a helper unit for the unit eig. The functions aren't documented,
    so if you find out what it does, please mail it to us.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit eigh1;
{$I DIRECT.INC}

interface

uses typ;

procedure tred1(var a: ArbFloat; n, rwidth: ArbInt; var d, cd: ArbFloat;
                var term: ArbInt);
procedure tred2(var a: ArbFloat; n, rwidtha: ArbInt; var d, cd, x: ArbFloat;
                  rwidthx: ArbInt; var term: ArbInt);
procedure tql1(var d, cd: ArbFloat; n: ArbInt;
               var lam: ArbFloat; var term: ArbInt);
procedure tql2(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat;
               rwidth: ArbInt; var term: ArbInt);
procedure bisect(var d, cd: ArbFloat; n, k1, k2: ArbInt; eps: ArbFloat;
                 var lam: ArbFloat; var term: ArbInt);
procedure trbak1(var a: ArbFloat; n, rwidtha: ArbInt; var cd: ArbFloat;
                 k1, k2: ArbInt; var x: ArbFloat; rwidthx: ArbInt);
procedure trsturm1(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat;
                   var x: ArbFloat; rwidth: ArbInt; var m2, term: ArbInt);
procedure transf(var a: ArbFloat; n, l: ArbInt; var b: ArbFloat; rwidthb: ArbInt);
procedure bandrd1(var a: ArbFloat; n, m, rwidth: ArbInt; var d, cd: ArbFloat);
procedure bandrd2(var a: ArbFloat; n, m, rwidtha: ArbInt; var d, cd, x: ArbFloat;
                  rwidthx: ArbInt);
procedure bandev(var a: ArbFloat; n, m, rwidth: ArbInt; lambda: ArbFloat;
                 var v: ArbFloat; var term: ArbInt);

implementation

procedure tred1(var a: ArbFloat; n, rwidth: ArbInt; var d, cd: ArbFloat;
                var term: ArbInt);

var  i, ii, jj, j, k, l, sr : ArbInt;
               f, g, h, tol : ArbFloat;
             pa, pd, pcd : ^arfloat1;
begin
  if n<1 then
  begin
      term:=3; exit
  end; {wrong input}
  pa:=@a; pd:=@d; pcd:=@cd;
  sr:=sizeof(ArbFloat);
  tol:=midget/macheps;
  for i:=1 to n do pd^[i]:=pa^[(i-1)*rwidth+i];
  for i:=n downto 1 do
  begin
      ii:=(i-1)*rwidth; l:=i-2; h:=0;
      if i=1 then f:=0 else f:=pa^[ii+i-1];
      for k:=1 to l do h:=h+sqr(pa^[ii+k]);
      if h <= tol then
        begin
          pcd^[i]:=f;
          for j:=1 to i-1 do pa^[ii+j]:=0;
        end else
        begin
          h:=h+f*f; l:=l+1;
          if f<0 then g:=sqrt(h) else g:=-sqrt(h);
          pcd^[i]:=g;
          h:=h-f*g; pa^[ii+i-1]:=f-g; f:=0;
          for j:=1 to l do
            begin
              g:=0;
              for k:=1 to j do g:=g+pa^[(j-1)*rwidth+k]*pa^[ii+k];
              for k:=j+1 to l do g:=g+pa^[(k-1)*rwidth+j]*pa^[ii+k];
              g:=g/h; pcd^[j]:=g; f:=f+g*pa^[ii+j]
            end; {j}
          h:=f/(h+h);
          for j:=1 to l do
            begin
              jj:=(j-1)*rwidth;
              f:=pa^[ii+j]; pcd^[j]:=pcd^[j]-h*f; g:=pcd^[j];
              for k:=1 to j do pa^[jj+k]:=pa^[jj+k]-f*pcd^[k]-g*pa^[ii+k]
            end {j}
        end;  {h > tol}
      h:=pd^[i]; pd^[i]:=pa^[ii+i]; pa^[ii+i]:=h
    end; {i}
  term:=1
end; {tred1}

procedure tred2(var a: ArbFloat; n, rwidtha: ArbInt; var d, cd, x: ArbFloat;
                  rwidthx: ArbInt; var term: ArbInt);

var i, j, k, l, ii, jj, kk : ArbInt;
         f , g, h, hh, tol : ArbFloat;
           pa, pd, pcd, px : ^arfloat1;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  tol:=midget/macheps;
  pa:=@a; pd:=@d; pcd:=@cd; px:=@x;
  for i:=1 to n do
    move(pa^[1+(i-1)*rwidtha], px^[1+(i-1)*rwidthx], i*sizeof(ArbFloat));
  for i:=n downto 2 do
    begin
      l:=i-2; ii:=(i-1)*rwidthx; f:=px^[i-1+ii];
      g:=0; for k:=1 to l do g:=g+sqr(px^[k+ii]);
      h:=g+f*f;
      if g<=tol then
        begin
          pcd^[i]:=f; pd^[i]:=0
        end else
        begin
          l:=l+1; if f<0 then g:=sqrt(h) else g:=-sqrt(h);
          pcd^[i]:=g;
          h:=h-f*g; px^[i-1+ii]:=f-g; f:=0;
          for j:=1 to l do
            begin
              jj:=(j-1)*rwidthx; px^[i+jj]:=px^[j+ii]/h;
              g:=0; for k:=1 to j do g:=g+px^[k+jj]*px^[k+ii];
              for k:=j+1 to l do g:=g+px^[j+(k-1)*rwidthx]*px^[k+ii];
              pcd^[j]:=g/h; f:=f+g*px^[i+jj]
            end;
          hh:=f/(h+h);
          for j:=1 to l do
            begin
              jj:=(j-1)*rwidthx; f:=px^[j+ii];
              pcd^[j]:=pcd^[j]-hh*f; g:=pcd^[j];
              for k:=1 to j do px^[k+jj]:=px^[k+jj]-f*pcd^[k]-g*px^[k+ii]
             end;
          pd^[i]:=h
        end
    end;
  pd^[1]:=0; pcd^[1]:=0;
  for i:=1 to n do
    begin
      ii:=(i-1)*rwidthx; l:=i-1;
      if pd^[i] <> 0 then
        for j:=1 to l do
          begin
            g:=0; for k:=1 to l do g:=g+px^[k+ii]*px^[j+(k-1)*rwidthx];
             for k:=1 to l do
               begin
                 kk:=(k-1)*rwidthx; px^[j+kk]:=px^[j+kk]-g*px^[i+kk]
               end
          end;
      pd^[i]:=px^[i+ii]; px^[i+ii]:=1;
      for j:=1 to l do
        begin
          px^[j+ii]:=0; px^[i+(j-1)*rwidthx]:=0
        end
    end;
  term:=1;
end {tred2};

procedure tql1(var d, cd: ArbFloat; n: ArbInt;
               var lam: ArbFloat; var term: ArbInt);

var                  i, j, l, m : ArbInt;
   meps, b, c, f, g, h, p, r, s : ArbFloat;
                    conv, shift : boolean;
                  pd, pcd, plam : ^arfloat1;

begin
  if n<1 then
    begin
      term:=3; exit
    end; {wrong input}
  pd:=@d; pcd:=@cd; plam:=@lam;
  conv:=true; meps:=macheps;
  for i:=2 to n do pcd^[i-1]:=pcd^[i];
  pcd^[n]:=0; f:=0; b:=0; l:=0;
  while (l<n) and conv do
    begin
      l:=l+1; j:=0; h:=meps*(abs(pd^[l])+abs(pcd^[l]));
      if b<h then b:=h;
      m:=l-1; repeat m:=m+1 until abs(pcd^[m]) <= b;
      while (abs(pcd^[l])>b) and conv do
        begin
          g:=pd^[l]; p:=(pd^[l+1]-g)/(2*pcd^[l]);
          if abs(p)>1 then r:=abs(p)*sqrt(1+sqr(1/p)) else r:=sqrt(sqr(p)+1);
          if p<0 then pd^[l]:=pcd^[l]/(p-r) else pd^[l]:=pcd^[l]/(p+r);
          h:=g-pd^[l];
          for i:=l+1 to n do pd^[i]:=pd^[i]-h;
          f:=f+h; p:=pd^[m]; c:=1; s:=0;
          for i:=m-1 downto l do
            begin
              g:=c*pcd^[i]; h:=c*p;
              if abs(p) >= abs(pcd^[i]) then
                begin
                  c:=pcd^[i]/p; r:=sqrt(c*c+1);
                  pcd^[i+1]:=s*p*r; s:=c/r; c:=1/r
                end
              else
                begin
                  c:=p/pcd^[i]; r:=sqrt(c*c+1);
                  pcd^[i+1]:=s*pcd^[i]*r; s:=1/r; c:=c/r
                end;
              p:=c*pd^[i]-s*g; pd^[i+1]:=h+s*(c*g+s*pd^[i])
            end; {i}
          pcd^[l]:=s*p; pd^[l]:=c*p; j:=j+1; conv:=j <= 30
        end; {while}
      if conv then
        begin
          p:=pd^[l]+f; i:=l; shift:=true;
          while shift and (i>1) do
            begin
              if p>=plam^[i-1] then shift:= false else plam^[i]:=plam^[i-1];
              i:=i-1
            end; {while}
          if not shift then plam^[i+1]:=p else plam^[i]:=p
        end  {if conv}
    end; {l}
  if conv then term:=1 else term:=2
end; {tql1}

procedure tql2(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat;
               rwidth: ArbInt; var term: ArbInt);
var i, j, k, l, m, kk, ki, ki1, jj, ji, jk, sr, ns, n1s : ArbInt;
                                                   conv : boolean;
                           meps, b, c, f, g, h, p, r, s : ArbFloat;
                            pd, pcd, plam, px, c1d, ccd : ^arfloat1;
begin
  if n<1 then
    begin
      term:=3; exit
    end;
  sr:=sizeof(ArbFloat); ns:=n*sizeof(ArbFloat); n1s:=ns-sr;
  getmem(c1d, ns); getmem(ccd, ns);
  pd:=@d; pcd:=@cd; plam:=@lam; px:=@x;
  move(pcd^[2], ccd^[1], n1s); ccd^[n]:=0; move(pd^[1], c1d^[1], ns);
  conv:= true; meps:=macheps; f:=0; b:=0; l:=0;
  while (l<n) and conv do
    begin
      l:=l+1; j:=0; h:=meps*(abs(c1d^[l])+abs(ccd^[l]));
      if b<h then b:=h;
      m:=l; while abs(ccd^[m])>b do m:=m+1;
      while (abs(ccd^[l])>b) and conv do
        begin
          g:=c1d^[l]; p:=(c1d^[l+1]-g)/(2*ccd^[l]);
          if abs(p)>1
          then r:=abs(p)*sqrt(1+sqr(1/p)) else r:=sqrt(sqr(p)+1);
          if p<0 then c1d^[l]:=ccd^[l]/(p-r) else c1d^[l]:=ccd^[l]/(p+r);
          h:=g-c1d^[l];
          for i:=l+1 to n do c1d^[i]:=c1d^[i]-h;
          f:=f+h; p:=c1d^[m]; c:=1; s:=0;
          for i:=m-1 downto l do
            begin
              g:=c*ccd^[i]; h:=c*p;
              if abs(p)>=abs(ccd^[i]) then
                 begin
                   c:=ccd^[i]/p; r:=sqrt(c*c+1);
                   ccd^[i+1]:=s*p*r; s:=c/r; c:=1/r
                 end else
                begin
                  c:=p/ccd^[i]; r:=sqrt(c*c+1);
                  ccd^[i+1]:=s*ccd^[i]*r; s:=1/r; c:=c/r
                end;
                p:=c*c1d^[i]-s*g; c1d^[i+1]:=h+s*(c*g+s*c1d^[i]);
                for k:=1 to n do
                  begin
                    kk:=(k-1)*rwidth; ki:=i+kk; ki1:=ki+1;
                    h:=px^[ki1]; px^[ki1]:=s*px^[ki]+c*h;
                    px^[ki]:=c*px^[ki]-s*h
                  end
              end;
            ccd^[l]:=s*p; c1d^[l]:=c*p; j:=j+1; conv:=j<=30
        end;
      if conv
      then
        plam^[l]:=c1d^[l]+f
    end;
  if conv then
    for i:=1 to n do
      begin
        k:=i; p:=plam^[i];
        for j:=i+1 to n do
          if plam^[j]<p then
            begin
              k:=j; p:=plam^[j]
            end;
          if k <> i then
            begin
              plam^[k]:=plam^[i]; plam^[i]:=p;
              for j:=1 to n do
                begin
                  jj:=(j-1)*rwidth; ji:=i+jj; jk:=k+jj;
                  p:=px^[ji]; px^[ji]:=px^[jk]; px^[jk]:=p
                end
            end
      end;
  if conv then term:=1 else term:=2;
  freemem(c1d, ns); freemem(ccd, ns)
end; {tql2}

procedure bisect(var d, cd: ArbFloat; n, k1, k2: ArbInt; eps: ArbFloat;
                 var lam: ArbFloat; var term: ArbInt);

var                  a, i, k, sr : ArbInt;
    pd, pcd, plam, codsq, xlower : ^arfloat1;
      meps, eps1, q, xmin, xmax,
       xl, xu, lambdak, h, diagi : ArbFloat;

begin
  if (n<1) or (k1<1) or (k2<k1) or (k2>n) then
    begin
      term:=3; exit
    end; {wrong input}
  term:=1;
  pd:=@d; pcd:=@cd; plam:=@lam;
  sr:=sizeof(ArbFloat);
  getmem(codsq, n*sr); getmem(xlower, n*sr);
  meps:=macheps;
  for i:=2 to n do codsq^[i]:=sqr(pcd^[i]);
  xmin:=pd^[n]; xmax:=xmin;
  if n > 1 then
    begin
      h:=abs(pcd^[n]); xmin:=xmin-h; xmax:=xmax+h
    end ;
  for i:=n-1 downto 1 do
    begin
      h:=abs(pcd^[i+1]);
      if i<>1 then h:=h+abs(pcd^[i]);
      diagi:=pd^[i];
      if diagi-h<xmin then xmin:=diagi-h;
      if diagi+h>xmax then xmax:=diagi+h
    end; {i}
  if xmin+xmax>0 then eps1:=meps*xmax
  else eps1:=-meps*xmin;
  if eps <= 0 then eps:=eps1;
  for i:=k1 to k2 do
    begin
      plam^[i-k1+1]:=xmax; xlower^[i]:=xmin
    end;
  xu:=xmax;
  for k:=k2 downto k1 do
    begin
      if xu>plam^[k-k1+1] then xu:=plam^[k-k1+1];
      i:=k; repeat xl:=xlower^[i]; i:=i-1 until (i<k1) or (xl>xmin);
      while xu-xl>2*meps*(abs(xl)+abs(xu))+eps do
        begin
          lambdak:=xl+(xu-xl)/2; q:=pd^[1]-lambdak;
          if q<0 then a:=1 else a:=0;
          for i:=2 to n do
            begin
              if q=0 then q:=meps;
              q:=pd^[i]-lambdak-codsq^[i]/q;
              if q<0 then a:=a+1
            end; {i}
          if a<k then
            begin
              if a<k1 then
                begin
                  xl:=lambdak; xlower^[k]:=lambdak
                end else
                begin
                  xl:=lambdak; xlower^[a+1]:=lambdak;
                  if plam^[a-k1+1]>lambdak then plam^[a-k1+1]:=lambdak
                end
            end else xu:=lambdak
        end; {while}
      plam^[k-k1+1]:=xl+(xu-xl)/2
    end;  {k}
  freemem(codsq, n*sr); freemem(xlower, n*sr)
end; {bisect}

procedure trbak1(var a: ArbFloat; n, rwidtha: ArbInt; var cd: ArbFloat;
                 k1, k2: ArbInt; var x: ArbFloat; rwidthx: ArbInt);

var  i, j, k, l, ii, ind : ArbInt;
                    h, s : ArbFloat;
             pa, px, pcd : ^arfloat1;
begin
  pa:=@a; px:=@x; pcd:=@cd;
  for i:=3 to n do
    begin
      ii:=(i-1)*rwidtha;
      l:=i-1; h:=pcd^[i]*pa^[ii+i-1];
      if h <> 0 then
      for j:=1 to k2-k1+1 do
        begin
          s:=0; for k:=1 to l do s:=s+pa^[ii+k]*px^[(k-1)*rwidthx+j]; s:=s/h;
          for k:=1 to l do
            begin
              ind:=(k-1)*rwidthx+j; px^[ind]:=px^[ind]+s*pa^[ii+k]
            end; {k}
        end  {j}
    end  {i}
end;  {trbak1}

procedure trsturm1(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat;
                   var x: ArbFloat; rwidth: ArbInt; var m2, term: ArbInt);

var
                     ns, nb, a, i, k, s, its, group, j : ArbInt;
                                      continu, nonfail : boolean;
       eps1, eps2, eps3, eps4, q,  xmin, xmax, xl, xu,
  x1, x0, u, v, norm, meps, lambdak, h, diagi, codiagi : ArbFloat;
      codsq, d1, e, f, y, z, xlower, pd, pcd, plam, px : ^arfloat1;
                                                   int : ^arbool1;
begin
  if (n<1) or (k1<1) or (k1>k2) or (k2>n) then
    begin
      term:=3; exit
    end; {wrong input}
  pd:=@d; pcd:=@cd; plam:=@lam; px:=@x;
  ns:=n*sizeof(ArbFloat); nb:=n*sizeof(boolean);
  getmem(codsq, ns); getmem(d1, ns); getmem(e, ns); getmem(f, ns);
  getmem(y, ns); getmem(z, ns); getmem(xlower, ns); getmem(int, nb);
  meps:=macheps;
  norm:=abs(pd^[1]);
  for i:=2 to n do norm:=norm+abs(pd^[i])+abs(pcd^[i]);
  if norm=0 then
    begin
  { matrix is nulmatrix: eigenwaarden zijn alle 0 en aan de
    eigenvectoren worden de eenheidsvectoren e(k1) t/m e(k2) toegekend }
      for k:=k1 to k2 do plam^[k-k1+1]:=0;
      for i:=1 to n do
        fillchar(px^[(i-1)*rwidth+1], (k2-k1+1)*sizeof(ArbFloat), 0);
      for k:=k1 to k2 do px^[(k-1)*rwidth+k-k1+1]:=1;
      m2:=k2; term:=1;
      freemem(codsq, ns); freemem(d1, ns); freemem(e, ns); freemem(f, ns);
      freemem(y, ns); freemem(z, ns); freemem(xlower, ns); freemem(int, nb);
      exit
    end; {norm=0}
  for i:=2 to n do codsq^[i]:=sqr(pcd^[i]);
  xmin:=pd^[n]; xmax:=xmin;
  if n>1 then
    begin
      h:=abs(pcd^[n]); xmin:=xmin-h; xmax:=xmax+h
    end;
  for i:=n-1 downto 1 do
    begin
      diagi:=pd^[i];
      h:=abs(pcd^[i+1]);
      if i<>1 then h:=h+abs(pcd^[i]);
      if diagi-h<xmin then xmin:=diagi-h;
      if diagi+h>xmax then xmax:=diagi+h;
    end; {i}
  if xmax+xmin>0 then eps1:=meps*xmax else eps1:=-meps*xmin;
  for i:=k1 to k2 do
    begin
      plam^[i-k1+1]:=xmax; xlower^[i]:=xmin
    end;
  xu:=xmax;
  for k:=k2 downto k1 do
    begin
      if xu>plam^[k-k1+1] then xu:=plam^[k-k1+1];
      i:=k; repeat xl:=xlower^[i]; i:=i-1 until (i<k1) or (xl>xmin);
      while xu-xl>2*eps1 do
        begin
          lambdak:=xl+(xu-xl)/2; q:=pd^[1]-lambdak;
          if q<0 then a:=1 else a:=0;
          for i:=2 to n do
            begin
              if q=0 then q:=meps;
              q:=pd^[i]-lambdak-codsq^[i]/q;
              if q<0 then a:=a+1;
            end; {i}
          if a<k then
            begin
              if a<k1 then
                begin
                  xl:=lambdak; xlower^[k]:=lambdak
                end else
                begin
                  xlower^[a+1]:=lambdak; xl:=lambdak;
                  if plam^[a-k1+1]>lambdak then plam^[a-k1+1]:=lambdak
                end
            end else xu:=lambdak
        end;  {while}
      plam^[k-k1+1]:=xl+(xu-xl)/2;
    end; {k}
  eps2:=norm*1e-3; eps3:=meps*norm; eps4:=eps3*n;
  group:=0; s:=1; k:=k1; nonfail:=true; m2:=k1-1;
  while (k <= k2) and nonfail do
    begin
      x1:=plam^[k-k1+1];
      if k <> k1 then
        begin
          if x1-x0<eps2 then group:=group+1 else group:=0;
          if x1 <= x0 then x1:=x0+eps3
        end; {k <> k1}
      u:=eps4/sqrt(n);
      for i:=1 to n do z^[i]:=u;
      u:=pd^[1]-x1;
      if n=1 then v:=0 else v:=pcd^[2];
      for i:=2 to n do
        begin
          if pcd^[i]=0 then codiagi:=eps3 else codiagi:=pcd^[i];
          if abs(codiagi) >= abs(u) then
            begin
              xu:=u/codiagi; y^[i]:=xu; d1^[i-1]:=codiagi;
              e^[i-1]:=pd^[i]-x1;
              if i=n then f^[i-1]:=0 else f^[i-1]:=pcd^[i+1];
              u:=v-xu*e^[i-1]; v:=-xu*f^[i-1];
              int^[i]:=true
            end else
            begin
              xu:=codiagi/u; y^[i]:=xu; d1^[i-1]:=u; e^[i-1]:=v;
              f^[i-1]:=0; u:=pd^[i]-x1-xu*v;
              if i<n then v:=pcd^[i+1];
              int^[i]:=false
            end
        end;  {i}
      if u=0 then d1^[n]:=eps3 else d1^[n]:=u;
      e^[n]:=0; f^[n]:=0;
      its:=1; continu:=true;
      while continu and nonfail do
        begin
          for i:=n downto 1 do
            begin
              z^[i]:=(z^[i]-u*e^[i]-v*f^[i])/d1^[i]; v:=u; u:=z^[i]
            end;
          for j:=m2-group+1 to m2 do
            begin
              xu:=0;
              for i:=1 to n do xu:=xu+z^[i]*px^[(i-1)*rwidth+j-k1+1];
              for i:=1 to n do z^[i]:=z^[i]-xu*px^[(i-1)*rwidth+j-k1+1]
            end; {j}
          norm:=0; for i:=1 to n do norm:=norm+abs(z^[i]);
          if norm<1 then
            begin
              if norm=0 then
                begin
                  z^[s]:=eps4;
                  if s<n then s:=s+1 else s:=1
                end else
                begin
                  xu:=eps4/norm;
                  for i:=1 to n do z^[i]:=z^[i]*xu
                end;
              for i:=2 to n do
                if int^[i] then
                  begin
                    u:=z^[i-1]; z^[i-1]:=z^[i]; z^[i]:=u-y^[i]*z^[i]
                  end else z^[i]:=z^[i]-y^[i]*z^[i-1];
              its:=its+1; if its=5 then nonfail:=false;
            end {norm < 1}
          else continu:=false
        end; {while continu ^ nonfail}
      if nonfail then
        begin
          u:=0; for i:=1 to n do u:=u+sqr(z^[i]);
          xu:=1/sqrt(u); m2:=m2+1;
          for i:=1 to n do px^[(i-1)*rwidth+m2-k1+1]:=z^[i]*xu;
          x0:=x1; k:=k+1;
        end
    end;  {k}
  if m2=k2 then term:=1 else term:=2;
  freemem(codsq, ns); freemem(d1, ns); freemem(e, ns); freemem(f, ns);
  freemem(y, ns); freemem(z, ns); freemem(xlower, ns); freemem(int, nb);
end  {trsturm1};

procedure transf(var a: ArbFloat; n, l: ArbInt; var b: ArbFloat; rwidthb: ArbInt);

{ a bevat de linksonder bandelementen van een symmetrische matrix A met
 lengte n en bandbreedte l, rijsgewijs aaneengesloten.
 na afloop bevatten de kolommen van b de diagonalen van A, met dien
 vestande dat de hoofddiagonaal van A in de eerste kolom van b staat,
 de een na langste codiagonaal in de tweede kolom
 (behalve de onderste plaats) enzovoort.
 De niet opgevulde plaatsen komen in b dus rechtsonder te staan. }

var             pa, pb: ^arfloat1;
     ii, jj, i, j, rwa: ArbInt;
begin
  pa:=@a; pb:=@b;
  ii:=1; jj:=0;
  for i:=1 to n do
  begin
    if i>l then rwa:=l+1 else rwa:=i;
    if i>l+1 then jj:=jj+rwidthb else jj:=jj+1;
    for j:=1 to rwa do pb^[jj+(j-1)*(rwidthb-1)]:=pa^[ii+j-1];
    ii:=ii+rwa;
  end
end;

procedure banddek(n, m1, m2: ArbInt; var au, l: ArbFloat; var p: ArbInt);
var                      pa, pl, norm: ^arfloat1;
                                   pp: ^arint1;
    i, j, ll, ii, k, t, pk, ind, ind1: ArbInt;
                   piv, c, x, maxnorm: ArbFloat;
begin
   pa:=@au; pl:=@l; pp:=@p;
   getmem(norm, n*sizeof(ArbFloat));
   t:=m1; ll:=m1+m2+1;
   for i:=1 to m1 do
   begin
     ind:=(i-1)*ll;
     for j:=m1+1-i to ll do pa^[ind+j-t]:=pa^[ind+j];
     t:=t-1;
     for j:=ll-t to ll do pa^[ind+j]:=0
   end;
   t:=1;
   for i:=n downto n-m2+1 do
   begin
     ind:=(i-1)*ll;
     for j:=t+m1+1 to ll do pa^[ind+j]:=0;
     t:=t+1
   end;
   maxnorm:=0;
   for k:=1 to n do
   begin
     c:=0; ind:=(k-1)*ll;
     for j:=1 to ll do c:=c+abs(pa^[ind+j]);
     if c>maxnorm then maxnorm:=c;
     if c=0 then norm^[k]:=1 else norm^[k]:=c
   end;
   t:=m1;
   for k:=1 to n do
   begin
     ind:=(k-1)*ll;
     x:=abs(pa^[ind+1])/norm^[k]; pk:=k;
     t:=t+1; if t>n then t:=n;
     for i:=k+1 to t do
     begin
       c:=abs(pa^[(i-1)*ll+1])/norm^[i];
       if c>x then
       begin
         x:=c; pk:=i
       end
     end;
     ind1:=(pk-1)*ll;
     pp^[k]:=pk;
     if pk <> k then
     begin
       for j:=1 to ll do
       begin
         c:=pa^[ind+j]; pa^[ind+j]:=pa^[ind1+j]; pa^[ind1+j]:=c
       end;
       norm^[pk]:=norm^[k]
     end;
     piv:=pa^[ind+1];
     if piv <> 0 then
     begin
       for i:=k+1 to t do
       begin
         ii:=(i-1)*ll;
         c:=pa^[ii+1]/piv; pl^[(k-1)*m1+i-k]:=c;
         for j:=2 to ll do pa^[ii+j-1]:=pa^[ii+j]-c*pa^[ind+j];
         pa^[ii+ll]:=0
       end
     end else
     begin
       pa^[ind+1]:=macheps*maxnorm;
       for i:=k+1 to t do
       begin
         ii:=(i-1)*ll;
         pl^[(k-1)*m1+i-k]:=0;
         for j:=2 to ll do pa^[ii+j-1]:=pa^[ii+j];
         pa^[ii+ll]:=0
       end {i}
     end {piv=0}
   end; {k}
  freemem(norm, n*sizeof(ArbFloat))
end; {banddek}

procedure bandsol(n, m1, m2: ArbInt; var au, l: ArbFloat;
                  var p: ArbInt; var b: ArbFloat);
var          pa, pl, pb: ^arfloat1;
                     pp: ^arint1;
  ll, i, j, k, pk, t, w: ArbInt;
                      x: ArbFloat;
begin
  pa:=@au; pl:=@l; pb:=@b; pp:=@p;
  for k:=1 to n do
  begin
    pk:=pp^[k];
    if pk <> k then
    begin
      x:=pb^[k]; pb^[k]:=pb^[pk]; pb^[pk]:=x
    end;
    t:=k+m1; if t>n then t:=n;
    for i:=k+1 to t do pb^[i]:=pb^[i]-pl^[(k-1)*m1+i-k]*pb^[k]
  end; {k}
  t:=1; ll:=m1+m2+1;
  for i:=n downto 1 do
  begin
    x:=pb^[i]; w:=i-1;
    for j:=2 to t do x:=x-pa^[(i-1)*ll+j]*pb^[j+w];
    pb^[i]:=x/pa^[(i-1)*ll+1];
    if t<ll then t:=t+1
  end {i}
end; {bandsol}

procedure bandrd1(var a: ArbFloat; n, m, rwidth: ArbInt; var d, cd: ArbFloat);

{ wilkinson linear algebra ii/8 procedure bandrd; matv = false }

var      j, k, l, r, maxr, maxl, ugl, ikr, jj, jj1, i, ll : ArbInt;
                            b, c, s, s2, c2, cs, u, u1, g : ArbFloat;
                                              pa, pd, pcd : ^arfloat1;
begin
  pa:=@a; pd:=@d; pcd:=@cd;
  for k:=1 to n-2 do
    begin
      if n-k<m then maxr:=n-k else maxr:=m;
      for r:=maxr downto 2 do
        begin
          ikr:=(k-1)*rwidth+r+1; g:=pa^[ikr]; j:=k+r;
          while (g <> 0) and (j <= n) do
            begin
              if j=k+r then
                begin
                  b:=-pa^[ikr-1]/pa^[ikr]; ugl:=k
                end else
                begin
                  b:=-pa^[(j-m-2)*rwidth+m+1]/g; ugl:=j-m
                end;
              s:=1/sqrt(1+b*b); c:=b*s; c2:=c*c; s2:=s*s; cs:=c*s;
              jj:=(j-1)*rwidth+1; jj1:=jj-rwidth;
              u:=c2*pa^[jj1]-2*cs*pa^[jj1+1]+s2*pa^[jj];
              u1:=s2*pa^[jj1]+2*cs*pa^[jj1+1]+c2*pa^[jj];
              pa^[jj1+1]:=cs*(pa^[jj1]-pa^[jj])+(c2-s2)*pa^[jj1+1];
              pa^[jj1]:=u; pa^[jj]:=u1;
              for l:=ugl to j-2 do
                begin
                  ll:=(l-1)*rwidth+j-l+1;
                  u:=c*pa^[ll-1]-s*pa^[ll];
                  pa^[ll]:=s*pa^[ll-1]+c*pa^[ll];
                  pa^[ll-1]:=u;
                end; {l}
              if j <> k+r then
                begin
                  i:=(j-m-2)*rwidth+m+1; pa^[i]:=c*pa^[i]-s*g
                end;
              if n-j < m-1 then maxl:=n-j else maxl:=m-1;
              for l:=1 to maxl do
                begin
                  u:=c*pa^[jj1+l+1]-s*pa^[jj+l];
                  pa^[jj+l]:=s*pa^[jj1+l+1]+c*pa^[jj+l];
                  pa^[jj1+l+1]:=u
                end; {l}
              if j+m <= n then
                begin
                  g:=-s*pa^[jj+m]; pa^[jj+m]:=c*pa^[jj+m]
                end;
              j:=j+m;
            end {j}
        end {r}
    end; {k}
  pd^[1]:=pa^[1]; pcd^[1]:=0;
  for j:=2 to n do
    begin
      pd^[j]:=pa^[(j-1)*rwidth+1];
      if m>0 then pcd^[j]:=pa^[(j-2)*rwidth+2] else pcd^[j]:=0
    end {j}
end; {bandrd1}

procedure bandrd2(var a: ArbFloat; n, m, rwidtha: ArbInt; var d, cd, x: ArbFloat;
                  rwidthx: ArbInt);

{ wilkinson linear algebra ii/8 procedure bandrd; matv = true }

var      j, k, l, r, maxr, maxl, ugl, ikr, jj, jj1, i, ll, ns : ArbInt;
                                b, c, s, s2, c2, cs, u, u1, g : ArbFloat;
                                              pa, pd, pcd, px : ^arfloat1;
begin
  pa:=@a; pd:=@d; pcd:=@cd; px:=@x; ns:=n*sizeof(ArbFloat);
  for j:=1 to n do fillchar(px^[(j-1)*rwidthx+1], ns, 0);
  for j:=1 to n do px^[(j-1)*rwidthx+j]:=1;
  for k:=1 to n-2 do
    begin
      if n-k<m then maxr:=n-k else maxr:=m;
      for r:=maxr downto 2 do
        begin
          ikr:=(k-1)*rwidtha+r+1; g:=pa^[ikr]; j:=k+r;
          while (g <> 0) and (j <= n) do
            begin
              if j=k+r then
                begin
                  b:=-pa^[ikr-1]/pa^[ikr]; ugl:=k
                end else
                begin
                  b:=-pa^[(j-m-2)*rwidtha+m+1]/g; ugl:=j-m
                end;
              s:=1/sqrt(1+b*b); c:=b*s; c2:=c*c; s2:=s*s; cs:=c*s;
              jj:=(j-1)*rwidtha+1; jj1:=jj-rwidtha;
              u:=c2*pa^[jj1]-2*cs*pa^[jj1+1]+s2*pa^[jj];
              u1:=s2*pa^[jj1]+2*cs*pa^[jj1+1]+c2*pa^[jj];
              pa^[jj1+1]:=cs*(pa^[jj1]-pa^[jj])+(c2-s2)*pa^[jj1+1];
              pa^[jj1]:=u; pa^[jj]:=u1;
              for l:=ugl to j-2 do
                begin
                  ll:=(l-1)*rwidtha+j-l+1; u:=c*pa^[ll-1]-s*pa^[ll];
                  pa^[ll]:=s*pa^[ll-1]+c*pa^[ll]; pa^[ll-1]:=u;
                end; {l}
              if j <> k+r then
                begin
                  i:=(j-m-2)*rwidtha+m+1; pa^[i]:=c*pa^[i]-s*g
                end;
              if n-j < m-1 then maxl:=n-j else maxl:=m-1;
              for l:=1 to maxl do
                begin
                  u:=c*pa^[jj1+l+1]-s*pa^[jj+l];
                  pa^[jj+l]:=s*pa^[jj1+l+1]+c*pa^[jj+l];
                  pa^[jj1+l+1]:=u
                end; {l}
              if j+m <= n then
                begin
                  g:=-s*pa^[jj+m]; pa^[jj+m]:=c*pa^[jj+m]
                end;
              for l:=1 to n do
                begin
                  ll:=(l-1)*rwidthx+j; u:=c*px^[ll-1]-s*px^[ll];
                  px^[ll]:=s*px^[ll-1]+c*px^[ll]; px^[ll-1]:=u
                end; {ll}
              j:=j+m;
            end {j}
        end {r}
    end; {k}
  pd^[1]:=pa^[1]; pcd^[1]:=0;
  for j:=2 to n do
    begin
      pd^[j]:=pa^[(j-1)*rwidtha+1];
      if m>0 then pcd^[j]:=pa^[(j-2)*rwidtha+2] else pcd^[j]:=0
    end {j}
end; {bandrd2}

procedure bandev(var a: ArbFloat; n, m, rwidth: ArbInt; lambda: ArbFloat;
                 var v: ArbFloat; var term: ArbInt);

var                              pa, pv, au, l, u : ^arfloat1;
                                                p : ^arint1;
          ind, ii, i, k, t, j, its, w, sr, ns, ll : ArbInt;
                 meps, eps, s, norm, x, y, r1, d1 : ArbFloat;
begin
  pa:=@a; pv:=@v;
  sr:=sizeof(ArbFloat); ns:=n*sr; ll:=2*m+1;
  getmem(au, ll*ns); getmem(l, m*ns); getmem(u, ns);
  getmem(p, n*sizeof(ArbInt));
  norm:=0; meps:=macheps;
  for i:=1 to n do
    begin
      s:=0; if i-m<1 then k:=i-1 else k:=m; ii:=(i-1)*rwidth;
      if n-i<m then w:=n-i+1 else w:=m+1;
      for j:=1 to w do s:=s+abs(pa^[ii+j]);
      for j:=1 to k do s:=s+abs(pa^[(i-j-1)*rwidth+j+1]);
      if s>norm then norm:=s
    end; {norm}
  eps:=norm*meps;
  if eps<midget then
    begin
      pv^[1]:=1;
      for j:=2 to n do pv^[j]:=0;
      term:=1;
      freemem(au, ll*ns); freemem(l, m*ns); freemem(u, ns);
      freemem(p, n*sizeof(ArbInt));
      exit
    end; {eps<midget}
  for i:=1 to n do
    begin
      if n-i<m then w:=n-i else w:=m;
      ind:=(i-1)*ll; ii:=(i-1)*rwidth;
      move(pa^[ii+2], au^[ind+m+2], w*sr);
      fillchar(au^[ind+m+w+2], (m-w)*sr, 0);
      if i-1<m then w:=i-1 else w:=m;
      for j:=1 to w do au^[ind+m-j+1]:=pa^[(i-j-1)*rwidth+j+1];
      fillchar(au^[ind+1], (m-w)*sr, 0);
      au^[ind+m+1]:=pa^[ii+1]-lambda
    end; {i}
  banddek(n, m, m, au^[1], l^[1], p^[1]);
  t:=-m;
  for i:=n downto 1 do
    begin
      ind:=(i-1)*ll;
      x:=1; w:=i+m;
      for j:=1-m to t do x:=x-au^[ind+m+j+1]*pv^[j+w];
      pv^[i]:=x/au^[ind+1];
      if t<m then t:=t+1
    end; {i}
  x:=0;
  for i:=1 to n do
    if abs(pv^[i])>abs(x) then
      begin
        x:=pv^[i]; j:=i
      end;
  its:=0; x:=1/x;
  for i:=1 to n do
    begin
      u^[i]:=x*pv^[i]; pv^[i]:=u^[i]
    end; {i}
  repeat
    bandsol(n, m, m, au^[1], l^[1], p^[1], pv^[1]);
    y:=1/pv^[j]; x:=0;
    for i:=1 to n do
      if abs(pv^[i])>abs(x) then
        begin
          x:=pv^[i]; j:=i
        end; {i}
    x:=1/x; d1:=0;
    for i:=1 to n do
      begin
        r1:=abs((u^[i]-y*pv^[i])*x);
        if r1>d1 then d1:=r1; u^[i]:=x*pv^[i]; pv^[i]:=u^[i]
      end; {i}
    its:=its+1
  until (d1<=eps) or (its>5);
  if d1<=eps then
    begin
      term:=1; x:=0; for i:=1 to n do x:=x+sqr(pv^[i]); x:=1/sqrt(x);
      for i:=1 to n do pv^[i]:=pv^[i]*x;
    end else term:=2;
  freemem(au, ll*ns); freemem(l, m*ns); freemem(u, ns);
  freemem(p, n*sizeof(ArbInt));
end; {bandev}
end.
