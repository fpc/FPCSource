{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             documentation by Michael van Canneyt (Michael@freepascal.org)

    Calculate inverses for different kinds of matrices (different with respect
                 to symmetry)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit inv;
{$I DIRECT.INC}

interface

uses typ;

{Calc inverse for a matrix with unknown symmetry. General version. }
procedure invgen(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);

{Calc inverse for a symmetrical matrix}
procedure invgsy(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);

{Calc inverse for a positive definite symmetrical matrix}
procedure invgpd(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);

implementation

uses mdt, dsl;

procedure invgen(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);
var
     success                          : boolean;
     inn, ii, i, j, k, kk, indexpivot : ArbInt;
     ca, h, pivot, s                  : ArbFloat;
     pa, save                         : ^arfloat1;
     p                                : ^arint1;

begin
  if (n<1) or (rwidth<1) then
  begin
      term:=3; exit
  end; {wrong input}
  pa:=@ai;
  getmem(p, n*sizeof(ArbInt)); getmem(save, n*sizeof(ArbFloat));
  mdtgen(n, rwidth, pa^[1], p^[1], ca, term);
  if term=1 then
  begin
      inn:=(n-1)*rwidth+n; pivot:=pa^[inn];
      if pivot=0 then success:=false else
      begin
          success:=true; pa^[inn]:=1/pivot; k:=n;
          while (k>1) and success do
          begin
              k:=k-1; kk:=(k-1)*rwidth;
              for i:=k+1 to n do save^[i]:=-pa^[(i-1)*rwidth+k];
              for i:=k+1 to n do
              begin
                  ii:=(i-1)*rwidth;
                  s:=0;
                  for j:=k+1 to n do s:=s+pa^[ii+j]*save^[j];
                  pa^[ii+k]:=s
              end; {i}
              for j:=k+1 to n do save^[j]:=pa^[kk+j];
              pivot:=pa^[kk+k];
              if pivot=0 then success:=false else
              begin
                  s:=0;
                  for i:=k+1 to n do s:=s-save^[i]*pa^[(i-1)*rwidth+k];
                  pa^[kk+k]:=(1+s)/pivot;
                  for j:=k+1 to n do
                  begin
                      s:=0;
                      for i:=k+1 to n do s:=s-save^[i]*pa^[(i-1)*rwidth+j];
                      pa^[(k-1)*rwidth+j]:=s/pivot
                  end {j}
              end {pivot <> 0}
          end; {k}
          if success then
          for k:=n downto 1 do
          begin
              indexpivot:=p^[k];
              if indexpivot <> k then
              for i:=1 to n do
              begin
                  ii:=(i-1)*rwidth;
                  h:=pa^[ii+k]; pa^[ii+k]:=pa^[ii+indexpivot];
                  pa^[ii+indexpivot]:=h
              end {i}
          end {k}
      end; {pivot <> 0}
      if (not success) then term:=2
  end else term:=2;
  freemem(p, n*sizeof(ArbInt)); freemem(save, n*sizeof(ArbFloat));
end; {invgen}

procedure invgsy(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);

var ind, ind1, i, m, pk, j,
    kmin1, k, imin2, nsr,
    imin1, jmin1, iplus1            : ArbInt;
    success                         : boolean;
    di, h, ca                       : ArbFloat;
    pa, l, d, u, v, e, e1, x        : ^arfloat1;
    p                               : ^arint1;
    q                               : ^arbool1;
begin
  if (n<1) or (rwidth<1) then
  begin
      term:=3; exit
  end; {wrong input}
  pa:=@ai;
  getmem(p, n*sizeof(ArbInt)); getmem(q, n*sizeof(boolean));
  nsr:=n*sizeof(ArbFloat);
  getmem(l, nsr); getmem(d, nsr); getmem(u, nsr);
  getmem(v, nsr); getmem(e, nsr); getmem(e1, nsr);
  getmem(x, ((n+1)*nsr) div 2);
  mdtgsy(n, rwidth, pa^[1], p^[1], q^[1], ca, term);
  if term=1 then
  begin
      success:=true; i:=1; ind:=1;
      while (i<>n+1) and success do
      begin
          success:=pa^[ind]<>0; ind:=ind+rwidth+1; i:=i+1
      end; {i}
      if success then
      begin
          d^[1]:=pa^[1]; di:=d^[1]; l^[1]:=pa^[rwidth+1];
          d^[2]:=pa^[rwidth+2]; di:=d^[2]; u^[1]:=pa^[2];
          imin1:=1; i:=2;
          while i<n do
          begin
              imin2:=imin1; imin1:=i; i:=i+1; ind:=imin1*rwidth;
              l^[imin1]:=pa^[ind+imin1]; d^[i]:=pa^[ind+i]; di:=d^[i];
              u^[imin1]:=pa^[ind-rwidth+i]; v^[imin2]:=pa^[ind-2*rwidth+i]
          end; {i}
          m:=0; k:=0;
          while k<n do
          begin
              kmin1:=k; k:=k+1;
              for i:=1 to kmin1 do e^[i]:=0;
              e^[k]:=1; i:=k;
              while i<n do
              begin
                  imin1:=i; i:=i+1; h:=0;
                  if k=1 then j:=1 else j:=kmin1;
                  while j<imin1 do
                  begin
                      jmin1:=j; j:=j+1;
                      h:=h-pa^[(i-1)*rwidth+jmin1]*e^[j]
                  end; {j}
                  e^[i]:=h
              end; {i}
              dslgtr(n, l^[1], d^[1], u^[1], v^[1], q^[1],
                     e^[1], e1^[1], term);
              i:=n+1; imin1:=n;
              while i>2 do
              begin
                  iplus1:=i; i:=imin1; imin1:=imin1-1; h:=e1^[i];
                  for j:=iplus1 to n do
                    h:=h-pa^[(j-1)*rwidth+imin1]*e1^[j];
                  e1^[i]:=h
              end; {i}
              for i:=k to n do
              begin
                  m:=m+1; x^[m]:=e1^[i]
              end
          end; {k}
          m:=0;
          for k:=1 to n do for i:=k to n do
          begin
              m:=m+1; pa^[(i-1)*rwidth+k]:=x^[m]
          end; {i,k}
          for k:=n-1 downto 2 do
          begin
              pk:=p^[k];
              if pk <> k then
              begin
                  kmin1:=k-1; ind:=(k-1)*rwidth; ind1:=(pk-1)*rwidth;
                  for j:=1 to kmin1 do
                  begin
                      h:=pa^[ind+j];
                      pa^[ind+j]:=pa^[ind1+j]; pa^[ind1+j]:=h
                  end; {j}
                  for j:=pk downto k do
                  begin
                      ind:=(j-1)*rwidth;
                      h:=pa^[ind+k];
                      pa^[ind+k]:=pa^[ind1+j]; pa^[ind1+j]:=h
                  end; {j}
                  for i:=pk to n do
                  begin
                      ind:=(i-1)*rwidth;
                      h:=pa^[ind+k];
                      pa^[ind+k]:=pa^[ind+pk]; pa^[ind+pk]:=h
                  end {i}
              end {pk <> k}
          end {k}
      end; {success}
      if (not success) then term:=2 else
      for i:=1 to n do
      begin
          ind:=(i-1)*rwidth;
          for j:=i+1 to n do pa^[ind+j]:=pa^[(j-1)*rwidth+i]
      end {i}
  end else term:=2;
  freemem(l, nsr); freemem(d, nsr); freemem(u, nsr);
  freemem(v, nsr); freemem(e, nsr); freemem(e1, nsr);
  freemem(x, ((n+1)*nsr) div 2);
end; {invgsy}

procedure invgpd(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);
var success      : boolean;
    i, j, k, ind : ArbInt;
    tk, h, ca    : ArbFloat;
    pa, t        : ^arfloat1;
begin
  if (n<1) or (rwidth<1) then
  begin
      term:=3; exit
  end; {wrong input}
  pa:=@ai;
  mdtgpd(n, rwidth, pa^[1], ca, term);
  getmem(t, n*sizeof(ArbFloat));
  if term=1 then
  begin
      success:=true; ind:=1; k:=1;
      while (k<>n+1) and success do
      begin
          success:=pa^[ind]<>0; k:=k+1; ind:=ind+rwidth+1
      end; {k}
      if success then
      begin
          for k:=n downto 1 do
          begin
              for i:=k to n do t^[i]:=pa^[(i-1)*rwidth+k];
              tk:=t^[k];
              for i:=n downto k do
              begin
                  if i=k then h:=1/tk else h:=0;
                  ind:=(i-1)*rwidth;
                  for j:=k+1 to i do h:=h-pa^[ind+j]*t^[j];
                  for j:=i+1 to n do h:=h-pa^[(j-1)*rwidth+i]*t^[j];
                  pa^[ind+k]:=h/tk
              end {i}
          end {k}
      end; {success}
      if (not success) then term:=2 else
      for i:=1 to n do
      begin
          ind:=(i-1)*rwidth;
          for j:=i+1 to n do pa^[ind+j]:=pa^[(j-1)*rwidth+i]
      end; {i}
  end; {term=1}
  freemem(t, n*sizeof(ArbFloat));
end; {invgpd}
end.
