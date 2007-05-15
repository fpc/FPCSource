{$ifdef fpc}
{$mode delphi}
{$endif}
program test;

uses
  SysUtils;
  
type
  vecteurF = array of extended;
  matriceF = array of vecteurF;
  matriceE = record
    err     :integer;
    x       :matriceF;
  end;

var
  A,B,C :matriceE;  

function copyM(A:matriceE):matriceE;
var
  i,j,nl,nc :integer;
  C   :matriceE;
begin
  nl:=succ(high(A.x));nc:=succ(high(A.x[0]));
  setlength(C.x,nl,nc);
  for i:=0 to pred(nl) do
    begin
      for j:=0 to pred(nc) do C.x[i,j]:=A.x[i,j];
    end;
  C.err:=A.err;
  Result:=C;
end;

procedure copyM2(A:matriceE;var C:matriceE);
var
  i,j,nl,nc :integer;
begin
  nl:=succ(high(A.x));nc:=succ(high(A.x[0]));
  setlength(C.x,nl,nc);
  for i:=0 to pred(nl) do
    begin
      for j:=0 to pred(nc) do C.x[i,j]:=A.x[i,j];
    end;
  C.err:=A.err;
end;

procedure writeM(A:matriceE;str:string);
var
  i,j :integer;
begin
  for i:=0 to high(A.x) do
    begin
      for j:=0 to high(A.x[i]) do write(format(str,[A.x[i,j]]));
      writeln;
    end;
end;

procedure checkM(const A,B:matriceE);
var
  i,j :integer;
begin
  if (high(A.x) <> high(B.x)) then
    halt(1);
  for i:=0 to high(A.x) do
    begin
      if (high(A.x[i]) <> high(B.x[i])) then
        halt(2);
      for j:=0 to high(A.x[i]) do
        if (A.x[i,j] <> B.x[i,j]) then
          halt(3);
    end;
end;


begin
  setlength(A.x,3,3);
  A.err:=0;
  A.x[0,0]:=0.5;A.x[0,1]:=0.2;A.x[0,2]:=0.8;
  A.x[1,0]:=0.2;A.x[1,1]:=0.2;A.x[1,2]:=0.9;
  A.x[2,0]:=0.8;A.x[2,1]:=0.9;A.x[2,2]:=3.1;
  writeln('matrix A,  number of lines : ',succ(high(A.x)));
  writeM(A,'%6.3f');
  writeln;
  B:=copyM(A);
  writeln('matrix B,  number of lines : ',succ(high(B.x)));
  checkM(A,B);
  writeln;
  copyM2(A,C);
  writeln('matrix C,  number of lines : ',succ(high(C.x)));
  checkM(A,C);
  writeln;
  writeln('end');
end.

