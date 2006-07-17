{ %opt=-Cfsse2 -O-2 }
{ %cpu=i386 }

(*

Testprogram to show sse2/sse3 bug

- works with fpu -O{1,2,3}
- works with -CfSSE{2,3} -O1
- access violation with -CfSSE{2,3} -O{2,3}

NOTE: It also works with -CfSSE3 -O3 if I
      declare DMLogicToCube as global var!


Expected Output:
    0.2500  ->   -115.4701
    0.1000  ->   -247.4874
    0.0500  ->    -20.4124
   20.0000  ->   -135.5047
   45.0000  ->     23.8797

*)
program ssetest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { add your units here }
  ,math, mmx;

type kfloat = double;
     TPosArr = array[0..4] of kfloat;
     VektorArr  = Array[0..2] of kfloat;
     Drehmatrix = Array[0..2] of VektorArr;

type
     TCube = class
       private
         DMLogicToCube : Drehmatrix;
       public
         Procedure InitDrehmatrizen;
         function LogicToCube(const apo: TPosArr) : TPosArr;
     end;


Function aTanG(const x,y : kfloat) : kfloat;
Var
  b : kfloat;
begin
  b:=x*x+y*y;
  if isZero(b) then result:=0
  else begin
    if IsZero(x) then
    begin
      if y >= 0 then result:=90 else result:=-90;
    end
    else begin
      if IsZero(y) then result:=0
      else begin;
        b:=ln(abs(y))-ln(abs(x));
        if (b > 230) then result:=90 else result:=180/pi*arctan(y/x);
      end;
    end;
    if (x < 0) then result:=result+180;
    if (result > 180) then result:=result-360;
  end;
end;

Function  SinD(const w : kfloat) : kfloat;
begin
  result:=sin(w*pi/180);
end;

Function  CosD(const w : kfloat) : kfloat;
begin
  result:=Cos(w*pi/180);
end;



Procedure EinheitsVektor(out v : VektorArr;const ph,th : kfloat);
var cth : kfloat;
begin
  cth:=cosD(th);
  v[0]:=cosD(ph)*cth;
  v[1]:=sinD(ph)*cth;
  v[2]:=sinD(th);
end;

Function Gettheta(const v : VektorArr) : kfloat;
begin
  result:=aTanG(sqrt(sqr(v[0])+sqr(v[1])),v[2]);
end;

Function Getphi(const v : VektorArr) : kfloat;
begin
  result:=aTanG(v[0],v[1]);
end;

Procedure TCube.InitDrehmatrizen;
begin
  DMLogicToCube[0,0]:=-cos(arctan(sqrt(2)));
  DMLogicToCube[1,0]:=-1/sqrt(2);
  DMLogicToCube[2,0]:=-cos(arctan(sqrt(2)))/sqrt(2);
  DMLogicToCube[0,1]:= cos(arctan(sqrt(2)));
  DMLogicToCube[1,1]:=-1/sqrt(2);
  DMLogicToCube[2,1]:= cos(arctan(sqrt(2)))/sqrt(2);
  DMLogicToCube[0,2]:=-cos(arctan(sqrt(2)));
  DMLogicToCube[1,2]:= 0;
  DMLogicToCube[2,2]:= sin(arctan(sqrt(2)));
end;

function TCube.LogicToCube(const apo: TPosArr) : TPosArr;
Var
  s,z : Integer;
  vzz,v : VektorArr;
  ee : KFloat;

begin
  EinheitsVektor(v,apo[3],apo[4]);
  for z:=0 to 2 do
  begin
    result[z]:=0;
    vzz[z]:=0;
    for s:=0 to 2 do begin
        ee:=DMLogicToCube[z,s]; //AccessViolation here
        result[z]:=result[z]+ee*apo[s];
        vzz[z]:=vzz[z]+ee*v[s];
    end;
    Result[z]:=result[z]*1000;
  end;
  result[3]:=Getphi(vzz);
  result[4]:=Gettheta(vzz);
end;

const useInput : TPosArr=(0.25,0.10,0.05,20.0,45.0);
var
    Cube : TCube;
    resu : TPosArr;
    i : integer;
    resstr: string;


const
  correctresults: array[0..4] of string[10] =
   (' -115.4701',' -247.4874','  -20.4124',' -135.5047','   23.8797');

begin
  if not is_sse2_cpu then
    halt(0);
  Cube:=TCube.Create;
  Cube.InitDrehmatrizen;
  resu:=Cube.LogicToCube(useInput);
  for i := 0 to High(resu) do
    begin
      writeln(useinput[i]:10:4,'  ->  ',resu[i]:10:4);
      str(resu[i]:10:4,resstr);
      if resstr <> correctresults[i] then
        halt(1);
    end;
  cube.free;
end.

