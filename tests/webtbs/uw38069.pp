{ %cpu=i386 }
{$mode objfpc}
{$modeswitch advancedrecords}
{$FPUTYPE SSE2}

unit uw38069;
INTERFACE
uses sysutils;
type  float  = double; //#zentral definieren
      complex = record
        public
          re, im: float;
          class operator * (const a, b: complex): complex; inline;
          class operator * (const a: complex; const x:float): complex; inline;
          class operator * (const x: float; const a: complex): complex; inline;

          class operator := (const x: float): complex;     inline;
          class operator  = (const a,b: complex): boolean; inline;
          class operator  - (const a: complex): complex;   inline;
        end;


procedure mul (const a,b: complex; var c: complex); inline; overload;
procedure mul (const a: complex; const b: float; var c: complex); inline; overload;
procedure mul (const a: float; const b: complex; var c: complex); inline; overload;


IMPLEMENTATION


procedure mul (const a,b: complex; var c: complex);
begin
  c.re := a.re*b.re - a.im*b.im;
  c.im := a.re*b.im + a.im*b.re;
end;

procedure mul (const a: complex; const b: float; var c: complex);
begin
  c.re := a.re*b;
  c.im := a.im*b;
end;


procedure mul (const a: float; const b: complex; var c: complex);
begin
  mul (b,a,c);
end;

function pow (x,y: float): float;
begin
  result := exp (y*ln(x));
end;


function ToComplex (a,b: float): complex;
begin
  result.re := a;
  result.im := b;
end;

//Operatoren complex-complex
class operator complex.* (const a,b: complex): complex;
begin
  mul (a,b,result);
end;

class operator complex.* (const x: float; const a: complex): complex;
begin
  mul (a,x,result);
end;

class operator complex.* (const a: complex; const x:float): complex;
begin
  mul (a,x,result);
end;

class operator complex.:= (const x: float): complex;
begin
  result.re := x;
  result.im := 0;
end;

class operator complex.= (const a,b: complex): boolean;
begin
  result := (a.re=b.re) and (a.im=b.im);
end;

class operator complex.- (const a: complex): complex;
begin
  result.re := -a.re;
  result.im := -a.im;
end;


begin
end.
