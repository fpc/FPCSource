{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by Pierre Muller,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit COMPLEX;

(* Bibliotheque mathematique pour type complexe *)
(* Version a fonctions et pointeurs *)
(* JD GAYRARD mai 95 *)

(* modified for FPK by Pierre Muller *)
(* FPK supports operator overloading *)
(* This library is based on functions instead of procedures.
   To allow a function to return complex type, the trick is
   is to use a pointer on the result of the function. All
   functions are of Pcomplex type (^complexe).
   In the main program the function computation is accessed
   by      z := function_name(param1, param2)^ *)

{$G+}
{$N+}
{$E-}

interface

uses MATHLIB, HYPERBOL;

const author  = 'GAYRARD J-D';
      version = 'ver 0.0 - 05/95';

type complex = record
                re : real;
                im : real;
                end;

pcomplex = ^complex;

const _i : complex = (re : 0.0; im : 1.0);
      _0 : complex = (re : 0.0; im : 0.0);


(* quatre operations : +, -, * , /  and = *)
operator + (z1, z2 : complex) z : complex;      (* addition *)
{ assignment overloading is not implement }
{ if it was the next two would not be unnecessary }
operator + (z1 : complex; r : real) z : complex;      (* addition *)
operator + (r : real; z1 : complex) z : complex;      (* addition *)

operator - (z1, z2 : complex) z : complex;      (* soustraction *)
operator - (z1 : complex;r : real) z : complex;      (* soustraction *)
operator - (r : real; z1 : complex) z : complex;      (* soustraction *)

operator * (z1, z2 : complex) z : complex;      (* multiplication *)
operator * (z1 : complex; r : real) z : complex;      (* multiplication *)
operator * (r : real; z1 : complex) z : complex;      (* multiplication *)

operator / (znum, zden : complex) z : complex;  (* division znum / zden *)
operator / (znum : complex; r : real) z : complex;  (* division znum / r *)
operator / (r : real; zden : complex) z : complex;  (* division r / zden *)

{ ^ is not built in FPK, but can be overloaded also }
operator ^ (r1, r2 : real) r : real;
operator ^ (z1, z2 : complex) z : complex;
operator ^ (z1 : complex; r2 : real) z : complex;
operator ^ (r : real; z1 : complex) z : complex;

operator = (z1, z2 : complex) b : boolean;
operator = (z1 : complex;r : real) b : boolean;
operator = (r : real; z1 : complex) b : boolean;

(* fonctions complexes particulieres *)
function cneg (z : complex) : complex;       (* negatif *)
function ccong (z : complex) : complex;      (* conjuge *)
function crcp (z : complex) : complex;       (* inverse *)
function ciz (z : complex) : complex;        (* multiplication par i *)
function c_iz (z : complex) : complex;       (* multiplication par -i *)
function czero : complex;                     (* return zero *)

(* fonctions complexes a retour non complex *)
function cmod (z : complex) : real;           (* module *)
function cequal (z1, z2 : complex) : boolean;  (* compare deux complexes *)
function carg (z : complex) : real;           (* argument : a / z = p.e^ia *)

(* fonctions elementaires *)
function cexp (z : complex) : complex;       (* exponantielle *)
function cln (z : complex) : complex;        (* logarithme naturel *)
function csqrt (z : complex) : complex;      (* racine carre *)

(* fonctions trigonometrique directe *)
function ccos (z : complex) : complex;       (* cosinus *)
function csin (z : complex) : complex;       (* sinus *)
function ctg  (z : complex) : complex;       (* tangente *)

(* fonctions trigonometriques inverses *)
function carc_cos (z : complex) : complex;   (* arc cosinus *)
function carc_sin (z : complex) : complex;   (* arc sinus *)
function carc_tg  (z : complex) : complex;   (* arc tangente *)

(* fonctions trigonometrique hyperbolique *)
function cch (z : complex) : complex;        (* cosinus hyperbolique *)
function csh (z : complex) : complex;        (* sinus hyperbolique *)
function cth (z : complex) : complex;        (* tangente hyperbolique *)

(* fonctions trigonometrique hyperbolique inverse *)
function carg_ch (z : complex) : complex;    (* arc cosinus hyperbolique *)
function carg_sh (z : complex) : complex;    (* arc sinus hyperbolique *)
function carg_th (z : complex) : complex;    (* arc tangente hyperbolique *)



implementation

(* quatre operations de base +, -, * , / *)

operator + (z1, z2 : complex) z : complex;
(* addition : z := z1 + z2 *)
begin
z.re := z1.re + z2.re;
z.im := z1.im + z2.im;
end;

operator + (z1 : complex; r : real) z : complex;
(* addition : z := z1 + r *)
begin
z.re := z1.re + r;
z.im := z1.im;
end;

operator + (r : real; z1 : complex) z : complex;
(* addition : z := r + z1 *)
begin
z.re := z1.re + r;
z.im := z1.im;
end;

operator - (z1, z2 : complex) z : complex;
(* substraction : z := z1 - z2 *)
begin
z.re := z1.re - z2.re;
z.im := z1.im - z2.im;
end;

operator - (z1 : complex; r : real) z : complex;
(* substraction : z := z1 - r *)
begin
z.re := z1.re - r;
z.im := z1.im;
end;

operator - (r : real; z1 : complex) z : complex;
(* substraction : z := r + z1 *)
begin
z.re := r - z1.re;
z.im := - z1.im;
end;

operator * (z1, z2 : complex) z : complex;
(* multiplication : z := z1 * z2 *)
begin
z.re := (z1.re * z2.re) - (z1.im * z2.im);
z.im := (z1.re * z2.im) + (z1.im * z2.re);
end;

operator * (z1 : complex; r : real) z : complex;
(* multiplication : z := z1 * r *)
begin
z.re := z1.re * r;
z.im := z1.im * r;
end;

operator * (r : real; z1 : complex) z : complex;
(* multiplication : z := r + z1 *)
begin
z.re := z1.re * r;
z.im := z1.im * r;
end;

operator / (znum, zden : complex) z : complex;
(* division : z := znum / zden *)
var denom : real;
begin
with zden do denom := (re * re) + (im * im);
if denom = 0.0
   then begin
        writeln('******** function Cdiv ********');
        writeln('******* DIVISION BY ZERO ******');
        runerror(200);
        end
   else begin
        z.re := ((znum.re * zden.re) + (znum.im * zden.im)) / denom;
        z.im := ((znum.im * zden.re) - (znum.re * zden.im)) / denom;
        end;
end;

operator / (znum : complex; r : real) z : complex;
(* division : z := znum / r *)
begin
if r = 0.0
   then begin
        writeln('******** function Cdiv ********');
        writeln('******* DIVISION BY ZERO ******');
        runerror(200);
        end
   else begin
        z.re := znum.re / r;
        z.im := znum.im / r;
        end;
end;

operator / (r : real; zden : complex) z : complex;
(* division : z := r / zden *)
var denom : real;
begin
with zden do denom := (re * re) + (im * im);
if denom = 0.0
   then begin
        writeln('******** function Cdiv ********');
        writeln('******* DIVISION BY ZERO ******');
        runerror(200);
        end
   else begin
        z.re := (r * zden.re) / denom;
        z.im := - (r * zden.im) / denom;
        end;
end;

(* fonctions complexes particulieres *)

function cneg (z : complex) : complex;
(* negatif : cneg = - z *)
begin
cneg.re := - z.re;
cneg.im := - z.im;
end;

function cmod (z : complex): real;
(* module : r = |z| *)
begin
with z do cmod := sqrt((re * re) + (im * im))
end;

function carg (z : complex): real;
(* argument : 0 / z = p ei0 *)
begin
carg := arctan2(z.re, z.im)
end;

function ccong (z : complex) : complex;
(* conjuge : z := x + i.y alors r = x - i.y *)
begin
ccong.re := z.re;
ccong.im := - z.im;
end;

function cinv (z : complex) : complex;
(* inverse : r := 1 / z *)
var denom : real;
begin
with z do denom := (re * re) + (im * im);
if denom = 0.0
   then begin
        writeln('******** function Cinv ********');
        writeln('******* DIVISION BY ZERO ******');
        runerror(200);
        end
   else begin
        cinv.re := z.re / denom;
        cinv.im := - z.im / denom
        end;
end;

function ciz (z : complex) : complex;
(* multiplication par i *)
(* z = x + i.y , r = i.z = - y + i.x *)
begin
ciz.re := - z.im;
ciz.im := z.re;
end;

function c_iz (z : complex) : complex;
(* multiplication par -i *)
(* z = x + i.y , r = -i.z = y - i.x *)
begin
c_iz.re := z.im;
c_iz.im := - z.re;
end;

function czero : complex;
(* return a zero complex *)
begin
czero.re := 0.0;
czero.im := 0.0;
end;

operator = (z1, z2 : complex) b : boolean;
(* returns TRUE if z1 = z2 *)
begin
b := (z1.re = z2.re) and (z1.im = z2.im)
end;

operator = (z1 : complex; r :real) b : boolean;
(* returns TRUE if z1 = r *)
begin
b := (z1.re = r) and (z1.im = 0.0)
end;

operator = (r : real; z1 : complex) b : boolean;
(* returns TRUE if z1 = r *)
begin
b := (z1.re = r) and (z1.im = 0.0)
end;


(* fonctions elementaires *)

function cexp (z : complex) : complex;
(* exponantielle : r := exp(z) *)
(* exp(x + iy) = exp(x).exp(iy) = exp(x).[cos(y) + i sin(y)] *)
var expz : real;
begin
expz := exp(z.re);
cexp.re := expz * cos(z.im);
cexp.im := expz * sin(z.im);
end;

function cln (z : complex) : complex;
(* logarithme naturel : r := ln(z) *)
(* ln( p exp(i0)) = ln(p) + i0 + 2kpi *)
var modz : real;
begin
with z do modz := (re * re) + (im * im);
if modz = 0.0
   then begin
        writeln('********* function Cln *********');
        writeln('****** LOGARITHME OF ZERO ******');
        runerror(200)
        end
   else begin
        cln.re := ln(modz);
        cln.im := arctan2(z.re, z.im);
        end
end;

function csqrt (z : complex) : complex;
(* racine carre : r := sqrt(z) *)
var root, q : real;
begin
if (z.re <> 0.0) or (z.im <> 0.0)
   then begin
        root := sqrt(0.5 * (abs(z.re) + cmod(z)));
        q := z.im / (2.0 * root);
        if z.re >= 0.0 then
                begin
                csqrt.re := root;
                csqrt.im := q;
                end
           else if z.im < 0.0
                   then
                        begin
                        csqrt.re := - q;
                        csqrt.im := - root
                        end
                   else
                        begin
                        csqrt.re :=  q;
                        csqrt.im :=  root
                        end
        end
   else result := z;
end;

operator ^ (z1, z2 : complex) z : complex;
(* multiplication : z := z1 * z2 *)
begin
z.re := (z1.re * z2.re) - (z1.im * z2.im);
z.im := (z1.re * z2.im) + (z1.im * z2.re);
end;

operator * (z1 : complex; r : real) z : complex;
(* multiplication : z := z1 * r *)
begin
z.re := z1.re * r;
z.im := z1.im * r;
end;

operator * (r : real; z1 : complex) z : complex;
(* multiplication : z := r + z1 *)
begin
z.re := z1.re * r;
z.im := z1.im * r;
end;

(* fonctions trigonometriques directes *)

function ccos (z : complex) : complex;
(* cosinus complex *)
(* cos(x+iy) = cos(x).cos(iy) - sin(x).sin(iy) *)
(* cos(ix) = ch(x) et sin(ix) = i.sh(x) *)
begin
ccos.re := cos(z.re) * ch(z.im);
ccos.im := - sin(z.re) * sh(z.im);
end;

function csin (z : complex) : complex;
(* sinus complex *)
(* sin(x+iy) = sin(x).cos(iy) + cos(x).sin(iy) *)
(* cos(ix) = ch(x) et sin(ix) = i.sh(x) *)
begin
csin.re := sin(z.re) * ch(z.im);
csin.im := cos(z.re) * sh(z.im);
end;

function ctg (z : complex) : complex;
(* tangente *)
var ccosz, temp : complex;
begin
ccosz := ccos(z);
if (ccosz.re = 0.0) and (ccosz.im = 0.0)
   then begin
        writeln('********* function Ctg *********');
        writeln('******* DIVISION BY ZERO ******');
        runerror(200)
        end
   else begin
        temp := csin(z);
        ctg := temp / ccosz;
        end
end;

(* fonctions trigonometriques inverses *)

function carc_cos (z : complex) : complex;
(* arc cosinus complex *)
(* arccos(z) = -i.argch(z) *)
begin
z := carg_ch(z);
carc_cos := c_iz(z);
end;

function carc_sin (z : complex) : complex;
(* arc sinus complex *)
(* arcsin(z) = -i.argsh(i.z) *)
begin
z := ciz(z);
z := carg_sh(z);
carc_sin := c_iz(z);
end;

function carc_tg (z : complex) : complex;
(* arc tangente complex *)
(* arctg(z) = -i.argth(i.z) *)
begin
z := ciz(z);
z := carg_th(z);
carc_tg := c_iz(z);
end;

(* fonctions trigonometriques hyperboliques *)

function cch (z : complex) : complex;
(* cosinus hyperbolique *)
(* ch(x+iy) = ch(x).ch(iy) + sh(x).sh(iy) *)
(* ch(iy) = cos(y) et sh(iy) = i.sin(y) *)
begin
cch.re := ch(z.re) * cos(z.im);
cch.im := sh(z.re) * sin(z.im);
end;

function csh (z : complex) : complex;
(* sinus hyperbolique *)
(* sh(x+iy) = sh(x).ch(iy) + ch(x).sh(iy) *)
(* ch(iy) = cos(y) et sh(iy) = i.sin(y) *)
begin
csh.re := sh(z.re) * cos(z.im);
csh.im := ch(z.re) * sin(z.im);
end;

function cth (z : complex) : complex;
(* tangente hyperbolique complex *)
(* th(x) = sh(x) / ch(x) *)
(* ch(x) > 1 qq x *)
var temp : complex;
begin
temp := cch(z);
z := csh(z);
cth := z / temp;
end;

(* fonctions trigonometriques hyperboliques inverses *)

function carg_ch (z : complex) : complex;
(*   arg cosinus hyperbolique    *)
(*                          _________  *)
(* argch(z) = -/+ ln(z + i.V 1 - z.z)  *)
var temp : complex;
begin
with temp do begin
             re := 1 - z.re * z.re + z.im * z.im;
             im := - 2 * z.re * z.im
             end;
temp := csqrt(temp);
temp := ciz(temp);
temp := temp + z;
temp := cln(temp);
carg_ch := cneg(temp);
end;

function carg_sh (z : complex) : complex;
(*   arc sinus hyperbolique    *)
(*                    ________  *)
(* argsh(z) = ln(z + V 1 + z.z) *)
var temp : complex;
begin
with temp do begin
             re := 1 + z.re * z.re - z.im * z.im;
             im := 2 * z.re * z.im
             end;
temp := csqrt(temp);
temp := cadd(temp, z);
carg_sh := cln(temp);
end;

function carg_th (z : complex) : complex;
(* arc tangente hyperbolique *)
(* argth(z) = 1/2 ln((z + 1) / (1 - z)) *)
var temp : complex;
begin
with temp do begin
             re := 1 + z.re;
             im := z.im
             end;
with result do begin
          re := 1 - re;
          im := - im
          end;
result := temp / result;
carg_th.re := 0.5 * re;
carg_th.im := 0.5 * im
end;

end.
{
  $Log$
  Revision 1.2  1998-05-12 10:42:44  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
