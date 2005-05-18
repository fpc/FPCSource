{
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             Documentation by Michael van Canneyt (Michael@freepascal.org)

    This unit exports all functions in the tpnumlib dll. (a header file more
    or less) Programs based on this unit don't require the other sources to
    compile/build, only the DLL, direct.inc and this file are needed.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit NumLib;

interface
{$I direct.inc}

uses typ;

CONST Numlib_dll_version=2;        {Original is 1, first FPC version=2}

{not wrapped to 80 columns yet, since this is easier for copying and
pasting, and adding of the external lines}

{Added; if the internal version of this unit and dll differ,
this function returns FALSE, and program can abort}
FUNCTION CheckVersion: BOOLEAN;

procedure detgen(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure detgsy(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure detgpd(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure detgba(n, l, r: ArbInt; var a, f: ArbFloat; var k, term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure detgpb(n, l: ArbInt; var a, f: ArbFloat; var k, term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure detgtr(n: ArbInt; var l, d, u, f: ArbFloat; var k, term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dslgen(n, rwidth: ArbInt; var alu: ArbFloat; var p: ArbInt;var b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dslgtr(n: ArbInt; var l1, d1, u1, u2: ArbFloat; var p: boolean; var b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dslgsy(n, rwidth: ArbInt; var alt: ArbFloat; var p: ArbInt;var q: boolean; var b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dslgpd(n, rwidth: ArbInt; var al, b, x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dslgba(n, lb, rb, rwa: ArbInt; var au: ArbFloat; rwl: ArbInt;var l: ArbFloat; var p: ArbInt; var b, x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dslgpb(n, lb, rwidth: ArbInt; var al, b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure dsldtr(n:ArbInt; var l, d, u, b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggs1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggs2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt;var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggs3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat;rwidthx: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggs4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat;rwidthx: ArbInt; var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigts1(var d, cd: ArbFloat; n: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigts2(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigts3(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat;rwidth: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigts4(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam, x: ArbFloat;rwidth: ArbInt; var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigbs1(var a: ArbFloat; n, l: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigbs2(var a: ArbFloat; n, l, k1, k2: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigbs3(var a: ArbFloat; n, l: ArbInt; var lam, x: ArbFloat;rwidthx: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigbs4(var a: ArbFloat; n, l, k1, k2: ArbInt;var lam, x: ArbFloat;  rwidthx: ArbInt;var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigge1(var a: ArbFloat; n, rwidth: ArbInt; var lam: complex;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigge3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: complex;rwidthx: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggg1(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggg2(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggg3(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eiggg4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigsv1(var a: ArbFloat; m, n, rwidth: ArbInt; var sig: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure eigsv3(var a: ArbFloat; m, n, rwidtha: ArbInt; var sig, u: ArbFloat;rwidthu: ArbInt; var v: ArbFloat; rwidthv: ArbInt;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure int1fr(f: rfunc1r; a, b, ae: ArbFloat; var integral, err: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure invgen(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure invgsy(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure invgpd(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure iomrev(var inp: text; var v: ArbFloat; n: ArbInt);
procedure iomrem(var inp: text; var a: ArbFloat; m, n, rwidth: ArbInt);
procedure iomwrv(var out: text; var v: ArbFloat; n, form: ArbInt);
procedure iomwrm(var out: text; var a: ArbFloat; m, n, rwidth, form: ArbInt);
procedure mdtgen(n, rwidth: ArbInt; var alu: ArbFloat; var p: ArbInt;var ca:ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure mdtgtr(n: ArbInt; var l, d, u, l1, d1, u1, u2: ArbFloat; var p: boolean; var ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure mdtgsy(n, rwidth: ArbInt; var a: ArbFloat; var pp:ArbInt;var qq:boolean; var ca:ArbFloat; var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure mdtgpd(n, rwidth: ArbInt; var al, ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure mdtgba(n, lb, rb, rwa: ArbInt; var a: ArbFloat; rwl: ArbInt;var l:ArbFloat; var p: ArbInt; var ca: ArbFloat; var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure mdtgpb(n, lb, rwidth: ArbInt; var al, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure mdtdtr(n: ArbInt; var l, d, u, l1, d1, u1: ArbFloat;var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure odeiv1(f: rfunc2r; a, ya: ArbFloat; var b, yb: ArbFloat;ae: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure odeiv2(f: oderk1n; a: ArbFloat; var ya, b, yb: ArbFloat;n: ArbInt; ae: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvinp(var a, b: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure omvmmm(var a: ArbFloat; m, n, rwa: ArbInt;var b: ArbFloat; k, rwb: ArbInt;var c: ArbFloat; rwc: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure omvmmv(var a: ArbFloat; m, n, rwidth: ArbInt; var b, c: ArbFloat); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvn1m(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvn1v(var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvn2v(var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvnfm(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvnmm(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function omvnmv(var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure omvtrm(var a: ArbFloat; m, n, rwa: ArbInt;var c: ArbFloat; rwc: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure roobin(n: ArbInt; a: complex; var z: complex; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure roof1r(f: rfunc1r; a, b, ae, re: ArbFloat; var x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure roopol(var a: ArbFloat; n: ArbInt; var z: complex;var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure rooqua(p, q: ArbFloat; var z1, z2: complex); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure roofnr(f: roofnrfunc; n: ArbInt; var x, residu: ArbFloat; re: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure sledtr(n: ArbInt; var l, d, u, b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegba(n, l, r: ArbInt;var a, b, x, ca: ArbFloat; var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegbal(n, l, r: ArbInt;var a1; var b1, x1, ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegen(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegenl(n: ArbInt;var a1;var b1, x1, ca: ArbFloat;                  var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegls(var a: ArbFloat; m, n, rwidtha: ArbInt; var b, x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure sleglsl(var a1; m, n: ArbInt; var b1, x1: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegpb(n, l: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegpbl(n, l: ArbInt;var a1; var b1, x1, ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegpd(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegpdl(n: ArbInt; var a1; var b1, x1, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegsy(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegsyl(n: ArbInt; var a1; var b1, x1, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure slegtr(n:ArbInt; var l, d, u, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function spebi0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function spebi1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function spebj0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function spebj1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function spebk0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function spebk1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function speby0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}
function speby1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function speent(x: ArbFloat): longint; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}
function speerf(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 25 oktober 1993}
function speefc(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 25 oktober 1993}
function spegam(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 25 oktober 1993}
function spelga(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 20 oktober 1993}
function spemax(a, b: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}
function spepol(x: ArbFloat; var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}
function spepow(a, b: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}
function spesgn(x: ArbFloat): ArbInt; {ok 26 oktober 1993}
function spears(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 21 oktober 1993}
function spearc(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 21 oktober 1993}
function spesih(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function specoh(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function spetah(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function speash(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function speach(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function speath(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}
function  spl1bspv(q: ArbInt; var kmin1, c1: ArbFloat; x: ArbFloat; var term: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function  spl2bspv(qx, qy: ArbInt; var kxmin1, kymin1, c11: ArbFloat; x, y: ArbFloat; var term: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl1bspf(M, Q: ArbInt; var XYW1: ArbFloat;var Kmin1, C1, residu: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl2bspf(M, Qx, Qy: ArbInt; var XYZW1: ArbFloat;var Kxmin1, Kymin1, C11, residu: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl1nati(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl1naki(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl1cmpi(n: ArbInt; var xyc1: ArbFloat; dy1, dyn: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl1peri(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function  spl1pprv(n: ArbInt; var xyc1: ArbFloat; t: ArbFloat; var term: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl1nalf(n: ArbInt; var xyw1: ArbFloat; lambda:ArbFloat;var xac1, residu: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function spl2natv(n: ArbInt; var xyg0: ArbFloat; u, v: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure spl2nalf(n: ArbInt; var xyzw1: ArbFloat; lambda:ArbFloat;var xyg0, residu: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
//procedure Intsle(l: ArbInt; e: ArbFloat); {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function dllversion:LONGINT; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function exp(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
function MachCnst(n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure ipffsn(n: ArbInt; var x, y, a, d2a: ArbFloat; var term: ArbInt);{$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure ipfisn(n: ArbInt; var x, y, d2s: ArbFloat; var term: ArbInt);{$IFDEF Needsstdcall} stdcall; {$ENDIF}
function  ipfspn(n: ArbInt; var x, y, d2s: ArbFloat; t: ArbFloat;var term: ArbInt): ArbFloat;{$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure ipfpol(m, n: ArbInt; var x, y, b: ArbFloat; var term: ArbInt);{$IFDEF Needsstdcall} stdcall; {$ENDIF}
function spline(n: ArbInt; x: complex; var ac: complex; var gammar: ArbFloat; u1: ArbFloat; pf: complex): ArbFloat;{$IFDEF Needsstdcall} stdcall; {$ENDIF}
procedure splineparameters (n: ArbInt; var ac, alfadc: complex; var lambda, gammar, u1, kwsom, energie: ArbFloat; var pf: complex);{$IFDEF Needsstdcall} stdcall; {$ENDIF}

implementation


procedure detgen(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                             external 'TpNumLib'  index   1;
procedure detgsy(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                             external 'TpNumLib'  index   2;
procedure detgpd(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                             external 'TpNumLib'  index   3;
procedure detgba(n, l, r: ArbInt; var a, f: ArbFloat; var k, term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                external 'TpNumLib'  index   4;
procedure detgpb(n, l: ArbInt; var a, f: ArbFloat; var k, term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                   external 'TpNumLib'  index   5;
procedure detgtr(n: ArbInt; var l, d, u, f: ArbFloat; var k, term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                external 'TpNumLib'  index   6;
procedure dslgen(n, rwidth: ArbInt; var alu: ArbFloat; var p: ArbInt;var b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                               external 'TpNumLib'  index   7;
procedure dslgtr(n: ArbInt; var l1, d1, u1, u2: ArbFloat; var p: boolean; var b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                          external 'TpNumLib'  index   8;
procedure dslgsy(n, rwidth: ArbInt; var alt: ArbFloat; var p: ArbInt;var q: boolean; var b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                               external 'TpNumLib'  index   9;
procedure dslgpd(n, rwidth: ArbInt; var al, b, x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                             external 'TpNumLib'  index   10;
procedure dslgba(n, lb, rb, rwa: ArbInt; var au: ArbFloat; rwl: ArbInt;var l: ArbFloat; var p: ArbInt; var b, x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}              external 'TpNumLib'  index   11;
procedure dslgpb(n, lb, rwidth: ArbInt; var al, b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                        external 'TpNumLib'  index   12;
procedure dsldtr(n:ArbInt; var l, d, u, b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                external 'TpNumLib'  index   13;
procedure eiggs1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                external 'TpNumLib'  index   14;
procedure eiggs2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt;var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                         external 'TpNumLib'  index   15;
procedure eiggs3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat;rwidthx: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                            external 'TpNumLib'  index   16;
procedure eiggs4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat;rwidthx: ArbInt; var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                external 'TpNumLib'  index   17;
procedure eigts1(var d, cd: ArbFloat; n: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                     external 'TpNumLib'  index   18;
procedure eigts2(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                             external 'TpNumLib'  index   19;
procedure eigts3(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat;rwidth: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                  external 'TpNumLib'  index   20;
procedure eigts4(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam, x: ArbFloat;rwidth: ArbInt; var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                      external 'TpNumLib'  index   21;
procedure eigbs1(var a: ArbFloat; n, l: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                      external 'TpNumLib'  index   22;
procedure eigbs2(var a: ArbFloat; n, l, k1, k2: ArbInt; var lam: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                              external 'TpNumLib'  index   23;
procedure eigbs3(var a: ArbFloat; n, l: ArbInt; var lam, x: ArbFloat;rwidthx: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                  external 'TpNumLib'  index   24;
procedure eigbs4(var a: ArbFloat; n, l, k1, k2: ArbInt;var lam, x: ArbFloat;  rwidthx: ArbInt;var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                      external 'TpNumLib'  index   25;
procedure eigge1(var a: ArbFloat; n, rwidth: ArbInt; var lam: complex;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                  external 'TpNumLib'  index   26;
procedure eigge3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: complex;rwidthx: ArbInt; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                             external 'TpNumLib'  index   27;
procedure eiggg1(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                              external 'TpNumLib'  index   28;
procedure eiggg2(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                      external 'TpNumLib'  index   29;
procedure eiggg3(var a: ArbFloat; n, rwidtha: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}           external 'TpNumLib'  index   30;
procedure eiggg4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var b: ArbFloat;rwidthb: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt;var m2, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}external 'TpNumLib'  index   31;
procedure eigsv1(var a: ArbFloat; m, n, rwidth: ArbInt; var sig: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                              external 'TpNumLib'  index   32;
procedure eigsv3(var a: ArbFloat; m, n, rwidtha: ArbInt; var sig, u: ArbFloat;rwidthu: ArbInt; var v: ArbFloat; rwidthv: ArbInt;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}        external 'TpNumLib'  index   33;
procedure int1fr(f: rfunc1r; a, b, ae: ArbFloat; var integral, err: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                           external 'TpNumLib'  index   34;
procedure invgen(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                  external 'TpNumLib'  index   35;
procedure invgsy(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                  external 'TpNumLib'  index   36;
procedure invgpd(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                  external 'TpNumLib'  index   37;
procedure iomrev(var inp: text; var v: ArbFloat; n: ArbInt);                                                                                                                                      external 'TpNumLib'  index   38;
procedure iomrem(var inp: text; var a: ArbFloat; m, n, rwidth: ArbInt);                                                                                                                           external 'TpNumLib'  index   39;
procedure iomwrv(var out: text; var v: ArbFloat; n, form: ArbInt);                                                                                                                                external 'TpNumLib'  index   40;
procedure iomwrm(var out: text; var a: ArbFloat; m, n, rwidth, form: ArbInt);                                                                                                                     external 'TpNumLib'  index   41;
procedure mdtgen(n, rwidth: ArbInt; var alu: ArbFloat; var p: ArbInt;var ca:ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                  external 'TpNumLib'  index   42;
procedure mdtgtr(n: ArbInt; var l, d, u, l1, d1, u1, u2: ArbFloat; var p: boolean; var ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                   external 'TpNumLib'  index   43;
procedure mdtgsy(n, rwidth: ArbInt; var a: ArbFloat; var pp:ArbInt;var qq:boolean; var ca:ArbFloat; var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                     external 'TpNumLib'  index   44;
procedure mdtgpd(n, rwidth: ArbInt; var al, ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                              external 'TpNumLib'  index   45;
procedure mdtgba(n, lb, rb, rwa: ArbInt; var a: ArbFloat; rwl: ArbInt;var l:ArbFloat; var p: ArbInt; var ca: ArbFloat; var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                  external 'TpNumLib'  index   46;
procedure mdtgpb(n, lb, rwidth: ArbInt; var al, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                           external 'TpNumLib'  index   47;
procedure mdtdtr(n: ArbInt; var l, d, u, l1, d1, u1: ArbFloat;var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                           external 'TpNumLib'  index   48;
procedure odeiv1(f: rfunc2r; a, ya: ArbFloat; var b, yb: ArbFloat;ae: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                        external 'TpNumLib'  index   49;
procedure odeiv2(f: oderk1n; a: ArbFloat; var ya, b, yb: ArbFloat;n: ArbInt; ae: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                             external 'TpNumLib'  index   50;
function omvinp(var a, b: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                 external 'TpNumLib'  index   51;
procedure omvmmm(var a: ArbFloat; m, n, rwa: ArbInt;var b: ArbFloat; k, rwb: ArbInt;var c: ArbFloat; rwc: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                        external 'TpNumLib'  index   52;
procedure omvmmv(var a: ArbFloat; m, n, rwidth: ArbInt; var b, c: ArbFloat); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                              external 'TpNumLib'  index   53;
function omvn1m(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                         external 'TpNumLib'  index   54;
function omvn1v(var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                    external 'TpNumLib'  index   55;
function omvn2v(var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                    external 'TpNumLib'  index   56;
function omvnfm(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                         external 'TpNumLib'  index   57;
function omvnmm(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                         external 'TpNumLib'  index   58;
function omvnmv(var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                    external 'TpNumLib'  index   59;
procedure omvtrm(var a: ArbFloat; m, n, rwa: ArbInt;var c: ArbFloat; rwc: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                        external 'TpNumLib'  index   60;
procedure roobin(n: ArbInt; a: complex; var z: complex; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                external 'TpNumLib'  index   61;
procedure roof1r(f: rfunc1r; a, b, ae, re: ArbFloat; var x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                   external 'TpNumLib'  index   62;
procedure roopol(var a: ArbFloat; n: ArbInt; var z: complex;var k, term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                         external 'TpNumLib'  index   63;
procedure rooqua(p, q: ArbFloat; var z1, z2: complex); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                    external 'TpNumLib'  index   64;
procedure roofnr(f: roofnrfunc; n: ArbInt; var x, residu: ArbFloat; re: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                       external 'TpNumLib'  index   65;
procedure sledtr(n: ArbInt; var l, d, u, b, x: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                               external 'TpNumLib'  index   66;
procedure slegba(n, l, r: ArbInt;var a, b, x, ca: ArbFloat; var term:ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                             external 'TpNumLib'  index   67;
procedure slegbal(n, l, r: ArbInt;var a1; var b1, x1, ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                    external 'TpNumLib'  index   68;
procedure slegen(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                          external 'TpNumLib'  index   69;
procedure slegenl(n: ArbInt;var a1;var b1, x1, ca: ArbFloat;                  var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                          external 'TpNumLib'  index   70;
procedure slegls(var a: ArbFloat; m, n, rwidtha: ArbInt; var b, x: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                            external 'TpNumLib'  index   71;
procedure sleglsl(var a1; m, n: ArbInt; var b1, x1: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                           external 'TpNumLib'  index   72;
procedure slegpb(n, l: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                               external 'TpNumLib'  index   73;
procedure slegpbl(n, l: ArbInt;var a1; var b1, x1, ca: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                       external 'TpNumLib'  index   74;
procedure slegpd(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                          external 'TpNumLib'  index   75;
procedure slegpdl(n: ArbInt; var a1; var b1, x1, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                          external 'TpNumLib'  index   76;
procedure slegsy(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                          external 'TpNumLib'  index   77;
procedure slegsyl(n: ArbInt; var a1; var b1, x1, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                          external 'TpNumLib'  index   78;
procedure slegtr(n:ArbInt; var l, d, u, b, x, ca: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                             external 'TpNumLib'  index   79;
function spebi0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   80;
function spebi1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   81;
function spebj0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   82;
function spebj1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   83;
function spebk0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   84;
function spebk1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   85;
function speby0(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 22 september 1993}                                                                                            external 'TpNumLib'  index   86;
function speby1(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index   87;
function speent(x: ArbFloat): longint; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}                                                                                               external 'TpNumLib'  index   88;
function speerf(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 25 oktober 1993}                                                                                              external 'TpNumLib'  index   89;
function speefc(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 25 oktober 1993}                                                                                              external 'TpNumLib'  index   90;
function spegam(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 25 oktober 1993}                                                                                              external 'TpNumLib'  index   91;
function spelga(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 20 oktober 1993}                                                                                              external 'TpNumLib'  index   92;
function spemax(a, b: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}                                                                                           external 'TpNumLib'  index   93;
function spepol(x: ArbFloat; var a: ArbFloat; n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}                                                                  external 'TpNumLib'  index   94;
function spepow(a, b: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 26 oktober 1993}                                                                                           external 'TpNumLib'  index   95;
function spesgn(x: ArbFloat): ArbInt; {ok 26 oktober 1993}                                                                                                                                        external 'TpNumLib'  index   96;
function spears(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 21 oktober 1993}                                                                                              external 'TpNumLib'  index   97;
function spearc(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 21 oktober 1993}                                                                                              external 'TpNumLib'  index   98;
function spesih(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index   99;
function specoh(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index  100;
function spetah(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index  101;
function speash(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index  102;
function speach(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index  103;
function speath(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF} {ok 28 september 1993}                                                                                            external 'TpNumLib'  index  104;
function  spl1bspv(q: ArbInt; var kmin1, c1: ArbFloat; x: ArbFloat; var term: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                          external 'TpNumLib'  index  105;
function  spl2bspv(qx, qy: ArbInt; var kxmin1, kymin1, c11: ArbFloat; x, y: ArbFloat; var term: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                        external 'TpNumLib'  index  106;
procedure spl1bspf(M, Q: ArbInt; var XYW1: ArbFloat;var Kmin1, C1, residu: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                    external 'TpNumLib'  index  107;
procedure spl2bspf(M, Qx, Qy: ArbInt; var XYZW1: ArbFloat;var Kxmin1, Kymin1, C11, residu: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                    external 'TpNumLib'  index  108;
procedure spl1nati(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                      external 'TpNumLib'  index  109;
procedure spl1naki(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                      external 'TpNumLib'  index  110;
procedure spl1cmpi(n: ArbInt; var xyc1: ArbFloat; dy1, dyn: ArbFloat;var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                   external 'TpNumLib'  index  111;
procedure spl1peri(n: ArbInt; var xyc1: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                      external 'TpNumLib'  index  112;
function  spl1pprv(n: ArbInt; var xyc1: ArbFloat; t: ArbFloat; var term: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                               external 'TpNumLib'  index  113;
procedure spl1nalf(n: ArbInt; var xyw1: ArbFloat; lambda:ArbFloat;var xac1, residu: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                          external 'TpNumLib'  index  114;
function spl2natv(n: ArbInt; var xyg0: ArbFloat; u, v: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                               external 'TpNumLib'  index  115;
procedure spl2nalf(n: ArbInt; var xyzw1: ArbFloat; lambda:ArbFloat;var xyg0, residu: ArbFloat; var term: ArbInt); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                         external 'TpNumLib'  index  116;
{procedure Intsle(l: ArbInt; e: ArbFloat); {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                                 external 'TpNumLib'  index  117;}
function dllversion:LONGINT; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                                              external 'TpNumLib'  index   117;
function exp(x: ArbFloat): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                                      external 'TpNumLib'  index  118;
function MachCnst(n: ArbInt): ArbFloat; {$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                                                   external 'TpNumLib'  index  119;
procedure ipffsn(n: ArbInt; var x, y, a, d2a: ArbFloat; var term: ArbInt);{$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                 external 'TpNumLib'  index  120;
procedure ipfisn(n: ArbInt; var x, y, d2s: ArbFloat; var term: ArbInt);{$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                    external 'TpNumLib'  index  121;
function  ipfspn(n: ArbInt; var x, y, d2s: ArbFloat; t: ArbFloat;var term: ArbInt): ArbFloat;{$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                              external 'TpNumLib'  index  122;
procedure ipfpol(m, n: ArbInt; var x, y, b: ArbFloat; var term: ArbInt);{$IFDEF Needsstdcall} stdcall; {$ENDIF}                                                                                   external 'TpNumLib'  index  123;
function spline(n: ArbInt; x: complex; var ac: complex; var gammar: ArbFloat; u1: ArbFloat; pf: complex): ArbFloat;{$IFDEF Needsstdcall} stdcall; {$ENDIF}                                        external 'TpNumLib'  index  124;
procedure splineparameters (n: ArbInt; var ac, alfadc: complex; var lambda, gammar, u1, kwsom, energie: ArbFloat; var pf: complex);{$IFDEF Needsstdcall} stdcall; {$ENDIF}                        external 'TpNumLib'  index  125;


FUNCTION CheckVersion: BOOLEAN;

BEGIN
 CheckVersion:=dllVersion=Numlib_dll_version;
END;

end.
