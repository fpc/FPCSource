{
    $Id$
    This file is part of the Numlib package.
    Copyright (c) 1986-2000 by
     Kees van Ginneken, Wil Kortsmit and Loek van Reij of the
     Computational centre of the Eindhoven University of Technology

    FPC port Code          by Marco van de Voort (marco@freepascal.org)
             Documentation by Michael van Canneyt (Michael@freepascal.org)

    This "library" imports 119 procedures from the numlib units, and throws
    them in a dll file. The dll file can be accessed via numlib.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

library tpnumlib;

uses  DET, TYP, DSL, EIG, INT, INV, IOM, MDT, ODE, OMV, ROO, SLE, SPE, SPL,IPF;

exports
 detgen          index      1,
 detgsy          index      2,
 detgpd          index      3,
 detgba          index      4,
 detgpb          index      5,
 detgtr          index      6,

 dslgen          index      7,
 dslgtr          index      8,
 dslgsy          index      9,
 dslgpd          index     10,
 dslgba          index     11,
 dslgpb          index     12,
 dsldtr          index     13,

 eiggs1          index     14,
 eiggs2          index     15,
 eiggs3          index     16,
 eiggs4          index     17,
 eigts1          index     18,
 eigts2          index     19,
 eigts3          index     20,
 eigts4          index     21,
 eigbs1          index     22,
 eigbs2          index     23,
 eigbs3          index     24,
 eigbs4          index     25,
 eigge1          index     26,
 eigge3          index     27,
 eiggg1          index     28,
 eiggg2          index     29,
 eiggg3          index     30,
 eiggg4          index     31,
 eigsv1          index     32,
 eigsv3          index     33,

 int1fr          index     34,

 invgen          index     35,
 invgsy          index     36,
 invgpd          index     37,

 iomrev          index     38,
 iomrem          index     39,
 iomwrv          index     40,
 iomwrm          index     41,

 mdtgen          index     42,
 mdtgtr          index     43,
 mdtgsy          index     44,
 mdtgpd          index     45,
 mdtgba          index     46,
 mdtgpb          index     47,
 mdtdtr          index     48,

 odeiv1          index     49,
 odeiv2          index     50,

 omvinp          index     51,
 omvmmm          index     52,
 omvmmv          index     53,
 omvn1m          index     54,
 omvn1v          index     55,
 omvn2v          index     56,
 omvnfm          index     57,
 omvnmm          index     58,
 omvnmv          index     59,
 omvtrm          index     60,

 roobin          index     61,
 roof1r          index     62,
 roopol          index     63,
 rooqua          index     64,
 roofnr          index     65,

 sledtr          index     66,
 slegba          index     67,
 slegbal         index     68,
 slegen          index     69,
 slegenl         index     70,
 slegls          index     71,
 sleglsl         index     72,
 slegpb          index     73,
 slegpbl         index     74,
 slegpd          index     75,
 slegpdl         index     76,
 slegsy          index     77,
 slegsyl         index     78,
 slegtr          index     79,

 spebi0          index     80,
 spebi1          index     81,
 spebj0          index     82,
 spebj1          index     83,
 spebk0          index     84,
 spebk1          index     85,
 speby0          index     86,
 speby1          index     87,
 speent          index     88,
 speerf          index     89,
 speefc          index     90,
 spegam          index     91,
 spelga          index     92,
 spemax          index     93,
 spepol          index     94,
 spepow          index     95,
 spesgn          index     96,
 spears          index     97,
 spearc          index     98,
 spesih          index     99,
 specoh          index    100,
 spetah          index    101,
 speash          index    102,
 speach          index    103,
 speath          index    104,

 spl1bspv        index    105,
 spl2bspv        index    106,
 spl1bspf        index    107,
 spl2bspf        index    108,
 spl1nati        index    109,
 spl1naki        index    110,
 spl1cmpi        index    111,
 spl1peri        index    112,
 spl1pprv        index    113,
 spl1nalf        index    114,
 spl2natv        index    115,
 spl2nalf        index    116,
 dllversion      index    117,
// int1fr          index    117,                {existed twice, now used for dllversion}
 exp             index    118,
 MachCnst        index    119,
 ipffsn          index    120,
 ipfisn          index    121,
 ipfspn          index    122,
 ipfpol          index    123,
 spline          index    124,
 splineparameters index   125;

begin
end.
