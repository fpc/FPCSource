{
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
 detgen          {$ifdef win32}  index      1{$endif} ,
 detgsy          {$ifdef win32}  index      2{$endif} ,
 detgpd          {$ifdef win32}  index      3{$endif} ,
 detgba          {$ifdef win32}  index      4{$endif} ,
 detgpb          {$ifdef win32}  index      5{$endif} ,
 detgtr          {$ifdef win32}  index      6{$endif} ,

 dslgen          {$ifdef win32}  index      7{$endif} ,
 dslgtr          {$ifdef win32}  index      8{$endif} ,
 dslgsy          {$ifdef win32}  index      9{$endif} ,
 dslgpd          {$ifdef win32}  index     10{$endif} ,
 dslgba          {$ifdef win32}  index     11{$endif} ,
 dslgpb          {$ifdef win32}  index     12{$endif} ,
 dsldtr          {$ifdef win32}  index     13{$endif} ,

 eiggs1          {$ifdef win32}  index     14{$endif} ,
 eiggs2          {$ifdef win32}  index     15{$endif} ,
 eiggs3          {$ifdef win32}  index     16{$endif} ,
 eiggs4          {$ifdef win32}  index     17{$endif} ,
 eigts1          {$ifdef win32}  index     18{$endif} ,
 eigts2          {$ifdef win32}  index     19{$endif} ,
 eigts3          {$ifdef win32}  index     20{$endif} ,
 eigts4          {$ifdef win32}  index     21{$endif} ,
 eigbs1          {$ifdef win32}  index     22{$endif} ,
 eigbs2          {$ifdef win32}  index     23{$endif} ,
 eigbs3          {$ifdef win32}  index     24{$endif} ,
 eigbs4          {$ifdef win32}  index     25{$endif} ,
 eigge1          {$ifdef win32}  index     26{$endif} ,
 eigge3          {$ifdef win32}  index     27{$endif} ,
 eiggg1          {$ifdef win32}  index     28{$endif} ,
 eiggg2          {$ifdef win32}  index     29{$endif} ,
 eiggg3          {$ifdef win32}  index     30{$endif} ,
 eiggg4          {$ifdef win32}  index     31{$endif} ,
 eigsv1          {$ifdef win32}  index     32{$endif} ,
 eigsv3          {$ifdef win32}  index     33{$endif} ,

 int1fr          {$ifdef win32}  index     34{$endif} ,

 invgen          {$ifdef win32}  index     35{$endif} ,
 invgsy          {$ifdef win32}  index     36{$endif} ,
 invgpd          {$ifdef win32}  index     37{$endif} ,

 iomrev          {$ifdef win32}  index     38{$endif} ,
 iomrem          {$ifdef win32}  index     39{$endif} ,
 iomwrv          {$ifdef win32}  index     40{$endif} ,
 iomwrm          {$ifdef win32}  index     41{$endif} ,

 mdtgen          {$ifdef win32}  index     42{$endif} ,
 mdtgtr          {$ifdef win32}  index     43{$endif} ,
 mdtgsy          {$ifdef win32}  index     44{$endif} ,
 mdtgpd          {$ifdef win32}  index     45{$endif} ,
 mdtgba          {$ifdef win32}  index     46{$endif} ,
 mdtgpb          {$ifdef win32}  index     47{$endif} ,
 mdtdtr          {$ifdef win32}  index     48{$endif} ,

 odeiv1          {$ifdef win32}  index     49{$endif} ,
 odeiv2          {$ifdef win32}  index     50{$endif} ,

 omvinp          {$ifdef win32}  index     51{$endif} ,
 omvmmm          {$ifdef win32}  index     52{$endif} ,
 omvmmv          {$ifdef win32}  index     53{$endif} ,
 omvn1m          {$ifdef win32}  index     54{$endif} ,
 omvn1v          {$ifdef win32}  index     55{$endif} ,
 omvn2v          {$ifdef win32}  index     56{$endif} ,
 omvnfm          {$ifdef win32}  index     57{$endif} ,
 omvnmm          {$ifdef win32}  index     58{$endif} ,
 omvnmv          {$ifdef win32}  index     59{$endif} ,
 omvtrm          {$ifdef win32}  index     60{$endif} ,

 roobin          {$ifdef win32}  index     61{$endif} ,
 roof1r          {$ifdef win32}  index     62{$endif} ,
 roopol          {$ifdef win32}  index     63{$endif} ,
 rooqua          {$ifdef win32}  index     64{$endif} ,
 roofnr          {$ifdef win32}  index     65{$endif} ,

 sledtr          {$ifdef win32}  index     66{$endif} ,
 slegba          {$ifdef win32}  index     67{$endif} ,
 slegbal         {$ifdef win32}  index     68{$endif} ,
 slegen          {$ifdef win32}  index     69{$endif} ,
 slegenl         {$ifdef win32}  index     70{$endif} ,
 slegls          {$ifdef win32}  index     71{$endif} ,
 sleglsl         {$ifdef win32}  index     72{$endif} ,
 slegpb          {$ifdef win32}  index     73{$endif} ,
 slegpbl         {$ifdef win32}  index     74{$endif} ,
 slegpd          {$ifdef win32}  index     75{$endif} ,
 slegpdl         {$ifdef win32}  index     76{$endif} ,
 slegsy          {$ifdef win32}  index     77{$endif} ,
 slegsyl         {$ifdef win32}  index     78{$endif} ,
 slegtr          {$ifdef win32}  index     79{$endif} ,

 spebi0          {$ifdef win32}  index     80{$endif} ,
 spebi1          {$ifdef win32}  index     81{$endif} ,
 spebj0          {$ifdef win32}  index     82{$endif} ,
 spebj1          {$ifdef win32}  index     83{$endif} ,
 spebk0          {$ifdef win32}  index     84{$endif} ,
 spebk1          {$ifdef win32}  index     85{$endif} ,
 speby0          {$ifdef win32}  index     86{$endif} ,
 speby1          {$ifdef win32}  index     87{$endif} ,
 speent          {$ifdef win32}  index     88{$endif} ,
 speerf          {$ifdef win32}  index     89{$endif} ,
 speefc          {$ifdef win32}  index     90{$endif} ,
 spegam          {$ifdef win32}  index     91{$endif} ,
 spelga          {$ifdef win32}  index     92{$endif} ,
 spemax          {$ifdef win32}  index     93{$endif} ,
 spepol          {$ifdef win32}  index     94{$endif} ,
 spepow          {$ifdef win32}  index     95{$endif} ,
 spesgn          {$ifdef win32}  index     96{$endif} ,
 spears          {$ifdef win32}  index     97{$endif} ,
 spearc          {$ifdef win32}  index     98{$endif} ,
 spesih          {$ifdef win32}  index     99{$endif} ,
 specoh          {$ifdef win32}  index    100{$endif} ,
 spetah          {$ifdef win32}  index    101{$endif} ,
 speash          {$ifdef win32}  index    102{$endif} ,
 speach          {$ifdef win32}  index    103{$endif} ,
 speath          {$ifdef win32}  index    104{$endif} ,

 spl1bspv        {$ifdef win32}  index    105{$endif} ,
 spl2bspv        {$ifdef win32}  index    106{$endif} ,
 spl1bspf        {$ifdef win32}  index    107{$endif} ,
 spl2bspf        {$ifdef win32}  index    108{$endif} ,
 spl1nati        {$ifdef win32}  index    109{$endif} ,
 spl1naki        {$ifdef win32}  index    110{$endif} ,
 spl1cmpi        {$ifdef win32}  index    111{$endif} ,
 spl1peri        {$ifdef win32}  index    112{$endif} ,
 spl1pprv        {$ifdef win32}  index    113{$endif} ,
 spl1nalf        {$ifdef win32}  index    114{$endif} ,
 spl2natv        {$ifdef win32}  index    115{$endif} ,
 spl2nalf        {$ifdef win32}  index    116{$endif} ,
 dllversion      {$ifdef win32}  index    117{$endif} ,
// int1fr          {$ifdef win32}  index    117{$endif} ,                {existed twice{$endif} , now used for dllversion}
 exp             {$ifdef win32}  index    118{$endif} ,
 MachCnst        {$ifdef win32}  index    119{$endif} ,
 ipffsn          {$ifdef win32}  index    120{$endif} ,
 ipfisn          {$ifdef win32}  index    121{$endif} ,
 ipfspn          {$ifdef win32}  index    122{$endif} ,
 ipfpol          {$ifdef win32}  index    123{$endif} ,
 spline          {$ifdef win32}  index    124{$endif} ,
 splineparameters {$ifdef win32}  index   125{$endif};

begin
end.
