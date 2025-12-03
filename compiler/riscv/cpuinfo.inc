{
    Copyright (c) 1998-2002 by the Free Pascal development team

    Basic Processor information common for all RiscV variants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  bestreal = double;
  bestrealrec = TDoubleRec;
  ts32real = single;
  ts64real = double;
  ts80real = extended;
  ts128real = extended;
  ts64comp = comp;

  pbestreal = ^bestreal;

  tcpuflags =
     (CPURV_HAS_MUL,
      CPURV_HAS_ATOMIC,
      CPURV_HAS_COMPACT,
      CPURV_HAS_16REGISTERS,
      CPURV_HAS_ZBA,
      CPURV_HAS_ZBB,
      CPURV_HAS_ZBC,
      CPURV_HAS_ZBS,
      CPURV_HAS_ZBKB,
      CPURV_HAS_ZBKC,
      CPURV_HAS_ZBKX,
      CPURV_HAS_CSR_INSTRUCTIONS,   { extension Zicsr    }
      CPURV_HAS_FETCH_FENCE,        { extension Zifencei }
      CPURV_HAS_ZIHINTPAUSE,
      CPURV_HAS_F,
      CPURV_HAS_D,
      CPURV_HAS_Q,
      CPURV_HAS_ZFH,
      CPURV_HAS_ZFHMIN,
      CPURV_HAS_ZFA,
      CPURV_HAS_ZFINX,
      CPURV_HAS_ZDINX,
      CPURV_HAS_ZHINX,
      CPURV_HAS_ZHINXMIN,
      CPURV_HAS_ZICOND,
      CPURV_HAS_ZMMUL
     );

  tfputype =
    (fpu_none,
    fpu_libgcc,
    fpu_soft,
    fpu_fd
    );

const
  fputypestr: array[tfputype] of string[8] = (
    'NONE',
    'LIBGCC',
    'SOFT',
    'FD'
    );


