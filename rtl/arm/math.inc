{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Implementation of mathematical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:in_abs_extended];
    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];
    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : extended) : extended;[internproc:in_sqrt_extended];
    {$define FPC_SYSTEM_HAS_ARCTAN}
    function arctan(d : extended) : extended;[internproc:in_arctan_extended];
    { lgn isn't supported by the linux fpe it seems
    {$define FPC_SYSTEM_HAS_LN}
    function ln(d : extended) : extended;[internproc:in_ln_extended];
    }
    {$define FPC_SYSTEM_HAS_SIN}
    function sin(d : extended) : extended;[internproc:in_sin_extended];
    {$define FPC_SYSTEM_HAS_COS}
    function cos(d : extended) : extended;[internproc:in_cos_extended];

{
  $Log$
  Revision 1.4  2004-03-16 22:02:26  florian
    * disabled internal ln

  Revision 1.3  2004/01/27 15:04:49  florian
    * some math nodes are inlined now

  Revision 1.2  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.1  2003/08/21 16:41:54  florian
    * empty dummy files
    + [long|set]jmp implemented
}
