{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 2003 Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';

{
  $Log$
  Revision 1.7  2004-11-14 12:21:08  marco
   * moved some calls from unix to baseunix. Darwin untested.

  Revision 1.6  2004/11/03 15:00:43  marco
   * Pathstr eliminated

  Revision 1.5  2004/01/01 17:07:21  marco
   * few small freebsd fixes backported from debugging linux

  Revision 1.4  2003/12/30 12:26:21  marco
   * FPC_USE_LIBC

}
