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

function fpNice(N:cint):cint;
Function fpGetPriority(Which,Who:cint):cint;
Function fpSetPriority(Which,Who,What:cint):cint;
Function fpLstat(path:pchar;Info:pstat):cint;
Function fpLstat(Filename: ansistring;Info:pstat):cint;
Function fpSymlink(oldname,newname:pchar):cint;
Function fpReadLink(name,linkname:pchar;maxlen:cint):cint;
Function Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; external name  'FPC_SYSC_MMAP';
Function Fpmunmap(start:pointer;len:size_t):cint;  external name 'FPC_SYSC_MUNMAP';
function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';

{
  $Log$
  Revision 1.6  2004-11-03 15:00:43  marco
   * Pathstr eliminated

  Revision 1.5  2004/01/01 17:07:21  marco
   * few small freebsd fixes backported from debugging linux

  Revision 1.4  2003/12/30 12:26:21  marco
   * FPC_USE_LIBC

}
