{
    $Id$
    Copyright (c) 2002 by Marco van de Voort

    deeper calls exported by unit unix    

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

//Function Fpmmap(adr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;off:off_t):pointer; external name 'FPC_SYSC_MMAP';
//Function Fpmunmap(adr:pointer;len:size_t):cint; external name 'FPC_SYSC_MUNMAP';

function fpgettimeofday(tp: ptimeval;tzp:ptimezone):cint; external name 'FPC_SYSC_GETTIMEOFDAY';
//function fpNice(N:cint):cint;
//Function fpGetPriority(Which,Who:cint):cint;
//Function fpSetPriority(Which,Who,What:cint):cint;
//Function fpSymlink(oldname,newname:pchar):cint;
//Function fpReadLink(name,linkname:pchar;maxlen:size_t):cint;

{
  $Log$
  Revision 1.7  2004-11-14 12:21:08  marco
   * moved some calls from unix to baseunix. Darwin untested.

  Revision 1.6  2004/11/03 15:00:43  marco
   * Pathstr eliminated

  Revision 1.5  2004/01/01 16:10:23  marco
   * fpreadlink(pathstr) moved to unxovl (since not platform specific),
  	small fixes for "make all OPT='-dFPC_USE_LIBC'

  Revision 1.4  2003/11/19 11:46:55  marco
   * changes due to the previous *BSD changes. Mainly moving constants from
     unix to systypes.inc (which acts as unxtypes.inc)


}