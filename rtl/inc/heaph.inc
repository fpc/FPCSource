{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Procedure release(var p : pointer);
Procedure mark(var p : pointer);
Procedure markheap(var oldfreelist,oldheapptr : pointer);
Procedure releaseheap(oldfreelist,oldheapptr : pointer);
Function  cal_memavail : longint;
Function  heapsize : longint;
{$ifdef TEMPHEAP}
  Procedure split_heap;
  Procedure switch_to_base_heap;
  Procedure switch_to_temp_heap;
  Procedure switch_heap;
  Procedure releasetempheap;
  Procedure gettempmem(var p : pointer;size : longint);
{$endif TEMPHEAP}

const
  allow_special : boolean =true;
  heapblocks    : boolean=false;
var
  heaporg,heapptr,heapend,heaperror,freelist : pointer;

{
  $Log$
  Revision 1.5  1998-07-02 14:11:30  michael
  Reinstated the heapsize function.

  Revision 1.3  1998/05/12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

  Revision 1.2  1998/04/21 10:23:15  peter
    + heapblocks
}
