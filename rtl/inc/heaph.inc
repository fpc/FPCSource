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

  var
     heaporg,heapptr,heapend,heaperror,freelist : pointer;

  procedure release(var p : pointer);
  procedure mark(var p : pointer);
  procedure markheap(var oldfreelist,oldheapptr : pointer);
  procedure releaseheap(oldfreelist,oldheapptr : pointer);
  function  cal_memavail : longint;
  function  heapsize : longint;
{$ifdef TEMPHEAP}
  procedure split_heap;
  procedure switch_to_base_heap;
  procedure switch_to_temp_heap;
  procedure switch_heap;
  procedure releasetempheap;
  procedure gettempmem(var p : pointer;size : longint);
{$endif TEMPHEAP}

  const
     allow_special : boolean =true;

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.3  1998/01/26 11:59:33  michael
  + Added log at the end


  
  Working file: rtl/inc/heaph.inc
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/01 12:08:03;  author: michael;  state: Exp;  lines: +11 -3
  + added copyright reference header.
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:46;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:46;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
