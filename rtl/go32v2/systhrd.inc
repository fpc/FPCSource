{
    $Id: systhrd.inc,v 1.1 2005/02/06 16:57:18 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    Linux (pthreads) threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Procedure InitSystemThreads;
begin
  { This should be changed to a real value during
    thread driver initialization if appropriate. }
  ThreadID := 1;
  SetNoThreadManager;
end;

{
  $Log: systhrd.inc,v $
  Revision 1.1  2005/02/06 16:57:18  peter
    * threads for go32v2,os,emx,netware

  Revision 1.1  2005/02/06 12:16:52  peter
    * bsd thread updates

  Revision 1.1  2005/02/06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

}

