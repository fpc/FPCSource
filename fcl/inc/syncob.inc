{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TCriticalSection.Enter;

  begin
     Acquire;
  end;

procedure TCriticalSection.Leave;

  begin
     Release;
  end;

{
  $Log$
  Revision 1.3  2000-01-06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:08  peter
    * moved to packages dir

  Revision 1.1  1999/06/07 15:51:51  michael
  + Renamed to syncob

  Revision 1.2  1998/09/30 13:41:04  florian
    * fixes to make it compilable

  Revision 1.1  1998/09/29 11:14:25  florian
    + initial revision

}
