{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by the Free Pascal development team

    International settings for Sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure InitInternationalGeneric;
  begin
    fillchar(SysLocale,sizeof(SysLocale),0);
  end;
  
{
  $Log$
  Revision 1.1  2005-02-26 19:25:01  florian
    + initial commit
}  