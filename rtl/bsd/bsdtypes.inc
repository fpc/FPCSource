{
    $Id$
    Copyright (c) 2000-2002 by Marco van de Voort

    Some non POSIX BSD types used internally in the system unit.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

Type
  timeval  = packed record
    tv_sec,tv_usec:clong;
  end;
  ptimeval = ^timeval;
  TTimeVal = timeval;

  timespec = packed record
    tv_sec   : time_t;
    tv_nsec  : clong;
  end;

  timezone = packed record
    minuteswest,
    dsttime  : cint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

{
 $Log$
 Revision 1.3  2002-10-27 11:58:30  marco
  * Modifications from Saturday.

 Revision 1.2  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.1  2002/08/19 12:29:11  marco
  * First working POSIX *BSD system unit.


}
