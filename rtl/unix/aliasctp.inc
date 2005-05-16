{
    $Id: aliasctp.inc,v 1.5 2005/03/16 22:26:12 florian Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Marco van de Voort
    Member of the Free Pascal development team

    Aliases for basic types for C interfacing, for reloading them
    in other units from unit unixtype in a typesafe way.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
    cInt8   = UnixType.cInt8;
    cUInt8  = UnixType.cUInt8;
    cUInt16 = UnixType.cUInt16;
    cInt16  = UnixType.cInt16;
    cInt32  = UnixType.cInt32;
    cUInt32 = UnixType.cUInt32;
    cInt64  = UnixType.cInt64;
    cUInt64 = UnixType.cUInt64;
    cuchar  = UnixType.cuchar;
    cchar   = UnixType.cchar;
    cInt    = UnixType.cInt;
    cUInt   = UnixType.cUInt;
    cLong   = UnixType.cLong;
    cuLong  = UnixType.cuLong;
    cshort  = UnixType.cshort;
    cushort = UnixType.cushort;
    cunsigned = UnixType.cunsigned;
    pcuchar = UnixType.pcuchar;
    pcchar  = UnixType.pcchar;
    pcInt   = UnixType.pcInt;
    pcUInt  = UnixType.pcUInt;
    pcLong  = UnixType.pcLong;
    pculong = UnixType.pculong;
    pcshort = UnixType.pcshort;
    pcushort= UnixType.pcushort;
    pcunsigned = UnixType.pcunsigned;

    { Floating point }
    cFloat    = UnixType.cFloat;
    cDouble   = UnixType.cDouble;
    clDouble  = UnixType.clDouble;
    pcFloat   = UnixType.pcFloat;
    pcDouble  = UnixType.pcDouble;
    pclDouble = UnixType.pclDouble;
{
   $Log: aliasctp.inc,v $
   Revision 1.5  2005/03/16 22:26:12  florian
     + ansi<->wide implemented using iconv

   Revision 1.4  2005/03/13 10:05:13  florian
     + floating point c types added

   Revision 1.3  2005/02/14 17:13:31  peter
     * truncate log

   Revision 1.2  2005/02/05 23:02:37  florian
     + added some missing c types

}

