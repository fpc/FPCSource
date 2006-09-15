{
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
    csint   = UnixType.csint;
    csigned = UnixType.csigned;
    cunsigned = UnixType.cunsigned;
    cbool   = UnixType.cbool;

    pcint8  = UnixType.pcint8;
    pcuint8 = UnixType.pcuint8;
    pcint16 = UnixType.pcint16;
    pcuint16= UnixType.pcuint16;
    pcint32 = UnixType.pcint32;
    pcuint32= UnixType.pcuint32;
    pcint64 = UnixType.pcint64;
    pcuint64= UnixType.pcuint64;
    pcuchar = UnixType.pcuchar;
    pcchar  = UnixType.pcchar;
    pcInt   = UnixType.pcInt;
    pcUInt  = UnixType.pcUInt;
    pcLong  = UnixType.pcLong;
    pculong = UnixType.pculong;
    pcshort = UnixType.pcshort;
    pcushort= UnixType.pcushort;
    pcsint  = UnixType.pcsint;
    pcsigned = UnixType.pcsigned;
    pcunsigned = UnixType.pcunsigned;
    pcbool  = UnixType.pcbool;

    { Floating point }
    cFloat    = UnixType.cFloat;
    cDouble   = UnixType.cDouble;
    clDouble  = UnixType.clDouble;
    pcFloat   = UnixType.pcFloat;
    pcDouble  = UnixType.pcDouble;
    pclDouble = UnixType.pclDouble;

