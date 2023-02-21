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
{$MACRO ON}

{$IFDEF FPC_DOTTEDUNITS}
{$define UT:=UnixApi.Types}
{$ELSE}
{$define UT:=UnixType}
{$ENDIF}

type
  cint8                  = UT.cint8;       pcint8                 = UT.pcint8;
  cuint8                 = UT.cuint8;      pcuint8                = UT.pcuint8;
  cchar                  = UT.cchar;       pcchar                 = UT.pcchar;
  cschar                 = UT.cschar;      pcschar                = UT.pcschar;
  cuchar                 = UT.cuchar;      pcuchar                = UT.pcuchar;

  cint16                 = UT.cint16;      pcint16                = UT.pcint16;
  cuint16                = UT.cuint16;     pcuint16               = UT.pcuint16;
  cshort                 = UT.cshort;      pcshort                = UT.pcshort;
  csshort                = UT.csshort;     pcsshort               = UT.pcsshort;
  cushort                = UT.cushort;     pcushort               = UT.pcushort;

  cint32                 = UT.cint32;      pcint32                = UT.pcint32;
  cuint32                = UT.cuint32;     pcuint32               = UT.pcuint32;
  cint                   = UT.cint;        pcint                  = UT.pcint;
  csint                  = UT.csint;       pcsint                 = UT.pcsint;
  cuint                  = UT.cuint;       pcuint                 = UT.pcuint;
  csigned                = UT.csigned;     pcsigned               = UT.pcsigned;
  cunsigned              = UT.cunsigned;   pcunsigned             = UT.pcunsigned;

  cint64                 = UT.cint64;      pcint64                = UT.pcint64;
  cuint64                = UT.cuint64;     pcuint64               = UT.pcuint64;
  clonglong              = UT.clonglong;   pclonglong             = UT.pclonglong;
  cslonglong             = UT.cslonglong;  pcslonglong            = UT.pcslonglong;
  culonglong             = UT.culonglong;  pculonglong            = UT.pculonglong;

  cbool                  = UT.cbool;       pcbool                 = UT.pcbool;

  clong                  = UT.clong;       pclong                 = UT.pclong;
  cslong                 = UT.cslong;      pcslong                = UT.pcslong;
  culong                 = UT.culong;      pculong                = UT.pculong;

{$ifndef FPUNONE}
  cfloat                 = UT.cfloat;      pcfloat                = UT.pcfloat;
  cdouble                = UT.cdouble;     pcdouble               = UT.pcdouble;
//  clongdouble            = UT.clongdouble; pclongdouble           = UT.pclongdouble;
{$endif}

  csize_t                = UT.size_t;      pcsize_t               = UT.psize_t;

  coff_t                 = UT.TOff;

