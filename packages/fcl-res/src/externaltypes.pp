{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Types and constants used by external resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit externaltypes;

{$MODE OBJFPC} {$H+}

interface

type
  TExternalResMagic = array[1..6] of char;

type
  TExtHeader = packed record
    magic : TExternalResMagic;  //'FPCRES'
    version : byte;             //EXT_CURRENT_VERSION
    endianess : byte;           //EXT_ENDIAN_BIG or EXT_ENDIAN_LITTLE
    count : longword;           //resource count
    nodesize : longword;        //size of header (up to string table, excluded)
    hdrsize  : longword;        //size of header (up to string table, included)
    reserved1 : longword;
    reserved2 : longword;
    reserved3 : longword;
  end;

  TResInfoNode = packed record
    nameid : longword;          //name offset / integer ID / languageID
    ncount : longword;          //named sub-entries count
    idcountsize : longword;     //id sub-entries count / resource size
    subptr : longword;          //first sub-entry offset
  end;

const
  EXTERNAL_RESMAGIC : TExternalResMagic = 'FPCRES';
  
  EXT_CURRENT_VERSION = 1;
  
  EXT_ENDIAN_BIG = 1;
  EXT_ENDIAN_LITTLE = 2;


implementation

end.
