{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Types used by COFF resource reader and writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cofftypes;

{$MODE OBJFPC}

interface

type
  TCoffMachineType = (cmti386, cmtarm, cmtx8664);

type
  TSectionName = array [0..7] of char;

const
  RSRCSectName : TSectionName = '.rsrc'+#0+#0+#0;

type
  TCoffHeader = packed record
    Machine : word;
    NumSects : word;
    TimeStamp : longword;
    SymTablePtr : longword;
    SymNum : longword;
    OptHdrSize : word;
    Characteristics : word;
  end;

  TCoffSectionHeader = packed record
    Name : TSectionName;
    VirtualSize : longword;
    VirtualAddress : longword;
    SizeOfRawData : longword;
    PointerToRawData : longword;
    PointerToRelocations : longword;
    PointerToLineNumbers : longword;
    NumberOfRelocations : word;
    NumberOfLineNumbers : word;
    Characteristics : longword;
  end;

  TCoffSectionTable = packed record
    Name : TSectionName;
    Value : longword;
    SectionNumber : word;
    _type : word;
    StorageClass : byte;
    NumAuxSymbol : byte;
  end;

  TResDirTable = packed record
    Characteristics : longword;
    TimeStamp : longword;
    VerMajor : word;
    VerMinor : word;
    NamedEntriesCount : word;
    IDEntriesCount : word;
  end;

  TResDirEntry = packed record
    NameID : longword;
    DataSubDirRVA : longword;
  end;

  TResDataEntry = packed record
    DataRVA : longword;
    Size : longword;
    Codepage : longword;
    Reserved : longword;
  end;

implementation

end.
