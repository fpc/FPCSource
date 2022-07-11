{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Pascal Ganaye,Micha Nelissen and other members of the
    Free Pascal development team

    DBF structs

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
const

//====================================================================
  FieldPropType_Required    = $01;
  FieldPropType_Min         = $02;
  FieldPropType_Max         = $03;
  FieldPropType_Default     = $04;
  FieldPropType_Constraint  = $06;

  FieldDescIII_AutoIncOffset = 19;
  FieldDescVII_AutoIncOffset = 42;

//====================================================================
// File structures
//====================================================================

type

  PDbfHdr = ^rDbfHdr;
  rDbfHdr = packed record
    VerDBF      : Byte;     // 0 DBF file type/version
    Year        : Byte;     // 1 year last updated
    Month       : Byte;     // 2 month last updated
    Day         : Byte;     // 3 day last updated
    RecordCount : Integer;  // 4-7 number of records in file
    FullHdrSize : Word;     // 8-9 size of the header in bytes
    RecordSize  : Word;     // 10-11 sum of all field sizes, including delete flag
    Dummy1      : Word;     // 12-13
    IncTrans    : Byte;     // 14
    Encrypt     : Byte;     // 15 DBase encryption flag
    MultiUse    : Integer;  // 16-19
    LastUserID  : Integer;  // 20-23
    Dummy2      : array[24..27] of Byte;
    // $01: mdx (or cdx for VFP) index file present
    // $02: (Visual FoxPro): associated memo file?
    // $04: (Visual FoxPro): is this a dbc/database container
    MDXFlag     : Byte;     // 28 Flags:
    Language    : Byte;     // 29 code page mark
    Dummy3      : Word;     // 30-31 reserved
  end;
//====================================================================
  // Data at end of the fixed part of the header for DBaseVII -
  // before the variable length part (e.g. the field descriptor array)
  PEndFixedHdrVII = ^rEndFixedHdrVII;
  rEndFixedHdrVII = packed record
    LanguageDriverName  : array[32..63] of Char; //starting position 32 of header
    Dummy               : array[64..67] of Byte;
  end;
//====================================================================
  // Data at end of header for Visual Foxpro, after the variable length part
  PEndHdrVFP = ^rEndHdrVFP;
  rEndHdrVFP = packed record
    { Relative path of an associated database (.dbc) file or filled with $00.
      If the first byte is $00, the file is a "free table", not associated with
      a database file. Therefore, database files always contain $00. }
    Backlink  : array[0..262] of Char; //263 bytees
  end;
//====================================================================
// DBase III,IV,FoxPro,VisualFoxPro field description
  PFieldDescIII = ^rFieldDescIII;
  rFieldDescIII = packed record
    FieldName         : array[0..10] of Char;
    FieldType         : Char;    // 11
    // FieldOffset: (V)FoxPro only: displacement of field in record
    // DBase III uses it for address in memory
    FieldOffset       : Integer; // 12..15
    FieldSize         : Byte;    // 16
    FieldPrecision    : Byte;    // 17, also known as decimal count
    // $01: system solumn (not user-visible)
    // $02: column can store null values
    // $04: binary column, e.g. don't interpret codepage (char/memo fields)
    // $0C: column is autoincrementing (only integer fields)
    VisualFoxProFlags : Byte;    // 18 Field Flags; flags can be combined
    // (!!not the next value for a new record!!); the next is calculated by
    // adding AutoIncrementStep first.
    // Value covers bytes 19..22 (so no WorkAreaID,Reserved1 for VFP)
    AutoIncrementNext : Byte;    // 19 VFP only: autoincrement value
    // WorkAreaID only for DBase III, is always $01
    WorkAreaID        : Byte;    // 20
    Reserved1         : array[21..22] of Byte;
    AutoIncrementStep : Byte;    // 23 VFP only: step value for autoincrement
    Reserved2         : array[24..30] of Byte;
    // DBase IV:
    // $00: no key for this field;
    // $01: key exists for this field in MDX index file
    // todo: implement this??
    MDXIndexField     : Byte;    //31
  end;
//====================================================================
// OH 2000-11-15 dBase7 support. Header Update (add fields like Next AutoInc Value)
  rFieldDescVII = packed record
    FieldName      : array [0..31] of Char;
    FieldType      : Char;  // 32
    FieldSize      : Byte;  // 33
    FieldPrecision : Byte;  // 34
    Reserved1      : Word;  // 35-36
    MDXFlag        : Byte;  // 37
    // NOTE: the docs say Reserved2 is 2 bytes, and Reserved3 is 4 bytes
    //   but testing shows BDE has them the other way around
    //   be BDE compatible :S
    Reserved2      : Cardinal; // 38-41
    NextAutoInc    : Cardinal; // 42-45
    Reserved3      : Word; // 46-47
  end;
//====================================================================
  PFieldPropsHdr = ^rFieldPropsHdr;
  rFieldPropsHdr = packed record
    NumStdProps      : Word;  // 0..1
    StartStdProps    : Word;  // 2..3
    NumCustomProps   : Word;  // 4..5
    StartCustomProps : Word;  // 6..7
    NumRIProps       : Word;  // 8..9
    StartRIProps     : Word;  // 10..11
    StartData        : Word;  // 12..13 ; this points past the Descriptor arrays to data used by the arrays - for example Custom property names are stored here.
    Size             : Word;  // 14..15 ; Actual size of structure, including data
  end;
//====================================================================
  PStdPropEntry = ^rStdPropEntry;
  rStdPropEntry = packed record
    GenNumber    : Word;  // 0..1   ; Generational number. More than one value may exist for a property. The current value is the value with the highest generational number.
    FieldOffset  : Word;  // 2..3   ; Table field offset - base one. 01 for the first field in the table, 02 for the second field, etc. Note: this will be 0 in the case of a constraint.
    PropType     : Byte;  // 4      ; Which property is described in this record:
                          //            01 Required
                          //            02 Min
                          //            03 Max
                          //            04 Default
                          //            06 Database constraint
    FieldType    : Byte;  // 5      ; Field Type:
                          //            00 No type - constraint
                          //            01 Char
                          //            02 Numeric
                          //            03 Memo
                          //            04 Logical
                          //            05 Date
                          //            06 Float
                          //            08 OLE
                          //            09 Binary
                          //            11 Long
                          //            12 Timestamp
                          //            13 Double
                          //            14 AutoIncrement (not settable from the Inspector)
    IsConstraint : Byte;  // 6      ; 0x00 if the array element is a constraint, 0x02 otherwise.
    Reserved     : array[7..10] of Char;
    DataOffset   : Word;  // 11..12 ; Offset from the start of this structure to the data for the property. The Required property has no data associated with it, so it is always 0.
    DataSize     : Word;  // 13..14 ; Width of database field associated with the property, and hence size of the data (includes 0 terminator in the case of a constraint).
  end;
//====================================================================
  PCustomPropEntry = ^rCustomPropEntry;
  rCustomPropEntry = packed record
    GenNumber    : Word;  // 0..1   ; same as standard
    FieldOffset  : Word;  // 2..3   ; same as standard
    FieldType    : Byte;  // 4      ; same as standard
    Reserved     : Byte;  // 5
    NameOffset   : Word;  // 6..7   ; Offset from the start of this structure to the Custom property name.
    NameLength   : Word;  // 8..9   ; Length of the Custom property name.
    DataOffset   : Word;  // 10..11 ; Offset from the start of this structure to the Custom property data.
    DataLength   : Word;  // 12..13 ; Length of the Custom property data (does not include null terminator).
  end;
//====================================================================
  PRIPropEntry = ^rRIPropEntry;
  rRIPropEntry = packed record
    RelationType    : Byte;  // 0      ; 0x07 if Master (parent), 0x08 if Dependent (child).
    Number          : Word;  // 1..2   ; Sequential number, 1 based counting. If this number is 0, this RI rule has been dropped.
    NameOffset      : Word;  // 3..4   ; Offset of the RI rule name - 0 terminated.
    NameSize        : Word;  // 5..6   ; Size of ...
    ForeignOffset   : Word;  // 7..8   ; Offset of the name of the Foreign Table - 0 terminated.
    ForeignSize     : Word;  // 9..10  ; Size of ...
    UpdateType      : Byte;  // 11     ; Update & delete behaviour: Update Cascade=0x10, Delete Cascade=0x01
    NumFieldsKey    : Word;  // 12..13 ; Number of fields in the linking key.
    LocalTagOffset  : Word;  // 14..15 ; Offset of the Local Table tag name - 0 terminated.
    LocalTagSize    : Word;  // 16..17 ; Size of ...
    ForeignTagOffset: Word;  // 18..19 ; Offset of the Foreign Table tag name - 0 terminated.
    ForeignTagSize  : Word;  // 20..21 ; Size of ...
  end;

