{
    This file is part of the Free Pascal run time library.

    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This unit provides the same Functionality as the TypInfo Unit }
{ of Delphi                                                     }

unit TypInfo;

  interface

{$MODE objfpc}
{$MODESWITCH AdvancedRecords}
{$inline on}
{$macro on}
{$h+}

  uses SysUtils;


// temporary types:

    type

{$MINENUMSIZE 1   this saves a lot of memory }
{$ifdef FPC_RTTI_PACKSET1}
{ for Delphi compatibility }
{$packset 1}
{$endif}

       { this alias and the following constant aliases are for backwards
         compatibility before TTypeKind was moved to System unit }
       TTypeKind = System.TTypeKind;

    const

       tkUnknown = System.tkUnknown;
       tkInteger = System.tkInteger;
       tkChar = System.tkChar;
       tkEnumeration = System.tkEnumeration;
       tkFloat = System.tkFloat;
       tkSet = System.tkSet;
       tkMethod = System.tkMethod;
       tkSString = System.tkSString;
       tkLString = System.tkLString;
       tkAString = System.tkAString;
       tkWString = System.tkWString;
       tkVariant = System.tkVariant;
       tkArray = System.tkArray;
       tkRecord = System.tkRecord;
       tkInterface = System.tkInterface;
       tkClass = System.tkClass;
       tkObject = System.tkObject;
       tkWChar = System.tkWChar;
       tkBool = System.tkBool;
       tkInt64 = System.tkInt64;
       tkQWord = System.tkQWord;
       tkDynArray = System.tkDynArray;
       tkInterfaceRaw = System.tkInterfaceRaw;
       tkProcVar = System.tkProcVar;
       tkUString = System.tkUString;
       tkUChar = System.tkUChar;
       tkHelper = System.tkHelper;
       tkFile = System.tkFile;
       tkClassRef = System.tkClassRef;
       tkPointer = System.tkPointer;

    type

       TOrdType  = (otSByte,otUByte,otSWord,otUWord,otSLong,otULong,otSQWord,otUQWord);

{$ifndef FPUNONE}
       TFloatType = (ftSingle,ftDouble,ftExtended,ftComp,ftCurr);
{$endif}
       TMethodKind = (mkProcedure,mkFunction,mkConstructor,mkDestructor,
                      mkClassProcedure,mkClassFunction,mkClassConstructor,
                      mkClassDestructor,mkOperatorOverload);
       TParamFlag     = (pfVar,pfConst,pfArray,pfAddress,pfReference,pfOut,pfConstRef
                         {$ifndef VER3_0},pfHidden,pfHigh,pfSelf,pfVmt,pfResult{$endif VER3_0}
                         );
       TParamFlags    = set of TParamFlag;
       TIntfFlag      = (ifHasGuid,ifDispInterface,ifDispatch,ifHasStrGUID);
       TIntfFlags     = set of TIntfFlag;
       TIntfFlagsBase = set of TIntfFlag;

       // don't rely on integer values of TCallConv since it includes all conventions
       // which both Delphi and FPC support. In the future Delphi can support more and
       // FPC's own conventions will be shifted/reordered accordingly
       TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall,
                    ccCppdecl, ccFar16, ccOldFPCCall, ccInternProc,
                    ccSysCall, ccSoftFloat, ccMWPascal);

{$push}
{$scopedenums on}
       TSubRegister = (
         None,
         Lo,
         Hi,
         Word,
         DWord,
         QWord,
         FloatSingle,
         FloatDouble,
         FloatQuad,
         MultiMediaSingle,
         MultiMediaDouble,
         MultiMediaWhole,
         MultiMediaX,
         MultiMediaY
       );

       TRegisterType = (
         Invalid,
         Int,
         FP,
         MMX,
         MultiMedia,
         Special,
         Address
       );
{$pop}

{$MINENUMSIZE DEFAULT}

   const
      ptField = 0;
      ptStatic = 1;
      ptVirtual = 2;
      ptConst = 3;

      RTTIFlagVisibilityMask   = 3;
      RTTIFlagStrictVisibility = 1 shl 2;

   type
      TTypeKinds = set of TTypeKind;
      ShortStringBase = string[255];

      PParameterLocation = ^TParameterLocation;
      TParameterLocation =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        LocType: Byte;
        Function GetRegType: TRegisterType; inline;
        Function GetReference: Boolean; inline;
        Function GetShiftVal: Int8; inline;
      public
        RegSub: TSubRegister;
        RegNumber: Word;
        { Stack offset if Reference, ShiftVal if not }
        Offset: SizeInt;
        { if Reference then the register is the index register otherwise the
          register in wihch (part of) the parameter resides }
        property Reference: Boolean read GetReference;
        property RegType: TRegisterType read GetRegType;
        { if Reference, otherwise 0 }
        property ShiftVal: Int8 read GetShiftVal;
      end;

      PParameterLocations = ^TParameterLocations;
      TParameterLocations =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetLocation(aIndex: Byte): PParameterLocation; inline;
        Function GetTail: Pointer; inline;
      public
        Count: Byte;
        property Location[Index: Byte]: PParameterLocation read GetLocation;
        property Tail: Pointer read GetTail;
      end;

      PVmtFieldClassTab = ^TVmtFieldClassTab;
      TVmtFieldClassTab =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
        Count: Word;
        ClassRef: array[0..0] of PClass;
      end;

      PVmtFieldEntry = ^TVmtFieldEntry;
      TVmtFieldEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetNext: PVmtFieldEntry; inline;
        Function GetTail: Pointer; inline;
      public
        FieldOffset: PtrUInt;
        TypeIndex: Word;
        Name: ShortString;
        property Tail: Pointer read GetTail;
        property Next: PVmtFieldEntry read GetNext;
      end;
      {The following is copied from the TObject.FieldAddress Function.
       If it is changed there, change it here as well ! }


      { TExtendedVmtFieldEntry }

      PExtendedVmtFieldEntry = ^TExtendedVmtFieldEntry;
      PExtendedFieldEntry = PExtendedVmtFieldEntry; // For records, there is no VMT, but currently the layout is identical
      TExtendedVmtFieldEntry = 
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT} 
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT} 
      record
      Private
        Function GetNext: PVmtFieldEntry;
        Function GetStrictVisibility: Boolean;
        Function GetTail: Pointer;
        Function GetVisibility: TVisibilityClass;
      Public
        FieldOffset: SizeUInt;
        FieldType: Pointer; // PPTypeInfo;
        Flags : Byte;
        Name: PShortString;
        Property FieldVisibility: TVisibilityClass Read GetVisibility;
        Property StrictVisibility : Boolean Read GetStrictVisibility;
        property Tail: Pointer read GetTail;
        property Next: PVmtFieldEntry read GetNext;
      end;

      PVmtExtendedFieldTable = ^TVmtExtendedFieldTable;
      PExtendedFieldTable = PVmtExtendedFieldTable; // For records, there is no VMT, but currently the layout is identical.

      { TVmtExtendedFieldTable }

      TVmtExtendedFieldTable = 
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT} 
      packed 
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT} 
      record
      private
        Function GetField(aIndex: Word): PExtendedVmtFieldEntry;
        Function GetTail: Pointer;
      Public
        FieldCount: Word;
        // Fields: array[0..0] of TExtendedFieldInfo;
        property Field[aIndex: Word]: PExtendedVmtFieldEntry read GetField;
        property Tail : Pointer Read GetTail;
      end;

      PExtendedFieldInfoTable = ^TExtendedFieldInfoTable;
      TExtendedFieldInfoTable = array[0..{$ifdef cpu16}(32768 div sizeof(PPropInfo))-2{$else}65535{$endif}] of PExtendedVmtFieldEntry;

      PVmtFieldTable = ^TVmtFieldTable;

      { TVmtFieldTable }

      TVmtFieldTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetField(aIndex: Word): PVmtFieldEntry;
        Function GetNext: Pointer;
        Function GetTail: Pointer;
      public
        Count: Word;
        ClassTab: PVmtFieldClassTab;
        { should be array[Word] of TFieldInfo;  but
          Elements have variant size! force at least proper alignment }
        Fields: array[0..0] of TVmtFieldEntry;
        property Field[aIndex: Word]: PVmtFieldEntry read GetField;
        property Tail : Pointer Read GetTail;
        property Next : Pointer Read GetNext;
      end;

{$PACKRECORDS 1}

      TTypeInfo = record
         Kind : TTypeKind;
         Name : ShortString;
         // here the type data follows as TTypeData record
      end;

      PTypeInfo = ^TTypeInfo;
      PPTypeInfo = ^PTypeInfo;

      PPropData = ^TPropData;

{ Note: these are only for backwards compatibility. New type references should
        only use PPTypeInfo directly! }
{$ifdef ver3_0}
{$define TypeInfoPtr := PTypeInfo}
{$else}
{$define TypeInfoPtr := PPTypeInfo}
{$endif}

{$PACKRECORDS C}

{$if not defined(VER3_0) and not defined(VER3_2)}
{$define PROVIDE_ATTR_TABLE}
{$endif}

      TAttributeProc = Function : TCustomAttribute;

      TAttributeEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif}
      record
        AttrType: PPTypeInfo;
        AttrCtor: CodePointer;
        AttrProc: TAttributeProc;
        ArgLen: Word;
        ArgData: Pointer;
      end;

{$ifdef CPU16}
      TAttributeEntryList = array[0..(High(SizeUInt) div SizeOf(TAttributeEntry))-1] of TAttributeEntry;
{$else CPU16}
      TAttributeEntryList = array[0..$ffff] of TAttributeEntry;
{$endif CPU16}

      TAttributeTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif}
      record
        AttributeCount: word;
        AttributesList: TAttributeEntryList;
      end;
      PAttributeTable = ^TAttributeTable;

      // members of TTypeData
      TArrayTypeData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetElType: PTypeInfo; inline;
        Function GetDims(aIndex: Byte): PTypeInfo; inline;
      public
        property ElType: PTypeInfo read GetElType;
        property Dims[Index: Byte]: PTypeInfo read GetDims;
      public
        Size: SizeInt;
        ElCount: SizeInt;
        ElTypeRef: TypeInfoPtr;
        DimCount: Byte;
        DimsRef: array[0..255] of TypeInfoPtr;
      end;

      PManagedField = ^TManagedField;
      TManagedField =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetTypeRef: PTypeInfo; inline;
      public
        property TypeRef: PTypeInfo read GetTypeRef;
      public
        TypeRefRef: TypeInfoPtr;
        FldOffset: SizeInt;
      end;

      PInitManagedField = ^TInitManagedField;
      TInitManagedField = TManagedField;

      PProcedureParam = ^TProcedureParam;
      TProcedureParam =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetParamType: PTypeInfo; inline;
        Function GetFlags: Byte; inline;
      public
        property ParamType: PTypeInfo read GetParamType;
        property Flags: Byte read GetFlags;
      public
        ParamFlags: TParamFlags;
        ParamTypeRef: TypeInfoPtr;
        Name: ShortString;
      end;

      TProcedureSignature =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetResultType: PTypeInfo; inline;
      public
        property ResultType: PTypeInfo read GetResultType;
      public
        Flags: Byte;
        CC: TCallConv;
        ResultTypeRef: TypeInfoPtr;
        ParamCount: Byte;
        {Params: array[0..ParamCount - 1] of TProcedureParam;}
        Function GetParam(ParamIndex: Integer): PProcedureParam;
      end;

      PVmtMethodParam = ^TVmtMethodParam;
      TVmtMethodParam =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetTail: Pointer; inline;
        Function GetNext: PVmtMethodParam; inline;
        Function GetName: ShortString; inline;
      public
        ParamType: PPTypeInfo;
        Flags: TParamFlags;
        NamePtr: PShortString;
        ParaLocs: PParameterLocations;
        property Name: ShortString read GetName;
        property Tail: Pointer read GetTail;
        property Next: PVmtMethodParam read GetNext;
      end;

      PIntfMethodEntry = ^TIntfMethodEntry;
      TIntfMethodEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetParam(Index: Word): PVmtMethodParam;
        Function GetResultLocs: PParameterLocations; inline;
        Function GetTail: Pointer; inline;
        Function GetNext: PIntfMethodEntry; inline;
        Function GetName: ShortString; inline;
      public
        ResultType: PPTypeInfo;
        CC: TCallConv;
        Kind: TMethodKind;
        ParamCount: Word;
        StackSize: SizeInt;
        NamePtr: PShortString;
        { Params: array[0..ParamCount - 1] of TVmtMethodParam }
        { ResultLocs: PParameterLocations (if ResultType != Nil) }
        property Name: ShortString read GetName;
        property Param[Index: Word]: PVmtMethodParam read GetParam;
        property ResultLocs: PParameterLocations read GetResultLocs;
        property Tail: Pointer read GetTail;
        property Next: PIntfMethodEntry read GetNext;
      end;

      PIntfMethodTable = ^TIntfMethodTable;
      TIntfMethodTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetMethod(Index: Word): PIntfMethodEntry;
      public
        Count: Word;
        { $FFFF if there is no further info, or the value of Count }
        RTTICount: Word;
        { Entry: array[0..Count - 1] of TIntfMethodEntry }
        property Method[Index: Word]: PIntfMethodEntry read GetMethod;
      end;

      PVmtMethodEntry = ^TVmtMethodEntry;
      TVmtMethodEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
        Name: PShortString;
        CodeAddress: CodePointer;
      end;

      PVmtMethodTable = ^TVmtMethodTable;
      TVmtMethodTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetEntry(Index: LongWord): PVmtMethodEntry; inline;
      public
        Count: LongWord;
        property Entry[Index: LongWord]: PVmtMethodEntry read GetEntry;
      private
        Entries: array[0..0] of TVmtMethodEntry;
      end;

      PVmtMethodExEntry = ^TVmtMethodExEntry;

      { TVmtMethodExEntry }

      TVmtMethodExEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetParamsStart : PByte; inline;
        Function GetMethodVisibility: TVisibilityClass;
        Function GetParam(Index: Word): PVmtMethodParam;
        Function GetResultLocs: PParameterLocations; inline;
        Function GetStrictVisibility: Boolean;
        Function GetTail: Pointer; inline;
        Function GetNext: PVmtMethodExEntry; inline;
        Function GetName: ShortString; inline;
      public
        ResultType: PPTypeInfo;
        CC: TCallConv;
        Kind: TMethodKind;
        ParamCount: Word;
        StackSize: SizeInt;
        NamePtr: PShortString;
        Flags : Byte;
        VmtIndex : Smallint;
        { Params: array[0..ParamCount - 1] of TVmtMethodParam }
        { ResultLocs: PParameterLocations (if ResultType != Nil) }
        property Name: ShortString read GetName;
        property Param[Index: Word]: PVmtMethodParam read GetParam;
        property ResultLocs: PParameterLocations read GetResultLocs;
        property Tail: Pointer read GetTail;
        property Next: PVmtMethodExEntry read GetNext;
        Property MethodVisibility: TVisibilityClass Read GetMethodVisibility;
        Property StrictVisibility : Boolean Read GetStrictVisibility;
      end;


      PVmtMethodExTable = ^TVmtMethodExTable;

      { TVmtMethodExTable }

      TVmtMethodExTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetMethod(Index: Word): PVmtMethodExEntry;
      public
        // LegacyCount,Count1: Word;
        Count: Word;
        { Entry: array[0..Count - 1] of TVmtMethodExEntry }
        property Method[Index: Word]: PVmtMethodExEntry read GetMethod;
      end;

      PExtendedMethodInfoTable = ^TExtendedMethodInfoTable;
      TExtendedMethodInfoTable = array[0..{$ifdef cpu16}(32768 div sizeof(PPropInfo))-2{$else}65535{$endif}] of PVmtMethodExEntry;

      TRecOpOffsetEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
        ManagementOperator: CodePointer;
        FieldOffset: SizeUInt;
      end;

      TRecOpOffsetTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
        Count: LongWord;
        Entries: array[0..0] of TRecOpOffsetEntry;
      end;
      PRecOpOffsetTable = ^TRecOpOffsetTable;

      PRecInitData = ^TRecInitData;

      { TRecInitData }

      TRecInitData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
        Public
        {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
        {$endif}
        case TTypeKind of
          tkRecord: (
            Terminator: Pointer;
            Size: Longint;
{$ifndef VER3_0}
            InitOffsetOp: PRecOpOffsetTable;
            ManagementOp: Pointer;
{$endif}
            ManagedFieldCount: Longint;
          { ManagedFields: array[0..ManagedFieldCount - 1] of TInitManagedField ; }
          );
          { include for proper alignment }
          tkInt64: (
            dummy : Int64
          );
      end;

      PRecMethodParam = PVmtMethodParam;
      TRecMethodParam = TVmtMethodParam;
      PRecMethodExEntry = ^TRecMethodExEntry;

      { TRecMethodExEntry }

      TRecMethodExEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetParamsStart : PByte; inline;
        Function GetMethodVisibility: TVisibilityClass;
        Function GetParam(Index: Word): PRecMethodParam;
        Function GetResultLocs: PParameterLocations; inline;
        Function GetStrictVisibility: Boolean;
        Function GetTail: Pointer; inline;
        Function GetNext: PRecMethodExEntry; inline;
        Function GetName: ShortString; inline;
      public
        ResultType: PPTypeInfo;
        CC: TCallConv;
        Kind: TMethodKind;
        ParamCount: Word;
        StackSize: SizeInt;
        NamePtr: PShortString;
        Flags : Byte;
        { Params: array[0..ParamCount - 1] of TRecMethodParam }
        { ResultLocs: PParameterLocations (if ResultType != Nil) }
        property Name: ShortString read GetName;
        property Param[Index: Word]: PRecMethodParam read GetParam;
        property ResultLocs: PParameterLocations read GetResultLocs;
        property Tail: Pointer read GetTail;
        property Next: PRecMethodExEntry read GetNext;
        Property MethodVisibility: TVisibilityClass Read GetMethodVisibility;
        Property StrictVisibility : Boolean Read GetStrictVisibility;
      end;


      PRecMethodExTable = ^TRecMethodExTable;

      { TRecMethodExTable }

      TRecMethodExTable =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetMethod(Index: Word): PRecMethodExEntry;
      public
        // LegacyCount,Count1: Word;
        Count: Word;
        { Entry: array[0..Count - 1] of TRecMethodExEntry }
        property Method[Index: Word]: PRecMethodExEntry read GetMethod;
      end;

      PRecordMethodInfoTable = ^TRecordMethodInfoTable;
      TRecordMethodInfoTable = array[0..{$ifdef cpu16}(32768 div sizeof(PPropInfo))-2{$else}65535{$endif}] of PRecMethodExEntry;

      PInterfaceData = ^TInterfaceData;
      TInterfaceData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetUnitName: ShortString; inline;
        Function GetPropertyTable: PPropData; inline;
        Function GetMethodTable: PIntfMethodTable; inline;
      public
        property UnitName: ShortString read GetUnitName;
        property PropertyTable: PPropData read GetPropertyTable;
        property MethodTable: PIntfMethodTable read GetMethodTable;
      public
      {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
      {$endif}
      case TTypeKind of
        tkInterface: (
          Parent: PPTypeInfo;
          Flags: TIntfFlagsBase;
          GUID: TGUID;
          UnitNameField: ShortString;
          { PropertyTable: TPropData }
          { MethodTable: TIntfMethodTable }
        );
        { include for proper alignment }
        tkInt64: (
          dummy : Int64
        );
{$ifndef FPUNONE}
        tkFloat:
          (FloatType : TFloatType
        );
{$endif}
      end;

      PInterfaceRawData = ^TInterfaceRawData;
      TInterfaceRawData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetUnitName: ShortString; inline;
        Function GetIIDStr: ShortString; inline;
        Function GetPropertyTable: PPropData; inline;
        Function GetMethodTable: PIntfMethodTable; inline;
      public
        property UnitName: ShortString read GetUnitName;
        property IIDStr: ShortString read GetIIDStr;
        property PropertyTable: PPropData read GetPropertyTable;
        property MethodTable: PIntfMethodTable read GetMethodTable;
      public
      {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
      {$endif}
        case TTypeKind of
          tkInterface: (
            Parent: PPTypeInfo;
            Flags : TIntfFlagsBase;
            IID: TGUID;
            UnitNameField: ShortString;
            { IIDStr: ShortString; }
            { PropertyTable: TPropData }
          );
          { include for proper alignment }
          tkInt64: (
            dummy : Int64
          );
{$ifndef FPUNONE}
          tkFloat:
            (FloatType : TFloatType
          );
{$endif}
      end;


      PPropDataEx = ^TPropDataEx;

      PClassData = ^TClassData;

      { TClassData }

      TClassData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetExMethodTable: PVmtMethodExTable;
        Function GetExPropertyTable: PPropDataEx;
        Function GetUnitName: ShortString; inline;
        Function GetPropertyTable: PPropData; inline;
      public
        property UnitName: ShortString read GetUnitName;
        property PropertyTable: PPropData read GetPropertyTable;
        property ExRTTITable: PPropDataEx read GetExPropertyTable;
        property ExMethodTable : PVmtMethodExTable Read GetExMethodTable;
      public
        {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
        {$endif}
        case TTypeKind of
          tkClass: (
            ClassType : TClass;
            Parent : PPTypeInfo;
            PropCount : SmallInt;
            UnitNameField : ShortString;
            { PropertyTable: TPropData }
            { ExRTTITable: TPropDataex }
          );
          { include for proper alignment }
          tkInt64: (
            dummy: Int64;
          );
{$ifndef FPUNONE}
          tkFloat: (
            FloatType : TFloatType
          );
{$endif}
      end;

      PRecordMethodTable = ^TRecordMethodTable;
      TRecordMethodTable = TRecMethodExTable;

      { TRecordData }
      PRecordData = ^TRecordData;
      TRecordData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetExPropertyTable: PPropDataEx;
        Function GetExtendedFieldCount: Longint;
        Function GetExtendedFields : PExtendedFieldTable;
        Function GetMethodTable : PRecordMethodTable;
      Public
        Property ExtendedFields : PExtendedFieldTable Read GetExtendedFields;
        Property ExtendedFieldCount : Longint Read GetExtendedFieldCount;
        property MethodTable: PRecordMethodTable read GetMethodTable;
        property ExRTTITable: PPropDataEx read GetExPropertyTable;
      public
        {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
        {$endif}
        case TTypeKind of
      tkRecord:
        (
  {$ifndef VER3_0}
          RecInitInfo: Pointer; { points to TTypeInfo followed by init table }
  {$endif VER3_0}
          RecSize: Longint;
          case Boolean of
            False: (ManagedFldCount: Longint deprecated 'Use RecInitData^.ManagedFieldCount or TotalFieldCount depending on your use case');
            True: (TotalFieldCount: Longint);
          {ManagedFields: array[1..TotalFieldCount] of TManagedField}
          { ExtendedFieldsCount : Longint }
          { ExtendedFields: Array[0..ExtendedFieldsCount-1] of PExtendedFieldEntry }
          { MethodTable : TRecordMethodTable }
          { Properties }
        );
         { include for proper alignment }
        tkInt64: (
          dummy : Int64
        );
{$ifndef FPUNONE}
        tkFloat:
          (FloatType : TFloatType
        );
{$endif}
     end;



      PTypeData = ^TTypeData;
      TTypeData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetBaseType: PTypeInfo; inline;
        Function GetCompType: PTypeInfo; inline;
        Function GetParentInfo: PTypeInfo; inline;
{$ifndef VER3_0}        
        Function GetRecInitData: PRecInitData; inline;
{$endif}
        Function GetHelperParent: PTypeInfo; inline;
        Function GetExtendedInfo: PTypeInfo; inline;
        Function GetIntfParent: PTypeInfo; inline;
        Function GetRawIntfParent: PTypeInfo; inline;
        Function GetIIDStr: ShortString; inline;
        Function GetElType: PTypeInfo; inline;
        Function GetElType2: PTypeInfo; inline;
        Function GetInstanceType: PTypeInfo; inline;
        Function GetRefType: PTypeInfo; inline;
      public
        { tkEnumeration }
        property BaseType: PTypeInfo read GetBaseType;
        { tkSet }
        property CompType: PTypeInfo read GetCompType;
        { tkClass }
        property ParentInfo: PTypeInfo read GetParentInfo;
        { tkRecord }
{$ifndef VER3_0}        
        property RecInitData: PRecInitData read GetRecInitData;
{$endif}
        { tkHelper }
        property HelperParent: PTypeInfo read GetHelperParent;
        property ExtendedInfo: PTypeInfo read GetExtendedInfo;
        { tkInterface }
        property IntfParent: PTypeInfo read GetIntfParent;
        { tkInterfaceRaw }
        property RawIntfParent: PTypeInfo read GetRawIntfParent;
        property IIDStr: ShortString read GetIIDStr;
        { tkDynArray }
        property ElType2: PTypeInfo read GetElType2;
        property ElType: PTypeInfo read GetElType;
        { tkClassRef }
        property InstanceType: PTypeInfo read GetInstanceType;
        { tkPointer }
        property RefType: PTypeInfo read GetRefType;
      public
         {$ifdef PROVIDE_ATTR_TABLE}
         AttributeTable : PAttributeTable;
         {$endif}
         case TTypeKind of
            tkUnKnown,tkLString,tkWString,tkVariant,tkUString:
              ();
            tkAString:
              (CodePage: Word);
{$ifndef VER3_0}
            tkInt64,tkQWord,
{$endif VER3_0}
            tkInteger,tkChar,tkEnumeration,tkBool,tkWChar,tkSet:
              (OrdType : TOrdType;
               case TTypeKind of
                  tkInteger,tkChar,tkEnumeration,tkBool,tkWChar : (
                    MinValue,MaxValue : Longint;
                    case TTypeKind of
                      tkEnumeration:
                        (
                        BaseTypeRef : TypeInfoPtr;
                        NameList : ShortString;
                        {EnumUnitName: ShortString;})
                    );
{$ifndef VER3_0}
                  {tkBool with OrdType=otSQWord }
                  tkInt64:
                    (MinInt64Value, MaxInt64Value: Int64);
                  {tkBool with OrdType=otUQWord }
                  tkQWord:
                    (MinQWordValue, MaxQWordValue: QWord);
{$endif VER3_0}
                  tkSet:
                    (
{$ifndef VER3_0}
                     SetSize : SizeInt;
{$endif VER3_0}
                     CompTypeRef : TypeInfoPtr
                    )
              );
{$ifndef FPUNONE}
            tkFloat:
              (FloatType : TFloatType);
{$endif}
            tkSString:
              (MaxLength : Byte);
            tkClass:
              (ClassType : TClass;
               ParentInfoRef : TypeInfoPtr;
               PropCount : SmallInt;
               UnitName : ShortString;
               // here the properties follow as array of TPropInfo:
               {
               PropData: TPropData;
               // Extended RTTI
               PropDataEx: TPropDataEx;
               ClassAttrData: TAttrData;
               ArrayPropCount: Word;
               ArrayPropData: array[1..ArrayPropCount] of TArrayPropInfo;
               }
              );
            tkRecord:
              (
{$ifndef VER3_0}
                RecInitInfo: Pointer; { points to TTypeInfo followed by init table }
{$endif VER3_0}
                RecSize: Longint;
                case Boolean of
                  False: (ManagedFldCount: Longint deprecated 'Use RecInitData^.ManagedFieldCount or TotalFieldCount depending on your use case');
                  True: (TotalFieldCount: Longint);
                {ManagedFields: array[1..TotalFieldCount] of TManagedField}
              );
            tkHelper:
              (HelperParentRef : TypeInfoPtr;
               ExtendedInfoRef : TypeInfoPtr;
               HelperProps : SmallInt;
               HelperUnit : ShortString
               // here the properties follow as array of TPropInfo
              );
            tkMethod:
              (MethodKind : TMethodKind;
               ParamCount : Byte;
               case Boolean of
                 False: (ParamList : array[0..1023] of Char);
                 { dummy for proper alignment }
                 True: (ParamListDummy : Word);
             {in reality ParamList is a array[1..ParamCount] of:
                  record
                    Flags : TParamFlags;
                    ParamName : ShortString;
                    TypeName : ShortString;
                  end;
              followed by
                  ResultType : ShortString     // for mkFunction, mkClassFunction only
                  ResultTypeRef : PPTypeInfo;  // for mkFunction, mkClassFunction only
                  CC : TCallConv;
                  ParamTypeRefs : array[1..ParamCount] of PPTypeInfo;}
              );
            tkProcVar:
              (ProcSig: TProcedureSignature);
{$ifdef VER3_0}
            tkInt64:
              (MinInt64Value, MaxInt64Value: Int64);
            tkQWord:
              (MinQWordValue, MaxQWordValue: QWord);
{$endif VER3_0}
            tkInterface:
              (
               IntfParentRef: TypeInfoPtr;
               IntfFlags : TIntfFlagsBase;
               GUID: TGUID;
               IntfUnit: ShortString;
               { PropertyTable: TPropData }
               { MethodTable: TIntfMethodTable }
              );
            tkInterfaceRaw:
              (
               RawIntfParentRef: TypeInfoPtr;
               RawIntfFlags : TIntfFlagsBase;
               IID: TGUID;
               RawIntfUnit: ShortString;
               { IIDStr: ShortString; }
               { PropertyTable: TPropData }
              );
            tkArray:
              (ArrayData: TArrayTypeData);
            tkDynArray:
              (
              elSize     : PtrUInt;
              elType2Ref : TypeInfoPtr;
              varType    : Longint;
              elTypeRef  : TypeInfoPtr;
              DynUnitName: ShortStringBase
              );
            tkClassRef:
              (InstanceTypeRef: TypeInfoPtr);
            tkPointer:
              (RefTypeRef: TypeInfoPtr);
      end;

      PPropInfo = ^TPropInfo;

      TPropData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        Function GetProp(Index: Word): PPropInfo;
        Function GetTail: Pointer; inline;
      public
        PropCount : Word;
        PropList : record _alignmentdummy : ptrint; end;
        property Prop[Index: Word]: PPropInfo read GetProp;
        property Tail: Pointer read GetTail;
      end;

      { TPropDataEx }
      PPropInfoEx = ^TPropInfoEx;

      TPropDataEx = 
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed 
      {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}  
      record
      private
        Function GetPropEx(Index: Word): PPropInfoEx;
        Function GetTail: Pointer; inline;
      Public
        PropCount: Word;
        PropList: record alignmentdummy: ptrint; end;
        {PropList: array[1..PropCount] of TPropInfoEx}
        property Prop[Index: Word]: PPropInfoex read GetPropEx;
        property Tail: Pointer read GetTail;
      end;


      { TPropInfoEx }

      TPropInfoEx = 
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed 
      {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT} 
      record
      private
        Function GetStrictVisibility: Boolean;
        Function GetTail: Pointer;
        Function GetVisiblity: TVisibilityClass;
      public
        Flags: Byte;
        Info: PPropInfo;
        // AttrData: TAttrData
        Property Tail : Pointer Read GetTail;
        Property Visibility : TVisibilityClass Read GetVisiblity;
        Property StrictVisibility : Boolean Read GetStrictVisibility;
      end;

      PPropListEx = ^TPropListEx;
      TPropListEx = array[0..{$ifdef cpu16}(32768 div sizeof(PPropInfo))-2{$else}65535{$endif}] of PPropInfoEx;


{$PACKRECORDS 1}
      TPropInfo = packed record
      private
        Function GetPropType: PTypeInfo; inline;
        Function GetTail: Pointer; inline;
        Function GetNext: PPropInfo; inline;
      public
        PropTypeRef : TypeInfoPtr;
        GetProc : CodePointer;
        SetProc : CodePointer;
        StoredProc : CodePointer;
        Index : Longint;
        Default : Longint;
        NameIndex : SmallInt;

        // contains the type of the Get/Set/Storedproc, see also ptxxx
        // bit 0..1 GetProc
        //     2..3 SetProc
        //     4..5 StoredProc
        //     6 : true, constant index property
        PropProcs : Byte;

        {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
        {$endif}
        Name : ShortString;
        property PropType: PTypeInfo read GetPropType;
        property Tail: Pointer read GetTail;
        property Next: PPropInfo read GetNext;
      end;

      TProcInfoProc = Procedure(PropInfo : PPropInfo) of object;

      PPropList = ^TPropList;
      TPropList = array[0..{$ifdef cpu16}(32768 div sizeof(PPropInfo))-2{$else}65535{$endif}] of PPropInfo;


   const
      tkString = tkSString;
      tkProcedure = tkProcVar; // for compatibility with Delphi
      tkAny = [Low(TTypeKind)..High(TTypeKind)];
      tkMethods = [tkMethod];
      tkProperties = tkAny-tkMethods-[tkUnknown];

// general property handling
Function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;
Function AlignTypeData(p : Pointer) : Pointer; inline;
Function AlignTParamFlags(p : Pointer) : Pointer; inline;
Function AlignPTypeInfo(p : Pointer) : Pointer; inline;
Generic Function ConstParamIsRef<T>(aCallConv: TCallConv = ccReg): Boolean; inline;

Function GetPropInfo(TypeInfo: PTypeInfo;const PropName: string): PPropInfo;
Function GetPropInfo(TypeInfo: PTypeInfo;const PropName: string; AKinds: TTypeKinds): PPropInfo;
Function GetPropInfo(Instance: TObject; const PropName: string): PPropInfo;
Function GetPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds): PPropInfo;
Function GetPropInfo(AClass: TClass; const PropName: string): PPropInfo;
Function GetPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds): PPropInfo;

Function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
Function FindPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds): PPropInfo;
Function FindPropInfo(AClass: TClass; const PropName: string): PPropInfo;
Function FindPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds): PPropInfo;

Procedure GetPropInfos(TypeInfo: PTypeInfo; PropList: PPropList);
Function GetPropList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropList; Sorted: boolean = true): longint;
Function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): SizeInt;
Function GetPropList(AClass: TClass; out PropList: PPropList): Integer;
Function GetPropList(Instance: TObject; out PropList: PPropList): Integer;

// extended RTTI

Function GetPropInfosEx(TypeInfo: PTypeInfo; PropList: PPropListEx; Visibilities : TVisibilityClasses = []) : Integer;
Function GetPropListEx(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropListEx; Sorted: boolean = true; Visibilities : TVisibilityClasses = []): longint;
Function GetPropListEx(TypeInfo: PTypeInfo; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): SizeInt;
Function GetPropListEx(AClass: TClass; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): Integer;
Function GetPropListEx(Instance: TObject; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): Integer;

Function GetFieldInfos(aClass: TClass; FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetFieldInfos(aRecord: PRecordData; FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetFieldInfos(TypeInfo: PTypeInfo; FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetFieldList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; out FieldList: PExtendedFieldInfoTable; Sorted: boolean = true; Visibilities : TVisibilityClasses = []): longint;
Function GetFieldList(TypeInfo: PTypeInfo; out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []): SizeInt;
Function GetRecordFieldList(aRecord: PRecordData; Out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetFieldList(AClass: TClass; out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []): Integer;
Function GetFieldList(Instance: TObject; out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []): Integer;

// Infos require initialized memory or nil to count
Function GetMethodInfos(aClass: TClass; MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetRecordMethodInfos(aRecordData: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities: TVisibilityClasses): Integer;
Function GetMethodInfos(aRecord: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
// List will initialize the memory
Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Sorted: boolean = true; Visibilities : TVisibilityClasses = []): longint;
Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []): longint;
Function GetMethodList(AClass: TClass; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []): Integer;
Function GetMethodList(Instance: TObject; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []): Integer;

Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PRecordMethodInfoTable; Sorted: boolean = true; Visibilities : TVisibilityClasses = []): longint;
Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []): longint;
Function GetRecordMethodList(aRecord: PRecordData; Out MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;


// Property information routines.
Function IsReadableProp(PropInfo : PPropInfo) : Boolean;
Function IsReadableProp(Instance: TObject; const PropName: string): Boolean;
Function IsReadableProp(AClass: TClass; const PropName: string): Boolean;
Function IsWriteableProp(PropInfo : PPropInfo) : Boolean;
Function IsWriteableProp(Instance: TObject; const PropName: string): Boolean;
Function IsWriteableProp(AClass: TClass; const PropName: string): Boolean;
Function IsStoredProp(Instance: TObject;PropInfo : PPropInfo) : Boolean;
Function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
Function IsPublishedProp(Instance: TObject; const PropName: string): Boolean;
Function IsPublishedProp(AClass: TClass; const PropName: string): Boolean;
Function PropType(Instance: TObject; const PropName: string): TTypeKind;
Function PropType(AClass: TClass; const PropName: string): TTypeKind;
Function PropIsType(Instance: TObject; const PropName: string; TypeKind: TTypeKind): Boolean;
Function PropIsType(AClass: TClass; const PropName: string; TypeKind: TTypeKind): Boolean;

// subroutines to read/write properties
Function  GetOrdProp(Instance: TObject; PropInfo : PPropInfo) : Int64;
Function  GetOrdProp(Instance: TObject; const PropName: string): Int64;
Procedure SetOrdProp(Instance: TObject; PropInfo : PPropInfo;  Value : Int64);
Procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);

Function  GetEnumProp(Instance: TObject; const PropName: string): string;
Function  GetEnumProp(Instance: TObject; const PropInfo: PPropInfo): string;
Procedure SetEnumProp(Instance: TObject; const PropName: string;const Value: string);
Procedure SetEnumProp(Instance: TObject; const PropInfo: PPropInfo;const Value: string);

Function  GetSetProp(Instance: TObject; const PropName: string): string;
Function  GetSetProp(Instance: TObject; const PropName: string; Brackets: Boolean): string;
Function  GetSetProp(Instance: TObject; const PropInfo: PPropInfo; Brackets: Boolean): string;
Procedure SetSetProp(Instance: TObject; const PropName: string; const Value: string);
Procedure SetSetProp(Instance: TObject; const PropInfo: PPropInfo; const Value: string);

Function  GetStrProp(Instance: TObject; PropInfo : PPropInfo) : Ansistring;
Function  GetStrProp(Instance: TObject; const PropName: string): string;
Procedure SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
Procedure SetStrProp(Instance: TObject; PropInfo : PPropInfo; const Value : Ansistring);

Function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): WideString;
Function GetWideStrProp(Instance: TObject; const PropName: string): WideString;
Procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
Procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: WideString);

Function GetUnicodeStrProp(Instance: TObject; PropInfo: PPropInfo): UnicodeString;
Function GetUnicodeStrProp(Instance: TObject; const PropName: string): UnicodeString;
Procedure SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);
Procedure SetUnicodeStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: UnicodeString);

Function GetRawbyteStrProp(Instance: TObject; PropInfo: PPropInfo): RawByteString;
Function GetRawByteStrProp(Instance: TObject; const PropName: string): RawByteString;
Procedure SetRawByteStrProp(Instance: TObject; const PropName: string; const Value: RawByteString);
Procedure SetRawByteStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: RawByteString);


{$ifndef FPUNONE}
Function  GetFloatProp(Instance: TObject; PropInfo : PPropInfo) : Extended;
Function  GetFloatProp(Instance: TObject; const PropName: string): Extended;
Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
Procedure SetFloatProp(Instance: TObject; PropInfo : PPropInfo;  Value : Extended);
{$endif}

Function  GetObjectProp(Instance: TObject; const PropName: string): TObject;
Function  GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
Function  GetObjectProp(Instance: TObject; PropInfo: PPropInfo): TObject;
Function  GetObjectProp(Instance: TObject; PropInfo: PPropInfo; MinClass: TClass): TObject;
Procedure SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
Procedure SetObjectProp(Instance: TObject; PropInfo: PPropInfo; Value: TObject);
Function  GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
Function  GetObjectPropClass(AClass: TClass; const PropName: string): TClass;

Function  GetMethodProp(Instance: TObject; PropInfo: PPropInfo) : TMethod;
Function  GetMethodProp(Instance: TObject; const PropName: string): TMethod;
Procedure SetMethodProp(Instance: TObject; PropInfo: PPropInfo;  const Value : TMethod);
Procedure SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);

Function  GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
Function  GetInt64Prop(Instance: TObject; const PropName: string): Int64;
Procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo;  const Value: Int64);
Procedure SetInt64Prop(Instance: TObject; const PropName: string;  const Value: Int64);

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;
Function GetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;
Function GetPropValue(Instance: TObject; PropInfo: PPropInfo; PreferStrings: Boolean): Variant;
Procedure SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);
Procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);
Function  GetVariantProp(Instance: TObject; PropInfo : PPropInfo): Variant;
Function  GetVariantProp(Instance: TObject; const PropName: string): Variant;
Procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
Procedure SetVariantProp(Instance: TObject; PropInfo : PPropInfo; const Value: Variant);

Function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;
Function GetInterfaceProp(Instance: TObject; PropInfo: PPropInfo): IInterface;
Procedure SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);
Procedure SetInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface);

Function GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;
Function GetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
Procedure SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);
Procedure SetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

Function GetDynArrayProp(Instance: TObject; const PropName: string): Pointer;
Function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
Procedure SetDynArrayProp(Instance: TObject; const PropName: string; const Value: Pointer);
Procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

// Extended RTTI
Function GetAttributeTable(TypeInfo: PTypeInfo): PAttributeTable;

Function GetPropAttribute(PropInfo: PPropInfo; AttributeNr: Word): TCustomAttribute; inline;

Function GetAttribute(AttributeTable: PAttributeTable; AttributeNr: Word): TCustomAttribute;

// Auxiliary routines, which may be useful
Function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
Function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;
Function GetEnumNameCount(enum1: PTypeInfo): SizeInt;
Procedure AddEnumElementAliases(aTypeInfo: PTypeInfo; const aNames: array of string; aStartValue: Integer = 0);
Procedure RemoveEnumElementAliases(aTypeInfo: PTypeInfo);
Function GetEnumeratedAliasValue(aTypeInfo: PTypeInfo; const aName: string): Integer;

function SetToArray(TypeInfo: PTypeInfo; Value: Pointer) : TBytes;
function SetToArray(PropInfo: PPropInfo; Value: Pointer) : TBytes;
function SetToArray(TypeInfo: PTypeInfo; Value: LongInt) : TBytes;
function SetToArray(PropInfo: PPropInfo; Value: LongInt) : TBytes;
function ArrayToSet(PropInfo: PPropInfo; const Value: array of Byte): LongInt;
function ArrayToSet(TypeInfo: PTypeInfo; const Value: array of Byte): LongInt;
procedure ArrayToSet(PropInfo: PPropInfo; const Value: array of Byte; Result: Pointer);
procedure ArrayToSet(TypeInfo: PTypeInfo; const Value: array of Byte; Result: Pointer);
Function SetToString(TypeInfo: PTypeInfo; Value: LongInt; Brackets: Boolean) : String;
Function SetToString(PropInfo: PPropInfo; Value: LongInt; Brackets: Boolean) : String;
Function SetToString(PropInfo: PPropInfo; Value: LongInt) : String;
Function SetToString(TypeInfo: PTypeInfo; Value: Pointer; Brackets: Boolean = False) : String;
Function SetToString(PropInfo: PPropInfo; Value: Pointer; Brackets: Boolean = False) : String;
function StringToSet(PropInfo: PPropInfo; const Value: string): LongInt;
function StringToSet(TypeInfo: PTypeInfo; const Value: string): LongInt;
procedure StringToSet(PropInfo: PPropInfo; const Value: String; Result: Pointer);
procedure StringToSet(TypeInfo: PTypeInfo; const Value: String; Result: Pointer);


const
    BooleanIdents: array[Boolean] of String = ('False', 'True');
    DotSep: String = '.';

Type
  EPropertyError  = Class(Exception);
  TGetPropValue   = Function (Instance: TObject; PropInfo: PPropInfo; PreferStrings: Boolean) : Variant;
  TSetPropValue   = Procedure (Instance: TObject; PropInfo: PPropInfo; const Value: Variant);
  TGetVariantProp = Function (Instance: TObject; PropInfo : PPropInfo): Variant;
  TSetVariantProp = Procedure (Instance: TObject; PropInfo : PPropInfo; const Value: Variant);

  EPropertyConvertError = class(Exception); // Not used (yet), but defined for compatibility.

Const
  OnGetPropValue   : TGetPropValue = Nil;
  OnSetPropValue   : TSetPropValue = Nil;
  OnGetVariantprop : TGetVariantProp = Nil;
  OnSetVariantprop : TSetVariantProp = Nil;

{ for inlining }
Function DerefTypeInfoPtr(Info: TypeInfoPtr): PTypeInfo; inline;

Implementation

uses rtlconsts;

type
  PMethod = ^TMethod;

{ ---------------------------------------------------------------------
  Auxiliary methods
  ---------------------------------------------------------------------}

Function aligntoptr(p : pointer) : pointer;inline;
   begin
{$ifdef CPUM68K}
     result:=AlignTypeData(p);
{$else CPUM68K}
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
{$endif CPUM68K}
   end;


Function DerefTypeInfoPtr(Info: TypeInfoPtr): PTypeInfo; inline;
begin
{$ifdef ver3_0}
  Result := Info;
{$else}
  if not Assigned(Info) then
    Result := Nil
  else
    Result := Info^;
{$endif}
end;

Function GetAttributeTable(TypeInfo: PTypeInfo): PAttributeTable;
{$ifdef PROVIDE_ATTR_TABLE}
var
  TD: PTypeData;
begin
  TD := GetTypeData(TypeInfo);
  Result:=TD^.AttributeTable;
{$else}
begin
  Result:=Nil;
{$endif}
end;

Function GetPropData(TypeInfo : PTypeInfo; TypeData: PTypeData) : PPropData; inline;
var
  p: PtrUInt;
begin
  p := PtrUInt(@TypeData^.UnitName) + SizeOf(TypeData^.UnitName[0]) + Length(TypeData^.UnitName);
  Result := PPropData(aligntoptr(Pointer(p)));
end;

Function GetAttribute(AttributeTable: PAttributeTable; AttributeNr: Word): TCustomAttribute;
begin
  if (AttributeTable=nil) or (AttributeNr>=AttributeTable^.AttributeCount) then
    result := nil
  else
    begin
      result := AttributeTable^.AttributesList[AttributeNr].AttrProc();
    end;
end;

Function GetPropAttribute(PropInfo: PPropInfo; AttributeNr: Word): TCustomAttribute;
begin
{$ifdef PROVIDE_ATTR_TABLE}
  Result := GetAttribute(PropInfo^.AttributeTable, AttributeNr);
{$else}
  Result := Nil;
{$endif}
end;

Function GetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;

  Var PS : PShortString;
      PT : PTypeData;

begin
  PT:=GetTypeData(TypeInfo);
  if TypeInfo^.Kind=tkBool then
    begin
      case Value of
        0,1:
          Result:=BooleanIdents[Boolean(Value)];
        else
          Result:='';
      end;
    end
 else
   begin
     PS:=@PT^.NameList;
     dec(Value,PT^.MinValue);
     While Value>0 Do
       begin
         PS:=PShortString(pointer(PS)+PByte(PS)^+1);
         Dec(Value);
       end;
     Result:=PS^;
   end;
end;


Function GetEnumValue(TypeInfo: PTypeInfo; const Name: string): Integer;

  Var PS : PShortString;
      PT : PTypeData;
      Count : longint;
      sName: shortstring;

begin
  If Length(Name)=0 then
    exit(-1);
  sName := Name;
  PT:=GetTypeData(TypeInfo);
  Count:=0;
  Result:=-1;

  if TypeInfo^.Kind=tkBool then
    begin
    If CompareText(BooleanIdents[false],Name)=0 then
      result:=0
    else if CompareText(BooleanIdents[true],Name)=0 then
      result:=1;
    end
 else
   begin
     PS:=@PT^.NameList;
     While (Result=-1) and (PByte(PS)^<>0) do
       begin
         If ShortCompareText(PS^, sName) = 0 then
           Result:=Count+PT^.MinValue;
         PS:=PShortString(pointer(PS)+PByte(PS)^+1);
         Inc(Count);
       end;
     if Result=-1 then
       Result:=GetEnumeratedAliasValue(TypeInfo,Name);
   end;
end;


Function GetEnumNameCount(enum1: PTypeInfo): SizeInt;
var
  PS: PShortString;
  PT: PTypeData;
  Count: SizeInt;
begin
  PT:=GetTypeData(enum1);
  if enum1^.Kind=tkBool then
    Result:=2
  else
    begin
      Count:=0;
      Result:=0;

      PS:=@PT^.NameList;
      While (PByte(PS)^<>0) do
        begin
          PS:=PShortString(pointer(PS)+PByte(PS)^+1);
          Inc(Count);
        end;
      { the last string is the unit name }
      Result := Count - 1;
    end;
end;


Function SetToString(PropInfo: PPropInfo; Value: LongInt; Brackets: Boolean): String;

begin
  Result:=SetToString(PropInfo^.PropType, Value, Brackets);
end;

Function SetToString(TypeInfo: PTypeInfo; Value: LongInt; Brackets: Boolean): String;
begin
{$if defined(FPC_BIG_ENDIAN)}
  { correctly adjust packed sets that are smaller than 32-bit }
  case GetTypeData(TypeInfo)^.OrdType of
    otSByte,otUByte: Value := Value shl (SizeOf(Integer)*8-8);
    otSWord,otUWord: Value := Value shl (SizeOf(Integer)*8-16);
  end;
{$endif}
  Result := SetToString(TypeInfo, @Value, Brackets);
end;

function SetToString(TypeInfo: PTypeInfo; Value: Pointer; Brackets: Boolean): String;
var
  A: TBytes;
  B: Byte;
  PTI : PTypeInfo;
begin
  PTI:=GetTypeData(TypeInfo)^.CompType;
  A:=SetToArray(TypeInfo, Value);
  Result := '';
  for B in A do
    If Result='' then
      Result:=GetEnumName(PTI,B)
    else
      Result:=Result+','+GetEnumName(PTI,B);
  if Brackets then
    Result:='['+Result+']';
end;

Function SetToString(PropInfo: PPropInfo; Value: LongInt) : String;

begin
  Result:=SetToString(PropInfo,Value,False);
end;

function SetToString(PropInfo: PPropInfo; Value: Pointer; Brackets: Boolean): String;
begin
  Result := SetToString(PropInfo^.PropType, Value, Brackets);
end;

function SetToArray(TypeInfo: PTypeInfo; Value: Pointer) : TBytes;
type
  tsetarr = bitpacked array[0..SizeOf(LongInt)*8-1] of 0..1;
Var
  I,El,Els,Rem,V,Max : Integer;
  PTD : PTypeData;
  ValueArr : PLongInt;
begin
  PTD := GetTypeData(TypeInfo);
  ValueArr := PLongInt(Value);
  Result:=[];
{$ifdef ver3_0}
  case PTD^.OrdType of
    otSByte, otUByte: begin
      Els := 0;
      Rem := 1;
    end;
    otSWord, otUWord: begin
      Els := 0;
      Rem := 2;
    end;
    otSLong, otULong: begin
      Els := 1;
      Rem := 0;
    end;
  end;
{$else}
  Els := PTD^.SetSize div SizeOf(LongInt);
  Rem := PTD^.SetSize mod SizeOf(LongInt);
{$endif}

{$ifdef ver3_0}
  El := 0;
{$else}
  for El := 0 to (PTD^.SetSize - 1) div SizeOf(LongInt) do
{$endif}
    begin
      if El = Els then
        Max := Rem
      else
        Max := SizeOf(LongInt);
      For I:=0 to Max*8-1 do
        begin
          if (tsetarr(ValueArr[El])[i]<>0) then
            begin
              V := I + SizeOf(LongInt) * 8 * El;
              SetLength(Result, Length(Result)+1);
              Result[High(Result)]:=V;
            end;
        end;
    end;
end;

function SetToArray(PropInfo: PPropInfo; Value: Pointer) : TBytes;
begin
  Result:=SetToArray(PropInfo^.PropType,Value);
end;

function SetToArray(TypeInfo: PTypeInfo; Value: LongInt) : TBytes;

begin
  Result:=SetToArray(TypeInfo,@Value);
end;


function SetToArray(PropInfo: PPropInfo; Value: LongInt) : TBytes;
begin
  Result:=SetToArray(PropInfo^.PropType,@Value);
end;

Const
  SetDelim = ['[',']',',',' '];

Function GetNextElement(Var S : String) : String;
Var
  J : Integer;
begin
  J:=1;
  Result:='';
  If Length(S)>0 then
    begin
      While (J<=Length(S)) and Not (S[j] in SetDelim) do
        Inc(j);
      Result:=Copy(S,1,j-1);
      Delete(S,1,j);
    end;
end;

Function StringToSet(PropInfo: PPropInfo; const Value: string): LongInt;

begin
  Result:=StringToSet(PropInfo^.PropType,Value);
end;

Function StringToSet(TypeInfo: PTypeInfo; const Value: string): LongInt;
begin
  StringToSet(TypeInfo, Value, @Result);
{$if defined(FPC_BIG_ENDIAN)}
  { correctly adjust packed sets that are smaller than 32-bit }
  case GetTypeData(TypeInfo)^.OrdType of
    otSByte,otUByte: Result := Result shr (SizeOf(Integer)*8-8);
    otSWord,otUWord: Result := Result shr (SizeOf(Integer)*8-16);
  end;
{$endif}
end;

Procedure StringToSet(TypeInfo: PTypeInfo; const Value: String; Result: Pointer);
Var
  S,T : String;
  I, ElOfs, BitOfs : Integer;
  PTD: PTypeData;
  PTI : PTypeInfo;
  A: TBytes;
begin
  PTD:=GetTypeData(TypeInfo);
  PTI:=PTD^.Comptype;
  S:=Value;
  I:=1;
  If Length(S)>0 then
    begin
      While (I<=Length(S)) and (S[i] in SetDelim) do
        Inc(I);
      Delete(S,1,i-1);
    end;
  A:=[];
  While (S<>'') do
    begin
      T:=GetNextElement(S);
      if T<>'' then
        begin
          I:=GetEnumValue(PTI,T);
          if (I<0) then
            raise EPropertyError.CreateFmt(SErrUnknownEnumValue, [T]);
          SetLength(A, Length(A)+1);
          A[High(A)]:=I;
        end;
    end;
  ArrayToSet(TypeInfo,A,Result);
end;

Procedure StringToSet(PropInfo: PPropInfo; const Value: String; Result: Pointer);
begin
  StringToSet(PropInfo^.PropType, Value, Result);
end;

Function ArrayToSet(PropInfo: PPropInfo; const Value: array of Byte): LongInt;

begin
  Result:=ArrayToSet(PropInfo^.PropType,Value);
end;

Function ArrayToSet(TypeInfo: PTypeInfo; const Value: array of Byte): LongInt;
begin
  ArrayToSet(TypeInfo, Value, @Result);
{$if defined(FPC_BIG_ENDIAN)}
  { correctly adjust packed sets that are smaller than 32-bit }
  case GetTypeData(TypeInfo)^.OrdType of
    otSByte,otUByte: Result := Result shr (SizeOf(Integer)*8-8);
    otSWord,otUWord: Result := Result shr (SizeOf(Integer)*8-16);
  end;
{$endif}
end;

procedure ArrayToSet(TypeInfo: PTypeInfo; const Value: array of Byte; Result: Pointer);
Var
  ElOfs, BitOfs : Integer;
  PTD: PTypeData;
  ResArr: PLongWord;
  B: Byte;

begin
  PTD:=GetTypeData(TypeInfo);
{$ifndef ver3_0}
  FillChar(Result^, PTD^.SetSize, 0);
{$else}
  PInteger(Result)^ := 0;
{$endif}
  ResArr := PLongWord(Result);
  for B in Value do
    begin
      ElOfs := B shr 5;
      BitOfs := B and $1F;
{$ifdef FPC_BIG_ENDIAN}
      { on Big Endian systems enum values start from the MSB, thus we need
        to reverse the shift }
      BitOfs := 31 - BitOfs;
{$endif}
      ResArr[ElOfs] := ResArr[ElOfs] or (LongInt(1) shl BitOfs);
    end;
end;

procedure ArrayToSet(PropInfo: PPropInfo; const Value: array of Byte; Result: Pointer);
begin
  ArrayToSet(PropInfo^.PropType, Value, Result);
end;

Function AlignTypeData(p: Pointer): Pointer;
{$packrecords c}
  type
    TAlignCheck = record
      b : byte;
      q : qword;
    end;
{$packrecords default}
begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
{$ifdef VER3_0}
  Result:=Pointer(align(p,SizeOf(Pointer)));
{$else VER3_0}
  Result:=Pointer(align(p,PtrInt(@TAlignCheck(nil^).q)))
{$endif VER3_0}
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
  Result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;


Function AlignTParamFlags(p: Pointer): Pointer;
{$packrecords c}
  type
    TAlignCheck = record
      b : byte;
      w : word;
    end;
{$packrecords default}
begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Result:=Pointer(align(p,PtrInt(@TAlignCheck(nil^).w)))
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
  Result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;


Function AlignPTypeInfo(p: Pointer): Pointer;
{$packrecords c}
  type
    TAlignCheck = record
      b : byte;
      p : pointer;
    end;
{$packrecords default}
begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  Result:=Pointer(align(p,PtrInt(@TAlignCheck(nil^).p)))
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
  Result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;


Generic Function ConstParamIsRef<T>(aCallConv: TCallConv): Boolean;

  Function SameAddrRegister(const aArg1: T; constref aArg2: T): Boolean; register;
  begin
    Result := @aArg1 = @aArg2;
  end;

  Function SameAddrCDecl(const aArg1: T; constref aArg2: T): Boolean; cdecl;
  begin
    Result := @aArg1 = @aArg2;
  end;

{$if defined(cpui8086) or defined(cpui386)}
  Function SameAddrPascal(const aArg1: T; constref aArg2: T): Boolean; pascal;
  begin
    Result := @aArg1 = @aArg2;
  end;
{$endif}

  Function SameAddrStdCall(const aArg1: T; constref aArg2: T): Boolean; stdcall;
  begin
    Result := @aArg1 = @aArg2;
  end;

  Function SameAddrCppDecl(const aArg1: T; constref aArg2: T): Boolean; cppdecl;
  begin
    Result := @aArg1 = @aArg2;
  end;

{$if defined(cpui386)}
  Function SameAddrOldFPCCall(const aArg1: T; constref aArg2: T): Boolean; oldfpccall;
  begin
    Result := @aArg1 = @aArg2;
  end;
{$endif}

  Function SameAddrMWPascal(const aArg1: T; constref aArg2: T): Boolean; mwpascal;
  begin
    Result := @aArg1 = @aArg2;
  end;

var
  v: T;
begin
  v := Default(T);
  case aCallConv of
    ccReg:
      Result := SameAddrRegister(v, v);
    ccCdecl:
      Result := SameAddrCDecl(v, v);
{$if defined(cpui386) or defined(cpui8086)}
    ccPascal:
      Result := SameAddrPascal(v, v);
{$endif}
{$if not defined(cpui386)}
    ccOldFPCCall,
{$endif}
{$if not defined(cpui386) and not defined(cpui8086)}
    ccPascal,
{$endif}
    ccStdCall:
      Result := SameAddrStdCall(v, v);
    ccCppdecl:
      Result := SameAddrCppDecl(v, v);
{$if defined(cpui386)}
    ccOldFPCCall:
      Result := SameAddrOldFPCCall(v, v);
{$endif}
    ccMWPascal:
      Result := SameAddrMWPascal(v, v);
    else
      raise EArgumentException.CreateFmt(SUnsupportedCallConv, [GetEnumName(PTypeInfo(TypeInfo(TCallConv)), Ord(aCallConv))]);
  end;
end;


Function GetTypeData(TypeInfo: PTypeInfo): PTypeData;
begin
  GetTypeData:=AlignTypeData(pointer(TypeInfo)+2+PByte(pointer(TypeInfo)+1)^);
end;


{ ---------------------------------------------------------------------
  Basic Type information functions.
  ---------------------------------------------------------------------}

Function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string): PPropInfo;
var
  hp : PTypeData;
  i : longint;
  p : shortstring;
  pd : PPropData;
begin
  P:=PropName;  // avoid Ansi<->short conversion in a loop
  while Assigned(TypeInfo) do
    begin
      // skip the name
      hp:=GetTypeData(Typeinfo);
      // the class info rtti the property rtti follows immediatly
      pd := GetPropData(TypeInfo,hp);
      Result:=PPropInfo(@pd^.PropList);
      for i:=1 to pd^.PropCount do
        begin
          // found a property of that name ?
          if ShortCompareText(Result^.Name, P) = 0 then
            exit;
          // skip to next property
          Result:=PPropInfo(aligntoptr(pointer(@Result^.Name)+byte(Result^.Name[0])+1));
        end;
      // parent class
      Typeinfo:=hp^.ParentInfo;
    end;
  Result:=Nil;
end;


Function GetPropInfo(TypeInfo: PTypeInfo; const PropName: string; AKinds: TTypeKinds): PPropInfo;
begin
  Result:=GetPropInfo(TypeInfo,PropName);
  If (Akinds<>[]) then
    If (Result<>Nil) then
      If Not (Result^.PropType^.Kind in AKinds) then
        Result:=Nil;
end;


Function GetPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds): PPropInfo;
begin
  Result:=GetPropInfo(PTypeInfo(AClass.ClassInfo),PropName,AKinds);
end;


Function GetPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds): PPropInfo;
begin
  Result:=GetPropInfo(Instance.ClassType,PropName,AKinds);
end;


Function GetPropInfo(Instance: TObject; const PropName: string): PPropInfo;
begin
  Result:=GetPropInfo(Instance,PropName,[]);
end;


Function GetPropInfo(AClass: TClass; const PropName: string): PPropInfo;
begin
  Result:=GetPropInfo(AClass,PropName,[]);
end;


Function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
begin
  result:=GetPropInfo(Instance, PropName);
  if Result=nil then
    Raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;


Function FindPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds): PPropInfo;
begin
  result:=GetPropInfo(Instance, PropName, AKinds);
  if Result=nil then
    Raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;


Function FindPropInfo(AClass: TClass; const PropName: string): PPropInfo;
begin
  result:=GetPropInfo(AClass, PropName);
  if result=nil then
    Raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;


Function FindPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds): PPropInfo;
begin
  result:=GetPropInfo(AClass, PropName, AKinds);
  if result=nil then
    Raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

Function IsReadableProp(PropInfo: PPropInfo): Boolean;
begin
  Result:=(((PropInfo^.PropProcs) and 3) in [ptField,ptStatic,ptVirtual]);
end;

Function IsReadableProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsReadableProp(FindPropInfo(Instance,PropName));
end;

Function IsReadableProp(AClass: TClass; const PropName: string): Boolean;
begin
  Result:=IsReadableProp(FindPropInfo(AClass,PropName));
end;

Function IsWriteableProp(PropInfo: PPropInfo): Boolean;
begin
  Result:=(((PropInfo^.PropProcs shr 2) and 3) in [ptField,ptStatic,ptVirtual]);
end;

Function IsWriteableProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsWriteableProp(FindPropInfo(Instance,PropName));
end;

Function IsWriteableProp(AClass: TClass; const PropName: string): Boolean;
begin
  Result:=IsWriteableProp(FindPropInfo(AClass,PropName));
end;

Function IsStoredProp(Instance: TObject; PropInfo: PPropInfo): Boolean;

type
  TBooleanIndexFunc=Function(Index:integer):boolean of object;
  TBooleanFunc=Function:boolean of object;

var
  AMethod : TMethod;

begin
  case (PropInfo^.PropProcs shr 4) and 3 of
    ptField:
      Result:=PBoolean(Pointer(Instance)+PtrUInt(PropInfo^.StoredProc))^;
    ptConst:
      Result:=LongBool(PropInfo^.StoredProc);
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs shr 4) and 3=ptstatic then
          AMethod.Code:=PropInfo^.StoredProc
        else
          AMethod.Code:=pcodepointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.StoredProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
           Result:=TBooleanIndexFunc(AMethod)(PropInfo^.Index)
        else
           Result:=TBooleanFunc(AMethod)();
      end;
  end;
end;

Function GetClassPropInfosEx(TypeInfo: PTypeInfo; PropList: PPropListEx; Visibilities: TVisibilityClasses): Integer;

Var
  TD : PPropDataEx;
  TP : PPropListEx;
  Offset,I,Count : Longint;

begin
  Result:=0;
  // Clear list
  repeat
    TD:=PClassData(GetTypeData(TypeInfo))^.ExRTTITable;
    if PropList<>Nil then
      FillChar(PropList^,TD^.PropCount*sizeof(TPropInfoEx),0);
    Count:=TD^.PropCount;
    // Now point TP to first propinfo record.
    Inc(Pointer(TP),SizeOF(Word));
    tp:=aligntoptr(tp);
    For I:=0 to Count-1 do
      if ([]=Visibilities) or (PropList^[Result]^.Visibility in Visibilities) then
        begin
        // When passing nil, we just need the count
        if Assigned(PropList) then
          PropList^[Result]:=TD^.Prop[i];
        Inc(Result);
        end;
    if PClassData(GetTypeData(TypeInfo))^.Parent<>Nil then
      TypeInfo:=Nil
    else
      TypeInfo:=PClassData(GetTypeData(TypeInfo))^.Parent^;
  until TypeInfo=nil;
end;


Function GetRecordPropInfosEx(TypeInfo: PTypeInfo; PropList: PPropListEx; Visibilities: TVisibilityClasses): Integer;

Var
  TD : PPropDataEx;
  TP : PPropListEx;
  Offset,I,Count : Longint;

begin
  Result:=0;
  // Clear list
  TD:=PRecordData(GetTypeData(TypeInfo))^.ExRTTITable;
  Count:=TD^.PropCount;
  // Now point TP to first propinfo record.
  Inc(Pointer(TP),SizeOF(Word));
  tp:=aligntoptr(tp);
  For I:=0 to Count-1 do
    if ([]=Visibilities) or (PropList^[Result]^.Visibility in Visibilities) then
      begin
      // When passing nil, we just need the count
      if Assigned(PropList) then
        PropList^[Result]:=TD^.Prop[i];
      Inc(Result);
      end;
end;


Function GetPropInfosEx(TypeInfo: PTypeInfo; PropList: PPropListEx; Visibilities: TVisibilityClasses): Integer;

begin
  if TypeInfo^.Kind=tkClass then
    Result:=GetClassPropInfosEx(TypeInfo,PropList,Visibilities)
  else if TypeInfo^.Kind=tkRecord then
    Result:=GetRecordPropInfosEx(TypeInfo,PropList,Visibilities)
  else
    Result:=0;
end;

Procedure InsertPropEx (PL : PProplistEx;PI : PPropInfoEx; Count : longint);

Var
  I : Longint;

begin
  I:=0;
  While (I<Count) and (PI^.Info^.Name>PL^[I]^.Info^.Name) do
    Inc(I);
  If I<Count then
    Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
  PL^[I]:=PI;
end;


Procedure InsertPropnosortEx (PL : PProplistEx;PI : PPropInfoEx; Count : longint);

begin
  PL^[Count]:=PI;
end;


Function GetPropListEx(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropListEx; Sorted: boolean;
  Visibilities: TVisibilityClasses): longint;

Type
   TInsertPropEx = Procedure (PL : PProplistEx;PI : PPropInfoex; Count : longint);
{
  Store Pointers to property information OF A CERTAIN KIND in the list pointed
  to by proplist. PRopList must contain enough space to hold ALL
  properties.
}

Var
  TempList : PPropListEx;
  PropInfo : PPropinfoEx;
  I,Count : longint;
  DoInsertPropEx : TInsertPropEx;

begin
  if sorted then
    DoInsertPropEx:=@InsertPropEx
  else
    DoInsertPropEx:=@InsertPropnosortEx;
  Result:=0;
  Count:=GetPropListEx(TypeInfo,TempList,Visibilities);
  Try
     For I:=0 to Count-1 do
       begin
       PropInfo:=TempList^[i];
       If PropInfo^.Info^.PropType^.Kind in TypeKinds then
         begin
         If (PropList<>Nil) then
           DoInsertPropEx(PropList,PropInfo,Result);
         Inc(Result);
         end;
       end;
  finally
    FreeMem(TempList,Count*SizeOf(Pointer));
  end;
end;


Function GetPropListEx(TypeInfo: PTypeInfo; out PropList: PPropListEx; Visibilities: TVisibilityClasses): SizeInt;

begin
  // When passing nil, we get the count
  result:=GetPropInfosEx(TypeInfo,Nil,Visibilities);
  if result>0 then
    begin
      getmem(PropList,result*sizeof(pointer));
      GetPropInfosEx(TypeInfo,PropList);
    end
  else
    PropList:=Nil;
end;


Function GetPropListEx(AClass: TClass; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): Integer;

begin
  Result:=GetPropListEx(PTypeInfo(aClass.ClassInfo),PropList,Visibilities);
end;


Function GetPropListEx(Instance: TObject; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): Integer;

begin
  Result:=GetPropListEx(Instance.ClassType,PropList,Visibilities);
end;


Function GetFieldInfos(aRecord: PRecordData; FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses): Integer;

Var
   FieldTable: PExtendedFieldTable;
   FieldEntry: PExtendedFieldEntry;
   I : Integer;

begin
  Result:=0;
  if aRecord=Nil then exit;
  FieldTable:=aRecord^.ExtendedFields;
  if FieldTable=Nil then exit;
  if FieldList<>Nil then
    FillChar(FieldList^[Result],FieldTable^.FieldCount*sizeof(Pointer),0);
  For I:=0 to FieldTable^.FieldCount-1 do
    begin
    FieldEntry:=FieldTable^.Field[i];
    if ([]=Visibilities) or (FieldEntry^.FieldVisibility in Visibilities) then
      begin
      if Assigned(FieldList) then
        FieldList^[Result]:=FieldEntry;
      Inc(Result);
      end;
    end;
end;


Function GetFieldInfos(aClass: TClass; FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses): Integer;

var
  vmt: PVmt;
  FieldTable: PVmtExtendedFieldTable;
  FieldEntry: PExtendedVmtFieldEntry;
  i: longint;

begin
  Result:=0;
  vmt := PVmt(AClass);
  while vmt <> nil do
    begin
    // a class can not have any fields...
    if vmt^.vFieldTable<>Nil then
      begin
      FieldTable := PVmtExtendedFieldTable(PVmtFieldTable(vmt^.vFieldTable)^.Next);
      if FieldList<>Nil then
        FillChar(FieldList^[Result],FieldTable^.FieldCount*sizeof(Pointer),0);
      For I:=0 to FieldTable^.FieldCount-1 do
        begin
        FieldEntry:=FieldTable^.Field[i];
        if ([]=Visibilities) or (FieldEntry^.FieldVisibility in Visibilities) then
          begin
          if Assigned(FieldList) then
            FieldList^[Result]:=FieldEntry;
          Inc(Result);
          end;
        end;
      end;
    { Go to parent type }
    vmt:=vmt^.vParent;
    end;
end;


Function GetFieldInfos(TypeInfo: PTypeInfo; FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  if TypeInfo^.Kind=tkRecord then
    Result:=GetFieldInfos(PRecordData(GetTypeData(TypeInfo)),FieldList,Visibilities)
  else if TypeInfo^.Kind=tkClass then
    Result:=GetFieldInfos((PClassData(GetTypeData(TypeInfo))^.ClassType),FieldList,Visibilities)
  else
    Result:=0
end;


Procedure InsertFieldEntry (PL : PExtendedFieldInfoTable;PI : PExtendedVmtFieldEntry; Count : longint);

Var
  I : Longint;

begin
  I:=0;
  While (I<Count) and (PI^.Name^>PL^[I]^.Name^) do
    Inc(I);
  If I<Count then
    Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
  PL^[I]:=PI;
end;


Procedure InsertFieldEntryNoSort (PL : PExtendedFieldInfoTable;PI : PExtendedVmtFieldEntry; Count : longint);

begin
  PL^[Count]:=PI;
end;

Function GetFieldList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; out FieldList: PExtendedFieldInfoTable; Sorted: boolean;
  Visibilities: TVisibilityClasses): longint;

Type
   TInsertField = Procedure (PL : PExtendedFieldInfoTable;PI : PExtendedVmtFieldEntry; Count : longint);
{
  Store Pointers to property information OF A CERTAIN KIND in the list pointed
  to by proplist. PRopList must contain enough space to hold ALL
  properties.
}

Var
  TempList : PExtendedFieldInfoTable;
  FieldEntry : PExtendedVmtFieldEntry;
  I,Count : longint;
  DoInsertField : TInsertField;

begin
  if sorted then
    DoInsertField:=@InsertFieldEntry
  else
    DoInsertField:=@InsertFieldEntryNoSort;
  Result:=0;
  Count:=GetFieldList(TypeInfo,TempList,Visibilities);
  Try
     For I:=0 to Count-1 do
       begin
       FieldEntry:=TempList^[i];
       If PPTypeInfo(FieldEntry^.FieldType)^^.Kind in TypeKinds then
         begin
         If (FieldList<>Nil) then
           DoInsertField(FieldList,FieldEntry,Result);
         Inc(Result);
         end;
       end;
  finally
    FreeMem(TempList);
  end;
end;


Function GetRecordFieldList(aRecord: PRecordData; out FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses
  ): Integer;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetFieldInfos(aRecord,Nil,[]);
  FieldList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetFieldInfos(aRecord,FieldList,Visibilities);
  except
    FreeMem(FieldList);
    Raise;
  end;
end;


Function GetFieldList(TypeInfo: PTypeInfo; out FieldList : PExtendedFieldInfoTable; Visibilities: TVisibilityClasses): SizeInt;

begin
  if TypeInfo^.Kind=tkRecord then
    Result:=GetRecordFieldList(PRecordData(GetTypeData(TypeInfo)),FieldList,Visibilities)
  else if TypeInfo^.Kind=tkClass then
    Result:=GetFieldInfos((PClassData(GetTypeData(TypeInfo))^.ClassType),FieldList,Visibilities)
  else
    Result:=0
end;


Function GetFieldList(AClass: TClass; out FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses): Integer;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetFieldInfos(aClass,Nil,Visibilities);
  FieldList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetFieldInfos(aClass,FieldList,Visibilities);
  except
    FreeMem(FieldList);
    Raise;
  end;
end;


Function GetFieldList(Instance: TObject; out FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  Result:=GetFieldList(Instance.ClassType,FieldList,Visibilities);
end;

{ -- Methods -- }

Function GetMethodInfos(aRecord: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  Result:=GetRecordMethodInfos(aRecord,MethodList,Visibilities)
end;

Function GetClassMethodInfos(aClassData: PClassData; MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses): Integer;


var
  MethodTable: PVmtMethodExTable;
  MethodEntry: PVmtMethodExEntry;
  i: longint;

begin
  Result:=0;
  While aClassData<>Nil do
    begin
    MethodTable:=aClassData^.ExMethodTable;
    // if LegacyCount=0 then Count1 and Count are not available.
    if (MethodTable<>Nil) and (MethodTable^.Count<>0) then
      begin
      For I:=0 to MethodTable^.Count-1 do
        begin
        MethodEntry:=MethodTable^.Method[i];
        if ([]=Visibilities) or (MethodEntry^.MethodVisibility in Visibilities) then
          begin
          if Assigned(MethodList) then
            MethodList^[Result]:=MethodEntry;
          Inc(Result);
          end;
        end;
      end;
    { Go to parent type }
    if aClassData^.Parent<>Nil then
      aClassData:=Nil
    else
      aClassData:=PClassData(GetTypeData(aClassData^.Parent^)); ;
    end;

end;

Function GetMethodInfos(aClass: TClass; MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  Result:=GetMethodInfos(PTypeInfo(aClass.ClassInfo),MethodList,Visibilities);
end;

Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;

begin
  if TypeInfo^.Kind=tkRecord then
   Result:=GetRecordMethodInfos(PRecordData(GetTypeData(TypeInfo)),MethodList,Visibilities)
  else
    Result:=0
end;

Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  if TypeInfo^.Kind=tkClass then
    Result:=GetClassMethodInfos(PClassData(GetTypeData(TypeInfo)),MethodList,Visibilities)
  else
    Result:=0
end;


Procedure InsertMethodEntry (PL : PExtendedMethodInfoTable;PI : PVmtMethodExEntry; Count : longint);

Var
  I : Longint;

begin
  I:=0;
  While (I<Count) and (PI^.GetName >PL^[I]^.GetName) do
    Inc(I);
  If I<Count then
    Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
  PL^[I]:=PI;
end;


Procedure InsertMethodEntryNoSort (PL : PExtendedMethodInfoTable;PI : PVmtMethodExEntry; Count : longint);

begin
  PL^[Count]:=PI;
end;


Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Sorted: boolean;
  Visibilities: TVisibilityClasses): longint;

Type
   TInsertMethod = Procedure (PL : PExtendedMethodInfoTable;PI : PVmtMethodExEntry; Count : longint);
{
  Store Pointers to method information OF A CERTAIN visibility in the list pointed
  to by methodlist. MethodList must contain enough space to hold ALL methods.
}

Var
  TempList : PExtendedMethodInfoTable;
  MethodEntry : PVmtMethodExEntry;
  I,aCount : longint;
  DoInsertMethod : TInsertMethod;

begin
  MethodList:=nil;
  Result:=0;
  aCount:=GetMethodList(TypeInfo,TempList,Visibilities);
  if aCount=0 then
    exit;
  if sorted then
    DoInsertMethod:=@InsertMethodEntry
  else
    DoInsertMethod:=@InsertMethodEntryNoSort;
  MethodList:=GetMem(aCount*SizeOf(Pointer));
  Try
     For I:=0 to aCount-1 do
       begin
       MethodEntry:=TempList^[i];
       DoInsertMethod(MethodList,MethodEntry,Result);
       Inc(Result);
       end;
  finally
    FreeMem(TempList);
  end;
end;

Procedure InsertRecMethodEntry (PL : PRecordMethodInfoTable;PI : PRecMethodExEntry; Count : longint);

Var
  I : Longint;

begin
  I:=0;
  While (I<Count) and (PI^.GetName >PL^[I]^.GetName) do
    Inc(I);
  If I<Count then
    Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
  PL^[I]:=PI;
end;


Procedure InsertRecMethodEntryNoSort (PL : PRecordMethodInfoTable;PI : PRecMethodExEntry; Count : longint);

begin
  PL^[Count]:=PI;
end;

Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PRecordMethodInfoTable; Sorted: boolean = true; Visibilities : TVisibilityClasses = []): longint;

Type
   TInsertMethod = Procedure (PL : PRecordMethodInfoTable;PI : PRecMethodExEntry; Count : longint);
{
  Store Pointers to method information OF A CERTAIN visibility in the list pointed
  to by methodlist. MethodList must contain enough space to hold ALL methods.
}

Var
  TempList : PRecordMethodInfoTable;
  MethodEntry : PRecMethodExEntry;
  I,aCount : longint;
  DoInsertMethod : TInsertMethod;

begin
  MethodList:=nil;
  Result:=0;
  aCount:=GetMethodList(TypeInfo,TempList,Visibilities);
  if aCount=0 then
    exit;
  if sorted then
    DoInsertMethod:=@InsertRecMethodEntry
  else
    DoInsertMethod:=@InsertRecMethodEntryNoSort;
  MethodList:=GetMem(aCount*SizeOf(Pointer));
  Try
     For I:=0 to aCount-1 do
       begin
       MethodEntry:=TempList^[i];
       DoInsertMethod(MethodList,MethodEntry,Result);
       Inc(Result);
       end;
  finally
    FreeMem(TempList);
  end;
end;


Function GetRecordMethodInfos(aRecordData: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities: TVisibilityClasses): Integer;


var
  MethodTable: PRecordMethodTable;
  MethodEntry: PRecMethodExEntry;
  i: longint;

begin
  Result:=0;
  if aRecordData=Nil then
    Exit;
  MethodTable:=aRecordData^.GetMethodTable;
  if MethodTable=Nil then
    Exit;
  For I:=0 to MethodTable^.Count-1 do
    begin
    MethodEntry:=MethodTable^.Method[i];
    if ([]=Visibilities) or (MethodEntry^.MethodVisibility in Visibilities) then
      begin
      if Assigned(MethodList) then
        MethodList^[Result]:=MethodEntry;
      Inc(Result);
      end;
    end;
end;

Function GetRecordMethodList(aRecord: PRecordData; out MethodList: PRecordMethodInfoTable; Visibilities: TVisibilityClasses
  ): Integer;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetRecordMethodInfos(aRecord,Nil,Visibilities);
  if aCount=0 then
    exit;
  MethodList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetRecordMethodInfos(aRecord,MethodList,Visibilities);
  except
    FreeMem(MethodList);
    Raise;
  end;
end;

Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []): longint;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetMethodInfos(TypeInfo,PRecordMethodInfoTable(Nil),Visibilities);
  MethodList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetMethodInfos(TypeInfo,MethodList,Visibilities);
  except
    FreeMem(MethodList);
    Raise;
  end;
end;

Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []): longint;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetMethodInfos(TypeInfo,PExtendedMethodInfoTable(Nil),Visibilities);
  MethodList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetMethodInfos(TypeInfo,MethodList,Visibilities);
  except
    FreeMem(MethodList);
    Raise;
  end;
end;

Function GetMethodList(AClass: TClass; out MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses): Integer;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetMethodInfos(aClass,Nil,[]);
  MethodList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetMethodInfos(aClass,MethodList,Visibilities);
  except
    FreeMem(MethodList);
    Raise;
  end;
end;


Function GetMethodList(Instance: TObject; out MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  Result:=GetMethodList(Instance.ClassType,MethodList,Visibilities);
end;


{ -- Properties -- }

Procedure GetPropInfos(TypeInfo: PTypeInfo; PropList: PPropList);
{
        Store Pointers to property information in the list pointed
        to by proplist. PRopList must contain enough space to hold ALL
        properties.
}
Var
  TD : PTypeData;
  TP : PPropInfo;
  Count : Longint;
begin
  // Get this objects TOTAL published properties count
  TD:=GetTypeData(TypeInfo);
  // Clear list
  FillChar(PropList^,TD^.PropCount*sizeof(Pointer),0);
  repeat
    TD:=GetTypeData(TypeInfo);
    // published properties count for this object
    TP:=PPropInfo(GetPropData(TypeInfo, TD));
    Count:=PWord(TP)^;
    // Now point TP to first propinfo record.
    Inc(Pointer(TP),SizeOF(Word));
    tp:=aligntoptr(tp);
    While Count>0 do
      begin
        // Don't overwrite properties with the same name
        if PropList^[TP^.NameIndex]=nil then
          PropList^[TP^.NameIndex]:=TP;
        // Point to TP next propinfo record.
        // Located at Name[Length(Name)+1] !
        TP:=aligntoptr(PPropInfo(pointer(@TP^.Name)+PByte(@TP^.Name)^+1));
        Dec(Count);
      end;
    TypeInfo:=TD^.Parentinfo;
  until TypeInfo=nil;
end;

Procedure InsertProp (PL : PProplist;PI : PPropInfo; Count : longint);
Var
  I : Longint;
begin
  I:=0;
  While (I<Count) and (PI^.Name>PL^[I]^.Name) do
    Inc(I);
  If I<Count then
    Move(PL^[I], PL^[I+1], (Count - I) * SizeOf(Pointer));
  PL^[I]:=PI;
end;

Procedure InsertPropnosort (PL : PProplist;PI : PPropInfo; Count : longint);
begin
  PL^[Count]:=PI;
end;

Type TInsertProp = Procedure (PL : PProplist;PI : PPropInfo; Count : longint);

//Const InsertProps : array[false..boolean] of TInsertProp = (InsertPropNoSort,InsertProp);

Function GetPropList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropList; Sorted: boolean): longint;

{
  Store Pointers to property information OF A CERTAIN KIND in the list pointed
  to by proplist. PRopList must contain enough space to hold ALL
  properties.
}

Var
  TempList : PPropList;
  PropInfo : PPropinfo;
  I,Count : longint;
  DoInsertProp : TInsertProp;
begin
  if sorted then
    DoInsertProp:=@InsertProp
  else
    DoInsertProp:=@InsertPropnosort;
  Result:=0;
  Count:=GetTypeData(TypeInfo)^.Propcount;
  If Count>0 then
    begin
      GetMem(TempList,Count*SizeOf(Pointer));
      Try
        GetPropInfos(TypeInfo,TempList);
        For I:=0 to Count-1 do
          begin
            PropInfo:=TempList^[i];
            If PropInfo^.PropType^.Kind in TypeKinds then
              begin
                If (PropList<>Nil) then
                  DoInsertProp(PropList,PropInfo,Result);
                Inc(Result);
              end;
          end;
      finally
        FreeMem(TempList,Count*SizeOf(Pointer));
      end;
    end;
end;


Function GetPropList(TypeInfo: PTypeInfo; out PropList: PPropList): SizeInt;
begin
  result:=GetTypeData(TypeInfo)^.Propcount;
  if result>0 then
    begin
      getmem(PropList,result*sizeof(pointer));
      GetPropInfos(TypeInfo,PropList);
    end
  else
    PropList:=Nil;
end;

Function GetPropList(AClass: TClass; out PropList: PPropList): Integer;
begin
  Result := GetPropList(PTypeInfo(AClass.ClassInfo), PropList);
end;

Function GetPropList(Instance: TObject; out PropList: PPropList): Integer;
begin
  Result := GetPropList(Instance.ClassType, PropList);
end;

{ ---------------------------------------------------------------------
  Property access functions
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  Ordinal properties
  ---------------------------------------------------------------------}

Function GetOrdProp(Instance: TObject; PropInfo: PPropInfo): Int64;

type
  TGetInt64ProcIndex=Function(index:longint):Int64 of object;
  TGetInt64Proc=Function():Int64 of object;
  TGetIntegerProcIndex=Function(index:longint):longint of object;
  TGetIntegerProc=Function:longint of object;
  TGetWordProcIndex=Function(index:longint):word of object;
  TGetWordProc=Function:word of object;
  TGetByteProcIndex=Function(index:longint):Byte of object;
  TGetByteProc=Function:Byte of object;
var
  TypeInfo: PTypeInfo;
  AMethod : TMethod;
  DataSize: Integer;
  OrdType: TOrdType;
  Signed: Boolean;
begin
  Result:=0;

  TypeInfo := PropInfo^.PropType;
  Signed := false;
  DataSize := 4;
  case TypeInfo^.Kind of
// We keep this for backwards compatibility, but internally it is no longer used.
{$ifdef cpu64}
    tkInterface,
    tkInterfaceRaw,
    tkDynArray,
    tkClass:
      DataSize:=8;
{$endif cpu64}
    tkChar, tkBool:
      DataSize:=1;
    tkWChar:
      DataSize:=2;
    tkSet,
    tkEnumeration,
    tkInteger:
      begin
        OrdType:=GetTypeData(TypeInfo)^.OrdType;
        case OrdType of
          otSByte,otUByte: DataSize := 1;
          otSWord,otUWord: DataSize := 2;
        end;
        Signed := OrdType in [otSByte,otSWord,otSLong];
      end;
    tkInt64 :
      begin
        DataSize:=8;
        Signed:=true;
      end;
    tkQword :
      begin
        DataSize:=8;
        Signed:=false;
      end;
  end;

  case (PropInfo^.PropProcs) and 3 of
    ptField:
      if Signed then begin
        case DataSize of
          1: Result:=PShortInt(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          2: Result:=PSmallInt(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          4: Result:=PLongint(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          8: Result:=PInt64(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
        end;
      end else begin
        case DataSize of
          1: Result:=PByte(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          2: Result:=PWord(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          4: Result:=PLongint(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          8: Result:=PInt64(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
        end;
      end;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then begin
          case DataSize of
            1: Result:=TGetByteProcIndex(AMethod)(PropInfo^.Index);
            2: Result:=TGetWordProcIndex(AMethod)(PropInfo^.Index);
            4: Result:=TGetIntegerProcIndex(AMethod)(PropInfo^.Index);
            8: result:=TGetInt64ProcIndex(AMethod)(PropInfo^.Index)
          end;
        end else begin
          case DataSize of
            1: Result:=TGetByteProc(AMethod)();
            2: Result:=TGetWordProc(AMethod)();
            4: Result:=TGetIntegerProc(AMethod)();
            8: result:=TGetInt64Proc(AMethod)();
          end;
        end;
        if Signed then begin
          case DataSize of
            1: Result:=ShortInt(Result);
            2: Result:=SmallInt(Result);
          end;
        end;
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;


Procedure SetOrdProp(Instance: TObject; PropInfo: PPropInfo; Value: Int64);

type
  TSetInt64ProcIndex=Procedure(index:longint;i:Int64) of object;
  TSetInt64Proc=Procedure(i:Int64) of object;
  TSetIntegerProcIndex=Procedure(index,i:longint) of object;
  TSetIntegerProc=Procedure(i:longint) of object;
var
  DataSize: Integer;
  AMethod : TMethod;
begin
  if PropInfo^.PropType^.Kind in [tkInt64,tkQword
  { why do we have to handle classes here, see also below? (FK) }
{$ifdef cpu64}
    ,tkInterface
    ,tkInterfaceRaw
    ,tkDynArray
    ,tkClass
{$endif cpu64}
    ] then
    DataSize := 8
  else
    DataSize := 4;
  if not(PropInfo^.PropType^.Kind in [tkInt64,tkQword,tkClass,tkInterface,tkInterfaceRaw,tkDynArray]) then
    begin
      { cut off unnecessary stuff }
      case GetTypeData(PropInfo^.PropType)^.OrdType of
        otSWord,otUWord:
          begin
            Value:=Value and $ffff;
            DataSize := 2;
          end;
        otSByte,otUByte:
          begin
            Value:=Value and $ff;
            DataSize := 1;
          end;
       end;
    end;
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptField:
      case DataSize of
        1: PByte(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Byte(Value);
        2: PWord(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Word(Value);
        4: PLongint(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Longint(Value);
        8: PInt64(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
      end;
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if datasize=8 then
          begin
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetInt64ProcIndex(AMethod)(PropInfo^.Index,Value)
            else
              TSetInt64Proc(AMethod)(Value);
          end
        else
          begin
            if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
              TSetIntegerProcIndex(AMethod)(PropInfo^.Index,Value)
            else
              TSetIntegerProc(AMethod)(Value);
          end;
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
  end;
end;


Function GetOrdProp(Instance: TObject; const PropName: string): Int64;
begin
  Result:=GetOrdProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetOrdProp(Instance: TObject; const PropName: string; Value: Int64);
begin
  SetOrdProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetEnumProp(Instance: TObject; const PropInfo: PPropInfo): string;
begin
  Result:=GetEnumName(PropInfo^.PropType, GetOrdProp(Instance, PropInfo));
end;


Function GetEnumProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetEnumProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetEnumProp(Instance: TObject; const PropName: string; const Value: string);
begin
  SetEnumProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetEnumProp(Instance: TObject; const PropInfo: PPropInfo; const Value: string);
Var
  PV : Longint;
begin
  If PropInfo<>Nil then
    begin
      PV:=GetEnumValue(PropInfo^.PropType, Value);
      if (PV<0) then
        raise EPropertyError.CreateFmt(SErrUnknownEnumValue, [Value]);
      SetOrdProp(Instance, PropInfo,PV);
    end;
end;


{ ---------------------------------------------------------------------
  Int64 wrappers
  ---------------------------------------------------------------------}

Function GetInt64Prop(Instance: TObject; PropInfo: PPropInfo): Int64;
begin
  Result:=GetOrdProp(Instance,PropInfo);
end;


Procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo; const Value: Int64);
begin
  SetOrdProp(Instance,PropInfo,Value);
end;


Function GetInt64Prop(Instance: TObject; const PropName: string): Int64;
begin
  Result:=GetInt64Prop(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetInt64Prop(Instance: TObject; const PropName: string; const Value: Int64);
begin
  SetInt64Prop(Instance,FindPropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  Set properties
  ---------------------------------------------------------------------}

Function GetSetProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetSetProp(Instance,PropName,False);
end;


Function GetSetProp(Instance: TObject; const PropName: string; Brackets: Boolean): string;
begin
  Result:=GetSetProp(Instance,FindPropInfo(Instance,PropName),Brackets);
end;


Function GetSetProp(Instance: TObject; const PropInfo: PPropInfo; Brackets: Boolean): string;
begin
  Result:=SetToString(PropInfo,GetOrdProp(Instance,PropInfo),Brackets);
end;


Procedure SetSetProp(Instance: TObject; const PropName: string; const Value: string);
begin
  SetSetProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetSetProp(Instance: TObject; const PropInfo: PPropInfo; const Value: string);
begin
  SetOrdProp(Instance,PropInfo,StringToSet(PropInfo,Value));
end;

{ ---------------------------------------------------------------------
  Pointer properties - internal only
  ---------------------------------------------------------------------}

Function GetPointerProp(Instance: TObject; PropInfo : PPropInfo): Pointer;

Type
  TGetPointerProcIndex = Function (index:longint): Pointer of object;
  TGetPointerProc = Function (): Pointer of object;

var
  AMethod : TMethod;

begin
  case (PropInfo^.PropProcs) and 3 of
  ptField:
    Result := PPointer(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
  ptStatic,
  ptVirtual:
    begin
      if (PropInfo^.PropProcs and 3)=ptStatic then
        AMethod.Code:=PropInfo^.GetProc
      else
        AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
      AMethod.Data:=Instance;
      if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
        Result:=TGetPointerProcIndex(AMethod)(PropInfo^.Index)
      else
        Result:=TGetPointerProc(AMethod)();
    end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;

Procedure SetPointerProp(Instance: TObject; PropInfo : PPropInfo;  Value: Pointer);

type
  TSetPointerProcIndex = Procedure(index: longint; p: pointer) of object;
  TSetPointerProc = Procedure(p: pointer) of object;

var
  AMethod : TMethod;

begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptField:
      PPointer(Pointer(Instance) + LongWord(PropInfo^.SetProc))^:=Value;
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetPointerProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetPointerProc(AMethod)(Value);
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
  end;
end;


{ ---------------------------------------------------------------------
  Object properties
  ---------------------------------------------------------------------}

Function GetObjectProp(Instance: TObject; const PropName: string): TObject;
begin
  Result:=GetObjectProp(Instance,PropName,Nil);
end;


Function GetObjectProp(Instance: TObject; const PropName: string; MinClass: TClass): TObject;
begin
  Result:=GetObjectProp(Instance,FindPropInfo(Instance,PropName),MinClass);
end;


Function GetObjectProp(Instance: TObject; PropInfo: PPropInfo): TObject;
begin
  Result:=GetObjectProp(Instance,PropInfo,Nil);
end;


Function GetObjectProp(Instance: TObject; PropInfo: PPropInfo; MinClass: TClass): TObject;
begin
  Result:=TObject(GetPointerProp(Instance,PropInfo));
  If (MinClass<>Nil) and (Result<>Nil) Then
    If Not Result.InheritsFrom(MinClass) then
      Result:=Nil;
end;


Procedure SetObjectProp(Instance: TObject; const PropName: string; Value: TObject);
begin
  SetObjectProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetObjectProp(Instance: TObject; PropInfo: PPropInfo; Value: TObject);

begin
  SetPointerProp(Instance,PropInfo,Pointer(Value));
end;


Function GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
begin
  Result:=GetTypeData(FindPropInfo(Instance,PropName,[tkClass])^.PropType)^.ClassType;
end;


Function GetObjectPropClass(AClass: TClass; const PropName: string): TClass;
begin
  Result:=GetTypeData(FindPropInfo(AClass,PropName,[tkClass])^.PropType)^.ClassType;
end;

{ ---------------------------------------------------------------------
    Interface wrapprers
  ---------------------------------------------------------------------}


Function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;

begin
  Result:=GetInterfaceProp(Instance,FindPropInfo(Instance,PropName));
end;


Function GetInterfaceProp(Instance: TObject; PropInfo: PPropInfo): IInterface;
type
  TGetInterfaceProc=Function:IInterface of object;
  TGetInterfaceProcIndex=Function(index:longint):IInterface of object;
var
  AMethod : TMethod;
begin
  Result:=nil;
  case (PropInfo^.PropProcs) and 3 of
    ptField:
      Result:=IInterface(PPointer(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^);
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=TGetInterfaceProcIndex(AMethod)(PropInfo^.Index)
        else
          Result:=TGetInterfaceProc(AMethod)();
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;


Procedure SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);

begin
  SetInterfaceProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

Procedure SetInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface);

type
  TSetIntfStrProcIndex=Procedure(index:longint;const i:IInterface) of object;
  TSetIntfStrProc=Procedure(i:IInterface) of object;

var
  AMethod : TMethod;

begin
  case Propinfo^.PropType^.Kind of
    tkInterface:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PInterface(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
          ptStatic,
          ptVirtual:
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetIntfStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetIntfStrProc(AMethod)(Value);
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
        end;
      end;
    tkInterfaceRaw:
      Raise Exception.Create('Cannot set RAW interface from IUnknown interface');
  end;
end;

{ ---------------------------------------------------------------------
    RAW (Corba) Interface wrapprers
  ---------------------------------------------------------------------}


Function GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;

begin
  Result:=GetRawInterfaceProp(Instance,FindPropInfo(Instance,PropName));
end;

Function GetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo): Pointer;

begin
  Result:=GetPointerProp(Instance,PropInfo);
end;

Procedure SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);

begin
  SetRawInterfaceProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

Procedure SetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

begin
  SetPointerProp(Instance,PropInfo,Value);
end;

{ ---------------------------------------------------------------------
  Dynamic array properties
  ---------------------------------------------------------------------}

Function GetDynArrayProp(Instance: TObject; const PropName: string): Pointer;
begin
  Result:=GetDynArrayProp(Instance,FindPropInfo(Instance,PropName));
end;

Function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;

type
  { we need a dynamic array as that type is usually passed differently from
    a plain pointer }
  TDynArray=array of Byte;
  TGetDynArrayProc=Function:TDynArray of object;
  TGetDynArrayProcIndex=Function(index:longint):TDynArray of object;

var
  AMethod : TMethod;

begin
  Result:=nil;
  if PropInfo^.PropType^.Kind<>tkDynArray then
    Exit;
  case (PropInfo^.PropProcs) and 3 of
    ptField:
      Result:=PPointer(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=Pointer(TGetDynArrayProcIndex(AMethod)(PropInfo^.Index))
        else
          Result:=Pointer(TGetDynArrayProc(AMethod)());
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;

Procedure SetDynArrayProp(Instance: TObject; const PropName: string; const Value: Pointer);
begin
  SetDynArrayProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

Procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

type
  { we need a dynamic array as that type is usually passed differently from
    a plain pointer }
  TDynArray=array of Byte;
  TSetDynArrayProcIndex=Procedure(index:longint;const i:TDynArray) of object;
  TSetDynArrayProc=Procedure(i:TDynArray) of object;

var
  AMethod: TMethod;

begin
  if PropInfo^.PropType^.Kind<>tkDynArray then
    Exit;
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptField:
      CopyArray(PPointer(Pointer(Instance)+PtrUInt(PropInfo^.SetProc)), @Value, PropInfo^.PropType, 1);
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetDynArrayProcIndex(AMethod)(PropInfo^.Index,TDynArray(Value))
        else
          TSetDynArrayProc(AMethod)(TDynArray(Value));
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
  end;
end;

{ ---------------------------------------------------------------------
  String properties
  ---------------------------------------------------------------------}

Function GetStrProp(Instance: TObject; PropInfo: PPropInfo): Ansistring;

type
  TGetShortStrProcIndex=Function(index:longint):ShortString of object;
  TGetShortStrProc=Function():ShortString of object;
  TGetAnsiStrProcIndex=Function(index:longint):AnsiString of object;
  TGetAnsiStrProc=Function():AnsiString of object;

var
  AMethod : TMethod;

begin
  Result:='';
  case Propinfo^.PropType^.Kind of
    tkWString:
      Result:=AnsiString(GetWideStrProp(Instance,PropInfo));
    tkUString:
      Result := AnsiString(GetUnicodeStrProp(Instance,PropInfo));
    tkSString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PShortString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
          ptStatic,
          ptVirtual:
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetShortStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetShortStrProc(AMethod)();
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
        end;
      end;
    tkAString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PAnsiString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
          ptStatic,
          ptVirtual:
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetAnsiStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetAnsiStrProc(AMethod)();
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;


Procedure SetStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: Ansistring);

type
  TSetShortStrProcIndex=Procedure(index:longint;const s:ShortString) of object;
  TSetShortStrProc=Procedure(const s:ShortString) of object;
  TSetAnsiStrProcIndex=Procedure(index:longint;s:AnsiString) of object;
  TSetAnsiStrProc=Procedure(s:AnsiString) of object;

var
  AMethod : TMethod;

begin
  case Propinfo^.PropType^.Kind of
    tkWString:
      SetWideStrProp(Instance,PropInfo,WideString(Value));
    tkUString:
       SetUnicodeStrProp(Instance,PropInfo,UnicodeString(Value));
    tkSString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PShortString(Pointer(Instance) + LongWord(PropInfo^.SetProc))^:=Value;
          ptStatic,
          ptVirtual:
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetShortStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetShortStrProc(AMethod)(Value);
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
        end;
      end;
    tkAString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PAnsiString(Pointer(Instance) + LongWord(PropInfo^.SetProc))^:=Value;
          ptStatic,
          ptVirtual:
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetAnsiStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetAnsiStrProc(AMethod)(Value);
            end;
          else
            raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;


Function GetStrProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetStrProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetStrProp(Instance: TObject; const PropName: string; const Value: AnsiString);
begin
  SetStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetWideStrProp(Instance: TObject; const PropName: string): WideString;
begin
  Result:=GetWideStrProp(Instance, FindPropInfo(Instance, PropName));
end;


Procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
begin
  SetWideStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): WideString;
type
  TGetWideStrProcIndex=Function(index:longint):WideString of object;
  TGetWideStrProc=Function():WideString of object;
var
  AMethod : TMethod;
begin
  Result:='';
  case Propinfo^.PropType^.Kind of
    tkSString,tkAString:
      Result:=WideString(GetStrProp(Instance,PropInfo));
    tkUString :
      Result := GetUnicodeStrProp(Instance,PropInfo);
    tkWString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PWideString(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          ptStatic,
          ptVirtual:
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetWideStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetWideStrProc(AMethod)();
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;


Procedure SetWideStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: WideString);
type
  TSetWideStrProcIndex=Procedure(index:longint;s:WideString) of object;
  TSetWideStrProc=Procedure(s:WideString) of object;
var
  AMethod : TMethod;
begin
  case Propinfo^.PropType^.Kind of
    tkSString,tkAString:
       SetStrProp(Instance,PropInfo,AnsiString(Value));
    tkUString:
       SetUnicodeStrProp(Instance,PropInfo,Value);
    tkWString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PWideString(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
          ptStatic,
          ptVirtual:
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetWideStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetWideStrProc(AMethod)(Value);
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;

Function GetUnicodeStrProp(Instance: TObject; const PropName: string): UnicodeString;

begin
  Result:=GetUnicodeStrProp(Instance, FindPropInfo(Instance, PropName));
end;


Procedure SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);

begin
  SetUnicodeStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetUnicodeStrProp(Instance: TObject; PropInfo: PPropInfo): UnicodeString;

type
  TGetUnicodeStrProcIndex=Function(index:longint):UnicodeString of object;
  TGetUnicodeStrProc=Function():UnicodeString of object;

var
  AMethod : TMethod;

begin
  Result:='';
  case Propinfo^.PropType^.Kind of
    tkSString,tkAString:
      Result:=UnicodeString(GetStrProp(Instance,PropInfo));
    tkWString:
      Result:=GetWideStrProp(Instance,PropInfo);
    tkUString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PUnicodeString(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
          ptStatic,
          ptVirtual:
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetUnicodeStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetUnicodeStrProc(AMethod)();
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;


Procedure SetUnicodeStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: UnicodeString);

type
  TSetUnicodeStrProcIndex=Procedure(index:longint;s:UnicodeString) of object;
  TSetUnicodeStrProc=Procedure(s:UnicodeString) of object;

var
  AMethod : TMethod;

begin
  case Propinfo^.PropType^.Kind of
    tkSString,tkAString:
       SetStrProp(Instance,PropInfo,AnsiString(Value));
    tkWString:
       SetWideStrProp(Instance,PropInfo,Value);
    tkUString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PUnicodeString(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
          ptStatic,
          ptVirtual:
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetUnicodeStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetUnicodeStrProc(AMethod)(Value);
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;

Function GetRawbyteStrProp(Instance: TObject; PropInfo: PPropInfo): RawByteString;

type
  TGetRawByteStrProcIndex=Function(index:longint): RawByteString of object;
  TGetRawByteStrProc=Function():RawByteString of object;

var
  AMethod : TMethod;

begin
  Result:='';
  case Propinfo^.PropType^.Kind of
    tkWString:
      Result:=RawByteString(GetWideStrProp(Instance,PropInfo));
    tkUString:
      Result:=RawByteString(GetUnicodeStrProp(Instance,PropInfo));
    tkSString:
      Result:=RawByteString(GetStrProp(Instance,PropInfo));
    tkAString:
      begin
        case (PropInfo^.PropProcs) and 3 of
          ptField:
            Result := PAnsiString(Pointer(Instance) + LongWord(PropInfo^.GetProc))^;
          ptStatic,
          ptVirtual:
            begin
              if (PropInfo^.PropProcs and 3)=ptStatic then
                AMethod.Code:=PropInfo^.GetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                Result:=TGetRawByteStrProcIndex(AMethod)(PropInfo^.Index)
              else
                Result:=TGetRawByteStrProc(AMethod)();
            end;
          else
            raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;

Function GetRawByteStrProp(Instance: TObject; const PropName: string): RawByteString;
begin
  Result:=GetRawByteStrProp(Instance,FindPropInfo(Instance,PropName));
end;

Procedure SetRawByteStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: RawByteString);

type
  TSetRawByteStrProcIndex=Procedure(index:longint;s:RawByteString) of object;
  TSetRawByteStrProc=Procedure(s:RawByteString) of object;

var
  AMethod : TMethod;

begin
  case Propinfo^.PropType^.Kind of
    tkWString:
      SetWideStrProp(Instance,PropInfo,WideString(Value));
    tkUString:
       SetUnicodeStrProp(Instance,PropInfo,UnicodeString(Value));
    tkSString:
      SetStrProp(Instance,PropInfo,Value); // Not 100% sure about this.
    tkAString:
      begin
        case (PropInfo^.PropProcs shr 2) and 3 of
          ptField:
            PAnsiString(Pointer(Instance) + LongWord(PropInfo^.SetProc))^:=Value;
          ptStatic,
          ptVirtual:
            begin
              if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
                AMethod.Code:=PropInfo^.SetProc
              else
                AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
              AMethod.Data:=Instance;
              if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
                TSetRawByteStrProcIndex(AMethod)(PropInfo^.Index,Value)
              else
                TSetRawByteStrProc(AMethod)(Value);
            end;
        else
          raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
        end;
      end;
  end;
end;

Procedure SetRawByteStrProp(Instance: TObject; const PropName: string; const Value: RawByteString);

begin
  SetRawByteStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;



{$ifndef FPUNONE}

{ ---------------------------------------------------------------------
  Float properties
  ---------------------------------------------------------------------}

Function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;

type
  TGetExtendedProc = Function:Extended of object;
  TGetExtendedProcIndex = Function(Index: integer): Extended of object;
  TGetDoubleProc = Function:Double of object;
  TGetDoubleProcIndex = Function(Index: integer): Double of object;
  TGetSingleProc = Function:Single of object;
  TGetSingleProcIndex = Function(Index: integer):Single of object;
  TGetCurrencyProc = Function : Currency of object;
  TGetCurrencyProcIndex = Function(Index: integer) : Currency of object;

var
  AMethod : TMethod;

begin
  Result:=0.0;
  case PropInfo^.PropProcs and 3 of
    ptField:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
       ftSingle:
         Result:=PSingle(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftDouble:
         Result:=PDouble(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftExtended:
         Result:=PExtended(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftcomp:
         Result:=PComp(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       ftcurr:
         Result:=PCurrency(Pointer(Instance)+PtrUInt(PropInfo^.GetProc))^;
       end;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetSingleProc(AMethod)()
            else
              Result:=TGetSingleProcIndex(AMethod)(PropInfo^.Index);
          ftDouble:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetDoubleProc(AMethod)()
            else
              Result:=TGetDoubleProcIndex(AMethod)(PropInfo^.Index);
          ftExtended:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetExtendedProc(AMethod)()
            else
              Result:=TGetExtendedProcIndex(AMethod)(PropInfo^.Index);
          ftCurr:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              Result:=TGetCurrencyProc(AMethod)()
            else
              Result:=TGetCurrencyProcIndex(AMethod)(PropInfo^.Index);
        end;
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;


Procedure SetFloatProp(Instance: TObject; PropInfo: PPropInfo; Value: Extended);

type
  TSetExtendedProc = Procedure(const AValue: Extended) of object;
  TSetExtendedProcIndex = Procedure(Index: integer; AValue: Extended) of object;
  TSetDoubleProc = Procedure(const AValue: Double) of object;
  TSetDoubleProcIndex = Procedure(Index: integer; AValue: Double) of object;
  TSetSingleProc = Procedure(const AValue: Single) of object;
  TSetSingleProcIndex = Procedure(Index: integer; AValue: Single) of object;
  TSetCurrencyProc = Procedure(const AValue: Currency) of object;
  TSetCurrencyProcIndex = Procedure(Index: integer;  AValue: Currency) of object;

Var
  AMethod : TMethod;

begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      Case GetTypeData(PropInfo^.PropType)^.FloatType of
        ftSingle:
          PSingle(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
        ftDouble:
          PDouble(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
        ftExtended:
          PExtended(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
{$ifdef FPC_COMP_IS_INT64}
        ftComp:
          PComp(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=trunc(Value);
{$else FPC_COMP_IS_INT64}
        ftComp:
          PComp(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Comp(Value);
{$endif FPC_COMP_IS_INT64}
        ftCurr:
          PCurrency(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^:=Value;
       end;
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        Case GetTypeData(PropInfo^.PropType)^.FloatType of
          ftSingle:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetSingleProc(AMethod)(Value)
            else
              TSetSingleProcIndex(AMethod)(PropInfo^.Index,Value);
          ftDouble:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetDoubleProc(AMethod)(Value)
            else
              TSetDoubleProcIndex(AMethod)(PropInfo^.Index,Value);
          ftExtended:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetExtendedProc(AMethod)(Value)
            else
              TSetExtendedProcIndex(AMethod)(PropInfo^.Index,Value);
          ftCurr:
            if ((PropInfo^.PropProcs shr 6) and 1)=0 then
              TSetCurrencyProc(AMethod)(Value)
            else
              TSetCurrencyProcIndex(AMethod)(PropInfo^.Index,Value);
        end;
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
  end;
end;


Function GetFloatProp(Instance: TObject; const PropName: string): Extended;
begin
  Result:=GetFloatProp(Instance,FindPropInfo(Instance,PropName))
end;


Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Extended);
begin
  SetFloatProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

{$endif}

{ ---------------------------------------------------------------------
  Method properties
  ---------------------------------------------------------------------}


Function GetMethodProp(Instance: TObject; PropInfo: PPropInfo): TMethod;

type
  TGetMethodProcIndex=Function(Index: Longint): TMethod of object;
  TGetMethodProc=Function(): TMethod of object;

var
  value: PMethod;
  AMethod : TMethod;

begin
  Result.Code:=nil;
  Result.Data:=nil;
  case (PropInfo^.PropProcs) and 3 of
    ptField:
      begin
        Value:=PMethod(Pointer(Instance)+PtrUInt(PropInfo^.GetProc));
        if Value<>nil then
          Result:=Value^;
      end;
    ptStatic,
    ptVirtual:
      begin
        if (PropInfo^.PropProcs and 3)=ptStatic then
          AMethod.Code:=PropInfo^.GetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.GetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          Result:=TGetMethodProcIndex(AMethod)(PropInfo^.Index)
        else
          Result:=TGetMethodProc(AMethod)();
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotReadProperty, [PropInfo^.Name]);
  end;
end;


Procedure SetMethodProp(Instance: TObject; PropInfo: PPropInfo; const Value: TMethod);

type
  TSetMethodProcIndex=Procedure(index:longint;p:TMethod) of object;
  TSetMethodProc=Procedure(p:TMethod) of object;

var
  AMethod : TMethod;

begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptField:
      PMethod(Pointer(Instance)+PtrUInt(PropInfo^.SetProc))^ := Value;
    ptStatic,
    ptVirtual:
      begin
        if ((PropInfo^.PropProcs shr 2) and 3)=ptStatic then
          AMethod.Code:=PropInfo^.SetProc
        else
          AMethod.Code:=PCodePointer(Pointer(Instance.ClassType)+PtrUInt(PropInfo^.SetProc))^;
        AMethod.Data:=Instance;
        if ((PropInfo^.PropProcs shr 6) and 1)<>0 then
          TSetMethodProcIndex(AMethod)(PropInfo^.Index,Value)
        else
          TSetMethodProc(AMethod)(Value);
      end;
  else
    raise EPropertyError.CreateFmt(SErrCannotWriteToProperty, [PropInfo^.Name]);
  end;
end;


Function GetMethodProp(Instance: TObject; const PropName: string): TMethod;
begin
  Result:=GetMethodProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetMethodProp(Instance: TObject; const PropName: string; const Value: TMethod);
begin
  SetMethodProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  Variant properties
  ---------------------------------------------------------------------}

Procedure CheckVariantEvent(P : CodePointer);

begin
  If (P=Nil) then
    Raise Exception.Create(SErrNoVariantSupport);
end;

Function GetVariantProp(Instance: TObject; PropInfo: PPropInfo): Variant;
begin
  CheckVariantEvent(CodePointer(OnGetVariantProp));
  Result:=OnGetVariantProp(Instance,PropInfo);
end;


Procedure SetVariantProp(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);
begin
  CheckVariantEvent(CodePointer(OnSetVariantProp));
  OnSetVariantProp(Instance,PropInfo,Value);
end;


Function GetVariantProp(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetVariantProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetVariantProp(Instance: TObject; const PropName: string; const Value: Variant);
begin
  SetVariantprop(instance,FindpropInfo(Instance,PropName),Value);
end;


{ ---------------------------------------------------------------------
  All properties through variant.
  ---------------------------------------------------------------------}

Function GetPropValue(Instance: TObject; const PropName: string): Variant;
begin
  Result := GetPropValue(Instance,FindPropInfo(Instance, PropName));
end;

Function GetPropValue(Instance: TObject; const PropName: string; PreferStrings: Boolean): Variant;

begin
  Result := GetPropValue(Instance,FindPropInfo(Instance, PropName),PreferStrings);
end;

Function GetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;
begin
  Result := GetPropValue(Instance, PropInfo, True);
end;

Function GetPropValue(Instance: TObject; PropInfo: PPropInfo; PreferStrings: Boolean): Variant;

begin
  CheckVariantEvent(CodePointer(OnGetPropValue));
  Result:=OnGetPropValue(Instance,PropInfo,PreferStrings);
end;

Procedure SetPropValue(Instance: TObject; const PropName: string; const Value: Variant);

begin
  SetPropValue(Instance, FindPropInfo(Instance, PropName), Value);
end;

Procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);

begin
  CheckVariantEvent(CodePointer(OnSetPropValue));
  OnSetPropValue(Instance,PropInfo,Value);
end;


{ ---------------------------------------------------------------------
  Easy access methods that appeared in Delphi 5
  ---------------------------------------------------------------------}

Function IsPublishedProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=GetPropInfo(Instance,PropName)<>Nil;
end;

Function IsPublishedProp(AClass: TClass; const PropName: string): Boolean;
begin
  Result:=GetPropInfo(AClass,PropName)<>Nil;
end;

Function PropIsType(Instance: TObject; const PropName: string; TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(Instance,PropName)=TypeKind
end;

Function PropIsType(AClass: TClass; const PropName: string; TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(AClass,PropName)=TypeKind
end;

Function PropType(Instance: TObject; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(Instance,PropName)^.PropType^.Kind;
end;

Function PropType(AClass: TClass; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(AClass,PropName)^.PropType^.Kind;
end;

Function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsStoredProp(instance,FindPropInfo(Instance,PropName));
end;

{ TVmtMethodExTable }

Function TVmtMethodExTable.GetMethod(Index: Word): PVmtMethodExEntry;

begin
  if (Index >= Count) then
    Result := Nil
  else
    begin
      Result := aligntoptr(PVmtMethodExEntry(PByte(@Count) + SizeOf(Count)));
      while Index > 0 do
      begin
        Result := Result^.Next;
        Dec(Index);
      end;
  end;

end;

{ TRecMethodExTable }

Function TRecMethodExTable.GetMethod(Index: Word): PRecMethodExEntry;

begin
  if (Index >= Count) then
    Result := Nil
  else
    begin
      Result := aligntoptr(PRecMethodExEntry(PByte(@Count) + SizeOf(Count)));
      while Index > 0 do
      begin
        Result := Result^.Next;
        Dec(Index);
      end;
  end;

end;

{ TRecordData }

Function TRecordData.GetExPropertyTable: PPropDataEx;

Var
  MT : PRecordMethodTable;

begin
  MT:=GetMethodTable;
  if MT^.Count=0 then
    Result:=PPropDataEx(aligntoptr(PByte(@(MT^.Count))+SizeOf(Word)))
  else
    Result:=PPropDataEx(MT^.Method[MT^.Count-1]^.Tail);
end;

Function TRecordData.GetExtendedFieldCount: Longint;
begin
  Result:= PLongint(PByte(@TotalFieldCount)+SizeOf(Longint)+(TotalFieldCount*SizeOf(TManagedField)))^
end;

Function TRecordData.GetExtendedFields: PExtendedFieldTable;
begin
  Result:=PExtendedFieldTable(PByte(@TotalFieldCount)+SizeOf(Longint)+(TotalFieldCount*SizeOf(TManagedField)))
end;

Function TRecordData.GetMethodTable: PRecordMethodTable;
begin
  if GetExtendedFieldCount=0 then
    // cannot use tail
    Result:=PRecordMethodTable(PByte(@TotalFieldCount)+SizeOf(Longint)+(TotalFieldCount*SizeOf(TManagedField))+SizeOf(Longint))
  else
    Result:=PRecordMethodTable(aligntoptr(GetExtendedFields^.Tail));
end;

{ TVmtExtendedFieldTable }

Function TVmtExtendedFieldTable.GetField(aIndex: Word): PExtendedVmtFieldEntry;
begin
  Result:=Nil;
  If aIndex>=FieldCount then exit;
  Result:=PExtendedVmtFieldEntry(PByte(@FieldCount)+SizeOf(Word)+aIndex *SizeOf(TExtendedVmtFieldEntry));
end;

Function TVmtExtendedFieldTable.GetTail: Pointer;
begin
  if FieldCount=0 then
    Result:=@FieldCount+SizeOf(Word)
  else
    Result:=GetField(FieldCount-1)^.Tail;
end;

{ TExtendedVmtFieldEntry }

Function TExtendedVmtFieldEntry.GetNext: PVmtFieldEntry;
begin
  Result := aligntoptr(Tail);
end;

Function TExtendedVmtFieldEntry.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

Function TExtendedVmtFieldEntry.GetTail: Pointer;
begin
  Result := PByte(@Name) + SizeOf(Pointer);
end;

Function TExtendedVmtFieldEntry.GetVisibility: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask); // For the time being, maybe we need a AND $07 or so later on.
end;

{ TPropInfoEx }

Function TPropInfoEx.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

Function TPropInfoEx.GetTail: Pointer;
begin
  Result := PByte(@Flags) + SizeOf(Self);
end;

Function TPropInfoEx.GetVisiblity: TVisibilityClass;

begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask);
end;


{ TPropDataEx }

Function TPropDataEx.GetPropEx(Index: Word): PPropInfoEx;
begin
  if Index >= PropCount then
      Result := Nil
    else
      begin
        Result := PPropInfoEx(aligntoptr(PByte(@PropCount) + SizeOf(PropCount)));
        while Index > 0 do
          begin
            Result := aligntoptr(Result^.Tail);
            Dec(Index);
          end;
      end;
end;

Function TPropDataEx.GetTail: Pointer;
begin
  if PropCount = 0 then
    Result := PByte(@PropCount) + SizeOf(PropCount)
  else
    Result := Prop[PropCount - 1]^.Tail;
end;

{ TParameterLocation }

Function TParameterLocation.GetReference: Boolean;
begin
  Result := (LocType and $80) <> 0;
end;

Function TParameterLocation.GetRegType: TRegisterType;
begin
  Result := TRegisterType(LocType and $7F);
end;

Function TParameterLocation.GetShiftVal: Int8;
begin
  if GetReference then begin
    if Offset < Low(Int8) then
      Result := Low(Int8)
    else if Offset > High(Int8) then
      Result := High(Int8)
    else
      Result := Offset;
  end else
    Result := 0;
end;

{ TParameterLocations }

Function TParameterLocations.GetLocation(aIndex: Byte): PParameterLocation;
begin
  if aIndex >= Count then
    Result := Nil
  else
    Result := PParameterLocation(PByte(aligntoptr(PByte(@Count) + SizeOf(Count))) + SizeOf(TParameterLocation) * aIndex);
end;

Function TParameterLocations.GetTail: Pointer;
begin
  Result := PByte(aligntoptr(PByte(@Count) + SizeOf(Count))) + SizeOf(TParameterLocation) * Count;
end;

{ TProcedureParam }

Function TProcedureParam.GetParamType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ParamTypeRef);
end;

Function TProcedureParam.GetFlags: Byte;
begin
  Result := PByte(@ParamFlags)^;
end;

{ TManagedField }

Function TManagedField.GetTypeRef: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(TypeRefRef);
end;

{ TArrayTypeData }

Function TArrayTypeData.GetElType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ElTypeRef);
end;

Function TArrayTypeData.GetDims(aIndex: Byte): PTypeInfo;
begin
  Result := DerefTypeInfoPtr(DimsRef[aIndex]);
end;

{ TProcedureSignature }

Function TProcedureSignature.GetResultType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ResultTypeRef);
end;

Function TProcedureSignature.GetParam(ParamIndex: Integer): PProcedureParam;
begin
  if (ParamIndex<0)or(ParamIndex>=ParamCount) then
    Exit(nil);
  Result := PProcedureParam(PByte(@Flags) + SizeOf(Self));
  while ParamIndex > 0 do
    begin
      Result := PProcedureParam(aligntoptr((PByte(@Result^.Name) + (Length(Result^.Name) + 1) * SizeOf(AnsiChar))));
      dec(ParamIndex);
    end;
end;

{ TVmtMethodParam }

Function TVmtMethodParam.GetTail: Pointer;
begin
  Result := PByte(@ParaLocs) + SizeOf(ParaLocs);
end;

Function TVmtMethodParam.GetNext: PVmtMethodParam;
begin
  Result := PVmtMethodParam(aligntoptr(Tail));
end;

Function TVmtMethodParam.GetName: ShortString;
begin
  Result := NamePtr^;
end;

{ TIntfMethodEntry }

Function TIntfMethodEntry.GetParam(Index: Word): PVmtMethodParam;
begin
  if Index >= ParamCount then
    Result := Nil
  else
    Result := PVmtMethodParam(PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr))) + Index * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam)))));
end;

Function TIntfMethodEntry.GetResultLocs: PParameterLocations;
begin
  if not Assigned(ResultType) then
    Result := Nil
  else
    Result := PParameterLocations(PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr))) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam)))));
end;

Function TIntfMethodEntry.GetTail: Pointer;
begin
  Result := PByte(@NamePtr) + SizeOf(NamePtr);
  if ParamCount > 0 then
    Result := PByte(aligntoptr(Result)) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam))));
  if Assigned(ResultType) then
    Result := PByte(aligntoptr(Result)) + SizeOf(PParameterLocations);
end;

Function TIntfMethodEntry.GetNext: PIntfMethodEntry;
begin
  Result := PIntfMethodEntry(aligntoptr(Tail));
end;

Function TIntfMethodEntry.GetName: ShortString;
begin
  Result := NamePtr^;
end;

{ TIntfMethodTable }

Function TIntfMethodTable.GetMethod(Index: Word): PIntfMethodEntry;
begin
  if (RTTICount = $FFFF) or (Index >= RTTICount) then
    Result := Nil
  else
    begin
      Result := aligntoptr(PIntfMethodEntry(PByte(@RTTICount) + SizeOf(RTTICount)));
      while Index > 0 do
        begin
          Result := Result^.Next;
          Dec(Index);
        end;
    end;
end;

{ TVMTMethodExEntry }

function TVmtMethodExEntry.GetParamsStart: PByte;
begin
  Result:=PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr)+SizeOf(FLags)+SizeOf(VmtIndex)));
end;

Function TVmtMethodExEntry.GetMethodVisibility: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask);
end;

Function TVMTMethodExEntry.GetParam(Index: Word): PVmtMethodParam;
begin
  if Index >= ParamCount then
    Result := Nil
  else
    Result := PVmtMethodParam(GetParamsStart + Index * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam)))));
end;

Function TVMTMethodExEntry.GetResultLocs: PParameterLocations;
begin
  if not Assigned(ResultType) then
    Result := Nil
  else
    Result := PParameterLocations(GetParamsStart + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam)))));
end;

Function TVmtMethodExEntry.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

Function TVMTMethodExEntry.GetTail: Pointer;
begin
//  Result := PByte(@Flags) + SizeOf(Flags);
  Result := PByte(@VmtIndex) + SizeOf(VmtIndex);
  if ParamCount > 0 then
    Result := PByte(aligntoptr(Result)) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam))));
  if Assigned(ResultType) then
    Result := PByte(aligntoptr(Result)) + SizeOf(PParameterLocations);
end;

Function TVmtMethodExEntry.GetNext: PVmtMethodExEntry;
begin
  Result := PVmtMethodExEntry(aligntoptr(Tail));
end;

Function TVMTMethodExEntry.GetName: ShortString;
begin
  Result := NamePtr^;
end;

{ TRecMethodExEntry }

function TRecMethodExEntry.GetParamsStart: PByte;
begin
  Result:=PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr)+SizeOf(FLags)));
end;

Function TRecMethodExEntry.GetMethodVisibility: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask);
end;

Function TRecMethodExEntry.GetParam(Index: Word): PRecMethodParam;
begin
  if Index >= ParamCount then
    Result := Nil
  else
    Result := PRecMethodParam(GetParamsStart + Index * PtrUInt(aligntoptr(Pointer(SizeOf(TRecMethodParam)))));
end;

Function TRecMethodExEntry.GetResultLocs: PParameterLocations;
begin
  if not Assigned(ResultType) then
    Result := Nil
  else
    Result := PParameterLocations(GetParamsStart + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TRecMethodParam)))));
end;

Function TRecMethodExEntry.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

Function TRecMethodExEntry.GetTail: Pointer;
begin
  Result := PByte(@Flags) + SizeOf(Flags);
  if ParamCount > 0 then
    Result := PByte(aligntoptr(Result)) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TRecMethodParam))));
  if Assigned(ResultType) then
    Result := PByte(aligntoptr(Result)) + SizeOf(PParameterLocations);
end;

Function TRecMethodExEntry.GetNext: PRecMethodExEntry;
begin
  Result := PRecMethodExEntry(aligntoptr(Tail));
end;

Function TRecMethodExEntry.GetName: ShortString;
begin
  Result := NamePtr^;
end;


{ TVmtMethodTable }

Function TVmtMethodTable.GetEntry(Index: LongWord): PVmtMethodEntry;
begin
  Result := PVmtMethodEntry(@Entries[0]) + Index;
end;

{ TVmtFieldTable }

Function TVmtFieldTable.GetField(aIndex: Word): PVmtFieldEntry;
var
  c: Word;
begin
  if aIndex >= Count  then
    Exit(Nil);
  c := aIndex;
  Result := @Fields;
  while c > 0 do begin
    Result := Result^.Next;
    Dec(c);
  end;
end;

Function TVmtFieldTable.GetNext: Pointer;
begin
  Result := aligntoptr(Tail);
end;

Function TVmtFieldTable.GetTail: Pointer;

begin
  if Count=0 then
    Result := @Fields
  else
    Result:=GetField(Count-1)^.Tail;
end;

{ TVmtFieldEntry }

Function TVmtFieldEntry.GetNext: PVmtFieldEntry;
begin
  Result := aligntoptr(Tail);
end;

Function TVmtFieldEntry.GetTail: Pointer;
begin
  Result := PByte(@Name) + Length(Name) + SizeOf(Byte);
end;

{ TInterfaceData }

Function TInterfaceData.GetUnitName: ShortString;
begin
  Result := UnitNameField;
end;

Function TInterfaceData.GetPropertyTable: PPropData;
var
  p: PByte;
begin
  p := PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField);
  Result := AlignTypeData(p);
end;

Function TInterfaceData.GetMethodTable: PIntfMethodTable;
begin
  Result := aligntoptr(PropertyTable^.Tail);
end;

{ TInterfaceRawData }

Function TInterfaceRawData.GetUnitName: ShortString;
begin
  Result := UnitNameField;
end;

Function TInterfaceRawData.GetIIDStr: ShortString;
begin
  Result := PShortString(AlignTypeData(PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField)))^;
end;

Function TInterfaceRawData.GetPropertyTable: PPropData;
var
  p: PByte;
begin
  p := AlignTypeData(PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField));
  p := p + SizeOf(p^) + p^;
  Result := aligntoptr(p);
end;

Function TInterfaceRawData.GetMethodTable: PIntfMethodTable;
begin
  Result := aligntoptr(PropertyTable^.Tail);
end;

{ TClassData }

Function TClassData.GetExMethodTable: PVmtMethodExTable;

  { Copied from objpas.inc}

type
   tmethodnamerec = packed record
      name : pshortstring;
      addr : codepointer;
   end;

   tmethodnametable = packed record
     count : dword;
     entries : packed array[0..0] of tmethodnamerec;
   end;

   pmethodnametable =  ^tmethodnametable;



Var
  ovmt : PVmt;
  methodtable: pmethodnametable;

begin
  oVmt:=PVmt(ClassType);
  methodtable:=pmethodnametable(ovmt^.vMethodTable);
  // Shift till after
  PByte(Result):=PByte(methodtable)+ SizeOf(dword)+SizeOf(tmethodnamerec) * methodtable^.count;
end;

Function TClassData.GetExPropertyTable: PPropDataEx;

begin
  Result:=aligntoptr(PPropDataEx(GetPropertyTable^.GetTail));
end;

Function TClassData.GetUnitName: ShortString;
begin
  Result := UnitNameField;
end;

Function TClassData.GetPropertyTable: PPropData;
var
  p: PByte;
begin
  p := PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField);
  Result := AlignToPtr(p);
end;

{ TTypeData }

Function TTypeData.GetBaseType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(BaseTypeRef);
end;

Function TTypeData.GetCompType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(CompTypeRef);
end;

Function TTypeData.GetParentInfo: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ParentInfoRef);
end;

{$ifndef VER3_0}
Function TTypeData.GetRecInitData: PRecInitData;
begin
  Result := PRecInitData(aligntoptr(PTypeData(RecInitInfo+2+PByte(RecInitInfo+1)^)));
end;
{$endif}

Function TTypeData.GetHelperParent: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(HelperParentRef);
end;

Function TTypeData.GetExtendedInfo: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ExtendedInfoRef);
end;

Function TTypeData.GetIntfParent: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(IntfParentRef);
end;

Function TTypeData.GetRawIntfParent: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(RawIntfParentRef);
end;

Function TTypeData.GetIIDStr: ShortString;
begin
  Result := PShortString(AlignTypeData(Pointer(@RawIntfUnit) + Length(RawIntfUnit) + 1))^;
end;

Function TTypeData.GetElType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(elTypeRef);
end;

Function TTypeData.GetElType2: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(elType2Ref);
end;

Function TTypeData.GetInstanceType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(InstanceTypeRef);
end;

Function TTypeData.GetRefType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(RefTypeRef);
end;

{ TPropData }

Function TPropData.GetProp(Index: Word): PPropInfo;
begin
  if Index >= PropCount then
    Result := Nil
  else
    begin
      Result := PPropInfo(aligntoptr(PByte(@PropCount) + SizeOf(PropCount)));
      while Index > 0 do
        begin
          Result := aligntoptr(Result^.Tail);
          Dec(Index);
        end;
    end;
end;

Function TPropData.GetTail: Pointer;
begin
  if PropCount = 0 then
    Result := PByte(@PropCount) + SizeOf(PropCount)
  else
    Result := Prop[PropCount - 1]^.Tail;
end;

{ TPropInfo }

Function TPropInfo.GetPropType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(PropTypeRef);
end;

Function TPropInfo.GetTail: Pointer;
begin
  Result := PByte(@Name[0]) + SizeOf(Name[0]) + Length(Name);
end;

Function TPropInfo.GetNext: PPropInfo;
begin
  Result := PPropInfo(aligntoptr(Tail));
end;

type
  TElementAlias = record
    Ordinal : Integer;
    Alias : string;
  end;
  TElementAliasArray = Array of TElementAlias;
  PElementAliasArray = ^TElementAliasArray;

  TEnumeratedAliases = record
    TypeInfo: PTypeInfo;
    Aliases: TElementAliasArray;
  end;
  TEnumeratedAliasesArray = Array of TEnumeratedAliases;

Var
  EnumeratedAliases : TEnumeratedAliasesArray;

Function IndexOfEnumeratedAliases(aTypeInfo : PTypeInfo) : integer;

begin
  Result:=High(EnumeratedAliases);
  while (Result>=0) and (EnumeratedAliases[Result].TypeInfo<>aTypeInfo) do
    Dec(Result);
end;

Function GetEnumeratedAliases(aTypeInfo : PTypeInfo) : PElementAliasArray;

Var
  I : integer;

begin
  I:=IndexOfEnumeratedAliases(aTypeInfo);
  if I=-1 then
    Result:=Nil
  else
    Result:=@EnumeratedAliases[i].Aliases
end;

Function AddEnumeratedAliases(aTypeInfo : PTypeInfo) : PElementAliasArray;

Var
  L : Integer;

begin
  L:=Length(EnumeratedAliases);
  SetLength(EnumeratedAliases,L+1);
  EnumeratedAliases[L].TypeInfo:=aTypeInfo;
  Result:=@EnumeratedAliases[L].Aliases;
end;

Procedure RemoveEnumElementAliases(aTypeInfo: PTypeInfo);

Var
  I,L : integer;
  A : TEnumeratedAliases;

begin
  I:=IndexOfEnumeratedAliases(aTypeInfo);
  if I=-1 then
    exit;
  A:=EnumeratedAliases[i];
  A.Aliases:=Nil;
  A.TypeInfo:=Nil;
  L:=High(EnumeratedAliases);
  EnumeratedAliases[i]:=EnumeratedAliases[L];
  EnumeratedAliases[L]:=A;
  SetLength(EnumeratedAliases,L);
end;

Resourcestring
  SErrNotAnEnumerated = 'Type information points to non-enumerated type';
  SErrInvalidEnumeratedCount = 'Invalid number of enumerated values';
  SErrDuplicateEnumerated = 'Duplicate alias for enumerated value';

Procedure AddEnumElementAliases(aTypeInfo: PTypeInfo; const aNames: array of string; aStartValue: Integer = 0);

var
  Aliases: PElementAliasArray;
  A : TElementAliasArray;
  L, I, J : Integer;
  N : String;
  PT : PTypeData;


begin
  if (aTypeInfo^.Kind<>tkEnumeration) then
    raise EArgumentException.Create(SErrNotAnEnumerated);
  PT:=GetTypeData(aTypeInfo);
  if (High(aNames)=-1) or ((aStartValue+High(aNames))> PT^.MaxValue) then
    raise EArgumentException.Create(SErrInvalidEnumeratedCount);
  Aliases:=GetEnumeratedAliases(aTypeInfo);
  if (Aliases=Nil) then
    Aliases:=AddEnumeratedAliases(aTypeInfo);
  A:=Aliases^;
  I:=0;
  L:=Length(a);
  SetLength(a,L+High(aNames)+1);
  try
    for N in aNames do
      begin
      for J:=0 to (L+I)-1 do
        if SameText(N,A[J].Alias) then
          raise EArgumentException.Create(SErrDuplicateEnumerated);
      with A[L+I] do
        begin
        Ordinal:=aStartValue+I;
        alias:=N;
        end;
      Inc(I);
      end;
  finally
    // In case of exception, we need to correct the length.
    if Length(A)<>I+L then
      SetLength(A,I+L);
    Aliases^:=A;
  end;
end;

Function GetEnumeratedAliasValue(aTypeInfo: PTypeInfo; const aName: string): Integer;

var
  I : Integer;
  Aliases: PElementAliasArray;

begin
  Result:=-1;
  Aliases:=GetEnumeratedAliases(aTypeInfo);
  if (Aliases=Nil) then
    Exit;
  I:=High(Aliases^);
  While (Result=-1) and (I>=0) do
    begin
    if SameText(Aliases^[I].Alias, aName) then
      Result:=Aliases^[I].Ordinal;
    Dec(I);
    end;
end;


end.
