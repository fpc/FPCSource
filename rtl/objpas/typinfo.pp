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

{$IFNDEF FPC_DOTTEDUNITS}
unit TypInfo;
{$ENDIF FPC_DOTTEDUNITS}

  interface

{$MODE objfpc}
{$MODESWITCH AdvancedRecords}
{$inline on}
{$macro on}
{$h+}

{$IFDEF FPC_DOTTEDUNITS}
  uses System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
  uses SysUtils;
{$ENDIF FPC_DOTTEDUNITS}


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

{$IF FPC_FULLVERSION>=30301}
{$DEFINE HAVE_INVOKEHELPER}
{$DEFINE HAVE_HIDDENTHUNKCLASS}
{$ENDIF}

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

      {$IFDEF HAVE_INVOKEHELPER}
      TInvokeHelper = procedure(Instance : Pointer; Args : PPointer);
      {$ENDIF}

      PParameterLocation = ^TParameterLocation;
      TParameterLocation =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        LocType: Byte;
        function GetRegType: TRegisterType; inline;
        function GetReference: Boolean; inline;
        function GetShiftVal: Int8; inline;
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
        function GetLocation(aIndex: Byte): PParameterLocation; inline;
        function GetTail: Pointer; inline;
      public
        Count: Byte;
        property Location[Index: Byte]: PParameterLocation read GetLocation;
        property Tail: Pointer read GetTail;
      end;

      { The following three types are essentially copies from the TObject.FieldAddress
        function. If something is changed there, change it here as well }

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
        function GetNext: PVmtFieldEntry; inline;
        function GetTail: Pointer; inline;
      public
        FieldOffset: SizeUInt;
        TypeIndex: Word;
        Name: ShortString;
        property Tail: Pointer read GetTail;
        property Next: PVmtFieldEntry read GetNext;
      end;

      PVmtFieldTable = ^TVmtFieldTable;
      TVmtFieldTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetField(aIndex: Word): PVmtFieldEntry;
        function GetNext: Pointer;
        function GetTail: Pointer;
      public
        Count: Word;
        ClassTab: PVmtFieldClassTab;
        { should be array[Word] of TFieldInfo;  but
          Elements have variant size! force at least proper alignment }
        Fields: array[0..0] of TVmtFieldEntry;
        property Field[aIndex: Word]: PVmtFieldEntry read GetField;
        property Tail: Pointer read GetTail;
        property Next: Pointer read GetNext;
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

      TAttributeProc = function : TCustomAttribute;

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
        function GetElType: PTypeInfo; inline;
        function GetDims(aIndex: Byte): PTypeInfo; inline;
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
        function GetTypeRef: PTypeInfo; inline;
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
        function GetParamType: PTypeInfo; inline;
        function GetFlags: Byte; inline;
      public
        property ParamType: PTypeInfo read GetParamType;
        property Flags: Byte read GetFlags;
      public
        ParamFlags: TParamFlags;
        ParamTypeRef: TypeInfoPtr;
        Name: ShortString;
      end;

      PProcedureSignature = ^TProcedureSignature;
      TProcedureSignature =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetResultType: PTypeInfo; inline;
      public
        property ResultType: PTypeInfo read GetResultType;
      public
        Flags: Byte;
        CC: TCallConv;
        ResultTypeRef: TypeInfoPtr;
        ParamCount: Byte;
        {Params: array[0..ParamCount - 1] of TProcedureParam;}
        function GetParam(ParamIndex: Integer): PProcedureParam;
      end;

      PVmtMethodParam = ^TVmtMethodParam;
      TVmtMethodParam =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetTail: Pointer; inline;
        function GetNext: PVmtMethodParam; inline;
        function GetName: ShortString; inline;
      public
        ParamType: PPTypeInfo;
        Flags: TParamFlags;
        NamePtr: PShortString;
        ParaLocs: PParameterLocations;
        property Name: ShortString read GetName;
        property Tail: Pointer read GetTail;
        property Next: PVmtMethodParam read GetNext;
      end;
      TVmtMethodParamArray = array[0..{$ifdef cpu16}(32768 div sizeof(TVmtMethodParam))-2{$else}65535{$endif}] of TVmtMethodParam;
      PVmtMethodParamArray = ^TVmtMethodParamArray;

      PIntfMethodEntry = ^TIntfMethodEntry;
      TIntfMethodEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetParam(Index: Word): PVmtMethodParam;
        function GetResultLocs: PParameterLocations; inline;
        function GetTail: Pointer; inline;
        function GetNext: PIntfMethodEntry; inline;
        function GetName: ShortString; inline;
      public
        ResultType: PPTypeInfo;
        CC: TCallConv;
        Kind: TMethodKind;
        ParamCount: Word;
        StackSize: SizeInt;
        {$IFDEF HAVE_INVOKEHELPER}
        InvokeHelper : TInvokeHelper;
        {$ENDIF}
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
        function GetMethod(Index: Word): PIntfMethodEntry;
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
        function GetEntry(Index: LongWord): PVmtMethodEntry; inline;
      public
        Count: LongWord;
        property Entry[Index: LongWord]: PVmtMethodEntry read GetEntry;
      private
        Entries: array[0..0] of TVmtMethodEntry;
      end;

      PVmtMethodExEntry = ^TVmtMethodExEntry;

      TVmtMethodExEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetParamsStart: PByte; inline;
        function GetMethodVisibility: TVisibilityClass;
        function GetParam(Index: Word): PVmtMethodParam;
        function GetResultLocs: PParameterLocations; inline;
        function GetStrictVisibility: Boolean;
        function GetTail: Pointer; inline;
        function GetNext: PVmtMethodExEntry; inline;
        function GetName: ShortString; inline;
      public
        ResultType: PPTypeInfo;
        CC: TCallConv;
        Kind: TMethodKind;
        ParamCount: Word;
        StackSize: SizeInt;
        {$IFDEF HAVE_INVOKEHELPER}
        InvokeHelper : TInvokeHelper;
        {$ENDIF}
        NamePtr: PShortString;
        Flags: Byte;
        VmtIndex: Smallint;
        {$IFNDEF VER3_2}
        CodeAddress : CodePointer;
        AttributeTable : PAttributeTable;
        {$ENDIF}
        property Name: ShortString read GetName;
        property Param[Index: Word]: PVmtMethodParam read GetParam;
        property ResultLocs: PParameterLocations read GetResultLocs;
        property Tail: Pointer read GetTail;
        property Next: PVmtMethodExEntry read GetNext;
        property MethodVisibility: TVisibilityClass read GetMethodVisibility;
        property StrictVisibility: Boolean read GetStrictVisibility;
      Private
        Params: array[0..0] of TVmtMethodParam;
       { ResultLocs: PParameterLocations (if ResultType != Nil) }
      end;
      TVmtMethodExEntryArray = array[0.. {$ifdef cpu16}(32768 div sizeof(TVmtMethodExEntry))-2{$else}65535{$endif}] of TVmtMethodExEntry;
      PVmtMethodExEntryArray = ^TVmtMethodExEntryArray;

      PVmtMethodExTable = ^TVmtMethodExTable;

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
        property Method[Index: Word]: PVmtMethodExEntry read GetMethod;
      private
        Entries: array[0..0] of TVmtMethodExEntry
      end;

      PExtendedMethodInfoTable = ^TExtendedMethodInfoTable;
      TExtendedMethodInfoTable = array[0..{$ifdef cpu16}(32768 div sizeof(PVmtMethodExEntry))-2{$else}65535{$endif}] of PVmtMethodExEntry;

      PExtendedVmtFieldEntry = ^TExtendedVmtFieldEntry;
      PExtendedFieldEntry = PExtendedVmtFieldEntry; // For records, there is no VMT, but currently the layout is identical
      TExtendedVmtFieldEntry =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetNext: PVmtFieldEntry;
        function GetStrictVisibility: Boolean;
        function GetTail: Pointer;
        function GetVisibility: TVisibilityClass;
      public
        FieldOffset: SizeUInt;
        FieldType: PPTypeInfo;
        Flags: Byte;
        Name: PShortString;
      {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable : PAttributeTable;
      {$endif}
        property FieldVisibility: TVisibilityClass read GetVisibility;
        property StrictVisibility: Boolean read GetStrictVisibility;
        property Tail: Pointer read GetTail;
        property Next: PVmtFieldEntry read GetNext;
      end;

      PVmtExtendedFieldTable = ^TVmtExtendedFieldTable;
      PExtendedFieldTable = PVmtExtendedFieldTable; // For records, there is no VMT, but currently the layout is identical.

      TVmtExtendedFieldTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetField(aIndex: Word): PExtendedVmtFieldEntry;
        function GetTail: Pointer;
      public
        FieldCount: Word;
        property Field[aIndex: Word]: PExtendedVmtFieldEntry read GetField;
        property Tail: Pointer read GetTail;
      private
        Entries: array[0..0] of TExtendedVmtFieldEntry;
      end;

      PExtendedFieldInfoTable = ^TExtendedFieldInfoTable;
      TExtendedFieldInfoTable = array[0..{$ifdef cpu16}(32768 div sizeof(PExtendedVmtFieldEntry))-2{$else}65535{$endif}] of PExtendedVmtFieldEntry;

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
      TRecInitData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
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

      TRecMethodExEntry =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetParamsStart: PByte; inline;
        function GetMethodVisibility: TVisibilityClass;
        function GetParam(Index: Word): PRecMethodParam;
        function GetResultLocs: PParameterLocations; inline;
        function GetStrictVisibility: Boolean;
        function GetTail: Pointer; inline;
        function GetNext: PRecMethodExEntry; inline;
        function GetName: ShortString; inline;
      public
        ResultType: PPTypeInfo;
        CC: TCallConv;
        Kind: TMethodKind;
        ParamCount: Word;
        StackSize: SizeInt;
        {$IFDEF HAVE_INVOKEHELPER}
        InvokeHelper : TInvokeHelper;
        {$ENDIF}
        NamePtr: PShortString;
        Flags: Byte;
        {$IFNDEF VER3_2}
        CodeAddress : CodePointer;
        AttributeTable : PAttributeTable;
        {$ENDIF}
        { Params: array[0..ParamCount - 1] of TRecMethodParam }
        { ResultLocs: PParameterLocations (if ResultType != Nil) }
        property Name: ShortString read GetName;
        property Param[Index: Word]: PRecMethodParam read GetParam;
        property ResultLocs: PParameterLocations read GetResultLocs;
        property Tail: Pointer read GetTail;
        property Next: PRecMethodExEntry read GetNext;
        property MethodVisibility: TVisibilityClass read GetMethodVisibility;
        property StrictVisibility: Boolean read GetStrictVisibility;
      end;

      PRecMethodExTable = ^TRecMethodExTable;

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
      TRecordMethodInfoTable = array[0..{$ifdef cpu16}(32768 div sizeof(PRecMethodExEntry))-2{$else}65535{$endif}] of PRecMethodExEntry;

      PInterfaceData = ^TInterfaceData;
      TInterfaceData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetUnitName: ShortString; inline;
        function GetPropertyTable: PPropData; inline;
        function GetMethodTable: PIntfMethodTable; inline;
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
          {$IFDEF HAVE_HIDDENTHUNKCLASS}
          ThunkClass : PPTypeInfo;
          {$ENDIF}
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
        function GetUnitName: ShortString; inline;
        function GetIIDStr: ShortString; inline;
        function GetPropertyTable: PPropData; inline;
        function GetMethodTable: PIntfMethodTable; inline;
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
            {$IFDEF HAVE_HIDDENTHUNKCLASS}
            ThunkClass : PPTypeInfo;
            {$ENDIF}
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

      TClassData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetExMethodTable: PVmtMethodExTable;
        function GetExPropertyTable: PPropDataEx;
        function GetUnitName: ShortString; inline;
        function GetPropertyTable: PPropData; inline;
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

      PRecordData = ^TRecordData;
      TRecordData =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetExPropertyTable: PPropDataEx;
        function GetExtendedFieldCount: Longint;
        function GetExtendedFields: PExtendedFieldTable;
        function GetMethodTable: PRecordMethodTable;
      Public
        property ExtendedFields: PExtendedFieldTable read GetExtendedFields;
        property ExtendedFieldCount: Longint read GetExtendedFieldCount;
        property MethodTable: PRecordMethodTable read GetMethodTable;
        property ExRTTITable: PPropDataEx read GetExPropertyTable;
      public
        {$ifdef PROVIDE_ATTR_TABLE}
        AttributeTable: PAttributeTable;
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
          { ExtendedFields: array[0..ExtendedFieldsCount-1] of PExtendedFieldEntry }
          { MethodTable : TRecordMethodTable }
          { Properties }
        );
         { include for proper alignment }
        tkInt64: (
          dummy: Int64
        );
{$ifndef FPUNONE}
        tkFloat:
          (FloatType: TFloatType
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
        function GetBaseType: PTypeInfo; inline;
        function GetCompType: PTypeInfo; inline;
        function GetParentInfo: PTypeInfo; inline;
{$ifndef VER3_0}        
        function GetRecInitData: PRecInitData; inline;
{$endif}
        function GetHelperParent: PTypeInfo; inline;
        function GetExtendedInfo: PTypeInfo; inline;
        function GetIntfParent: PTypeInfo; inline;
        function GetRawIntfParent: PTypeInfo; inline;
        function GetIIDStr: ShortString; inline;
        function GetElType: PTypeInfo; inline;
        function GetElType2: PTypeInfo; inline;
        function GetInstanceType: PTypeInfo; inline;
        function GetRefType: PTypeInfo; inline;
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
                 False: (ParamList : array[0..1023] of AnsiChar);
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
               ThunkClass : PPTypeInfo;
               IntfUnit: ShortString;
               { PropertyTable: TPropData }
               { MethodTable: TIntfMethodTable }
              );
            tkInterfaceRaw:
              (
               RawIntfParentRef: TypeInfoPtr;
               RawIntfFlags : TIntfFlagsBase;
               IID: TGUID;
               RawThunkClass : PPTypeInfo;
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
        function GetProp(Index: Word): PPropInfo;
        function GetTail: Pointer; inline;
      public
        PropCount : Word;
        PropList : record _alignmentdummy : ptrint; end;
        property Prop[Index: Word]: PPropInfo read GetProp;
        property Tail: Pointer read GetTail;
      end;

      TPropInfoEx =
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed
      {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
      record
      private
        function GetStrictVisibility: Boolean;
        function GetTail: Pointer;
        function GetVisiblity: TVisibilityClass;
      public
        Flags: Byte;
        Info: PPropInfo;
        // AttrData: TAttrData
        property Tail: Pointer read GetTail;
        property Visibility: TVisibilityClass read GetVisiblity;
        property StrictVisibility: Boolean read GetStrictVisibility;
      end;

      PPropInfoEx = ^TPropInfoEx;

      TPropDataEx = 
      {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
      packed 
      {$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}  
      record
      private
        function GetPropEx(Index: Word): PPropInfoEx;
        function GetTail: Pointer; inline;
      public
        PropCount: Word;
        // PropList: record alignmentdummy: ptrint; end;
        property Prop[Index: Word]: PPropInfoex read GetPropEx;
        property Tail: Pointer read GetTail;
      private
        // Dummy declaration
        PropList: array[0..0] of TPropInfoEx;
      end;

      PPropListEx = ^TPropListEx;
      TPropListEx = array[0..{$ifdef cpu16}(32768 div sizeof(PPropInfoEx))-2{$else}65535{$endif}] of PPropInfoEx;

{$PACKRECORDS 1}
      TPropInfo = packed record
      private
        function GetPropType: PTypeInfo; inline;
        function GetTail: Pointer; inline;
        function GetNext: PPropInfo; inline;
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
function GetPropList(AClass: TClass; out PropList: PPropList): Integer;
function GetPropList(Instance: TObject; out PropList: PPropList): Integer;

// extended RTTI

Function GetPropInfosEx(TypeInfo: PTypeInfo; PropList: PPropListEx; Visibilities : TVisibilityClasses = []) : Integer;
Function GetPropListEx(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; PropList: PPropListEx; Sorted: boolean = true; Visibilities : TVisibilityClasses = []): longint;
Function GetPropListEx(TypeInfo: PTypeInfo; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): SizeInt;
Function GetPropListEx(AClass: TClass; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): Integer;
Function GetPropListEx(Instance: TObject; out PropList: PPropListEx; Visibilities : TVisibilityClasses = []): Integer;

Function GetFieldInfos(aClass: TClass; FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True) : Integer;
Function GetFieldInfos(aRecord: PRecordData; FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetFieldInfos(TypeInfo: PTypeInfo; FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True) : Integer;
Function GetFieldList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds; out FieldList: PExtendedFieldInfoTable; Sorted: boolean = true; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): longint;
Function GetFieldList(TypeInfo: PTypeInfo; out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): SizeInt;
Function GetRecordFieldList(aRecord: PRecordData; Out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetFieldList(AClass: TClass; out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): Integer;
Function GetFieldList(Instance: TObject; out FieldList: PExtendedFieldInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): Integer;

// Infos require initialized memory or nil to count
Function GetMethodInfos(aClass: TClass; MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True) : Integer;
Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True) : Integer;
Function GetRecordMethodInfos(aRecordData: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities: TVisibilityClasses): Integer;
Function GetMethodInfos(aRecord: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;
// List will initialize the memory
Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Sorted: boolean = true; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): longint;
Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): longint;
Function GetMethodList(AClass: TClass; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): Integer;
Function GetMethodList(Instance: TObject; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): Integer;

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

function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;
function GetInterfaceProp(Instance: TObject; PropInfo: PPropInfo): IInterface;
procedure SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);
procedure SetInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface);

function GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;
function GetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
procedure SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);
procedure SetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

function GetDynArrayProp(Instance: TObject; const PropName: string): Pointer;
function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
procedure SetDynArrayProp(Instance: TObject; const PropName: string; const Value: Pointer);
procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);


// Extended RTTI
function GetAttributeTable(TypeInfo: PTypeInfo): PAttributeTable;

function GetPropAttribute(PropInfo: PPropInfo; AttributeNr: Word): TCustomAttribute; inline;

function GetAttribute(AttributeTable: PAttributeTable; AttributeNr: Word): TCustomAttribute;

{$IFDEF HAVE_INVOKEHELPER}
procedure CallInvokeHelper(aTypeInfo : PTypeInfo; Instance: Pointer; const aMethod : String; aArgs : PPointer);
{$ENDIF}

// Auxiliary routines, which may be useful
Function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;
Function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;
function GetEnumNameCount(enum1: PTypeInfo): SizeInt;
procedure AddEnumElementAliases(aTypeInfo: PTypeInfo; const aNames: array of string; aStartValue: Integer = 0);
procedure RemoveEnumElementAliases(aTypeInfo: PTypeInfo);
function GetEnumeratedAliasValue(aTypeInfo: PTypeInfo; const aName: string): Integer;

function SetToArray(TypeInfo: PTypeInfo; Value: Pointer) : TBytes;
function SetToArray(PropInfo: PPropInfo; Value: Pointer) : TBytes;
function SetToArray(TypeInfo: PTypeInfo; Value: LongInt) : TBytes;
function SetToArray(PropInfo: PPropInfo; Value: LongInt) : TBytes;
function SetToString(TypeInfo: PTypeInfo; Value: LongInt; Brackets: Boolean) : String;
function SetToString(PropInfo: PPropInfo; Value: LongInt; Brackets: Boolean) : String;
function SetToString(PropInfo: PPropInfo; Value: LongInt) : String;
function SetToString(TypeInfo: PTypeInfo; Value: Pointer; Brackets: Boolean = False) : String;
function SetToString(PropInfo: PPropInfo; Value: Pointer; Brackets: Boolean = False) : String;
function ArrayToSet(PropInfo: PPropInfo; const Value: array of Byte): LongInt;
function ArrayToSet(TypeInfo: PTypeInfo; const Value: array of Byte): LongInt;
procedure ArrayToSet(PropInfo: PPropInfo; const Value: array of Byte; Result: Pointer);
procedure ArrayToSet(TypeInfo: PTypeInfo; const Value: array of Byte; Result: Pointer);
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
function DerefTypeInfoPtr(Info: TypeInfoPtr): PTypeInfo; inline;

Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.RtlConsts;
{$ELSE FPC_DOTTEDUNITS}
uses rtlconsts;
{$ENDIF FPC_DOTTEDUNITS}

type
  PMethod = ^TMethod;

{ ---------------------------------------------------------------------
  Auxiliary methods
  ---------------------------------------------------------------------}

function aligntoptr(p : pointer) : pointer;inline;
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


function DerefTypeInfoPtr(Info: TypeInfoPtr): PTypeInfo; inline;
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

function GetAttributeTable(TypeInfo: PTypeInfo): PAttributeTable;
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

function GetPropData(TypeInfo : PTypeInfo; TypeData: PTypeData) : PPropData; inline;
var
  p: PtrUInt;
begin
  p := PtrUInt(@TypeData^.UnitName) + SizeOf(TypeData^.UnitName[0]) + Length(TypeData^.UnitName);
  Result := PPropData(aligntoptr(Pointer(p)));
end;

function GetAttribute(AttributeTable: PAttributeTable; AttributeNr: Word): TCustomAttribute;
begin
  if (AttributeTable=nil) or (AttributeNr>=AttributeTable^.AttributeCount) then
    result := nil
  else
    begin
      result := AttributeTable^.AttributesList[AttributeNr].AttrProc();
    end;
end;

function GetPropAttribute(PropInfo: PPropInfo; AttributeNr: Word): TCustomAttribute;
begin
{$ifdef PROVIDE_ATTR_TABLE}
  Result := GetAttribute(PropInfo^.AttributeTable, AttributeNr);
{$else}
  Result := Nil;
{$endif}
end;

Function GetEnumName(TypeInfo : PTypeInfo;Value : Integer) : string;

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
 else if TypeInfo^.Kind=tkEnumeration then
   begin
     PS:=@PT^.NameList;
     dec(Value,PT^.MinValue);
     While Value>0 Do
       begin
         PS:=PShortString(pointer(PS)+PByte(PS)^+1);
         Dec(Value);
       end;
     Result:=PS^;
   end
 else if TypeInfo^.Kind=tkInteger then
   Result:=IntToStr(Value)
 else
   Result:='';  
end;


Function GetEnumValue(TypeInfo : PTypeInfo;const Name : string) : Integer;

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

function GetEnumNameCount(enum1: PTypeInfo): SizeInt;
var
  PS: PShortString;
begin
  if enum1^.Kind=tkBool then
    Result:=2
  else
    begin
      { the last string is the unit name, so start at -1 }
      PS:=@GetTypeData(enum1)^.NameList;
      Result:=-1;
      While (PByte(PS)^<>0) do
        begin
          PS:=PShortString(pointer(PS)+PByte(PS)^+1);
          Inc(Result);
        end;
    end;
end;


Function SetToString(PropInfo: PPropInfo; Value: LongInt; Brackets: Boolean) : String;

begin
  Result:=SetToString(PropInfo^.PropType, Value, Brackets);
end;

Function SetToString(TypeInfo: PTypeInfo; Value: LongInt; Brackets: Boolean) : String;
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

procedure StringToSet(TypeInfo: PTypeInfo; const Value: String; Result: Pointer);
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

procedure StringToSet(PropInfo: PPropInfo; const Value: String; Result: Pointer);
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

Function AlignTypeData(p : Pointer) : Pointer;
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


Function AlignTParamFlags(p : Pointer) : Pointer; inline;
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


Function AlignPTypeInfo(p : Pointer) : Pointer; inline;
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


Function GetTypeData(TypeInfo : PTypeInfo) : PTypeData;
begin
  GetTypeData:=AlignTypeData(pointer(TypeInfo)+2+PByte(pointer(TypeInfo)+1)^);
end;


{ ---------------------------------------------------------------------
  Basic Type information functions.
  ---------------------------------------------------------------------}

Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string) : PPropInfo;
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


Function GetPropInfo(TypeInfo : PTypeInfo;const PropName : string; Akinds : TTypeKinds) : PPropInfo;
begin
  Result:=GetPropInfo(TypeInfo,PropName);
  If (Akinds<>[]) then
    If (Result<>Nil) then
      If Not (Result^.PropType^.Kind in AKinds) then
        Result:=Nil;
end;


Function GetPropInfo(AClass: TClass; const PropName: string; AKinds: TTypeKinds) : PPropInfo;
begin
  Result:=GetPropInfo(PTypeInfo(AClass.ClassInfo),PropName,AKinds);
end;


Function GetPropInfo(Instance: TObject; const PropName: string; AKinds: TTypeKinds) : PPropInfo;
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

function IsReadableProp(PropInfo: PPropInfo): Boolean;
begin
  Result:=(((PropInfo^.PropProcs) and 3) in [ptField,ptStatic,ptVirtual]);
end;

function IsReadableProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsReadableProp(FindPropInfo(Instance,PropName));
end;

function IsReadableProp(AClass: TClass; const PropName: string): Boolean;
begin
  Result:=IsReadableProp(FindPropInfo(AClass,PropName));
end;

function IsWriteableProp(PropInfo: PPropInfo): Boolean;
begin
  Result:=(((PropInfo^.PropProcs shr 2) and 3) in [ptField,ptStatic,ptVirtual]);
end;

function IsWriteableProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsWriteableProp(FindPropInfo(Instance,PropName));
end;

function IsWriteableProp(AClass: TClass; const PropName: string): Boolean;
begin
  Result:=IsWriteableProp(FindPropInfo(AClass,PropName));
end;

Function IsStoredProp(Instance: TObject;PropInfo : PPropInfo) : Boolean;
type
  TBooleanIndexFunc=function(Index:integer):boolean of object;
  TBooleanFunc=function:boolean of object;
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
  TP : PPropInfoEx;
  I,Count : Longint;

begin
  Result:=0;
  repeat
    TD:=PClassData(GetTypeData(TypeInfo))^.ExRTTITable;
    Count:=TD^.PropCount;
    // Now point TP to first propinfo record.
    For I:=0 to Count-1 do
      begin
      TP:=TD^.Prop[I];
      if ([]=Visibilities) or (TP^.Visibility in Visibilities) then
        begin
        // When passing nil, we just need the count
        if Assigned(PropList) then
          PropList^[Result]:=TD^.Prop[i];
        Inc(Result);
        end;
      end;
    if PClassData(GetTypeData(TypeInfo))^.Parent=Nil then
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



Function GetFieldInfos(aClass: TClass; FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

var
  vmt: PVmt;
  FieldTable: PVmtExtendedFieldTable;
  FieldEntry: PExtendedVmtFieldEntry;
  FieldEntryD: TExtendedVmtFieldEntry;
  i: longint;

  function AlignToFieldEntry(aPtr: Pointer): Pointer; inline;
  begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    { align to largest field of TVmtFieldInfo }
    Result := Align(aPtr, SizeOf(PtrUInt));
{$else}
    Result := aPtr;
{$endif}
  end;

begin
  Result:=0;
  vmt := PVmt(AClass);
  while vmt <> nil do
    begin
    // a class can have 0 fields...
    if vmt^.vFieldTable<>Nil then
      begin
      FieldTable := PVmtExtendedFieldTable(AlignToFieldEntry(PVmtFieldTable(vmt^.vFieldTable)^.Next));
      For I:=0 to FieldTable^.FieldCount-1 do
        begin
        FieldEntry:=FieldTable^.Field[i];
        FieldEntryD:=FieldEntry^;
        if ([]=Visibilities) or (FieldEntry^.FieldVisibility in Visibilities) then
          begin
          if Assigned(FieldList) then
            FieldList^[Result]:=FieldEntry;
          Inc(Result);
          end;
        end;
      end;
    { Go to parent type }
    if IncludeInherited then
      vmt:=vmt^.vParent
    else
      vmt:=Nil;
    end;
end;


Function GetFieldInfos(TypeInfo: PTypeInfo; FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

begin
  if TypeInfo^.Kind=tkRecord then
    Result:=GetFieldInfos(PRecordData(GetTypeData(TypeInfo)),FieldList,Visibilities)
  else if TypeInfo^.Kind=tkClass then
    Result:=GetFieldInfos((PClassData(GetTypeData(TypeInfo))^.ClassType),FieldList,Visibilities,IncludeInherited)
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
  Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): longint;

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
  Count:=GetFieldList(TypeInfo,TempList,Visibilities,IncludeInherited);
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


Function GetFieldList(AClass: TClass; out FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetFieldInfos(aClass,Nil,Visibilities,IncludeInherited);
  FieldList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetFieldInfos(aClass,FieldList,Visibilities,IncludeInherited);
  except
    FreeMem(FieldList);
    Raise;
  end;
end;

Function GetFieldList(Instance: TObject; out FieldList: PExtendedFieldInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

begin
  Result:=GetFieldList(Instance.ClassType,FieldList,Visibilities,IncludeInherited);
end;


Function GetFieldList(TypeInfo: PTypeInfo; out FieldList : PExtendedFieldInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): SizeInt;

begin
  if TypeInfo^.Kind=tkRecord then
    Result:=GetRecordFieldList(PRecordData(GetTypeData(TypeInfo)),FieldList,Visibilities)
  else if TypeInfo^.Kind=tkClass then
    Result:=GetFieldList(GetTypeData(TypeInfo)^.ClassType,FieldList,Visibilities,IncludeInherited)
  else
    Result:=0
end;

{ -- Methods -- }

Function GetMethodInfos(aRecord: PRecordData; MethodList: PRecordMethodInfoTable; Visibilities: TVisibilityClasses): Integer;

begin
  Result:=GetRecordMethodInfos(aRecord,MethodList,Visibilities)
end;

Function GetClassMethodInfos(aClassData: PClassData; MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean): Integer;


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
    if (aClassData^.Parent=Nil) or Not IncludeInherited then
      aClassData:=Nil
    else
      aClassData:=PClassData(GetTypeData(aClassData^.Parent^)); ;
    end;

end;

Function GetMethodInfos(aClass: TClass; MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

begin
  Result:=GetMethodInfos(PTypeInfo(aClass.ClassInfo),MethodList,Visibilities,IncludeInherited);
end;

Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PRecordMethodInfoTable; Visibilities : TVisibilityClasses = []) : Integer;

begin
  if TypeInfo^.Kind=tkRecord then
   Result:=GetRecordMethodInfos(PRecordData(GetTypeData(TypeInfo)),MethodList,Visibilities)
  else
    Result:=0
end;

Function GetMethodInfos(TypeInfo: PTypeInfo; MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

begin
  if TypeInfo^.Kind=tkClass then
    Result:=GetClassMethodInfos(PClassData(GetTypeData(TypeInfo)),MethodList,Visibilities,IncludeInherited)
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
  Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): longint;

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
  aCount:=GetMethodList(TypeInfo,TempList,Visibilities,IncludeInherited);
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

Function GetMethodList(TypeInfo: PTypeInfo; out MethodList: PExtendedMethodInfoTable; Visibilities : TVisibilityClasses = []; IncludeInherited : Boolean = True): longint;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetMethodInfos(TypeInfo,PExtendedMethodInfoTable(Nil),Visibilities,IncludeInherited);
  MethodList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetMethodInfos(TypeInfo,MethodList,Visibilities,IncludeInherited);
  except
    FreeMem(MethodList);
    Raise;
  end;
end;

Function GetMethodList(AClass: TClass; out MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

Var
  aCount : Integer;

begin
  Result:=0;
  aCount:=GetMethodInfos(aClass,Nil,[],IncludeInherited);
  MethodList:=Getmem(aCount*SizeOf(Pointer));
  try
    Result:=GetMethodInfos(aClass,MethodList,Visibilities,IncludeInherited);
  except
    FreeMem(MethodList);
    Raise;
  end;
end;


Function GetMethodList(Instance: TObject; out MethodList: PExtendedMethodInfoTable; Visibilities: TVisibilityClasses; IncludeInherited : Boolean = True): Integer;

begin
  Result:=GetMethodList(Instance.ClassType,MethodList,Visibilities,IncludeInherited);
end;


{ -- Properties -- }

Procedure GetPropInfos(TypeInfo : PTypeInfo;PropList : PPropList);
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

Function  GetPropList(TypeInfo : PTypeInfo;TypeKinds : TTypeKinds; PropList : PPropList;Sorted : boolean = true):longint;

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

function GetPropList(AClass: TClass; out PropList: PPropList): Integer;
begin
  Result := GetPropList(PTypeInfo(AClass.ClassInfo), PropList);
end;

function GetPropList(Instance: TObject; out PropList: PPropList): Integer;
begin
  Result := GetPropList(Instance.ClassType, PropList);
end;

{ ---------------------------------------------------------------------
  Property access functions
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  Ordinal properties
  ---------------------------------------------------------------------}

Function GetOrdProp(Instance : TObject;PropInfo : PPropInfo) : Int64;

type
  TGetInt64ProcIndex=function(index:longint):Int64 of object;
  TGetInt64Proc=function():Int64 of object;
  TGetIntegerProcIndex=function(index:longint):longint of object;
  TGetIntegerProc=function:longint of object;
  TGetWordProcIndex=function(index:longint):word of object;
  TGetWordProc=function:word of object;
  TGetByteProcIndex=function(index:longint):Byte of object;
  TGetByteProc=function:Byte of object;
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


Procedure SetOrdProp(Instance : TObject;PropInfo : PPropInfo;Value : Int64);

type
  TSetInt64ProcIndex=procedure(index:longint;i:Int64) of object;
  TSetInt64Proc=procedure(i:Int64) of object;
  TSetIntegerProcIndex=procedure(index,i:longint) of object;
  TSetIntegerProc=procedure(i:longint) of object;
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


Procedure SetOrdProp(Instance: TObject; const PropName: string;  Value: Int64);
begin
  SetOrdProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetEnumProp(Instance: TObject; Const PropInfo: PPropInfo): string;
begin
  Result:=GetEnumName(PropInfo^.PropType, GetOrdProp(Instance, PropInfo));
end;


Function GetEnumProp(Instance: TObject; const PropName: string): string;
begin
  Result:=GetEnumProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetEnumProp(Instance: TObject; const PropName: string;  const Value: string);
begin
  SetEnumProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetEnumProp(Instance: TObject; Const PropInfo : PPropInfo; const Value: string);
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


procedure SetInt64Prop(Instance: TObject; PropInfo: PPropInfo; const Value: Int64);
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
  TGetPointerProcIndex = function (index:longint): Pointer of object;
  TGetPointerProc = function (): Pointer of object;

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
  TSetPointerProcIndex = procedure(index: longint; p: pointer) of object;
  TSetPointerProc = procedure(p: pointer) of object;

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


Function GetObjectProp(Instance: TObject; PropInfo : PPropInfo): TObject;
begin
  Result:=GetObjectProp(Instance,PropInfo,Nil);
end;


Function GetObjectProp(Instance: TObject; PropInfo : PPropInfo; MinClass: TClass): TObject;
begin
  Result:=TObject(GetPointerProp(Instance,PropInfo));
  If (MinClass<>Nil) and (Result<>Nil) Then
    If Not Result.InheritsFrom(MinClass) then
      Result:=Nil;
end;


Procedure SetObjectProp(Instance: TObject; const PropName: string;  Value: TObject);
begin
  SetObjectProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Procedure SetObjectProp(Instance: TObject; PropInfo : PPropInfo;  Value: TObject);

begin
  SetPointerProp(Instance,PropInfo,Pointer(Value));
end;


Function GetObjectPropClass(Instance: TObject; const PropName: string): TClass;
begin
  Result:=GetTypeData(FindPropInfo(Instance,PropName,[tkClass])^.PropType)^.ClassType;
end;


Function  GetObjectPropClass(AClass: TClass; const PropName: string): TClass;
begin
  Result:=GetTypeData(FindPropInfo(AClass,PropName,[tkClass])^.PropType)^.ClassType;
end;

{ ---------------------------------------------------------------------
    Interface wrapprers
  ---------------------------------------------------------------------}


function GetInterfaceProp(Instance: TObject; const PropName: string): IInterface;

begin
  Result:=GetInterfaceProp(Instance,FindPropInfo(Instance,PropName));
end;


function GetInterfaceProp(Instance: TObject; PropInfo: PPropInfo): IInterface;
type
  TGetInterfaceProc=function:IInterface of object;
  TGetInterfaceProcIndex=function(index:longint):IInterface of object;
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


procedure SetInterfaceProp(Instance: TObject; const PropName: string; const Value: IInterface);

begin
  SetInterfaceProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: IInterface);

type
  TSetIntfStrProcIndex=procedure(index:longint;const i:IInterface) of object;
  TSetIntfStrProc=procedure(i:IInterface) of object;

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


function GetRawInterfaceProp(Instance: TObject; const PropName: string): Pointer;

begin
  Result:=GetRawInterfaceProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo): Pointer;

begin
  Result:=GetPointerProp(Instance,PropInfo);
end;

procedure SetRawInterfaceProp(Instance: TObject; const PropName: string; const Value: Pointer);

begin
  SetRawInterfaceProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetRawInterfaceProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

begin
  SetPointerProp(Instance,PropInfo,Value);
end;

{ ---------------------------------------------------------------------
  Dynamic array properties
  ---------------------------------------------------------------------}

function GetDynArrayProp(Instance: TObject; const PropName: string): Pointer;
begin
  Result:=GetDynArrayProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;

type
  { we need a dynamic array as that type is usually passed differently from
    a plain pointer }
  TDynArray=array of Byte;
  TGetDynArrayProc=function:TDynArray of object;
  TGetDynArrayProcIndex=function(index:longint):TDynArray of object;

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

procedure SetDynArrayProp(Instance: TObject; const PropName: string; const Value: Pointer);
begin
  SetDynArrayProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo; const Value: Pointer);

type
  { we need a dynamic array as that type is usually passed differently from
    a plain pointer }
  TDynArray=array of Byte;
  TSetDynArrayProcIndex=procedure(index:longint;const i:TDynArray) of object;
  TSetDynArrayProc=procedure(i:TDynArray) of object;

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

Function GetStrProp(Instance: TObject; PropInfo: PPropInfo): AnsiString;

type
  TGetShortStrProcIndex=function(index:longint):ShortString of object;
  TGetShortStrProc=function():ShortString of object;
  TGetAnsiStrProcIndex=function(index:longint):AnsiString of object;
  TGetAnsiStrProc=function():AnsiString of object;

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


Procedure SetStrProp(Instance : TObject;PropInfo : PPropInfo; const Value : AnsiString);

type
  TSetShortStrProcIndex=procedure(index:longint;const s:ShortString) of object;
  TSetShortStrProc=procedure(const s:ShortString) of object;
  TSetAnsiStrProcIndex=procedure(index:longint;s:AnsiString) of object;
  TSetAnsiStrProc=procedure(s:AnsiString) of object;

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


procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString);
begin
  SetWideStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetWideStrProp(Instance: TObject; PropInfo: PPropInfo): WideString;
type
  TGetWideStrProcIndex=function(index:longint):WideString of object;
  TGetWideStrProc=function():WideString of object;
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
  TSetWideStrProcIndex=procedure(index:longint;s:WideString) of object;
  TSetWideStrProc=procedure(s:WideString) of object;
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


procedure SetUnicodeStrProp(Instance: TObject; const PropName: string; const Value: UnicodeString);

begin
  SetUnicodeStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;


Function GetUnicodeStrProp(Instance: TObject; PropInfo: PPropInfo): UnicodeString;

type
  TGetUnicodeStrProcIndex=function(index:longint):UnicodeString of object;
  TGetUnicodeStrProc=function():UnicodeString of object;

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
  TSetUnicodeStrProcIndex=procedure(index:longint;s:UnicodeString) of object;
  TSetUnicodeStrProc=procedure(s:UnicodeString) of object;

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

function GetRawbyteStrProp(Instance: TObject; PropInfo: PPropInfo): RawByteString;

type
  TGetRawByteStrProcIndex=function(index:longint): RawByteString of object;
  TGetRawByteStrProc=function():RawByteString of object;

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

function GetRawByteStrProp(Instance: TObject; const PropName: string): RawByteString;
begin
  Result:=GetRawByteStrProp(Instance,FindPropInfo(Instance,PropName));
end;

procedure SetRawByteStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: RawByteString);

type
  TSetRawByteStrProcIndex=procedure(index:longint;s:RawByteString) of object;
  TSetRawByteStrProc=procedure(s:RawByteString) of object;

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

procedure SetRawByteStrProp(Instance: TObject; const PropName: string; const Value: RawByteString);

begin
  SetRawByteStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;



{$ifndef FPUNONE}

{ ---------------------------------------------------------------------
  Float properties
  ---------------------------------------------------------------------}

function GetFloatProp(Instance : TObject;PropInfo : PPropInfo) : Extended;

type
  TGetExtendedProc = function:Extended of object;
  TGetExtendedProcIndex = function(Index: integer): Extended of object;
  TGetDoubleProc = function:Double of object;
  TGetDoubleProcIndex = function(Index: integer): Double of object;
  TGetSingleProc = function:Single of object;
  TGetSingleProcIndex = function(Index: integer):Single of object;
  TGetCurrencyProc = function : Currency of object;
  TGetCurrencyProcIndex = function(Index: integer) : Currency of object;

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


Procedure SetFloatProp(Instance : TObject;PropInfo : PPropInfo; Value : Extended);

type
  TSetExtendedProc = procedure(const AValue: Extended) of object;
  TSetExtendedProcIndex = procedure(Index: integer; AValue: Extended) of object;
  TSetDoubleProc = procedure(const AValue: Double) of object;
  TSetDoubleProcIndex = procedure(Index: integer; AValue: Double) of object;
  TSetSingleProc = procedure(const AValue: Single) of object;
  TSetSingleProcIndex = procedure(Index: integer; AValue: Single) of object;
  TSetCurrencyProc = procedure(const AValue: Currency) of object;
  TSetCurrencyProcIndex = procedure(Index: integer;  AValue: Currency) of object;

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


function GetFloatProp(Instance: TObject; const PropName: string): Extended;
begin
  Result:=GetFloatProp(Instance,FindPropInfo(Instance,PropName))
end;


Procedure SetFloatProp(Instance: TObject; const PropName: string;  Value: Extended);
begin
  SetFloatProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

{$endif}

{ ---------------------------------------------------------------------
  Method properties
  ---------------------------------------------------------------------}


Function GetMethodProp(Instance : TObject;PropInfo : PPropInfo) : TMethod;

type
  TGetMethodProcIndex=function(Index: Longint): TMethod of object;
  TGetMethodProc=function(): TMethod of object;

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


Procedure SetMethodProp(Instance : TObject;PropInfo : PPropInfo; const Value : TMethod);

type
  TSetMethodProcIndex=procedure(index:longint;p:TMethod) of object;
  TSetMethodProc=procedure(p:TMethod) of object;

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


Procedure SetMethodProp(Instance: TObject; const PropName: string;  const Value: TMethod);
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

Function GetVariantProp(Instance : TObject;PropInfo : PPropInfo): Variant;
begin
  CheckVariantEvent(CodePointer(OnGetVariantProp));
  Result:=OnGetVariantProp(Instance,PropInfo);
end;


Procedure SetVariantProp(Instance : TObject;PropInfo : PPropInfo; const Value: Variant);
begin
  CheckVariantEvent(CodePointer(OnSetVariantProp));
  OnSetVariantProp(Instance,PropInfo,Value);
end;


Function GetVariantProp(Instance: TObject; const PropName: string): Variant;
begin
  Result:=GetVariantProp(Instance,FindPropInfo(Instance,PropName));
end;


Procedure SetVariantProp(Instance: TObject; const PropName: string;  const Value: Variant);
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

Procedure SetPropValue(Instance: TObject; const PropName: string;  const Value: Variant);

begin
  SetPropValue(Instance, FindPropInfo(Instance, PropName), Value);
end;

Procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo;  const Value: Variant);

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

function TVmtMethodExTable.GetMethod(Index: Word): PVmtMethodExEntry;

var
  Arr : PVmtMethodExEntryArray;

begin
  if (Index >= Count) then
    Result := Nil
  else
    begin
{      Arr:=PVmtMethodExEntryArray(@Entries[0]);
      Result:=@(Arr^[Index]);}
      Result := PVmtMethodExEntry(@Entries[0]);
      while Index > 0 do
      begin
        Result := Result^.Next;
        Dec(Index);
      end;
    end;
end;

{ TRecMethodExTable }

function TRecMethodExTable.GetMethod(Index: Word): PRecMethodExEntry;

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

function TRecordData.GetExPropertyTable: PPropDataEx;

var
  MT : PRecordMethodTable;

begin
  MT:=GetMethodTable;
  if MT^.Count=0 then
    Result:=PPropDataEx(aligntoptr(PByte(@(MT^.Count))+SizeOf(Word)))
  else
    Result:=PPropDataEx(MT^.Method[MT^.Count-1]^.Tail);
end;

function TRecordData.GetExtendedFieldCount: Longint;
begin
  Result:= PLongint(PByte(@TotalFieldCount)+SizeOf(Longint)+(TotalFieldCount*SizeOf(TManagedField)))^
end;

function TRecordData.GetExtendedFields: PExtendedFieldTable;
begin
  Result:=PExtendedFieldTable(PByte(@TotalFieldCount)+SizeOf(Longint)+(TotalFieldCount*SizeOf(TManagedField)))
end;

function TRecordData.GetMethodTable: PRecordMethodTable;
begin
    Result:=PRecordMethodTable(GetExtendedFields^.Tail);
end;

{ TVmtExtendedFieldTable }

function TVmtExtendedFieldTable.GetField(aIndex: Word): PExtendedVmtFieldEntry;
begin
  Result:=Nil;
  If aIndex>=FieldCount then exit;
  Result:=PExtendedVmtFieldEntry(@Entries +aIndex *SizeOf(TExtendedVmtFieldEntry));
end;

function TVmtExtendedFieldTable.GetTail: Pointer;
begin
  if FieldCount=0 then
    Result:=@FieldCount+SizeOf(Word)
  else
    Result:=GetField(FieldCount-1)^.Tail;
end;

{ TExtendedVmtFieldEntry }

function TExtendedVmtFieldEntry.GetNext: PVmtFieldEntry;
begin
  Result := aligntoptr(Tail);
end;

function TExtendedVmtFieldEntry.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

function TExtendedVmtFieldEntry.GetTail: Pointer;
begin

  Result := PByte(@Name) + SizeOf(Pointer) ;
  {$ifdef PROVIDE_ATTR_TABLE}
  Result := Result + SizeOf(Pointer) ;
  {$ENDIF}
end;

function TExtendedVmtFieldEntry.GetVisibility: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask); // For the time being, maybe we need a AND $07 or so later on.
end;

{ TPropInfoEx }

function TPropInfoEx.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

function TPropInfoEx.GetTail: Pointer;
begin
  Result := PByte(@Flags) + SizeOf(Self);
end;

function TPropInfoEx.GetVisiblity: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask);
end;


{ TPropDataEx }

function TPropDataEx.GetPropEx(Index: Word): PPropInfoEx;
begin
  if Index >= PropCount then
      Result := Nil
    else
      begin
        Result := PPropInfoEx(aligntoptr(@PropList));
        while Index > 0 do
          begin
            Result := aligntoptr(Result^.Tail);
            Dec(Index);
          end;
      end;
end;

function TPropDataEx.GetTail: Pointer;
begin
  if PropCount = 0 then
    Result := @Proplist
  else
    Result := Prop[PropCount - 1]^.Tail;
end;

{ TParameterLocation }

function TParameterLocation.GetReference: Boolean;
begin
  Result := (LocType and $80) <> 0;
end;

function TParameterLocation.GetRegType: TRegisterType;
begin
  Result := TRegisterType(LocType and $7F);
end;

function TParameterLocation.GetShiftVal: Int8;
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

function TParameterLocations.GetLocation(aIndex: Byte): PParameterLocation;
begin
  if aIndex >= Count then
    Result := Nil
  else
    Result := PParameterLocation(PByte(aligntoptr(PByte(@Count) + SizeOf(Count))) + SizeOf(TParameterLocation) * aIndex);
end;

function TParameterLocations.GetTail: Pointer;
begin
  Result := PByte(aligntoptr(PByte(@Count) + SizeOf(Count))) + SizeOf(TParameterLocation) * Count;
end;

{ TProcedureParam }

function TProcedureParam.GetParamType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ParamTypeRef);
end;

function TProcedureParam.GetFlags: Byte;
begin
  Result := PByte(@ParamFlags)^;
end;

{ TManagedField }

function TManagedField.GetTypeRef: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(TypeRefRef);
end;

{ TArrayTypeData }

function TArrayTypeData.GetElType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ElTypeRef);
end;

function TArrayTypeData.GetDims(aIndex: Byte): PTypeInfo;
begin
  Result := DerefTypeInfoPtr(DimsRef[aIndex]);
end;

{ TProcedureSignature }

function TProcedureSignature.GetResultType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ResultTypeRef);
end;

function TProcedureSignature.GetParam(ParamIndex: Integer): PProcedureParam;
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

function TVmtMethodParam.GetTail: Pointer;
begin
  Result := PByte(@ParaLocs) + SizeOf(ParaLocs);
end;

function TVmtMethodParam.GetNext: PVmtMethodParam;
begin
  Result := PVmtMethodParam(aligntoptr(Tail));
end;

function TVmtMethodParam.GetName: ShortString;
begin
  Result := NamePtr^;
end;

{ TIntfMethodEntry }

function TIntfMethodEntry.GetParam(Index: Word): PVmtMethodParam;
begin
  if Index >= ParamCount then
    Result := Nil
  else
    Result := PVmtMethodParam(PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr))) + Index * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam)))));
end;

function TIntfMethodEntry.GetResultLocs: PParameterLocations;
begin
  if not Assigned(ResultType) then
    Result := Nil
  else
    Result := PParameterLocations(PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr))) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam)))));
end;

function TIntfMethodEntry.GetTail: Pointer;
begin
  Result := PByte(@NamePtr) + SizeOf(NamePtr);
  if ParamCount > 0 then
    Result := PByte(aligntoptr(Result)) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TVmtMethodParam))));
  if Assigned(ResultType) then
    Result := PByte(aligntoptr(Result)) + SizeOf(PParameterLocations);
end;

function TIntfMethodEntry.GetNext: PIntfMethodEntry;
begin
  Result := PIntfMethodEntry(aligntoptr(Tail));
end;

function TIntfMethodEntry.GetName: ShortString;
begin
  Result := NamePtr^;
end;

{ TIntfMethodTable }

function TIntfMethodTable.GetMethod(Index: Word): PIntfMethodEntry;
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

{ TVmtMethodExEntry }

function TVmtMethodExEntry.GetParamsStart: PByte;
begin
  Result:=@Params
end;

function TVmtMethodExEntry.GetMethodVisibility: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask);
end;

function TVMTMethodExEntry.GetParam(Index: Word): PVmtMethodParam;
begin
  if Index >= ParamCount then
    Result := Nil
  else
    Result := PVmtMethodParam(@params) + Index;
end;

function TVMTMethodExEntry.GetResultLocs: PParameterLocations;
begin
  if not Assigned(ResultType) then
    Result := Nil
  else
    Result := PParameterLocations(AlignToPtr(Param[ParamCount-1]^.Tail))
end;

function TVmtMethodExEntry.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

function TVMTMethodExEntry.GetTail: Pointer;

var
  I : integer;

begin
  if ParamCount = 0 then
{$IFNDEF VER3_2}
    Result := PByte(@CodeAddress) + SizeOf(CodePointer)+SizeOf(AttributeTable)
{$ELSE}
    Result := PByte(@VmtIndex) + SizeOf(VmtIndex)
{$ENDIF}
  else
    Result:=Param[ParamCount-1]^.GetTail;
  if Assigned(ResultType) then
    Result := PByte(aligntoptr(Result)) + SizeOf(PParameterLocations);
end;

function TVmtMethodExEntry.GetNext: PVmtMethodExEntry;
begin
  Result := PVmtMethodExEntry(Tail);
end;

function TVMTMethodExEntry.GetName: ShortString;
begin
  Result := NamePtr^;
end;

{ TRecMethodExEntry }

function TRecMethodExEntry.GetParamsStart: PByte;
begin
  Result:=PByte(aligntoptr(PByte(@NamePtr) + SizeOf(NamePtr)+SizeOf(FLags)));
  {$IFNDEF VER3_2}
  Result:=Result+SizeOf(CodeAddress)+SizeOf(AttributeTable);
  {$ENDIF}
end;

function TRecMethodExEntry.GetMethodVisibility: TVisibilityClass;
begin
  Result:=TVisibilityClass(Flags and RTTIFlagVisibilityMask);
end;

function TRecMethodExEntry.GetParam(Index: Word): PRecMethodParam;
begin
  if Index >= ParamCount then
    Result := Nil
  else
    Result := PRecMethodParam(GetParamsStart + Index * PtrUInt(aligntoptr(Pointer(SizeOf(TRecMethodParam)))));
end;

function TRecMethodExEntry.GetResultLocs: PParameterLocations;
begin
  if not Assigned(ResultType) then
    Result := Nil
  else
    Result := PParameterLocations(GetParamsStart + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TRecMethodParam)))));
end;

function TRecMethodExEntry.GetStrictVisibility: Boolean;
begin
  Result:=(Flags and RTTIFlagStrictVisibility)<>0;
end;

function TRecMethodExEntry.GetTail: Pointer;
begin
  Result := GetParamsStart;
  if ParamCount > 0 then
    Result := PByte(aligntoptr(Result)) + ParamCount * PtrUInt(aligntoptr(Pointer(SizeOf(TRecMethodParam))));
  if Assigned(ResultType) then
    Result := PByte(aligntoptr(Result)) + SizeOf(PParameterLocations);
end;

function TRecMethodExEntry.GetNext: PRecMethodExEntry;
begin
  Result := PRecMethodExEntry(aligntoptr(Tail));
end;

function TRecMethodExEntry.GetName: ShortString;
begin
  Result := NamePtr^;
end;


{ TVmtMethodTable }

function TVmtMethodTable.GetEntry(Index: LongWord): PVmtMethodEntry;
begin
  Result := PVmtMethodEntry(@Entries[0]) + Index;
end;

{ TVmtFieldTable }

function TVmtFieldTable.GetField(aIndex: Word): PVmtFieldEntry;
var
  c: Word;
begin
  if aIndex >= Count then
    Exit(Nil);
  c := aIndex;
  Result := @Fields;
  while c > 0 do begin
    Result := Result^.Next;
    Dec(c);
  end;
end;

function TVmtFieldTable.GetNext: Pointer;
begin
  Result := Tail;
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  { align to largest field of TVmtFieldEntry(!) }
  Result := Align(Result, SizeOf(PtrUInt));
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

function TVmtFieldTable.GetTail: Pointer;
begin
  if Count=0 then
    Result := @Fields
  else
    Result:=GetField(Count-1)^.Tail;
end;

{ TVmtFieldEntry }

function TVmtFieldEntry.GetNext: PVmtFieldEntry;
begin
  Result := Tail;
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  { align to largest field of TVmtFieldEntry }
  Result := Align(Result, SizeOf(PtrUInt));
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

function TVmtFieldEntry.GetTail: Pointer;
begin
  Result := PByte(@Name) + Length(Name) + SizeOf(Byte);
end;

{ TInterfaceData }

function TInterfaceData.GetUnitName: ShortString;
begin
  Result := UnitNameField;
end;

function TInterfaceData.GetPropertyTable: PPropData;
var
  p: PByte;
begin
  p := PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField);
  Result := AlignTypeData(p);
end;

function TInterfaceData.GetMethodTable: PIntfMethodTable;
begin
  Result := aligntoptr(PropertyTable^.Tail);
end;

{ TInterfaceRawData }

function TInterfaceRawData.GetUnitName: ShortString;
begin
  Result := UnitNameField;
end;

function TInterfaceRawData.GetIIDStr: ShortString;
begin
  Result := PShortString(AlignTypeData(PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField)))^;
end;

function TInterfaceRawData.GetPropertyTable: PPropData;
var
  p: PByte;
begin
  p := AlignTypeData(PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField));
  p := p + SizeOf(p^) + p^;
  Result := aligntoptr(p);
end;

function TInterfaceRawData.GetMethodTable: PIntfMethodTable;
begin
  Result := aligntoptr(PropertyTable^.Tail);
end;

{ TClassData }

function TClassData.GetExMethodTable: PVmtMethodExTable;

  { Copied from objpas.inc}

type
   {$push}
   {$packrecords normal}
   tmethodnamerec =
   {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   packed
   {$endif}
   record
      name : pshortstring;
      addr : codepointer;
   end;

   tmethodnametable =
   {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
   packed
   {$endif}
   record
     count : dword;
     entries : packed array[0..0] of tmethodnamerec;
   end;
   {$pop}

   pmethodnametable =  ^tmethodnametable;



var
  ovmt : PVmt;
  methodtable: pmethodnametable;

begin
  Result:=Nil;
  oVmt:=PVmt(ClassType);
  methodtable:=pmethodnametable(ovmt^.vMethodTable);
  // Shift till after
  if methodtable<>Nil then
    PByte(Result):=PByte(@methodtable^.Entries)+ SizeOf(tmethodnamerec) * methodtable^.count;
end;

function TClassData.GetExPropertyTable: PPropDataEx;
begin
  Result:=aligntoptr(PPropDataEx(GetPropertyTable^.GetTail));
end;

function TClassData.GetUnitName: ShortString;
begin
  Result := UnitNameField;
end;

function TClassData.GetPropertyTable: PPropData;
var
  p: PByte;
begin
  p := PByte(@UnitNameField[0]) + SizeOf(UnitNameField[0]) + Length(UnitNameField);
  Result := AlignToPtr(p);
end;

{ TTypeData }

function TTypeData.GetBaseType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(BaseTypeRef);
end;

function TTypeData.GetCompType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(CompTypeRef);
end;

function TTypeData.GetParentInfo: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ParentInfoRef);
end;

{$ifndef VER3_0}
function TTypeData.GetRecInitData: PRecInitData;
begin
  Result := PRecInitData(aligntoptr(PTypeData(RecInitInfo+2+PByte(RecInitInfo+1)^)));
end;
{$endif}

function TTypeData.GetHelperParent: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(HelperParentRef);
end;

function TTypeData.GetExtendedInfo: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(ExtendedInfoRef);
end;

function TTypeData.GetIntfParent: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(IntfParentRef);
end;

function TTypeData.GetRawIntfParent: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(RawIntfParentRef);
end;

function TTypeData.GetIIDStr: ShortString;
begin
  Result := PShortString(AlignTypeData(Pointer(@RawIntfUnit) + Length(RawIntfUnit) + 1))^;
end;

function TTypeData.GetElType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(elTypeRef);
end;

function TTypeData.GetElType2: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(elType2Ref);
end;

function TTypeData.GetInstanceType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(InstanceTypeRef);
end;

function TTypeData.GetRefType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(RefTypeRef);
end;

{ TPropData }

function TPropData.GetProp(Index: Word): PPropInfo;
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

function TPropData.GetTail: Pointer;
begin
  if PropCount = 0 then
    Result := PByte(@PropCount) + SizeOf(PropCount)
  else
    Result := Prop[PropCount - 1]^.Tail;
end;

{ TPropInfo }

function TPropInfo.GetPropType: PTypeInfo;
begin
  Result := DerefTypeInfoPtr(PropTypeRef);
end;

function TPropInfo.GetTail: Pointer;
begin
  Result := PByte(@Name[0]) + SizeOf(Name[0]) + Length(Name);
end;

function TPropInfo.GetNext: PPropInfo;
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

procedure RemoveEnumElementAliases(aTypeInfo: PTypeInfo);

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

procedure AddEnumElementAliases(aTypeInfo: PTypeInfo; const aNames: array of string; aStartValue: Integer = 0);

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

function GetEnumeratedAliasValue(aTypeInfo: PTypeInfo; const aName: string): Integer;

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

{$IFDEF HAVE_INVOKEHELPER}
procedure CallInvokeHelper(Instance: Pointer; aMethod : PIntfMethodEntry; aArgs : PPointer);

begin
  if (aMethod=Nil) then
    Raise EArgumentNilException.Create('Cannot call invoke helper on nil method info');
  if (aMethod^.InvokeHelper=Nil) then
    Raise EArgumentException.CreateFmt('Method %s has no invoke helper.',[aMethod^.Name]);
  aMethod^.InvokeHelper(Instance,aArgs);
end;

procedure CallInvokeHelper(aTypeInfo : PTypeInfo; Instance: Pointer; const aMethod : String; aArgs : PPointer);

Var
  Data : PInterfaceData;
  DataR : PInterfaceRawData;
  MethodTable : PIntfMethodTable;
  MethodEntry : PIntfMethodEntry;
  I : Integer;

begin
  If Instance=Nil then
    Raise EArgumentNilException.Create('Cannot call invoke helper on nil instance');
  if not (aTypeInfo^.Kind in [tkInterface,tkInterfaceRaw]) then
    Raise EArgumentException.Create('Cannot call invoke helper non non-interfaces');
  // Get method table
  if (aTypeInfo^.Kind=tkInterface) then
    begin
    Data:=PInterfaceData(GetTypeData(aTypeInfo));
    MethodTable:=Data^.MethodTable;
    end
  else
    begin
    DataR:=PInterfaceRawData(GetTypeData(aTypeInfo));
    MethodTable:=DataR^.MethodTable;
    end;
  // Search method in method table
  MethodEntry:=nil;
  I:=MethodTable^.Count-1;
  While (MethodEntry=Nil) and (I>=0) do
    begin
    MethodEntry:=MethodTable^.Method[i];
    if not SameText(MethodEntry^.Name,aMethod) then
      MethodEntry:=Nil;
    Dec(I);
    end;
  if MethodEntry=Nil then
    Raise EArgumentException.CreateFmt('Interface %s has no method %s.',[aTypeInfo^.Name,aMethod]);
  CallInvokeHelper(Instance,MethodEntry,aArgs);
end;
{$ENDIF HAVE_INVOKEHELPER}

end.
