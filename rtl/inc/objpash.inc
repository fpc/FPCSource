{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible,
    defining several internal structures for classes, interfaces, and
    resource strings.

    Additionally this file defines the interface of TObject, providing
    their basic implementation in the corresponding objpas.inc file.

    WARNING: IF YOU CHANGE SOME OF THESE INTERNAL RECORDS, MAKE SURE
    TO MODIFY THE COMPILER AND OBJPAS.INC ACCORDINGLY, OTHERWISE
    THIS WILL LEAD TO CRASHES IN THE RESULTING COMPILER AND/OR RTL.

    IN PARTICULAR, THE IMPLEMENTATION PART OF THIS INCLUDE FILE,
    OBJPAS.INC, USES SOME HARDCODED RECORD MEMBER OFFSETS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                            Basic Types/constants
*****************************************************************************}

    type
      TextFile = Text;

      PGuid = ^TGuid;

      { TGuid }

      TGuid = packed record
      Public
        class operator =(const aLeft, aRight: TGUID): Boolean;
        class operator <>(const aLeft, aRight: TGUID): Boolean; inline;
        class function Empty: TGUID; static;
        class function Create(const aData; aBigEndian: Boolean = False): TGUID; overload; static;
        class function Create(const aData: array of Byte; aStartIndex: Cardinal; aBigEndian: Boolean = False): TGUID; overload; static;
        class function Create(const aData : PByte; aBigEndian: Boolean = False): TGUID; overload; static;
        function IsEmpty: Boolean;
      Public
         case integer of
            1 : (
                 Data1 : DWord;
                 Data2 : word;
                 Data3 : word;
                 Data4 : array[0..7] of byte;
                );
            2 : (
                 D1 : DWord;
                 D2 : word;
                 D3 : word;
                 D4 : array[0..7] of byte;
                );
            3 : ( { uuid fields according to RFC4122 }
                 time_low : dword;			// The low field of the timestamp
                 time_mid : word;                      // The middle field of the timestamp
                 time_hi_and_version : word;           // The high field of the timestamp multiplexed with the version number
                 clock_seq_hi_and_reserved : byte;     // The high field of the clock sequence multiplexed with the variant
                 clock_seq_low : byte;                 // The low field of the clock sequence
                 node : array[0..5] of byte;           // The spatially unique node identifier
                );
      end;

{$ifdef FPC_HAS_FEATURE_CLASSES}
    const
       vmtInstanceSize         = 0;
       vmtParent               = sizeof(SizeInt)*2;
       { These were negative value's, but are now positive, else classes
         couldn't be used with shared linking which copies only all data from
         the .global directive and not the data before the directive (PFV) }
       vmtClassName            = vmtParent+sizeof(pointer);
       vmtDynamicTable         = vmtParent+sizeof(pointer)*2;
       vmtMethodTable          = vmtParent+sizeof(pointer)*3;
       vmtFieldTable           = vmtParent+sizeof(pointer)*4;
       vmtTypeInfo             = vmtParent+sizeof(pointer)*5;
       vmtInitTable            = vmtParent+sizeof(pointer)*6;
       vmtAutoTable            = vmtParent+sizeof(pointer)*7;
       vmtIntfTable            = vmtParent+sizeof(pointer)*8;
       vmtMsgStrPtr            = vmtParent+sizeof(pointer)*9;
       { methods }
       vmtMethodStart          = vmtParent+sizeof(pointer)*10;
       vmtDestroy              = vmtMethodStart;
       vmtNewInstance          = vmtMethodStart+sizeof(codepointer);
       vmtFreeInstance         = vmtMethodStart+sizeof(codepointer)*2;
       vmtSafeCallException    = vmtMethodStart+sizeof(codepointer)*3;
       vmtDefaultHandler       = vmtMethodStart+sizeof(codepointer)*4;
       vmtAfterConstruction    = vmtMethodStart+sizeof(codepointer)*5;
       vmtBeforeDestruction    = vmtMethodStart+sizeof(codepointer)*6;
       vmtDefaultHandlerStr    = vmtMethodStart+sizeof(codepointer)*7;
       vmtDispatch             = vmtMethodStart+sizeof(codepointer)*8;
       vmtDispatchStr          = vmtMethodStart+sizeof(codepointer)*9;
       vmtEquals               = vmtMethodStart+sizeof(codepointer)*10;
       vmtGetHashCode          = vmtMethodStart+sizeof(codepointer)*11;
       vmtToString             = vmtMethodStart+sizeof(codepointer)*12;

       { IInterface }
       S_OK          = 0;
       S_FALSE       = 1;
       E_NOINTERFACE = hresult($80004002);
       E_UNEXPECTED  = hresult($8000FFFF);
       E_NOTIMPL     = hresult($80004001);

     type
       { now the let's declare the base classes for the class object
         model. The compiler expects TObject and IUnknown to be defined
         first as forward classes }
       TObject = class;
       IUnknown = interface;

       TClass  = class of tobject;
       PClass  = ^tclass;


       { to access the message table from outside }
       TMsgStrTable = record
          name   : pshortstring;
          method : codepointer;
       end;

       PMsgStrTable = ^TMsgStrTable;

       TStringMessageTable = record
          count : longint;
          msgstrtable : array[0..0] of tmsgstrtable;
       end;

       pstringmessagetable = ^tstringmessagetable;
       pinterfacetable = ^tinterfacetable;

       PVmt = ^TVmt;
       PPVmt = ^PVmt;
       TVmt = record
         vInstanceSize: SizeInt;
         vInstanceSize2: SizeInt;
         vParentRef: {$ifdef VER3_0}PVmt{$else}PPVmt{$endif};
         vClassName: PShortString;
         vDynamicTable: Pointer;
         vMethodTable: Pointer;
         vFieldTable: Pointer;
         vTypeInfo: Pointer;
         vInitTable: Pointer;
         vAutoTable: Pointer;
         vIntfTable: PInterfaceTable;
         vMsgStrPtr: pstringmessagetable;
         vDestroy: CodePointer;
         vNewInstance: CodePointer;
         vFreeInstance: CodePointer;
         vSafeCallException: CodePointer;
         vDefaultHandler: CodePointer;
         vAfterConstruction: CodePointer;
         vBeforeDestruction: CodePointer;
         vDefaultHandlerStr: CodePointer;
         vDispatch: CodePointer;
         vDispatchStr: CodePointer;
         vEquals: CodePointer;
         vGetHashCode: CodePointer;
         vToString: CodePointer;
       private
         function GetvParent: PVmt; inline;
       public
         property vParent: PVmt read GetvParent;
       end;

       // This enumerate is found both in the rtl and compiler. Do not change the order of the fields.
       tinterfaceentrytype = (etStandard,
         etVirtualMethodResult,
         etStaticMethodResult,
         etFieldValue,
         etVirtualMethodClass,
         etStaticMethodClass,
         etFieldValueClass
       );

       pinterfaceentry = ^tinterfaceentry;
       tinterfaceentry = record
       private
         function GetIID: pguid; inline;
         function GetIIDStr: pshortstring; inline;
       public
         property IID: pguid read GetIID;
         property IIDStr: pshortstring read GetIIDStr;
       public
         IIDRef      : {$IFNDEF VER3_0}^{$ENDIF}pguid; { if assigned(IID) then Com else Corba}
         VTable      : Pointer;
         case integer of
           1 : (
                IOffset: sizeuint;
               );
           2 : (
                IOffsetAsCodePtr: CodePointer;
                IIDStrRef   : {$IFNDEF VER3_0}^{$ENDIF}pshortstring; { never nil. Com: upper(GuidToString(IID^)) }
                IType       : tinterfaceentrytype;
               );
       end;

       tinterfacetable = record
         EntryCount : sizeuint;
         Entries    : array[0..0] of tinterfaceentry;
       end;

       PMethod = ^TMethod;
       TMethod = record
         Code : CodePointer;
         Data : Pointer;
       public
         class operator =(const aLeft, aRight: TMethod): Boolean; inline;
         class operator <>(const aLeft, aRight: TMethod): Boolean; inline;
         class operator >(const aLeft, aRight: TMethod): Boolean; inline;
         class operator >=(const aLeft, aRight: TMethod): Boolean; inline;
         class operator <(const aLeft, aRight: TMethod): Boolean; inline;
         class operator <=(const aLeft, aRight: TMethod): Boolean; inline;
       end;

       // "Maximum" available stringtype : Shortstring, AnsiString or WideString
       {$ifdef FPC_HAS_FEATURE_ANSISTRINGS}

       {$IFNDEF UNICODERTL}
       RTLString = ansistring;
       {$ELSE UNICODERTL}
       RTLString = unicodestring;
       {$ENDIF UNICODERTL}

       {$else FPC_HAS_FEATURE_ANSISTRINGS}

       RTLString = shortstring;

       {$endif FPC_HAS_FEATURE_ANSISTRINGS}
       
       // Dispatch needs a DWord as the first 4 bytes in its untyped parameter.
       // Note that this is different from Delphi, which uses only a word.
       TDispatchMessage = record
         MsgID: DWord;
       end;  

       TObject = class
       {$IFDEF SYSTEM_HAS_FEATURE_MONITOR}
       strict private
          _MonitorData : Pointer;
       private
          function SetMonitorData(aData,aCheckOld : Pointer) : Pointer; inline;
          function GetMonitorData: Pointer; inline;
       {$ENDIF}
       protected
          function GetDisposed : Boolean; inline;
       public
          { please don't change the order of virtual methods, because
            their vmt offsets are used by some assembler code which uses
            hard coded addresses      (FK)                                 }
          constructor Create;
          { the virtual procedures must be in THAT order }
          destructor Destroy;virtual;
          class function newinstance : tobject;virtual;
          procedure FreeInstance;virtual;
          function SafeCallException(exceptobject : tobject;
            exceptaddr : codepointer) : HResult;virtual;
          procedure DefaultHandler(var message);virtual;

          procedure Free;
          class function InitInstance(instance : pointer) : tobject;
          procedure CleanupInstance;
          class function ClassType : tclass;{$ifdef SYSTEMINLINE}inline;{$endif}
          class function ClassInfo : pointer;
          class function ClassName : shortstring;
          class function ClassNameIs(const name : RTLString) : boolean;
          class function ClassParent : tclass;{$ifdef SYSTEMINLINE}inline;{$endif}
          class function InstanceSize : SizeInt;// {$ifdef SYSTEMINLINE}inline;{$endif}
          class function InheritsFrom(aclass : tclass) : boolean;
          class function StringMessageTable : pstringmessagetable;

          class function MethodAddress(const name : shortstring) : codepointer;
          class function MethodName(address : codepointer) : shortstring;
          function FieldAddress(const name : shortstring) : pointer;

          { new since Delphi 4 }
          procedure AfterConstruction;virtual;
          procedure BeforeDestruction;virtual;

          { new for gtk, default handler for text based messages }
          procedure DefaultHandlerStr(var message);virtual;

          { message handling routines }
          procedure Dispatch(var message);virtual;
          procedure DispatchStr(var message);virtual;

          { interface functions }
          function GetInterface(const iid : tguid; out obj) : boolean;
          function GetInterface(const iidstr : shortstring;out obj) : boolean;
          function GetInterfaceByStr(const iidstr : shortstring; out obj) : boolean;
          function GetInterfaceWeak(const iid : tguid; out obj) : boolean; // equal to GetInterface but the interface returned is not referenced
          class function GetInterfaceEntry(const iid : tguid) : pinterfaceentry;
          class function GetInterfaceEntryByStr(const iidstr : shortstring) : pinterfaceentry;
          class function GetInterfaceTable : pinterfacetable;

          { new since Delphi 2009 }
          class function UnitName : RTLString;
          class function QualifiedClassName: RTLString;
          Procedure DisposeOf; inline;
          Procedure CheckDisposed; inline;
          Property Disposed : Boolean Read GetDisposed;
          function Equals(Obj: TObject) : boolean;virtual;
          function GetHashCode: PtrInt;virtual;
          function ToString: RTLString; virtual;
       end;

       IUnknown = interface
         ['{00000000-0000-0000-C000-000000000046}']
         function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
         function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
         function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
       end;
       IInterface = IUnknown;

       {$M+}
       IInvokable = interface(IInterface)
       end;
       {$M-}

       { enumerator support }
       IEnumerator = interface(IInterface)
         function GetCurrent: TObject;
         function MoveNext: Boolean;
         procedure Reset;
         property Current: TObject read GetCurrent;
       end;

       IEnumerable = interface(IInterface)
         function GetEnumerator: IEnumerator;
       end;

       { for native dispinterface support }
       IDispatch = interface(IUnknown)
          ['{00020400-0000-0000-C000-000000000046}']
          function GetTypeInfoCount(out count : longint) : HResult;stdcall;
          function GetTypeInfo(Index,LocaleID : longint;
            out TypeInfo): HResult;stdcall;
          function GetIDsOfNames(const iid: TGUID; names: Pointer;
            NameCount, LocaleID: LongInt; DispIDs: Pointer) : HResult;stdcall;
          function Invoke(DispID: LongInt;const iid : TGUID;
            LocaleID : longint; Flags: Word;var params;
            VarResult,ExcepInfo,ArgErr : pointer) : HResult;stdcall;
       end;

       { TInterfacedObject }

       TInterfacedObject = class(TObject,IUnknown)
       protected
          FRefCount : longint;
          FDestroyCount : longint;
          { implement methods of IUnknown }
          function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
          function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
          function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
        public
          destructor Destroy; override;
          procedure AfterConstruction;override;
          procedure BeforeDestruction;override;
          class function NewInstance : TObject;override;
          property RefCount : longint read FRefCount;
       end;
       TInterfacedClass = class of TInterfacedObject;

       TAggregatedObject = class(TObject)
       private
          fcontroller: Pointer;
          function GetController: IUnknown;
       protected
          { implement methods of IUnknown }
          function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
          function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
          function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
       public
          constructor Create(const aController: IUnknown);
          property Controller : IUnknown read GetController;
       end;

       TContainedObject = class(TAggregatedObject,IInterface)
         protected
           function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
         end;

       TNoRefCountObject =  class(TObject, IInterface)
         protected
           function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
           function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
           function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
       end;

       
       TInterfaceThunk = Class(TInterfacedObject)
       Public
         Type
           TArgData = record
             addr : pointer;   // Location
             info : pointer;   // type info (if available: nil for untyped args)
             idx  : smallint;  // param index in rtti
             ahigh : sizeint;  // For open arrays, high()
           end;
           PArgData = ^TargData;
         TThunkCallBack = Procedure(aInstance: Pointer; aMethod,aCount : Longint; aData : PArgData) of object;
       Private  
         FCallback : TThunkCallback;
       Protected  
         function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
         Procedure Thunk(aMethod: Longint; aCount : Longint; aData : PArgData); virtual;
       Public  
         constructor create(aCallBack : TThunkCallback);
         function InterfaceVMTOffset : word; virtual;
       end;  
       TInterfaceThunkClass = class of TInterfaceThunk;

       { some pointer definitions }
       PUnknown = ^IUnknown;
       PPUnknown = ^PUnknown;
       PDispatch = ^IDispatch;
       PPDispatch = ^PDispatch;
       PInterface = PUnknown;

       {*****************************************************************************
                                     Exception support
        *****************************************************************************}

{$ifdef FPC_USE_PSABIEH}

{$if (defined(CPUARMEL) or defined(CPUARMHF)) and not defined(darwin)}
{$define __ARM_EABI_UNWINDER__}
{$endif}

       { needed here for TExceptObject (rest is in psabiehh.inc) }
       FPC_Unwind_Reason_Code = longint; {cint}
       FPC_Unwind_Action = longint; {cint}
{$ifdef __ARM_EABI_UNWINDER__}
       FPC_Unwind_State = longint; {cint}
{$endif}

       PFPC_Unwind_Exception = ^FPC_Unwind_Exception;

       FPC_Unwind_Exception_Cleanup_Fn =
         procedure(reason: FPC_Unwind_Reason_Code; exc: PFPC_Unwind_Exception); cdecl;

       FPC_Unwind_Exception = record
         { qword instead of array of AnsiChar to ensure proper alignment and
           padding, and also easier to compare }
         exception_class: qword;
         exception_cleanup: FPC_Unwind_Exception_Cleanup_Fn;

{$ifdef __ARM_EABI_UNWINDER__}
         { rest of UCB }
         // Unwinder cache, private fields for the unwinder's use
         unwinder_cache: record
           reserved1, // init reserved1 to 0, then don't touch
           reserved2,
           reserved3,
           reserved4,
           reserved5: UInt32;
         end;
         // Propagation barrier cache (valid after phase 1):
         barrier_cache: record
           sp: PtrUInt;
           bitpattern: array[0..4] of UInt32;
         end;
         // Cleanup cache (preserved over cleanup):
         cleanup_cache: record
           bitpattern: array[0..3] of UInt32;
         end;
         // Pr cache (for pr's benefit):
         pr_cache: record
           fnstart: UInt32;      // function start address
           ehtp: pointer;        // pointer to EHT entry header word
           additional: UInt32;   // additional data
           reserved1: UInt32;
         end;
{$else}
         private_1: ptruint;
         private_2: ptruint;
         private_3: ptruint;
         private_4: ptruint;
         private_5: ptruint;
         private_6: ptruint;
{$endif}
       end;
{$endif FPC_USE_PSABIEH}

       TExceptProc = Procedure (Obj : TObject; Addr : CodePointer; FrameCount:Longint; Frame: PCodePointer);

       { Exception object stack }
       PExceptObject = ^TExceptObject;
       TExceptObject = record
         FObject    : TObject;
         Addr       : codepointer;
         Next       : PExceptObject;
         refcount   : Longint;
         Framecount : Longint;
         Frames     : PCodePointer;
{$ifdef FPC_USE_WIN32_SEH}
         SEHFrame   : Pointer;
         ExceptRec  : Pointer;
         ReraiseBuf : jmp_buf;
{$endif FPC_USE_WIN32_SEH}
{$ifdef FPC_USE_PSABIEH}
{$ifndef __ARM_EABI_UNWINDER__}
         { cached info from unwind phase for action phase }
         handler_switch_value: longint;
         language_specific_data: PByte;
         landing_pad: PtrUInt;
{$endif __ARM_EABI_UNWINDER__}
         { libunwind exception handling data (must be last!) }
         unwind_exception: FPC_Unwind_Exception;
{$endif FPC_USE_PSABIEH}
       end;


    Const
       ExceptProc : TExceptProc = Nil;
       RaiseProc : TExceptProc = Nil;
       RaiseMaxFrameCount : Longint = 16;

    Function RaiseList : PExceptObject;

    { @abstract(increase exception reference count)
      When leaving an except block, the exception object is normally
      freed automatically. To avoid this, call this function.
      If within the exception object you decide that you don't need
      the exception after all, call @link(ReleaseExceptionObject).
      Otherwise, if the reference count is > 0, the exception object
      goes into your "property" and you need to free it manually.
      The effect of this function is countered by re-raising an exception
      via "raise;", this zeroes the reference count again.
      Calling this method is only valid within an except block.
      @return(pointer to the exception object) }
    function AcquireExceptionObject: Pointer;

    { @abstract(decrease exception reference count)
      After calling @link(AcquireExceptionObject) you can call this method
      to decrease the exception reference count again.
      If the reference count is > 0, the exception object
      goes into your "property" and you need to free it manually.
      Calling this method is only valid within an except block. }
    procedure ReleaseExceptionObject;

  const
    { for safe as operator support }
    IObjectInstance: TGuid = '{D91C9AF4-3C93-420F-A303-BF5BA82BFD23}';

    {*****************************************************************************
                                  Attribute support
     *****************************************************************************}
  Type
  {$PUSH}
  { disable the warning that the constructor should be public }
  {$WARN 3018 OFF}
  TCustomAttribute = class(TObject)
  private
    { if the user wants to use a parameterless constructor they need to
      explicitely declare it in their type }
    constructor Create;
  end;
  {$POP}

  TUnimplementedAttribute = class(TCustomAttribute)
  public
    constructor Create; unimplemented;
  end;

  WeakAttribute = class(TUnimplementedAttribute);
  UnsafeAttribute = class(TUnimplementedAttribute);
  RefAttribute = class(TUnimplementedAttribute);
  VolatileAttribute = class(TUnimplementedAttribute);

  StoredAttribute = Class(TCustomAttribute)
  Private
     FFlag : Boolean;
     FName : ShortString;
  Public
    Constructor Create;
    Constructor Create(Const aFlag : Boolean);
    Constructor Create(Const aName : ShortString);
    Property Flag : Boolean Read FFlag;
    Property Name : ShortString Read FName;
  end;

  {*****************************************************************************
                                TMonitor support
   *****************************************************************************}
  {$IFDEF SYSTEM_HAS_FEATURE_MONITOR}
  Type

  PPMonitor = ^PMonitor;
  PMonitor = ^TMonitor;
  TMonitor = record
  Private
    class procedure FreeMonitorData(aData : Pointer); static;
  public
    class procedure SetDefaultSpinCount(const aSpinCount: Longint); static;
    class function GetDefaultSpinCount : Longint; static;
    class procedure Enter(Const aObject: TObject); overload; static; inline;
    class function Enter(Const aObject: TObject; aTimeout: Cardinal): Boolean; overload; static;
    class procedure Exit(Const aObject: TObject); overload; static;
    class function TryEnter(Const aObject: TObject): Boolean; overload; static;
    class function Wait(Const aObject: TObject; aTimeout: Cardinal): Boolean; overload; static;
    class function Wait(Const aObject, aLock: TObject; aTimeout: Cardinal): Boolean; overload; static;
    class procedure Pulse(Const aObject: TObject); overload; static;
    class procedure PulseAll(Const aObject: TObject); overload; static;
    class property DefaultSpinCount: Longint read GetDefaultSpinCount write SetDefaultSpinCount;
  end;


  TMonitorManager = record
  Public type
    TMonitorSetSpinCountProc = Procedure(const aSpinCount : LongInt);
    TMonitorGetSpinCountProc = Function : LongInt;
    TMonitorProc = Procedure(const aObject : TObject);
    TMonitorFunc = function(const aObject : TObject) : Boolean;
    TMonitorTimeoutFunc = function(const aObject : TObject; aTimeout : Cardinal) : Boolean;
    TMonitorLockTimeoutFunc = function(const aObject,aLock : TObject; aTimeout : Cardinal) : Boolean;
    TMonitorSetObjectDataProc = function (const aObject : TObject; aData,aComparand : Pointer) : Pointer;
    TMonitorGetObjectDataFunc = function (const aObject : TObject): Pointer;
    TMonitorFreeDataProc = procedure (aData : Pointer);
  Public
    DoSetDefaultSpinCount : TMonitorSetSpinCountProc;
    DoGetDefaultSpinCount : TMonitorGetSpinCountProc;
    DoEnter : TMonitorProc;
    DoEnterTimeout : TMonitorTimeoutFunc;
    DoExit : TMonitorProc;
    DoTryEnter : TMonitorFunc;
    DoWait : TMonitorTimeoutFunc;
    DoWaitLock : TMonitorLockTimeoutFunc;
    DoPulse : TMonitorProc;
    DoPulseAll : TMonitorProc;
    DoFreeMonitorData : TMonitorFreeDataProc;
    // Will be set by SetMonitorManager
    DoGetMonitorObjectData : TMonitorGetObjectDataFunc;
    DoSetMonitorObjectData : TMonitorSetObjectDataProc;
  end;

  const
    INFINITE = CARDINAL($FFFFFFFF);

  function MonitorEnter(Const aObject: TObject; aTimeout: Cardinal = INFINITE): Boolean; inline;
  function MonitorTryEnter(Const aObject: TObject): Boolean; inline;
  procedure MonitorExit(Const aObject: TObject); inline;
  function MonitorWait(Const aObject: TObject; aTimeout: Cardinal): Boolean; inline; overload;
  function MonitorWait(Const aObject, ALock: TObject; aTimeout: Cardinal): Boolean; inline; overload;
  procedure MonitorPulse(Const aObject: TObject); inline;
  procedure MonitorPulseAll(Const aObject: TObject); inline;

  // Will set Do(S|G)etMonitorObjectData fields on aNew, and returns the old manager
  function SetMonitorManager (var aNew : TMonitorManager) : TMonitorManager;
  function GetMonitorManager : TMonitorManager;
  {$ENDIF}

{$endif FPC_HAS_FEATURE_CLASSES}

{*****************************************************************************
                              Array of const support
*****************************************************************************}

   const
      vtInteger       = 0;
      vtBoolean       = 1;
      vtChar          = 2;
{$ifndef FPUNONE}
      vtExtended      = 3;
{$endif}
      vtString        = 4;
      vtPointer       = 5;
      vtPChar         = 6;
      vtObject        = 7;
      vtClass         = 8;
      vtWideChar      = 9;
      vtPWideChar     = 10;
      vtAnsiString    = 11;
      vtCurrency      = 12;
      vtVariant       = 13;
      vtInterface     = 14;
      vtWideString    = 15;
      vtInt64         = 16;
      vtQWord         = 17;
      vtUnicodeString = 18;

   type
      PVarRec = ^TVarRec;
      TVarRec = record
         case VType : sizeint of
{$ifdef ENDIAN_BIG}
           vtInteger       : ({$IFDEF CPU64}integerdummy1 : Longint;{$ENDIF CPU64}VInteger: Longint);
           vtBoolean       : ({$IFDEF CPU64}booldummy : Longint;{$ENDIF CPU64}booldummy1,booldummy2,booldummy3: byte; VBoolean: Boolean);
           vtChar          : ({$IFDEF CPU64}chardummy : Longint;{$ENDIF CPU64}chardummy1,chardummy2,chardummy3: byte; VChar: AnsiChar);
           vtWideChar      : ({$IFDEF CPU64}widechardummy : Longint;{$ENDIF CPU64}wchardummy1,VWideChar: WideChar);
{$else ENDIAN_BIG}
           vtInteger       : (VInteger: Longint);
           vtBoolean       : (VBoolean: Boolean);
           vtChar          : (VChar: AnsiChar);
           vtWideChar      : (VWideChar: WideChar);
{$endif ENDIAN_BIG}
{$ifndef FPUNONE}
           vtExtended      : (VExtended: PExtended);
{$endif}
           vtString        : (VString: PShortString);
           vtPointer       : (VPointer: Pointer);
           vtPChar         : (VPChar: PAnsiChar);
{$ifdef FPC_HAS_FEATURE_CLASSES}
           vtObject        : (VObject: TObject);
           vtClass         : (VClass: TClass);
{$endif FPC_HAS_FEATURE_CLASSES}
           vtPWideChar     : (VPWideChar: PWideChar);
           vtAnsiString    : (VAnsiString: Pointer);
           vtCurrency      : (VCurrency: PCurrency);
{$ifdef FPC_HAS_FEATURE_VARIANTS}
           vtVariant       : (VVariant: PVariant);
{$endif FPC_HAS_FEATURE_VARIANTS}
           vtInterface     : (VInterface: Pointer);
           vtWideString    : (VWideString: Pointer);
           vtInt64         : (VInt64: PInt64);
           vtUnicodeString : (VUnicodeString: Pointer);
           vtQWord         : (VQWord: PQWord);
       end;

  var
    DispCallByIDProc : codepointer;

{*****************************************************************************
                              Resourcestring support
*****************************************************************************}

{$ifdef FPC_HAS_FEATURE_RESOURCES}
  type
    PResourceStringRecord = ^TResourceStringRecord;
    TResourceStringRecord = Record
       Name : AnsiString;
       CurrentValue,
       DefaultValue : RTLString;
       HashValue    : LongWord;
     end;
{$endif FPC_HAS_FEATURE_RESOURCES}

{*****************************************************************************
                              Various Delphi elements
 *****************************************************************************}

Type
  TPtrWrapper = record
  private
    FValue: Pointer;
    class function GetNilValue: TPtrWrapper; inline; static;
  public
    constructor Create(AValue: PtrInt); overload;
    constructor Create(AValue: Pointer); overload;
    function ToPointer: Pointer; inline;
    function ToInteger: PtrInt; inline;
    class property NilValue: TPtrWrapper read GetNilValue;
    class operator =(Left, Right: TPtrWrapper): Boolean; inline;
    { ...to allow convenient and direct reading without relying on inline... and convenient writing from SysUtils until TMarshal is moved here... }
    property Value: Pointer read FValue write FValue;
  end;
  TPtrWrapperArray = Array of TPtrWrapper;

  { Generic array type. 
    Slightly Less useful in FPC, since dyn array compatibility is at the element level. 
    But still useful for generic methods and of course Delphi compatibility}
  generic TArray<T> = array of T;


  TMarshal = class sealed
  public
    Type 
      TUnicodeCharArray = Array of UnicodeChar;
      Tint8Array = Array of int8;
      Tint16Array = Array of int16;
      Tint32Array = Array of int32;
      Tint64Array = Array of int64;
      TUint8Array = Array of Uint8;
      TUint16Array = Array of Uint16;
      TUint32Array = Array of Uint32;
      TUint64Array = Array of Uint64;
      
  Public
    constructor Create;

    class function AllocMem(Size: SizeInt): TPtrWrapper; static; inline;
    class function ReallocMem(OldPtr: TPtrWrapper; NewSize: SizeInt): TPtrWrapper; static; inline;
    class procedure FreeMem(Ptr: TPtrWrapper); static; inline;
    class procedure Move(Src, Dest: TPtrWrapper; Count: SizeInt); static; inline;

    class function UnsafeAddrOf(var Value): TPtrWrapper; static; inline;

{$IFDEF FPC_HAS_FEATURE_UNICODESTRINGS}
    class function AsAnsi(const S: UnicodeString): AnsiString; static; inline;
    class function AsAnsi(S: PUnicodeChar): AnsiString; static; inline;

    class function InOutString(const S: UnicodeString): PUnicodeChar; static; inline;
    class function InString(const S: UnicodeString): PUnicodeChar; static; inline;
    class function OutString(const S: UnicodeString): PUnicodeChar; static; inline;

    class function AllocStringAsAnsi(const Str: UnicodeString): TPtrWrapper; static; inline;
    class function AllocStringAsAnsi(const Str: UnicodeString; CodePage: Word): TPtrWrapper; static; inline;
    class function AllocStringAsAnsi(S: PUnicodeChar): TPtrWrapper; static; inline;
    class function AllocStringAsAnsi(S: PUnicodeChar; CodePage: Word): TPtrWrapper; static; inline;
    class function AllocStringAsUnicode(const Str: UnicodeString): TPtrWrapper; static;
    class function AllocStringAsUtf8(const Str: UnicodeString): TPtrWrapper; static; inline;
    class function AllocStringAsUtf8(S: PUnicodeChar): TPtrWrapper; static; inline;

    { Generalization of all AllocStringAsAnsi* above, public because used in TMarshaller. }
    class function AllocStringAsAnsi(S: PUnicodeChar; Len: SizeInt; CodePage: Word): TPtrWrapper; static;

    class procedure Copy(const Src: TUnicodeCharArray; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class function FixString(var Str: UnicodeString): TPtrWrapper; static;
    class function UnsafeFixString(const Str: UnicodeString): TPtrWrapper; static;
    class procedure UnfixString(Ptr: TPtrWrapper); static;

    class function ReadStringAsAnsi(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static; inline;
    class function ReadStringAsAnsi(CodePage: Word; Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static;
    class function ReadStringAsAnsiUpTo(CodePage: Word; Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString; static;

    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt; CodePage: Word); static; inline;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure WriteStringAsAnsi(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt; CodePage: Word); static;

    class function ReadStringAsUnicode(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static;
    class function ReadStringAsUnicodeUpTo(Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString; static;
    class procedure WriteStringAsUnicode(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static;
    class procedure WriteStringAsUnicode(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static;

    class function ReadStringAsUtf8(Ptr: TPtrWrapper; Len: SizeInt = -1): UnicodeString; static; inline;
    class function ReadStringAsUtf8UpTo(Ptr: TPtrWrapper; MaxLen: SizeInt): UnicodeString; static; inline;
    class procedure WriteStringAsUtf8(Ptr: TPtrWrapper; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure WriteStringAsUtf8(Ptr: TPtrWrapper; Ofs: SizeInt; const Value: UnicodeString; MaxCharsIncNull: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TUnicodeCharArray; StartIndex: SizeInt; Count: SizeInt); static; inline;
{$ENDIF}

    class procedure Copy(const Src: TUint8Array; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TUint8Array; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: TInt8Array; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TInt8Array; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: TUInt16Array; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TUInt16Array; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: TInt16Array; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TInt16Array; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: TInt32Array; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TInt32Array; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: TInt64Array; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TInt64Array; StartIndex: SizeInt; Count: SizeInt); static; inline;
    class procedure Copy(const Src: TPtrWrapperArray; StartIndex: SizeInt; Dest: TPtrWrapper; Count: SizeInt); static; inline;
    class procedure Copy(Src: TPtrWrapper; var Dest: TPtrWrapperArray; StartIndex: SizeInt; Count: SizeInt); static; inline;

    generic class function FixArray<T>(const Arr: specialize TArray<T>): TPtrWrapper; static;
    generic class procedure UnfixArray<T>(ArrPtr: TPtrWrapper); static;



    class function ReadByte(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Byte; static; inline;
    class procedure WriteByte(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Byte); static; inline;
    class procedure WriteByte(Ptr: TPtrWrapper; Value: Byte); static; inline;

    class function ReadInt16(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int16; static; inline;
    class procedure WriteInt16(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int16); static; inline;
    class procedure WriteInt16(Ptr: TPtrWrapper; Value: Int16); static; inline;

    class function ReadInt32(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int32; static; inline;
    class procedure WriteInt32(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int32); static; inline;
    class procedure WriteInt32(Ptr: TPtrWrapper; Value: Int32); static; inline;

    class function ReadInt64(Ptr: TPtrWrapper; Ofs: SizeInt = 0): Int64; static; inline;
    class procedure WriteInt64(Ptr: TPtrWrapper; Ofs: SizeInt; Value: Int64); static; inline;
    class procedure WriteInt64(Ptr: TPtrWrapper; Value: Int64); static; inline;

    class function ReadPtr(Ptr: TPtrWrapper; Ofs: SizeInt = 0): TPtrWrapper; static; inline;
    class procedure WritePtr(Ptr: TPtrWrapper; Ofs: SizeInt; Value: TPtrWrapper); static; inline;
    class procedure WritePtr(Ptr, Value: TPtrWrapper); static; inline;
  end;

Const
  // In Delphi System.SysInit
  PtrToNil: Pointer = nil;

