{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                            Basic Types/constants
*****************************************************************************}

    const
       vmtInstanceSize         = 0;
       vmtParent               = 8;
       { These were negative value's, but are now positive, else classes
         couldn't be used with shared linking which copies only all data from
         the .global directive and not the data before the directive (PFV) }
       vmtClassName            = 12;
       vmtDynamicTable         = 16;
       vmtMethodTable          = 20;
       vmtFieldTable           = 24;
       vmtTypeInfo             = 28;
       vmtInitTable            = 32;
       vmtAutoTable            = 36;
       vmtIntfTable            = 40;
       vmtMsgStrPtr            = 44;
       { methods }
       vmtMethodStart          = 48;
       vmtDestroy              = vmtMethodStart;
       vmtNewInstance          = vmtMethodStart+4;
       vmtFreeInstance         = vmtMethodStart+8;
       vmtSafeCallException    = vmtMethodStart+12;
       vmtDefaultHandler       = vmtMethodStart+16;
       vmtAfterConstruction    = vmtMethodStart+20;
       vmtBeforeDestruction    = vmtMethodStart+24;
       vmtDefaultHandlerStr    = vmtMethodStart+28;

    type
       TextFile = Text;

       { now the let's declare the base classes for the class object }
       { model                                                       }
       TObject = class;
       TClass  = class of tobject;
       PClass  = ^tclass;


       { to access the message table from outside }
       TMsgStrTable = record
          name : pshortstring;
          method : pointer;
       end;

       PMsgStrTable = ^TMsgStrTable;

       TStringMessageTable = record
          count : dword;
          msgstrtable : array[0..0] of tmsgstrtable;
       end;

       pstringmessagetable = ^tstringmessagetable;

       PGuid = ^TGuid;
       TGuid = packed record
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
       end;

       pinterfaceentry = ^tinterfaceentry;
       tinterfaceentry = packed record
         IID: pguid; { if assigned(IID) then Com else Corba}
         VTable: Pointer;
         IOffset: LongInt;
         IIDStr: pshortstring; { never nil. Com: upper(GuidToString(IID^)) }
       end;

       pinterfacetable = ^tinterfacetable;
       tinterfacetable = packed record
         EntryCount: Word;
         Entries: array[0..0] of tinterfaceentry;
       end;

       TObject = class
       public
          { please don't change the order of virtual methods, because      }
          { their vmt offsets are used by some assembler code which uses   }
          { hard coded addresses      (FK)                                 }
          constructor Create;
          { the virtual procedures must be in THAT order }
          destructor Destroy;virtual;
          class function newinstance : tobject;virtual;
          procedure FreeInstance;virtual;
          function SafeCallException(exceptobject : tobject;
            exceptaddr : pointer) : longint;virtual;
          procedure DefaultHandler(var message);virtual;

          procedure Free;
          class function InitInstance(instance : pointer) : tobject;
          procedure CleanupInstance;
          function ClassType : tclass;
          class function ClassInfo : pointer;
          class function ClassName : shortstring;
          class function ClassNameIs(const name : string) : boolean;
          class function ClassParent : tclass;
          class function InstanceSize : longint;
          class function InheritsFrom(aclass : tclass) : boolean;
          class function StringMessageTable : pstringmessagetable;
          { message handling routines }
          procedure Dispatch(var message);
          procedure DispatchStr(var message);

          class function MethodAddress(const name : shortstring) : pointer;
          class function MethodName(address : pointer) : shortstring;
          function FieldAddress(const name : shortstring) : pointer;

          { new since Delphi 4 }
          procedure AfterConstruction;virtual;
          procedure BeforeDestruction;virtual;

          { new for gtk, default handler for text based messages }
          procedure DefaultHandlerStr(var message);virtual;

{$ifdef HASINTF}
          { interface functions }
          function GetInterface(const iid : tguid; out obj) : boolean;
          function GetInterfaceByStr(const iidstr : string; out obj) : boolean;
          class function GetInterfaceEntry(const iid : tguid) : pinterfaceentry;
          class function GetInterfaceEntryByStr(const iidstr : string) : pinterfaceentry;
          class function GetInterfaceTable : pinterfacetable;
{$endif HASINTF}
       end;

{$ifdef HASINTF}
       IUnknown = interface
         ['{00000000-0000-0000-C000-000000000046}']
         function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
         function _AddRef : longint;stdcall;
         function _Release : longint;stdcall;
       end;
       IInterface = IUnknown;

       {$M+}
       IInvokable = interface(IInterface)
       end;
       {$M-}

       { for native dispinterface support }
       IDispatch = interface(IUnknown)
          ['{00020400-0000-0000-C000-000000000046}']
          function GetTypeInfoCount(out count : longint) : longint;stdcall;
          function GetTypeInfo(Index,LocaleID : longint;
            out TypeInfo): LongInt;stdcall;
          function GetIDsOfNames(const iid: TGUID; names: Pointer;
            NameCount, LocaleID: LongInt; DispIDs: Pointer) : longint;stdcall;
          function Invoke(DispID: LongInt;const iid : TGUID;
            LocaleID : longint; Flags: Word;var params;
            VarResult,ExcepInfo,ArgErr : pointer) : longint;stdcall;
       end;

       TInterfacedObject = class(TObject,IUnknown)
       protected
          frefcount : longint;
          { implement methods of IUnknown }
          function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
          function _AddRef : longint;stdcall;
          function _Release : longint;stdcall;
        public
          procedure AfterConstruction;override;
          procedure BeforeDestruction;override;
          class function NewInstance : TObject;override;
          property RefCount : longint read frefcount;
       end;
       TInterfacedClass = class of TInterfacedObject;

       { some pointer definitions }
       PUnknown = ^IUnknown;
       PPUnknown = ^PUnknown;
       PDispatch = ^IDispatch;
       PPDispatch = ^PDispatch;

{$endif HASINTF}

       TExceptProc = Procedure (Obj : TObject; Addr,Frame: Pointer);

       { Exception object stack }
       PExceptObject = ^TExceptObject;
       TExceptObject = record
         FObject : TObject;
         Addr,
         Frame   : pointer;
         Next    : PExceptObject;
       end;

    Const
       ExceptProc : TExceptProc = Nil;
       RaiseProc : TExceptProc = Nil;

    Function RaiseList : PExceptObject;

{*****************************************************************************
                              Array of const support
*****************************************************************************}

   const
      vtInteger    = 0;
      vtBoolean    = 1;
      vtChar       = 2;
      vtExtended   = 3;
      vtString     = 4;
      vtPointer    = 5;
      vtPChar      = 6;
      vtObject     = 7;
      vtClass      = 8;
      vtWideChar   = 9;
      vtPWideChar  = 10;
      vtAnsiString = 11;
      vtCurrency   = 12;
      vtVariant    = 13;
      vtInterface  = 14;
      vtWideString = 15;
      vtInt64      = 16;
      vtQWord      = 17;

   type
      PVarRec = ^TVarRec;
      TVarRec = record
         case VType : Longint of
           vtInteger    : (VInteger: Longint);
           vtBoolean    : (VBoolean: Boolean);
           vtChar       : (VChar: Char);
           vtExtended   : (VExtended: PExtended);
           vtString     : (VString: PShortString);
           vtPointer    : (VPointer: Pointer);
           vtPChar      : (VPChar: PChar);
           vtObject     : (VObject: TObject);
           vtClass      : (VClass: TClass);
           vtWideChar   : (VWideChar: WideChar);
           vtPWideChar  : (VPWideChar: PWideChar);
           vtAnsiString : (VAnsiString: Pointer);
{$ifdef HASCURRENCY}
           vtCurrency   : (VCurrency: PCurrency);
{$endif HASCURRENCY}
{$ifdef HASVARIANT}
           vtVariant    : (VVariant: PVariant);
{$endif HASVARIANT}
           vtInterface  : (VInterface: Pointer);
           vtWideString : (VWideString: Pointer);
           vtInt64      : (VInt64: PInt64);
           vtQWord      : (VQWord: PQWord);
       end;

{
  $Log$
  Revision 1.15  2002-09-26 14:43:24  florian
    + tvarrec field vcurrency for compilers with hascurrency released

  Revision 1.14  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

}
