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
       { some pointer definitions }
       pshortstring = ^shortstring;
       plongstring  = ^longstring;
       pansistring  = ^ansistring;
       pwidestring  = ^widestring;
       // pstring   = pansistring;
       pextended    = ^extended;
       ppointer     = ^pointer;

       { now the let's declare the base classes for the class object }
       { model                                                       }
       tobject = class;
       tclass  = class of tobject;
       pclass  = ^tclass;


       { to access the message table from outside }
       tmsgstrtable = record
          name : pshortstring;
          method : pointer;
       end;

       pmsgstrtable = ^tmsgstrtable;

       tstringmessagetable = record
          count : dword;
          msgstrtable : array[0..0] of tmsgstrtable;
       end;

       pstringmessagetable = ^tstringmessagetable;

       pguid = ^tguid;
       tguid = packed record
         D1: LongWord;
         D2: Word;
         D3: Word;
         D4: array[0..7] of Byte;
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

       tobject = class
       public
          { please don't change the order of virtual methods, because      }
          { their vmt offsets are used by some assembler code which uses   }
          { hard coded addresses      (FK)                                 }
          constructor create;
          { the virtual procedures must be in THAT order }
          destructor destroy;virtual;
          class function newinstance : tobject;virtual;
          procedure freeinstance;virtual;
          function safecallexception(exceptobject : tobject;
            exceptaddr : pointer) : longint;virtual;
          procedure defaulthandler(var message);virtual;

          procedure free;
          class function initinstance(instance : pointer) : tobject;
          procedure cleanupinstance;
          function classtype : tclass;
          class function classinfo : pointer;
          class function classname : shortstring;
          class function classnameis(const name : string) : boolean;
          class function classparent : tclass;
          class function instancesize : longint;
          class function inheritsfrom(aclass : tclass) : boolean;
          class function stringmessagetable : pstringmessagetable;
          { message handling routines }
          procedure dispatch(var message);
          procedure dispatchstr(var message);

          class function methodaddress(const name : shortstring) : pointer;
          class function methodname(address : pointer) : shortstring;
          function fieldaddress(const name : shortstring) : pointer;

          { new since Delphi 4 }
          procedure AfterConstruction;virtual;
          procedure BeforeDestruction;virtual;

          { new for gtk, default handler for text based messages }
          procedure DefaultHandlerStr(var message);virtual;

{$ifndef ver1_0}
          { interface functions }
          function getinterface(const iid : tguid; out obj) : boolean;
          function getinterfacebystr(const iidstr : string; out obj) : boolean;
          class function getinterfaceentry(const iid : tguid) : pinterfaceentry;
          class function getinterfaceentrybystr(const iidstr : string) : pinterfaceentry;
          class function getinterfacetable : pinterfacetable;
{$endif ver1_0}
       end;

{$ifndef ver1_0}
       IUnknown = interface
         ['{00000000-0000-0000-C000-000000000046}']
         function QueryInterface(const iid: tguid; var {out} obj): LongInt; stdcall;
         function _AddRef: LongInt; stdcall;
         function _Release: LongInt; stdcall;
       end;

       { for native dispinterface support }
       IDispatch = interface(IUnknown)
         ['{00020400-0000-0000-C000-000000000046}']
         function GetTypeInfoCount({out}var count: LongInt): LongInt; stdcall;
         function GetTypeInfo(Index, LocaleID: LongInt;
           var {out} TypeInfo): LongInt; stdcall;
         function GetIDsOfNames(const iid: TGUID; names: Pointer;
           NameCount, LocaleID: LongInt; DispIDs: Pointer): LongInt; stdcall;
         function Invoke(DispID: LongInt; const iid: TGUID;
           LocaleID: LongInt; Flags: Word; var params;
           VarResult, ExcepInfo, ArgErr: Pointer): Integer; stdcall;
       end;
{$endif ver1_0}

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
                              Variant Type
*****************************************************************************}

    Const
       varEmpty     = $0000;
       varNull      = $0001;
       varSmallint  = $0002;
       varInteger   = $0003;
       varSingle    = $0004;
       varDouble    = $0005;
       varCurrency  = $0006;
       varDate      = $0007;
       varOleStr    = $0008;
       varDispatch  = $0009;
       varError     = $000A;
       varBoolean   = $000B;
       varVariant   = $000C;
       varUnknown   = $000D;
       varByte      = $0011;
       varString    = $0100;
       varAny       = $0101;
       varTypeMask  = $0FFF;
       varArray     = $2000;
       varByRef     = $4000;

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

    Type
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
//           vtWideChar   : (VWideChar: WideChar);
//           vtPWideChar  : (VPWideChar: PWideChar);
           vtAnsiString : (VAnsiString: Pointer);
//           vtCurrency   : (VCurrency: PCurrency);
//           vtVariant    : (VVariant: PVariant);
           vtInterface  : (VInterface: Pointer);
           vtWideString : (VWideString: Pointer);
           vtInt64      : (VInt64: PInt64);
           vtQWord      : (VQWord: PQWord);
       end;
{
  $Log$
  Revision 1.5  2000-11-04 16:28:55  florian
    + interfaces support

  Revision 1.4  2000/09/30 07:38:07  sg
  * Added 'RaiseProc': A user-definable callback procedure which gets
    called whenever an exception is being raised

  Revision 1.3  2000/07/14 10:33:10  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:45  michael
  + removed logs

}