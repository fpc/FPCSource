{
    $Id$

    Fake browcol unit

}
unit browcol;
interface
uses
  objects;

const
    SymbolTypLen : integer=6;

type
    TStoreCollection = object(TStringCollection)
      function Add(const S: string): PString;
    end;

    PModuleNameCollection = ^TModuleNameCollection;
    TModuleNameCollection = object(TStoreCollection)
    end;

    PTypeNameCollection = ^TTypeNameCollection;
    TTypeNameCollection = object(TStoreCollection)
    end;

    PSymbol = ^TSymbol;

    PReference = ^TReference;
    TReference = object(TObject)
      FileName  : PString;
      Position  : TPoint;
      function    GetFileName: string;
    end;

    PSymbolCollection = ^TSymbolCollection;
    PSortedSymbolCollection = ^TSortedSymbolCollection;
    PReferenceCollection = ^TReferenceCollection;

    TSymbol = object(TObject)
      Name       : PString;
      ParamCount : Sw_integer;
      Params     : PPointerArray;
      References : PReferenceCollection;
      Items      : PSymbolCollection;
      procedure   SetParams(AParamCount: Sw_integer; AParams: PPointerArray);
      function    GetReferenceCount: Sw_integer;
      function    GetReference(Index: Sw_integer): PReference;
      function    GetItemCount: Sw_integer;
      function    GetItem(Index: Sw_integer): PSymbol;
      function    GetName: string;
      function    GetText: string;
      function    GetTypeName: string;
    end;

    TSymbolCollection = object(TSortedCollection)
       function  At(Index: Sw_Integer): PSymbol;
       procedure Insert(Item: Pointer); virtual;
       function  LookUp(const S: string; var Idx: sw_integer): string; virtual;
    end;

    TSortedSymbolCollection = object(TSymbolCollection)
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      procedure Insert(Item: Pointer); virtual;
      function  LookUp(const S: string; var Idx: sw_integer): string; virtual;
    end;

    TReferenceCollection = object(TCollection)
       function At(Index: Sw_Integer): PReference;
    end;

const
  Modules     : PSymbolCollection = nil;
  ModuleNames : PModuleNameCollection = nil;
  TypeNames   : PTypeNameCollection = nil;

procedure InitBrowserCol;
procedure DoneBrowserCol;


implementation

{****************************************************************************
                                TStoreCollection
****************************************************************************}

function TStoreCollection.Add(const S: string): PString;
begin
  Add:=nil;
end;


{****************************************************************************
                                TSymbolCollection
****************************************************************************}

function TSymbolCollection.At(Index: Sw_integer): PSymbol;
begin
  At:=nil;
end;

procedure TSymbolCollection.Insert(Item: Pointer);
begin
end;

function TSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
begin
  Idx:=-1;
  LookUp:='';
end;


{****************************************************************************
                               TReferenceCollection
****************************************************************************}

function TReferenceCollection.At(Index: Sw_integer): PReference;
begin
  At:=nil;
end;


{****************************************************************************
                            TSortedSymbolCollection
****************************************************************************}

function TSortedSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
begin
  Compare:=0;
end;

procedure TSortedSymbolCollection.Insert(Item: Pointer);
begin
end;

function TSortedSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
begin
  Idx:=-1;
  LookUp:='';
end;


{****************************************************************************
                                TReference
****************************************************************************}

function TReference.GetFileName: string;
begin
  GetFileName:='';
end;


{****************************************************************************
                                   TSymbol
****************************************************************************}

procedure TSymbol.SetParams(AParamCount: Sw_integer; AParams: PPointerArray);
begin
end;

function TSymbol.GetReferenceCount: Sw_integer;
begin
  GetReferenceCount:=0;
end;

function TSymbol.GetReference(Index: Sw_integer): PReference;
begin
  GetReference:=nil;
end;

function TSymbol.GetItemCount: Sw_integer;
begin
  GetItemCount:=0;
end;

function TSymbol.GetItem(Index: Sw_integer): PSymbol;
begin
  GetItem:=nil;
end;

function TSymbol.GetName: string;
begin
  GetName:='';
end;

function TSymbol.GetText: string;
begin
  GetText:='';
end;

function TSymbol.GetTypeName: string;
begin
  GetTypeName:='';
end;

procedure CreateBrowserCols;
begin
end;


{*****************************************************************************
                                 Initialize
*****************************************************************************}

var
  oldexit : pointer;

procedure browcol_exit;{$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
  if assigned(Modules) then
   begin
     dispose(Modules,Done);
     Modules:=nil;
   end;
  if assigned(ModuleNames) then
   begin
     dispose(ModuleNames,Done);
     ModuleNames:=nil;
   end;
  if assigned(TypeNames) then
   begin
     dispose(TypeNames,Done);
     TypeNames:=nil;
   end;
end;


procedure InitBrowserCol;
begin
  New(Modules, Init(1,1));
  New(ModuleNames, Init(1,1));
  New(TypeNames, Init(1,1));
end;


procedure DoneBrowserCol;
begin
  CreateBrowserCols;
end;


begin
  oldexit:=exitproc;
  exitproc:=@browcol_exit;
end.
{
  $Log$
  Revision 1.1  1999-01-28 19:56:12  peter
    * moved to include compiler/gdb independent of each other

  Revision 1.3  1999/01/22 10:24:16  peter
    + gdbcon fake unit

  Revision 1.2  1999/01/21 11:54:08  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.1  1999/01/12 15:00:46  peter
    * fake unit

}
