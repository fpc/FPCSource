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
    { possible types for symtable entries }
    tsymtyp = (abstractsym,varsym,typesym,procsym,unitsym,programsym,
               constsym,enumsym,typedconstsym,errorsym,syssym,
               labelsym,absolutesym,propertysym,funcretsym,
               macrosym);

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

    PSymbolCollection       = ^TSymbolCollection;
    PSortedSymbolCollection = ^TSortedSymbolCollection;
    PReferenceCollection    = ^TReferenceCollection;

    PReference = ^TReference;
    TReference = object(TObject)
      FileName  : PString;
      Position  : TPoint;
      constructor Init(AFileName: PString; ALine, AColumn: Sw_integer);
      function    GetFileName: string;
      destructor  Done; virtual;
    end;

    PSymbolMemInfo = ^TSymbolMemInfo;
    TSymbolMemInfo = record
      Addr      : longint;
      LocalAddr : longint;
      Size      : longint;
      PushSize  : longint;
    end;

    PSymbol = ^TSymbol;
    TSymbol = object(TObject)
      Name       : PString;
      Typ        : tsymtyp;
      Params     : PString;
      References : PReferenceCollection;
      Items      : PSymbolCollection;
      DType      : PString;
      VType      : PString;
      ObjectID   : longint;
      AncestorID : longint;
      Ancestor   : PSymbol;
      Flags      : longint;
      MemInfo    : PSymbolMemInfo;
      constructor Init(const AName: string; ATyp: tsymtyp; AParams: string; AMemInfo: PSymbolMemInfo);
      procedure   SetMemInfo(const AMemInfo: TSymbolMemInfo);
      function    GetReferenceCount: Sw_integer;
      function    GetReference(Index: Sw_integer): PReference;
      function    GetItemCount: Sw_integer;
      function    GetItem(Index: Sw_integer): PSymbol;
      function    GetName: string;
      function    GetText: string;
      function    GetTypeName: string;
      destructor  Done; virtual;
    end;

    PObjectSymbolCollection = ^TObjectSymbolCollection;

    PObjectSymbol = ^TObjectSymbol;
    TObjectSymbol = object(TObject)
      Parent     : PObjectSymbol;
      Symbol     : PSymbol;
      Expanded   : boolean;
      constructor Init(AParent: PObjectSymbol; ASymbol: PSymbol);
      constructor InitName(const AName: string);
      function    GetName: string;
      function    GetDescendantCount: sw_integer;
      function    GetDescendant(Index: sw_integer): PObjectSymbol;
      procedure   AddDescendant(P: PObjectSymbol);
      destructor  Done; virtual;
    private
      Name: PString;
      Descendants: PObjectSymbolCollection;
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

    PIDSortedSymbolCollection = ^TIDSortedSymbolCollection;
    TIDSortedSymbolCollection = object(TSymbolCollection)
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      procedure Insert(Item: Pointer); virtual;
      function  SearchSymbolByID(AID: longint): PSymbol;
    end;

    TObjectSymbolCollection = object(TSortedCollection)
      function  Compare(Key1, Key2: Pointer): Sw_Integer; virtual;
      function  LookUp(const S: string; var Idx: sw_integer): string; virtual;
       function At(Index: Sw_Integer): PObjectSymbol;
    end;

    TReferenceCollection = object(TCollection)
       function At(Index: Sw_Integer): PReference;
    end;

    PSourceFile = ^TSourceFile;
    TSourceFile = object(TObject)
      SourceFileName: PString;
      ObjFileName: PString;
      PPUFileName: PString;
      constructor Init(ASourceFileName, AObjFileName, APPUFileName: string);
      destructor  Done; virtual;
      function    GetSourceFilename: string;
      function    GetObjFileName: string;
      function    GetPPUFileName: string;
    end;

    PSourceFileCollection = ^TSourceFileCollection;
    TSourceFileCollection = object(TCollection)
      function At(Index: sw_Integer): PSourceFile;
    end;

const
  Modules     : PSymbolCollection = nil;
  ModuleNames : PModuleNameCollection = nil;
  TypeNames   : PTypeNameCollection = nil;
  ObjectTree  : PObjectSymbol = nil;
  SourceFiles : PSourceFileCollection = nil;

function SearchObjectForSymbol(O: PSymbol): PObjectSymbol;

procedure InitBrowserCol;
procedure DoneBrowserCol;

function  LoadBrowserCol(S: PStream): boolean;
function  StoreBrowserCol(S: PStream): boolean;
procedure RegisterSymbols;

implementation


{****************************************************************************
                                   Helpers
****************************************************************************}

function GetStr(P: PString): string;
begin
  if P=nil then
    GetStr:=''
  else
    GetStr:=P^;
end;


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
                           TIDSortedSymbolCollection
****************************************************************************}

function TIDSortedSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
begin
  Compare:=0;
end;

procedure TIDSortedSymbolCollection.Insert(Item: Pointer);
begin
end;

function TIDSortedSymbolCollection.SearchSymbolByID(AID: longint): PSymbol;
begin
  SearchSymbolByID:=nil;
end;


{****************************************************************************
                           TObjectSymbolCollection
****************************************************************************}

function TObjectSymbolCollection.At(Index: Sw_Integer): PObjectSymbol;
begin
  At:=nil;
end;

function TObjectSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
begin
  Compare:=0;
end;

function TObjectSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
begin
  LookUp:='';
end;


{****************************************************************************
                                TReference
****************************************************************************}

constructor TReference.Init(AFileName: PString; ALine, AColumn: Sw_integer);
begin
end;

function TReference.GetFileName: string;
begin
  GetFileName:='';
end;

destructor TReference.Done;
begin
end;


{****************************************************************************
                                   TSymbol
****************************************************************************}

constructor TSymbol.Init(const AName: string; ATyp: tsymtyp; AParams: string; AMemInfo: PSymbolMemInfo);
begin
end;

procedure TSymbol.SetMemInfo(const AMemInfo: TSymbolMemInfo);
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

destructor TSymbol.Done;
begin
end;


{*****************************************************************************
                                 TObjectSymbol
*****************************************************************************}

constructor TObjectSymbol.Init(AParent: PObjectSymbol; ASymbol: PSymbol);
begin
end;

constructor TObjectSymbol.InitName(const AName: string);
begin
end;

function TObjectSymbol.GetName: string;
begin
  GetName:='';
end;

function TObjectSymbol.GetDescendantCount: sw_integer;
begin
  GetDescendantCount:=0;
end;

function TObjectSymbol.GetDescendant(Index: sw_integer): PObjectSymbol;
begin
  GetDescendant:=nil;
end;

procedure TObjectSymbol.AddDescendant(P: PObjectSymbol);
begin
end;

destructor TObjectSymbol.Done;
begin
end;

{****************************************************************************
                                TSourceFile
****************************************************************************}

constructor TSourceFile.Init(ASourceFileName, AObjFileName, APPUFileName: string);
begin
  inherited Init;
  SourceFileName:=NewStr(ASourceFileName);
  ObjFileName:=NewStr(AObjFileName);
  PPUFileName:=NewStr(APPUFileName);
end;

destructor TSourceFile.Done;
begin
  inherited Done;
  if assigned(SourceFileName) then DisposeStr(SourceFileName);
  if assigned(ObjFileName) then DisposeStr(ObjFileName);
  if assigned(PPUFileName) then DisposeStr(PPUFileName);
end;

function TSourceFile.GetSourceFilename: string;
begin
  GetSourceFilename:=GetStr(SourceFileName);
end;

function TSourceFile.GetObjFileName: string;
begin
  GetObjFilename:=GetStr(ObjFileName);
end;

function TSourceFile.GetPPUFileName: string;
begin
  GetPPUFilename:=GetStr(PPUFileName);
end;

function TSourceFileCollection.At(Index: sw_Integer): PSourceFile;
begin
  At:=inherited At(Index);
end;

{*****************************************************************************
                              Main Routines
*****************************************************************************}

procedure CreateBrowserCols;
begin
end;


function SearchObjectForSymbol(O: PSymbol): PObjectSymbol;
begin
  SearchObjectForSymbol:=nil;
end;


{*****************************************************************************
                                 Load/Store
*****************************************************************************}

function  LoadBrowserCol(S: PStream): boolean;
begin
  LoadBrowserCol:=true;
end;

function StoreBrowserCol(S: PStream): boolean;
begin
  StoreBrowserCol:=true;
end;

procedure RegisterSymbols;
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
  Revision 1.1  2000-07-13 09:48:33  michael
  + Initial import

  Revision 1.5  2000/01/27 23:42:33  peter
    * storebrowsercol returns boolean now

  Revision 1.4  1999/08/17 13:25:16  peter
    * updates with the compiler browcol

  Revision 1.3  1999/08/05 16:54:35  peter
    * win32 fixes

  Revision 1.2  1999/04/07 21:55:39  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.1  1999/01/28 19:56:12  peter
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
