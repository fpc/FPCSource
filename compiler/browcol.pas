{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Pierre Muller

    Support routines for getting browser info in collections

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$ifdef TP}
  {$N+,E+}
{$endif}
unit browcol;
interface
uses
  cobjects,objects,symconst,symtable;

{$ifndef FPC}
  type
    sw_integer = integer;
{$endif FPC}

const
  SymbolTypLen : integer = 6;

  RecordTypes : set of tsymtyp =
    ([typesym,unitsym,programsym]);

    sfRecord        = $00000001;
    sfObject        = $00000002;
    sfClass         = $00000004;
    sfHasMemInfo    = $80000000;

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
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
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
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
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
      constructor Load(var S: TStream);
      procedure   Store(S: TStream);
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

procedure DisposeBrowserCol;
procedure NewBrowserCol;
procedure CreateBrowserCol;
procedure InitBrowserCol;
procedure DoneBrowserCol;

function  LoadBrowserCol(S: PStream): boolean;
procedure StoreBrowserCol(S: PStream);

procedure BuildObjectInfo;

procedure BuildSourceList;

function SearchObjectForSymbol(O: PSymbol): PObjectSymbol;

procedure RegisterSymbols;

implementation

uses
  Dos,Drivers,{Views,App,}
  aasm,globtype,globals,files,comphook;

const
  RModuleNameCollection: TStreamRec = (
     ObjType: 3001;
     VmtLink: Ofs(TypeOf(TModuleNameCollection)^);
     Load:    @TModuleNameCollection.Load;
     Store:   @TModuleNameCollection.Store
  );
  RTypeNameCollection: TStreamRec = (
     ObjType: 3002;
     VmtLink: Ofs(TypeOf(TTypeNameCollection)^);
     Load:    @TTypeNameCollection.Load;
     Store:   @TTypeNameCollection.Store
  );
  RReference: TStreamRec = (
     ObjType: 3003;
     VmtLink: Ofs(TypeOf(TReference)^);
     Load:    @TReference.Load;
     Store:   @TReference.Store
  );
  RSymbol: TStreamRec = (
     ObjType: 3004;
     VmtLink: Ofs(TypeOf(TSymbol)^);
     Load:    @TSymbol.Load;
     Store:   @TSymbol.Store
  );
  RObjectSymbol: TStreamRec = (
     ObjType: 3005;
     VmtLink: Ofs(TypeOf(TObjectSymbol)^);
     Load:    @TObjectSymbol.Load;
     Store:   @TObjectSymbol.Store
  );
  RSymbolCollection: TStreamRec = (
     ObjType: 3006;
     VmtLink: Ofs(TypeOf(TSymbolCollection)^);
     Load:    @TSymbolCollection.Load;
     Store:   @TSymbolCollection.Store
  );
  RSortedSymbolCollection: TStreamRec = (
     ObjType: 3007;
     VmtLink: Ofs(TypeOf(TSortedSymbolCollection)^);
     Load:    @TSortedSymbolCollection.Load;
     Store:   @TSortedSymbolCollection.Store
  );
  RIDSortedSymbolCollection: TStreamRec = (
     ObjType: 3008;
     VmtLink: Ofs(TypeOf(TIDSortedSymbolCollection)^);
     Load:    @TIDSortedSymbolCollection.Load;
     Store:   @TIDSortedSymbolCollection.Store
  );
  RObjectSymbolCollection: TStreamRec = (
     ObjType: 3009;
     VmtLink: Ofs(TypeOf(TObjectSymbolCollection)^);
     Load:    @TObjectSymbolCollection.Load;
     Store:   @TObjectSymbolCollection.Store
  );
  RReferenceCollection: TStreamRec = (
     ObjType: 3010;
     VmtLink: Ofs(TypeOf(TReferenceCollection)^);
     Load:    @TReferenceCollection.Load;
     Store:   @TReferenceCollection.Store
  );

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

function IntToStr(L: longint): string;
var S: string;
begin
  Str(L,S);
  IntToStr:=S;
end;

function UpcaseStr(S: string): string;
var I: integer;
begin
  for I:=1 to length(S) do
      S[I]:=Upcase(S[I]);
  UpcaseStr:=S;
end;

function FloatToStr(E: extended): string;
var S: string;
begin
  Str(E:0:24,S);
  if Pos('.',S)>0 then
    begin
      while (length(S)>0) and (S[length(S)]='0') do
        Delete(S,length(S),1);
      if (length(S)>0) and (S[length(S)]='.') then
        Delete(S,length(S),1);
    end;
  if S='' then S:='0';
  FloatToStr:=S;
end;

{****************************************************************************
                                TStoreCollection
****************************************************************************}

function TStoreCollection.Add(const S: string): PString;
var P: PString;
    Index: Sw_integer;
begin
  if S='' then P:=nil else
  if Search(@S,Index) then P:=At(Index) else
    begin
      P:=NewStr(S);
      Insert(P);
    end;
  Add:=P;
end;


{****************************************************************************
                                TSymbolCollection
****************************************************************************}

function TSymbolCollection.At(Index: Sw_Integer): PSymbol;
begin
  At:=inherited At(Index);
end;

procedure TSymbolCollection.Insert(Item: Pointer);
begin
  TCollection.Insert(Item);
end;

function TSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
begin
  Idx:=-1;
  LookUp:='';
end;

{****************************************************************************
                               TReferenceCollection
****************************************************************************}

function TReferenceCollection.At(Index: Sw_Integer): PReference;
begin
  At:=inherited At(Index);
end;


{****************************************************************************
                            TSortedSymbolCollection
****************************************************************************}

function TSortedSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PSymbol absolute Key1;
    K2: PSymbol absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=Upper(K1^.GetName);
  S2:=Upper(K2^.GetName);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
  R:=0;
  Compare:=R;
end;

procedure TSortedSymbolCollection.Insert(Item: Pointer);
begin
  TSortedCollection.Insert(Item);
end;

function TSortedSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
var OLI,ORI,Left,Right,Mid: integer;
    LeftP,RightP,MidP: PSymbol;
    RL: integer;
    LeftS,MidS,RightS: string;
    FoundS: string;
    UpS : string;
begin
  Idx:=-1; FoundS:='';
  Left:=0; Right:=Count-1;
  UpS:=Upper(S);
  if Left<Right then
  begin
    while (Left<Right) do
    begin
      OLI:=Left; ORI:=Right;
      Mid:=Left+(Right-Left) div 2;
      LeftP:=At(Left); RightP:=At(Right); MidP:=At(Mid);
      LeftS:=Upper(LeftP^.GetName); MidS:=Upper(MidP^.GetName);
      RightS:=Upper(RightP^.GetName);
      if copy(MidS,1,length(UpS))=UpS then
        begin
          Idx:=Mid;
          FoundS:=MidS;
        end;
{      else}
        if UpS<MidS then
          Right:=Mid
        else
          Left:=Mid;
      if (OLI=Left) and (ORI=Right) then
        Break;
    end;
  end;
  LookUp:=FoundS;
end;

{****************************************************************************
                           TIDSortedSymbolCollection
****************************************************************************}

function TIDSortedSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PSymbol absolute Key1;
    K2: PSymbol absolute Key2;
    R: Sw_integer;
begin
  if K1^.ObjectID<K2^.ObjectID then R:=-1 else
  if K1^.ObjectID>K2^.ObjectID then R:=1 else
  R:=0;
  Compare:=R;
end;

procedure TIDSortedSymbolCollection.Insert(Item: Pointer);
begin
  TSortedCollection.Insert(Item);
end;

function TIDSortedSymbolCollection.SearchSymbolByID(AID: longint): PSymbol;
var S: TSymbol;
    Index: sw_integer;
    P: PSymbol;
begin
  S.ObjectID:=AID;
  if Search(@S,Index)=false then P:=nil else
    P:=At(Index);
  SearchSymbolByID:=P;
end;

{****************************************************************************
                           TObjectSymbolCollection
****************************************************************************}

function TObjectSymbolCollection.At(Index: Sw_Integer): PObjectSymbol;
begin
  At:=inherited At(Index);
end;

function TObjectSymbolCollection.Compare(Key1, Key2: Pointer): Sw_Integer;
var K1: PObjectSymbol absolute Key1;
    K2: PObjectSymbol absolute Key2;
    R: Sw_integer;
    S1,S2: string;
begin
  S1:=Upper(K1^.GetName);
  S2:=Upper(K2^.GetName);
  if S1<S2 then R:=-1 else
  if S1>S2 then R:=1 else
  R:=0;
  Compare:=R;
end;

function TObjectSymbolCollection.LookUp(const S: string; var Idx: sw_integer): string;
var OLI,ORI,Left,Right,Mid: integer;
    LeftP,RightP,MidP: PObjectSymbol;
    RL: integer;
    LeftS,MidS,RightS: string;
    FoundS: string;
    UpS : string;
begin
  Idx:=-1; FoundS:='';
  Left:=0; Right:=Count-1;
  UpS:=Upper(S);
  if Left<Right then
  begin
    while (Left<Right) do
    begin
      OLI:=Left; ORI:=Right;
      Mid:=Left+(Right-Left) div 2;
      LeftP:=At(Left); RightP:=At(Right); MidP:=At(Mid);
      LeftS:=Upper(LeftP^.GetName); MidS:=Upper(MidP^.GetName);
      RightS:=Upper(RightP^.GetName);
      if copy(MidS,1,length(UpS))=UpS then
        begin
          Idx:=Mid;
          FoundS:=MidS;
        end;
{      else}
        if UpS<MidS then
          Right:=Mid
        else
          Left:=Mid;
      if (OLI=Left) and (ORI=Right) then
        Break;
    end;
  end;
  LookUp:=FoundS;
end;

{****************************************************************************
                                TReference
****************************************************************************}

constructor TReference.Init(AFileName: PString; ALine, AColumn: Sw_integer);
begin
  inherited Init;
  FileName:=AFileName;
  Position.X:=AColumn;
  Position.Y:=ALine;
end;

function TReference.GetFileName: string;
begin
  GetFileName:=GetStr(FileName);
end;

destructor TReference.Done;
begin
  inherited Done;
end;

constructor TReference.Load(var S: TStream);
begin
  S.Read(Position, SizeOf(Position));

  { --- items needing fixup --- }
  S.Read(FileName, SizeOf(FileName)); { ->ModulesNames^.Item }
end;

procedure TReference.Store(var S: TStream);
begin
  S.Write(Position, SizeOf(Position));

  { --- items needing fixup --- }
  S.Write(FileName, SizeOf(FileName));
end;

{****************************************************************************
                                   TSymbol
****************************************************************************}

constructor TSymbol.Init(const AName: string; ATyp: tsymtyp; AParams: string; AMemInfo: PSymbolMemInfo);
begin
  inherited Init;
  Name:=NewStr(AName); Typ:=ATyp;
  if AMemInfo<>nil then
    SetMemInfo(AMemInfo^);
  New(References, Init(20,50));
  if ATyp in RecordTypes then
    begin
      Items:=New(PSortedSymbolCollection, Init(50,100));
    end;
end;

procedure TSymbol.SetMemInfo(const AMemInfo: TSymbolMemInfo);
begin
  if MemInfo=nil then New(MemInfo);
  Move(AMemInfo,MemInfo^,SizeOf(MemInfo^));
  Flags:=Flags or sfHasMemInfo;
end;

function TSymbol.GetReferenceCount: Sw_integer;
var Count: Sw_integer;
begin
  if References=nil then Count:=0 else
    Count:=References^.Count;
  GetReferenceCount:=Count;
end;

function TSymbol.GetReference(Index: Sw_integer): PReference;
begin
  GetReference:=References^.At(Index);
end;

function TSymbol.GetItemCount: Sw_integer;
var Count: Sw_integer;
begin
  if Items=nil then Count:=0 else
    Count:=Items^.Count;
  GetItemCount:=Count;
end;

function TSymbol.GetItem(Index: Sw_integer): PSymbol;
begin
  GetItem:=Items^.At(Index);
end;

function TSymbol.GetName: string;
begin
  GetName:=GetStr(Name);
end;

function TSymbol.GetText: string;
var S: string;
    I: Sw_integer;
begin
  S:=GetTypeName;
  if length(S)>SymbolTypLen then
   S:=Copy(S,1,SymbolTypLen)
  else
   begin
     while length(S)<SymbolTypLen do
      S:=S+' ';
   end;
  S:=S+' '+GetName;
  if (Flags and sfRecord)<>0 then
    S:=S+' = record'
  else
  if (Flags and sfObject)<>0 then
    begin
      S:=S+' = ';
      if (Flags and sfClass)<>0 then
        S:=S+'class'
      else
        S:=S+'object';
      if Ancestor<>nil then
        S:=S+'('+Ancestor^.GetName+')';
    end
  else
    begin
      if Assigned(DType) then
        S:=S+' = '+DType^;
      if Assigned(Params) then
        S:=S+'('+Params^+')';
      if Assigned(VType) then
        S:=S+': '+VType^;
    end;
  GetText:=S;
end;

function TSymbol.GetTypeName: string;
var S: string;
begin
  case Typ of
    abstractsym  : S:='abst';
    varsym       : S:='var';
    typesym      : S:='type';
    procsym      : if VType=nil then
                     S:='proc'
                   else
                     S:='func';
    unitsym      : S:='unit';
    programsym   : S:='prog';
    constsym     : S:='const';
    enumsym      : S:='enum';
    typedconstsym: S:='const';
    errorsym     : S:='error';
    syssym       : S:='sys';
    labelsym     : S:='label';
    absolutesym  : S:='abs';
    propertysym  : S:='prop';
    funcretsym   : S:='res';
    macrosym     : S:='macro';
  else S:='';
  end;
  GetTypeName:=S;
end;

destructor TSymbol.Done;
begin
  inherited Done;
  if assigned(MemInfo) then
    Dispose(MemInfo);
  if assigned(References) then
    Dispose(References, Done);
  if assigned(Items) then
    Dispose(Items, Done);
  if assigned(Name) then
    DisposeStr(Name);
{  if assigned(Params) then
    DisposeStr(Params);
  if assigned(VType) then
    DisposeStr(VType);
  if assigned(DType) then
    DisposeStr(DType);
  if assigned(Ancestor) then
    DisposeStr(Ancestor);}
end;

constructor TSymbol.Load(var S: TStream);
var MI: TSymbolMemInfo;
    W: word;
begin
  TObject.Init;

  S.Read(Typ,SizeOf(Typ));
  S.Read(ObjectID, SizeOf(ObjectID));
  S.Read(AncestorID, SizeOf(AncestorID));
  S.Read(Flags, SizeOf(Flags));
  Name:=S.ReadStr;
  Params:=S.ReadStr;
  if (Flags and sfHasMemInfo)<>0 then
    begin
      S.Read(MI,SizeOf(MI));
      SetMemInfo(MI);
    end;

  W:=0;
  S.Read(W,SizeOf(W));
  if (W and 1)<>0 then
    New(References, Load(S));
  if (W and 2)<>0 then
    New(Items, Load(S));

  { --- items needing fixup --- }
  S.Read(DType, SizeOf(DType));
  S.Read(VType, SizeOf(VType));
  S.Read(Ancestor, SizeOf(Ancestor));
end;

procedure TSymbol.Store(var S: TStream);
var W: word;
begin
  S.Write(Typ,SizeOf(Typ));
  S.Write(ObjectID, SizeOf(ObjectID));
  S.Write(AncestorID, SizeOf(AncestorID));
  S.Write(Flags, SizeOf(Flags));
  S.WriteStr(Name);
  S.WriteStr(Params);

  if (Flags and sfHasMemInfo)<>0 then
    S.Write(MemInfo^,SizeOf(MemInfo^));

  W:=0;
  if Assigned(References) then W:=W or 1;
  if Assigned(Items) then W:=W or 2;
  S.Write(W,SizeOf(W));
  if Assigned(References) then References^.Store(S);
  if Assigned(Items) then Items^.Store(S);

  { --- items needing fixup --- }
  S.Write(DType, SizeOf(DType));
  S.Write(VType, SizeOf(VType));
  S.Write(Ancestor, SizeOf(Ancestor));
end;


constructor TObjectSymbol.Init(AParent: PObjectSymbol; ASymbol: PSymbol);
begin
  inherited Init;
  Parent:=AParent;
  Symbol:=ASymbol;
end;

constructor TObjectSymbol.InitName(const AName: string);
begin
  inherited Init;
  Name:=NewStr(AName);
end;

function TObjectSymbol.GetName: string;
begin
  if Name<>nil then
    GetName:=Name^
  else
    GetName:=Symbol^.GetName;
end;

function TObjectSymbol.GetDescendantCount: sw_integer;
var Count: sw_integer;
begin
  if Descendants=nil then Count:=0 else
    Count:=Descendants^.Count;
  GetDescendantCount:=Count;
end;

function TObjectSymbol.GetDescendant(Index: sw_integer): PObjectSymbol;
begin
  GetDescendant:=Descendants^.At(Index);
end;

procedure TObjectSymbol.AddDescendant(P: PObjectSymbol);
begin
  if Descendants=nil then
    New(Descendants, Init(50,10));
  Descendants^.Insert(P);
end;

destructor TObjectSymbol.Done;
begin
  if Assigned(Name) then DisposeStr(Name); Name:=nil;
  if Assigned(Descendants) then Dispose(Descendants, Done); Descendants:=nil;
  inherited Done;
end;

constructor TObjectSymbol.Load(var S: TStream);
begin
end;

procedure TObjectSymbol.Store(S: TStream);
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

procedure DisposeBrowserCol;
begin
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
  if assigned(ObjectTree) then
    begin
      Dispose(ObjectTree, Done);
      ObjectTree:=nil;
    end;
end;


procedure NewBrowserCol;
begin
  New(Modules, Init(50,50));
  New(ModuleNames, Init(50,50));
  New(TypeNames, Init(1000,5000));
end;


  procedure ProcessSymTable(OwnerSym: PSymbol; var Owner: PSymbolCollection; Table: PSymTable);
  var I,J,defcount,symcount: longint;
      Ref: PRef;
      Sym,ParSym: PSym;
      Symbol: PSymbol;
      Reference: PReference;
      ParamCount: Sw_integer;
      Params: array[0..20] of PString;
      inputfile : pinputfile;
      Idx: sw_integer;
      S: string;
  procedure SetVType(Symbol: PSymbol; VType: string);
  begin
    Symbol^.VType:=TypeNames^.Add(VType);
  end;
  procedure SetDType(Symbol: PSymbol; DType: string);
  begin
    Symbol^.DType:=TypeNames^.Add(DType);
  end;
  function GetDefinitionStr(def: pdef): string; forward;
  function GetEnumDefStr(def: penumdef): string;
  var Name: string;
      esym: penumsym;
      Count: integer;
  begin
    Name:='(';
    esym:=def^.Firstenum; Count:=0;
    while (esym<>nil) do
      begin
        if Count>0 then
          Name:=Name+', ';
        Name:=Name+esym^.name;
        esym:=esym^.nextenum;
        Inc(Count);
      end;
    Name:=Name+')';
    GetEnumDefStr:=Name;
  end;
  function GetArrayDefStr(def: parraydef): string;
  var Name: string;
  begin
    Name:='array ['+IntToStr(def^.lowrange)+'..'+IntToStr(def^.highrange)+'] of ';
    if assigned(def^.elementtype.def) then
      Name:=Name+GetDefinitionStr(def^.elementtype.def);
    GetArrayDefStr:=Name;
  end;
  function GetFileDefStr(def: pfiledef): string;
  var Name: string;
  begin
    Name:='';
    case def^.filetyp of
      ft_text    : Name:='text';
      ft_untyped : Name:='file';
      ft_typed   : Name:='file of '+GetDefinitionStr(def^.typedfiletype.def);
    end;
    GetFileDefStr:=Name;
  end;
  function GetStringDefStr(def: pstringdef): string;
  var Name: string;
  begin
    Name:='';
    case def^.string_typ of
      st_shortstring :
        if def^.len=255 then
          Name:='shortstring'
        else
          Name:='string['+IntToStr(def^.len)+']';
      st_longstring :
        Name:='longstring';
      st_ansistring :
        Name:='ansistring';
      st_widestring :
        Name:='widestring';
    else ;
    end;
    GetStringDefStr:=Name;
  end;
  function retdefassigned(def: pabstractprocdef): boolean;
  var OK: boolean;
  begin
    OK:=false;
    if assigned(def^.rettype.def) then
      if UpcaseStr(GetDefinitionStr(def^.rettype.def))<>'VOID' then
        OK:=true;
    retdefassigned:=OK;
  end;
  function GetAbsProcParmDefStr(def: pabstractprocdef): string;
  var Name: string;
      dc: pparaitem;
      Count: integer;
      CurName: string;
  begin
    Name:='';
    dc:=pparaitem(def^.para^.first);
    Count:=0;
    while assigned(dc) do
     begin
       CurName:='';
       case dc^.paratyp of
         vs_Value : ;
         vs_Const : CurName:=CurName+'const ';
         vs_Var   : CurName:=CurName+'var ';
       end;
       if assigned(dc^.paratype.def) then
         CurName:=CurName+GetDefinitionStr(dc^.paratype.def);
       if dc^.next<>nil then
         CurName:=', '+CurName;
       Name:=CurName+Name;
       dc:=pparaitem(dc^.next);
       Inc(Count);
     end;
    GetAbsProcParmDefStr:=Name;
  end;
  function GetAbsProcDefStr(def: pabstractprocdef): string;
  var Name: string;
  begin
    Name:=GetAbsProcParmDefStr(def);
    if Name<>'' then Name:='('+Name+')';
    if retdefassigned(def) then
      Name:='function'+Name+': '+GetDefinitionStr(def^.rettype.def)
    else
      Name:='procedure'+Name;
    GetAbsProcDefStr:=Name;
  end;
  function GetProcDefStr(def: pprocdef): string;
  var DName: string;
      J: integer;
  begin
{    DName:='';
    if assigned(def) then
    begin
      if assigned(def^.parast) then
        begin
          with def^.parast^ do
          for J:=1 to number_symbols do
            begin
              if J<>1 then DName:=DName+', ';
              ParSym:=GetsymNr(J);
              if ParSym=nil then Break;
              DName:=DName+ParSym^.Name;
            end;
        end
    end;}
    DName:=GetAbsProcDefStr(def);
    GetProcDefStr:=DName;
  end;
  function GetProcVarDefStr(def: pprocvardef): string;
  begin
    GetProcVarDefStr:=GetAbsProcDefStr(def);
  end;
  function GetSetDefStr(def: psetdef): string;
  var Name: string;
  begin
    Name:='';
    case def^.settype of
      normset  : Name:='set';
      smallset : Name:='set';
      varset   : Name:='varset';
    end;
    Name:=Name+' of ';
    Name:=Name+GetDefinitionStr(def^.elementtype.def);
    GetSetDefStr:=Name;
  end;
  function GetDefinitionStr(def: pdef): string;
  var Name: string;
      sym: psym;
  begin
    Name:='';
    if def<>nil then
    begin
      if assigned(def^.typesym) then
        Name:=def^.typesym^.name;
      if Name='' then
      case def^.deftype of
        arraydef :
          Name:=GetArrayDefStr(parraydef(def));
        stringdef :
          Name:=GetStringDefStr(pstringdef(def));
        enumdef :
          Name:=GetEnumDefStr(penumdef(def));
        procdef :
          Name:=GetProcDefStr(pprocdef(def));
        procvardef :
          Name:=GetProcVarDefStr(pprocvardef(def));
        filedef :
          Name:=GetFileDefStr(pfiledef(def));
        setdef :
          Name:=GetSetDefStr(psetdef(def));
      end;
    end;
    GetDefinitionStr:=Name;
  end;
  function GetEnumItemName(Sym: penumsym): string;
  var Name: string;
      ES: penumsym;
  begin
    Name:='';
    if assigned(sym) and assigned(sym^.definition) then
      if assigned(sym^.definition^.typesym) then
      begin
{        ES:=sym^.definition^.First;
        while (ES<>nil) and (ES^.Value<>sym^.Value) do
          ES:=ES^.next;
        if assigned(es) and (es^.value=sym^.value) then
          Name:=}
        Name:=sym^.definition^.typesym^.name;
        if Name<>'' then
          Name:=Name+'('+IntToStr(sym^.value)+')';
      end;
    GetEnumItemName:=Name;
  end;
  function GetConstValueName(sym: pconstsym): string;
  var Name: string;
  begin
    Name:='';
{    if assigned(sym^.definition) then
     if assigned(sym^.definition^.sym) then
       Name:=sym^.definition^.sym^.name;}
    if Name='' then
    case sym^.consttyp of
      constord :
        Name:=sym^.consttype.def^.typesym^.name+'('+IntToStr(sym^.value)+')';
      constresourcestring,
      conststring :
        Name:=''''+GetStr(PString(sym^.Value))+'''';
      constreal:
        Name:=FloatToStr(PBestReal(sym^.Value)^);
      constbool:
{        if boolean(sym^.Value)=true then
          Name:='TRUE'
        else
          Name:='FALSE';}
        Name:='Longbool('+IntToStr(sym^.Value)+')';
      constint:
        Name:=IntToStr(sym^.value);
      constchar:
        Name:=''''+chr(sym^.Value)+'''';
      constset:
{        Name:=SetToStr(pnormalset(sym^.Value))};
      constnil: ;
    end;
    GetConstValueName:=Name;
  end;
  procedure ProcessDefIfStruct(definition: pdef);
  begin
    { still led to infinite recursions
      only usefull for unamed types PM }
    if assigned(definition) and not assigned(definition^.typesym) then
    begin
      case definition^.deftype of
        recorddef :
          if precorddef(definition)^.symtable<>Table then
            ProcessSymTable(Symbol,Symbol^.Items,precorddef(definition)^.symtable);
        objectdef :
          if pobjectdef(definition)^.symtable<>Table then
            ProcessSymTable(Symbol,Symbol^.Items,pobjectdef(definition)^.symtable);
        { leads to infinite loops !!
        pointerdef :
          with ppointerdef(definition)^ do
            if assigned(definition) then
              if assigned(definition^.sym) then
                ProcessDefIfStruct(definition^.sym^.definition);}
      end;
    end;
  end;
  var MemInfo: TSymbolMemInfo;
      ObjDef: pobjectdef;
  begin
    if not Assigned(Table) then
     Exit;
    if Owner=nil then
     Owner:=New(PSortedSymbolCollection, Init(10,50));
    sym:=psym(Table^.symindex^.first);
    while assigned(sym) do
      begin
        ParamCount:=0;
        New(Symbol, Init(Sym^.Name,Sym^.Typ,'',nil));
        case Sym^.Typ of
          varsym :
             with pvarsym(sym)^ do
             begin
               if assigned(vartype.def) then
                 if assigned(vartype.def^.typesym) then
                   SetVType(Symbol,vartype.def^.typesym^.name)
                 else
                   SetVType(Symbol,GetDefinitionStr(vartype.def));
               ProcessDefIfStruct(vartype.def);
               MemInfo.Addr:=address;
               if assigned(localvarsym) then
                 MemInfo.LocalAddr:=localvarsym^.address
               else
                 MemInfo.LocalAddr:=0;
               MemInfo.Size:=getsize;
               MemInfo.PushSize:=getpushsize;
               Symbol^.SetMemInfo(MemInfo);
             end;
          constsym :
             SetDType(Symbol,GetConstValueName(pconstsym(sym)));
          enumsym :
            if assigned(penumsym(sym)^.definition) then
             SetDType(Symbol,GetEnumItemName(penumsym(sym)));
          unitsym :
            begin
  {            ProcessSymTable(Symbol^.Items,punitsym(sym)^.unitsymtable);}
            end;
          syssym :
{            if assigned(Table^.Name) then
            if Table^.Name^='SYSTEM' then}
              begin
                Symbol^.Params:=TypeNames^.Add('...');
              end;
          funcretsym :
            if Assigned(OwnerSym) then
            with pfuncretsym(sym)^ do
              if assigned(rettype.def) then
                if assigned(rettype.def^.typesym) then
                   SetVType(OwnerSym,rettype.def^.typesym^.name);
          procsym :
            begin
              with pprocsym(sym)^ do
              if assigned(definition) then
              begin
                if cs_local_browser in aktmoduleswitches then
                  ProcessSymTable(Symbol,Symbol^.Items,definition^.parast);
                if assigned(definition^.parast) then
                  begin
                    Symbol^.Params:=TypeNames^.Add(GetAbsProcParmDefStr(definition));
                  end
                else { param-definition is NOT assigned }
                  if assigned(Table^.Name) then
                  if Table^.Name^='SYSTEM' then
                  begin
                    Symbol^.Params:=TypeNames^.Add('...');
                  end;
                if cs_local_browser in aktmoduleswitches then
                 begin
                   if assigned(definition^.localst) and
                     (definition^.localst^.symtabletype<>staticsymtable) then
                    ProcessSymTable(Symbol,Symbol^.Items,definition^.localst);
                 end;
              end;
            end;
          typesym :
            begin
            with ptypesym(sym)^ do
              if assigned(restype.def) then
                case restype.def^.deftype of
                  arraydef :
                    SetDType(Symbol,GetArrayDefStr(parraydef(restype.def)));
                  enumdef :
                    SetDType(Symbol,GetEnumDefStr(penumdef(restype.def)));
                  procdef :
                    SetDType(Symbol,GetProcDefStr(pprocdef(restype.def)));
                  procvardef :
                    SetDType(Symbol,GetProcVarDefStr(pprocvardef(restype.def)));
                  objectdef :
                    with pobjectdef(restype.def)^ do
                    begin
                      ObjDef:=childof;
                      Symbol^.ObjectID:=longint(restype.def);
                      if ObjDef<>nil then
                        Symbol^.AncestorID:=longint(ObjDef);{TypeNames^.Add(S);}
                      Symbol^.Flags:=(Symbol^.Flags or sfObject);
                      if is_class then
                        Symbol^.Flags:=(Symbol^.Flags or sfClass);
                      ProcessSymTable(Symbol,Symbol^.Items,pobjectdef(restype.def)^.symtable);
                    end;
                  recorddef :
                    begin
                      Symbol^.Flags:=(Symbol^.Flags or sfRecord);
                      ProcessSymTable(Symbol,Symbol^.Items,precorddef(restype.def)^.symtable);
                    end;
                  filedef :
                    SetDType(Symbol,GetFileDefStr(pfiledef(restype.def)));
                  setdef :
                    SetDType(Symbol,GetSetDefStr(psetdef(restype.def)));
                end;
            end;
        end;
        Ref:=Sym^.defref;
        while Assigned(Symbol) and assigned(Ref) do
          begin
            inputfile:=get_source_file(ref^.moduleindex,ref^.posinfo.fileindex);
            if Assigned(inputfile) and Assigned(inputfile^.name) then
              begin
                New(Reference, Init(ModuleNames^.Add(inputfile^.name^),
                  ref^.posinfo.line,ref^.posinfo.column));
                Symbol^.References^.Insert(Reference);
              end;
            Ref:=Ref^.nextref;
          end;
        if Assigned(Symbol) then
          Owner^.Insert(Symbol);
        sym:=psym(sym^.next);
      end;
  end;

procedure CreateBrowserCol;
var
  T: PSymTable;
  UnitS: PSymbol;
  hp : pmodule;
begin
  DisposeBrowserCol;
  if (cs_browser in aktmoduleswitches) then
    NewBrowserCol;
  hp:=pmodule(loaded_units.first);
  if (cs_browser in aktmoduleswitches) then
   while assigned(hp) do
    begin
       t:=psymtable(hp^.globalsymtable);
       if assigned(t) then
         begin
           New(UnitS, Init(T^.Name^,unitsym,'',nil));
           Modules^.Insert(UnitS);
           ProcessSymTable(UnitS,UnitS^.Items,T);
           if cs_local_browser in aktmoduleswitches then
             begin
                t:=psymtable(hp^.localsymtable);
                if assigned(t) then
                  ProcessSymTable(UnitS,UnitS^.Items,T);
             end;
         end;
       hp:=pmodule(hp^.next);
    end;
  if (cs_browser in aktmoduleswitches) then
    BuildObjectInfo;
  { can allways be done
    needed to know when recompilation of sources is necessary }
  BuildSourceList;
end;

procedure BuildObjectInfo;
var C: PIDSortedSymbolCollection;
    ObjectC: PObjectSymbolCollection;
    ObjectsSymbol: PObjectSymbol;
procedure InsertSymbolCollection(Symbols: PSymbolCollection);
var I: sw_integer;
    P: PSymbol;
begin
  for I:=0 to Symbols^.Count-1 do
    begin
      P:=Symbols^.At(I);
      if (P^.Flags and sfObject)<>0 then
        C^.Insert(P);
      if P^.Items<>nil then
        InsertSymbolCollection(P^.Items);
    end;
end;
function SearchObjectForSym(O: PSymbol): PObjectSymbol;
var I,Idx: sw_integer;
    OS,P: PObjectSymbol;
begin
  P:=nil;
  for I:=0 to ObjectC^.Count-1 do
    begin
      OS:=ObjectC^.At(I);
      if OS^.Symbol=O then
        begin P:=OS; Break; end;
    end;
  SearchObjectForSym:=P;
end;
procedure BuildTree;
var I: sw_integer;
    Symbol: PSymbol;
    Parent,OS: PObjectSymbol;
begin
  I:=0;
  while (I<C^.Count) do
    begin
      Symbol:=C^.At(I);
      if Symbol^.Ancestor=nil then
        Parent:=ObjectsSymbol
      else
        Parent:=SearchObjectForSym(Symbol^.Ancestor);
      if Parent<>nil then
        begin
          New(OS, Init(Parent, Symbol));
          Parent^.AddDescendant(OS);
          ObjectC^.Insert(OS);
          C^.AtDelete(I);
        end
      else
        Inc(I);
    end;
end;
var Pass: integer;
    I: sw_integer;
    P: PSymbol;
begin
  New(C, Init(1000,5000));
  InsertSymbolCollection(Modules);

  { --- Resolve ancestor<->descendant references --- }
  for I:=0 to C^.Count-1 do
    begin
      P:=C^.At(I);
      if P^.AncestorID<>0 then
        P^.Ancestor:=C^.SearchSymbolByID(P^.AncestorID);
    end;

  { --- Build object tree --- }
  if assigned(ObjectTree) then Dispose(ObjectTree, Done);
  New(ObjectsSymbol, InitName('Objects'));
  ObjectTree:=ObjectsSymbol;

  New(ObjectC, Init(C^.Count,100));

  Pass:=0;
  if C^.Count>0 then
  repeat
    BuildTree;
    Inc(Pass);
  until (C^.Count=0) or (Pass>20); { more than 20 levels ? - then there must be a bug }

  ObjectC^.DeleteAll; Dispose(ObjectC, Done);
  C^.DeleteAll; Dispose(C, Done);
end;

function SearchObjectForSymbol(O: PSymbol): PObjectSymbol;
function ScanObjectCollection(Parent: PObjectSymbol): PObjectSymbol;
var I: sw_integer;
    OS,P: PObjectSymbol;
    ObjectC: PObjectSymbolCollection;
begin
  P:=nil;
  if Parent<>nil then
  if Parent^.Descendants<>nil then
  begin
    ObjectC:=Parent^.Descendants;
    for I:=0 to ObjectC^.Count-1 do
      begin
        OS:=ObjectC^.At(I);
        if OS^.Symbol=O then
          begin P:=OS; Break; end;
        if OS^.Descendants<>nil then
          begin
            P:=ScanObjectCollection(OS);
            if P<>nil then Break;
          end;
      end;
  end;
  ScanObjectCollection:=P;
end;
begin
  SearchObjectForSymbol:=ScanObjectCollection(ObjectTree);
end;

procedure BuildSourceList;
var m: pmodule;
    s: pinputfile;
    p: cobjects.pstring;
    ppu,obj: string;
    source: string;
begin
  if Assigned(SourceFiles) then
    begin Dispose(SourceFiles, Done); SourceFiles:=nil; end;
  if assigned(loaded_units.first) then
  begin
    New(SourceFiles, Init(50,10));
    m:=pmodule(loaded_units.first);
    while assigned(m) do
    begin
      obj:=fexpand(m^.objfilename^);
      ppu:=''; source:='';
      if m^.is_unit then
        ppu:=fexpand(m^.ppufilename^);
      if (m^.is_unit=false) and (m^.islibrary=false) then
        ppu:=fexpand(m^.exefilename^);
      if assigned(p) then
      begin
        if assigned(m^.sourcefiles) then
        begin
          s:=m^.sourcefiles^.files;
          while assigned(s) do
          begin
            source:='';
            p:=s^.path;
            if assigned(p) then
              source:=source+p^;
            p:=s^.name;
            if assigned(p) then
              source:=source+p^;
            source:=fexpand(source);

            SourceFiles^.Insert(New(PSourceFile, Init(source,obj,ppu)));
            s:=s^.next;
          end;
        end;
      end;

      m:=pmodule(m^.next);
    end;
  end;
end;

{*****************************************************************************
                                 Initialize
*****************************************************************************}



var
  oldexit : pointer;

procedure browcol_exit;{$ifndef FPC}far;{$endif}
begin
  exitproc:=oldexit;
  DisposeBrowserCol;
end;


procedure InitBrowserCol;
begin
end;


procedure DoneBrowserCol;
begin
  { nothing, the collections are freed in the exitproc - ??? }
  { nothing? then why do we've this routine for ? IMHO, either we should
    remove this, or it should destroy the browser info when it's called. - BG }
end;

type
     PPointerXRef = ^TPointerXRef;
     TPointerXRef = record
       PtrValue : pointer;
       DataPtr  : pointer;
     end;

     PPointerDictionary = ^TPointerDictionary;
     TPointerDictionary = object(TSortedCollection)
       function  At(Index: sw_Integer): PPointerXRef;
       function  Compare(Key1, Key2: Pointer): sw_Integer; virtual;
       procedure FreeItem(Item: Pointer); virtual;
       function  SearchXRef(PtrValue: pointer): PPointerXRef;
       function  AddPtr(PtrValue, DataPtr: pointer): PPointerXRef;
       procedure Resolve(var P);
     end;

function NewPointerXRef(APtrValue, ADataPtr: pointer): PPointerXRef;
var P: PPointerXRef;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  with P^ do begin PtrValue:=APtrValue; DataPtr:=ADataPtr; end;
  NewPointerXRef:=P;
end;

procedure DisposePointerXRef(P: PPointerXRef);
begin
  if Assigned(P) then Dispose(P);
end;

function TPointerDictionary.At(Index: sw_Integer): PPointerXRef;
begin
  At:=inherited At(Index);
end;

function TPointerDictionary.Compare(Key1, Key2: Pointer): sw_Integer;
var K1: PPointerXRef absolute Key1;
    K2: PPointerXRef absolute Key2;
    R: integer;
begin
  if longint(K1^.PtrValue)<longint(K2^.PtrValue) then R:=-1 else
  if longint(K1^.PtrValue)>longint(K2^.PtrValue) then R:= 1 else
  R:=0;
  Compare:=R;
end;

procedure TPointerDictionary.FreeItem(Item: Pointer);
begin
  if Assigned(Item) then DisposePointerXRef(Item);
end;

function TPointerDictionary.SearchXRef(PtrValue: pointer): PPointerXRef;
var P: PPointerXRef;
    T: TPointerXRef;
    Index: sw_integer;
begin
  T.PtrValue:=PtrValue;
  if Search(@T,Index)=false then P:=nil else
    P:=At(Index);
  SearchXRef:=P;
end;

function TPointerDictionary.AddPtr(PtrValue, DataPtr: pointer): PPointerXRef;
var P: PPointerXRef;
begin
  P:=NewPointerXRef(PtrValue,DataPtr);
  Insert(P);
  AddPtr:=P;
end;

procedure TPointerDictionary.Resolve(var P);
var X: PPointerXRef;
    V: pointer;
begin
  Move(P,V,SizeOf(V));
  X:=SearchXRef(V);
  if X=nil then V:=nil else
    V:=X^.DataPtr;
  Move(V,P,SizeOf(V));
end;

procedure ReadPointers(S: PStream; C: PCollection; D: PPointerDictionary);
var W,I: sw_integer;
    P: pointer;
begin
  S^.Read(W,SizeOf(W));
  for I:=0 to W-1 do
  begin
    S^.Read(P,SizeOf(P));
    D^.AddPtr(P,C^.At(I));
  end;
end;

function LoadBrowserCol(S: PStream): boolean;
var PD: PPointerDictionary;
procedure FixupPointers;
procedure FixupReference(P: PReference); {$ifndef FPC}far;{$endif}
begin
  PD^.Resolve(P^.FileName);
end;
procedure FixupSymbol(P: PSymbol); {$ifndef FPC}far;{$endif}
var I: sw_integer;
begin
  PD^.Resolve(P^.DType);
  PD^.Resolve(P^.VType);
  PD^.Resolve(P^.Ancestor);
  if Assigned(P^.References) then
    with P^.References^ do
     for I:=0 to Count-1 do
       FixupReference(At(I));
  if Assigned(P^.Items) then
    with P^.Items^ do
     for I:=0 to Count-1 do
       FixupSymbol(At(I));
end;
begin
  Modules^.ForEach(@FixupSymbol);
end;
procedure ReadSymbolPointers(P: PSymbol); {$ifndef FPC}far;{$endif}
var I: sw_integer;
    PV: pointer;
begin
  S^.Read(PV, SizeOf(PV));
  PD^.AddPtr(PV,P);
  if Assigned(P^.Items) then
    with P^.Items^ do
     for I:=0 to Count-1 do
       ReadSymbolPointers(At(I));
end;
begin
  DisposeBrowserCol;

  New(ModuleNames, Load(S^));
  New(TypeNames, Load(S^));
  New(Modules, Load(S^));

  New(PD, Init(4000,1000));
  ReadPointers(S,ModuleNames,PD);
  ReadPointers(S,TypeNames,PD);
  ReadPointers(S,Modules,PD);
  Modules^.ForEach(@ReadSymbolPointers);
  FixupPointers;
  Dispose(PD, Done);

  BuildObjectInfo;
  LoadBrowserCol:=(S^.Status=stOK);
end;

procedure StorePointers(S: PStream; C: PCollection);
var W,I: sw_integer;
    P: pointer;
begin
  W:=C^.Count;
  S^.Write(W,SizeOf(W));
  for I:=0 to W-1 do
  begin
    P:=C^.At(I);
    S^.Write(P,SizeOf(P));
  end;
end;

procedure StoreBrowserCol(S: PStream);
procedure WriteSymbolPointers(P: PSymbol); {$ifndef FPC}far;{$endif}
var I: sw_integer;
begin
  S^.Write(P, SizeOf(P));
  if Assigned(P^.Items) then
    with P^.Items^ do
     for I:=0 to Count-1 do
       WriteSymbolPointers(At(I));
end;
var W: sw_integer;
begin
  ModuleNames^.Store(S^);
  TypeNames^.Store(S^);
  Modules^.Store(S^);

  StorePointers(S,ModuleNames);
  StorePointers(S,TypeNames);
  StorePointers(S,Modules);
  Modules^.ForEach(@WriteSymbolPointers);
end;

procedure RegisterSymbols;
begin
  RegisterType(RModuleNameCollection);
  RegisterType(RTypeNameCollection);
  RegisterType(RReference);
  RegisterType(RSymbol);
  RegisterType(RObjectSymbol);
  RegisterType(RSymbolCollection);
  RegisterType(RSortedSymbolCollection);
  RegisterType(RIDSortedSymbolCollection);
  RegisterType(RObjectSymbolCollection);
  RegisterType(RReferenceCollection);
end;

begin
  oldexit:=exitproc;
  exitproc:=@browcol_exit;
end.
{
  $Log$
  Revision 1.31  2000-01-07 01:14:19  peter
    * updated copyright to 2000

  Revision 1.30  1999/12/01 11:11:19  pierre
   * don't redefine sw_integer for FPC : corrected version

  Revision 1.29  1999/12/01 11:05:47  pierre
   * don't redefine sw_integer for FPC

  Revision 1.28  1999/11/30 10:40:42  peter
    + ttype, tsymlist

  Revision 1.27  1999/11/10 00:42:42  pierre
    * LookUp function now returns the complete name in browcol
      and fpsymbol only yakes a part of LoopUpStr

  Revision 1.26  1999/11/06 14:34:17  peter
    * truncated log to 20 revs

  Revision 1.25  1999/10/26 12:30:40  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.24  1999/09/16 07:54:48  pierre
   * BuildSourceList allways called for dependency in FP

  Revision 1.23  1999/09/07 15:07:49  pierre
   * avoid some infinite recursions

  Revision 1.22  1999/08/16 18:25:49  peter
    * fixes from gabor

  Revision 1.21  1999/08/09 14:09:04  peter
    * updated for symtable updates

  Revision 1.20  1999/08/03 22:02:29  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.17  1999/06/22 16:24:39  pierre
   * local browser stuff corrected

  Revision 1.16  1999/05/13 21:59:20  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.15  1999/04/29 09:36:55  peter
    * fixed crash
    * check if localbrowser is set

  Revision 1.14  1999/04/15 09:01:32  peter
    * fixed set loading
    * object inheritance support for browser

  Revision 1.13  1999/04/14 18:59:52  peter
    * fixed wrong variable names

  Revision 1.12  1999/04/10 16:15:00  peter
    * fixed browcol
    + -ar to show regalloc info in .s file

  Revision 1.11  1999/04/08 10:17:42  peter
    + objects support

  Revision 1.8  1999/03/03 01:38:11  pierre
   * avoid infinite recursion in ProcessDefIfStruct

  Revision 1.7  1999/02/22 11:51:32  peter
    * browser updates from gabor

  Revision 1.6  1999/02/04 09:31:59  pierre
   + added objects and records symbol tables

  Revision 1.5  1999/02/03 09:44:32  pierre
    * symbol nubering begins with 1 in number_symbols
    * program tmodule has globalsymtable for its staticsymtable
      (to get it displayed in IDE globals list)
    + list of symbol (browcol) greatly improved for IDE

  Revision 1.4  1999/02/02 16:38:38  peter
    * no endless loop with localst=staticsymtable

  Revision 1.3  1999/01/22 10:19:43  peter
    * fixed typo

  Revision 1.2  1999/01/21 11:49:14  peter
    * updates from gabor

}
