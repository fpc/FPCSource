{
    $Id$
    Copyright (c) 1993-98 by the FPC development team

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
  objects,symtable;

const
  SymbolTypLen : integer = 6;

  RecordTypes : set of tsymtyp =
    ([typesym,unitsym,programsym]);

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

    PSymbol = ^TSymbol;
    TSymbol = object(TObject)
      Name       : PString;
      Typ        : tsymtyp;
      ParamCount : Sw_integer;
      Params     : PPointerArray;
      References : PReferenceCollection;
      Items      : PSymbolCollection;
      constructor Init(const AName: string; ATyp: tsymtyp; AParamCount: Sw_integer; AParams: PPointerArray);
      procedure   SetParams(AParamCount: Sw_integer; AParams: PPointerArray);
      function    GetReferenceCount: Sw_integer;
      function    GetReference(Index: Sw_integer): PReference;
      function    GetItemCount: Sw_integer;
      function    GetItem(Index: Sw_integer): PSymbol;
      function    GetName: string;
      function    GetText: string;
      function    GetTypeName: string;
      destructor  Done; virtual;
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


procedure DisposeBrowserCol;
procedure NewBrowserCol;
procedure CreateBrowserCol;
procedure InitBrowserCol;
procedure DoneBrowserCol;


implementation

uses
  Drivers,Views,App,
  globals,files,comphook;

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
var P: PString;
    Index: Sw_integer;
begin
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
          Idx:=Mid; FoundS:=copy(MidS,1,length(S));
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


{****************************************************************************
                                   TSymbol
****************************************************************************}

constructor TSymbol.Init(const AName: string; ATyp: tsymtyp; AParamCount: Sw_integer; AParams: PPointerArray);
begin
  inherited Init;
  Name:=NewStr(AName); Typ:=ATyp;
  SetParams(AParamCount,AParams);
  New(References, Init(20,50));
  if ATyp in RecordTypes then
    begin
      Items:=New(PSortedSymbolCollection, Init(50,100));
    end;
end;

procedure TSymbol.SetParams(AParamCount: Sw_integer; AParams: PPointerArray);
begin
  if AParams=nil then AParamCount:=0 else
  if AParamCount=0 then AParams:=nil;
  ParamCount:=AParamCount;
  if (ParamCount>0) and (AParams<>nil) then
  begin
    GetMem(Params, ParamCount*4);
    Move(AParams^,Params^,ParamCount*4);
  end;
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
  if ParamCount>0 then
    begin
      S:=S+'(';
      for I:=1 to ParamCount do
        begin
          S:=S+GetStr(Params^[I-1]);
          if I<>ParamCount then S:=S+', ';
        end;
      S:=S+')';
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
    procsym      : S:='proc';
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
    funcretsym   : S:='func';
    macrosym     : S:='macro';
  else S:='';
  end;
  GetTypeName:=S;
end;

destructor TSymbol.Done;
begin
  inherited Done;
  if assigned(References) then
    Dispose(References, Done);
  if assigned(Items) then
    Dispose(Items, Done);
  if assigned(Name) then
    DisposeStr(Name);
  if assigned(Params) then
    FreeMem(Params,ParamCount*2);
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
end;


procedure NewBrowserCol;
begin
  New(Modules, Init(50,50));
  New(ModuleNames, Init(50,50));
  New(TypeNames, Init(1000,5000));
end;


procedure CreateBrowserCol;

  procedure ProcessSymTable(var Owner: PSymbolCollection; Table: PSymTable);
  var I,J,defcount, symcount: longint;
      Ref: PRef;
      Sym,ParSym: PSym;
      Symbol: PSymbol;
      Reference: PReference;
      ParamCount: Sw_integer;
      Params: array[0..20] of PString;
      inputfile : pinputfile;
  begin
    if Assigned(Table)=false then Exit;
    if Owner=nil then Owner:=New(PSortedSymbolCollection, Init(10,50));
    defcount:=Table^.number_defs;
    symcount:=Table^.number_symbols;
  {  for I:=0 to defcount-1 do
     begin
       Def:=Table^.GetDefNr(I);
     end;}
    for I:=1 to symcount-1 do
      begin
        Sym:=Table^.GetsymNr(I);
        if Sym=nil then Continue;
        ParamCount:=0;
        New(Symbol, Init(Sym^.Name,Sym^.Typ,0,nil));
        case Sym^.Typ of
          unitsym :
            begin
  {            ProcessSymTable(Symbol^.Items,punitsym(sym)^.unitsymtable);}
            end;
          procsym :
            with pprocsym(sym)^ do
            if assigned(definition) then
            begin
              if assigned(definition^.parast) then
                begin
                  with definition^.parast^ do
                  for J:=1 to number_symbols do
                    begin
                      ParSym:=GetsymNr(J);
                      if ParSym=nil then Break;
                      Inc(ParamCount);
                      Params[ParamCount-1]:=TypeNames^.Add(ParSym^.Name);
                    end;
                end;
              if assigned(definition^.localst) then
                ProcessSymTable(Symbol^.Items,definition^.localst);
            end;
          typesym :
            begin
            end;
        end;
        Ref:=Sym^.defref;
        while assigned(Ref) do
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
        Owner^.Insert(Symbol);
      end;
  end;

var
  T: PSymTable;
  UnitS: PSymbol;
begin
  DisposeBrowserCol;
  NewBrowserCol;
  T:=SymTableStack;
  while assigned(T) do
   begin
     New(UnitS, Init(T^.Name^,unitsym, 0, nil));
     Modules^.Insert(UnitS);
     ProcessSymTable(UnitS^.Items,T);
     T:=T^.Next;
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
  { nothing, the collections are freed in the exitproc }
end;


begin
  oldexit:=exitproc;
  exitproc:=@browcol_exit;
end.
{
  $Log$
  Revision 1.3  1999-01-22 10:19:43  peter
    * fixed typo

  Revision 1.2  1999/01/21 11:49:14  peter
    * updates from gabor

  Revision 1.1  1999/01/12 14:25:24  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser
}
