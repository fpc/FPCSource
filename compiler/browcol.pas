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
      Params     : PString;
      References : PReferenceCollection;
      Items      : PSymbolCollection;
      DType      : PString;
      VType      : PString;
      Ancestor   : PString;
      IsRecord   : boolean;
      IsClass    : boolean;
      constructor Init(const AName: string; ATyp: tsymtyp; AParams: string);
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
  aasm,globtype,globals,files;

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

constructor TSymbol.Init(const AName: string; ATyp: tsymtyp; AParams: string);
begin
  inherited Init;
  Name:=NewStr(AName); Typ:=ATyp;
  New(References, Init(20,50));
  if ATyp in RecordTypes then
    begin
      Items:=New(PSortedSymbolCollection, Init(50,100));
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
  if IsRecord then
    S:=S+' = record'
  else
  if Ancestor<>nil then
    begin
      S:=S+' = ';
      if IsClass then
        S:=S+'class'
      else
        S:=S+'object';
      if Ancestor^<>'.' then
        S:=S+'('+Ancestor^+')';
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
{  if assigned(Params) then
    DisposeStr(Params);
  if assigned(VType) then
    DisposeStr(VType);
  if assigned(DType) then
    DisposeStr(DType);
  if assigned(Ancestor) then
    DisposeStr(Ancestor);}
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
    esym:=def^.First; Count:=0;
    while (esym<>nil) do
      begin
        if Count>0 then Name:=Name+', ';
        Name:=Name+esym^.name;
        esym:=esym^.next; Inc(Count);
      end;
    Name:=Name+')';
    GetEnumDefStr:=Name;
  end;
  function GetArrayDefStr(def: parraydef): string;
  var Name: string;
  begin
    Name:='array ['+IntToStr(def^.lowrange)+'..'+IntToStr(def^.highrange)+'] of ';
    if assigned(def^.definition) then
      Name:=Name+GetDefinitionStr(def^.definition);
    GetArrayDefStr:=Name;
  end;
  function GetFileDefStr(def: pfiledef): string;
  var Name: string;
  begin
    Name:='';
    case def^.filetype of
      ft_text    : Name:='text';
      ft_untyped : Name:='file';
      ft_typed   : Name:='file of '+GetDefinitionStr(def^.typed_as);
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
    if assigned(def^.retdef) then
      if UpcaseStr(GetDefinitionStr(def^.retdef))<>'VOID' then
        OK:=true;
    retdefassigned:=OK;
  end;
  function GetAbsProcParmDefStr(def: pabstractprocdef): string;
  var Name: string;
      dc: pdefcoll;
      Count: integer;
      CurName: string;
  begin
    Name:='';
    dc:=def^.para1; Count:=0;
    while dc<>nil do
     begin
       CurName:='';
       case dc^.paratyp of
         vs_Value : ;
         vs_Const : CurName:=CurName+'const ';
         vs_Var   : CurName:=CurName+'var ';
       end;
       if assigned(dc^.data) then
         CurName:=CurName+GetDefinitionStr(dc^.data);
       if dc^.next<>nil then
         CurName:=', '+CurName;
       Name:=CurName+Name;
       dc:=dc^.next; Inc(Count);
     end;
    GetAbsProcParmDefStr:=Name;
  end;
  function GetAbsProcDefStr(def: pabstractprocdef): string;
  var Name: string;
  begin
    Name:=GetAbsProcParmDefStr(def);
    if Name<>'' then Name:='('+Name+')';
    if retdefassigned(def) then
      Name:='function'+Name+': '+GetDefinitionStr(def^.retdef)
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
    Name:=Name+GetDefinitionStr(def^.setof);
    GetSetDefStr:=Name;
  end;
  function GetDefinitionStr(def: pdef): string;
  var Name: string;
      sym: psym;
  begin
    Name:='';
    if def<>nil then
    begin
      if assigned(def^.sym) then
        Name:=def^.sym^.name;
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
      if assigned(sym^.definition^.sym) then
      begin
{        ES:=sym^.definition^.First;
        while (ES<>nil) and (ES^.Value<>sym^.Value) do
          ES:=ES^.next;
        if assigned(es) and (es^.value=sym^.value) then
          Name:=}
        Name:=sym^.definition^.sym^.name;
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
    case sym^.consttype of
      constord :
        Name:=sym^.definition^.sym^.name+'('+IntToStr(sym^.value)+')';
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
    if assigned(definition) then
    begin
      case definition^.deftype of
        recorddef :
          if precdef(definition)^.symtable<>Table then
            ProcessSymTable(Symbol,Symbol^.Items,precdef(definition)^.symtable);
        objectdef :
          if precdef(definition)^.symtable<>Table then
            ProcessSymTable(Symbol,Symbol^.Items,pobjectdef(definition)^.publicsyms);
        { leads to infinite loops !!
        pointerdef :
          with ppointerdef(definition)^ do
            if assigned(definition) then
              if assigned(definition^.sym) then
                ProcessDefIfStruct(definition^.sym^.definition);}
      end;
    end;
  end;

  begin
    if not Assigned(Table) then
     Exit;
    if Owner=nil then
     Owner:=New(PSortedSymbolCollection, Init(10,50));
    defcount:=Table^.number_defs;
    symcount:=Table^.number_symbols;
  {  for I:=0 to defcount-1 do
     begin
       Def:=Table^.GetDefNr(I);
     end;}
    for I:=1 to symcount do
      begin
        Sym:=Table^.GetsymNr(I);
        if Sym=nil then Continue;
        ParamCount:=0;
        New(Symbol, Init(Sym^.Name,Sym^.Typ,''));
        case Sym^.Typ of
          varsym :
             with pvarsym(sym)^ do
             begin
               if assigned(definition) then
                 if assigned(definition^.sym) then
                   SetVType(Symbol,definition^.sym^.name)
                 else
                   SetVType(Symbol,GetDefinitionStr(definition));
               ProcessDefIfStruct(definition);
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
              if assigned(funcretdef) then
                if assigned(funcretdef^.sym) then
                   SetVType(OwnerSym,funcretdef^.sym^.name);
          procsym :
            begin
              with pprocsym(sym)^ do
              if assigned(definition) then
              begin
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
                if assigned(definition^.localst) and
                   (definition^.localst^.symtabletype<>staticsymtable) then
                  ProcessSymTable(Symbol,Symbol^.Items,definition^.localst);
              end;
            end;
          typesym :
            begin
            with ptypesym(sym)^ do
              if assigned(definition) then
                case definition^.deftype of
                  arraydef :
                    SetDType(Symbol,GetArrayDefStr(parraydef(definition)));
                  enumdef :
                    SetDType(Symbol,GetEnumDefStr(penumdef(definition)));
                  procdef :
                    SetDType(Symbol,GetProcDefStr(pprocdef(definition)));
                  procvardef :
                    SetDType(Symbol,GetProcVarDefStr(pprocvardef(definition)));
                  objectdef :
                    with pobjectdef(definition)^ do
                    begin
                      if childof=nil then
                        S:='.'
                      else
                        S:=childof^.name^;
                      Symbol^.Ancestor:=TypeNames^.Add(S);
                      Symbol^.IsClass:=(options and oo_is_class)<>0;
                      ProcessSymTable(Symbol,Symbol^.Items,pobjectdef(definition)^.publicsyms);
                    end;
                  recorddef :
                    begin
                      Symbol^.IsRecord:=true;
                      ProcessSymTable(Symbol,Symbol^.Items,precdef(definition)^.symtable);
                    end;
                  filedef :
                    SetDType(Symbol,GetFileDefStr(pfiledef(definition)));
                  setdef :
                    SetDType(Symbol,GetSetDefStr(psetdef(definition)));
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
      end;
  end;

var
  T: PSymTable;
  UnitS: PSymbol;
  hp : pmodule;
begin
  DisposeBrowserCol;
  NewBrowserCol;
  hp:=pmodule(loaded_units.first);
  while assigned(hp) do
    begin
       t:=psymtable(hp^.globalsymtable);
       if assigned(t) then
         begin
           New(UnitS, Init(T^.Name^,unitsym,''));
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
  Revision 1.9  1999-03-24 23:16:44  peter
    * fixed bugs 212,222,225,227,229,231,233

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

  Revision 1.1  1999/01/12 14:25:24  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser
}
