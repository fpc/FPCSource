{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Options/config routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPCfgs;

interface

uses
  Objects,
  Systems;

const
     MinStackSize    = 1024;
     MaxStackSize    = 67107840;
     MinHeapSize     = 1024;
     MaxHeapSize     = 67107840;

type
    TOptionMode = (om_Normal,om_Debug,om_Release);

    TOptionToggle = array[TOptionMode] of boolean;
    TOptionString = array[TOptionMode] of string;
    TOptionValue  = array[TOptionMode] of longint;

    POptionItem = ^TOptionItem;
    TOptionItem = record
      Name      : string[30];
      Param     : string[10];
      IsSet     : TOptionToggle;
    end;

    POptionItemCollection = ^TOptionItemCollection;
    TOptionItemCollection = object(TCollection)
      function  At(Index: Integer): POptionItem;
      procedure FreeItem(Item: Pointer); virtual;
    end;

    POptions = ^TOptions;
    TOptions = object
      constructor InitToggle(ch:char);
      constructor InitSel(ch:char);
      destructor  Done;
      { items }
      procedure AddItem(const name,param:string);
      function  ItemCount:integer;
      function  ItemName(index:integer):string;
      function  ItemParam(index:integer):string;
      function  ItemIsSet(index:integer):boolean;
      procedure ItemSet(index:integer;b:boolean);
      function  GetCurrSel:integer;
      procedure SetCurrSel(index:integer);
      { read / write to cfgfile which must be open }
      procedure WriteItemsCfg;
      function  ReadItemsCfg(const s:string):boolean;
    private
      IsSel  : boolean;
      Prefix : char;
      SelNr  : integer;
      Items  : POptionItemCollection;
    end;

    PDirectories = ^TDirectories;
    TDirectories = record
      LibraryDirs,
      IncludeDirs,
      UnitDirs,
      ObjectDirs  : TOptionString;
    end;

const
    OptionMode : TOptionMode = om_Normal;

var
    SyntaxOptions,
    VerboseOptions,
    CodegenOptions,
    OptimizationOptions,
    ProcessorOptions,
    AsmReaderOptions,
    TargetOptions : POptions;
    Dirs          : PDirectories;
    CondDefs      : TOptionString;

{ other settings }
function GetConditionalDefines: string;
procedure SetConditionalDefines(const Defs: string);

{ write/read the options to ppc.cfg file }
procedure WriteOptions(const fn:string);
procedure ReadOptions(const fn:string);

{ initialize }
procedure InitOptions;
procedure DoneOptions;


implementation

uses
  GlobType,Tokens,Compiler;

var
  CfgFile : text;


{*****************************************************************************
                             TOptionItemCollection
*****************************************************************************}

function  TOptionItemCollection.At(Index: Integer): POptionItem;
begin
  At:=inherited At(Index);
end;

procedure TOptionItemCollection.FreeItem(Item: Pointer);
begin
  if assigned(Item) then
   Dispose(POptionItem(Item));
end;


{*****************************************************************************
                                   TOption
*****************************************************************************}

function NewOptionItem(const Name,Param:string):POptionItem;
var
  P : POptionItem;
begin
  New(P);
  P^.Name:=Name;
  P^.Param:=Param;
  FillChar(P^.IsSet,sizeof(P^.IsSet),0);
  NewOptionItem:=P;
end;


constructor TOptions.InitToggle(ch:char);
begin
  new(Items,Init(10,5));
  Prefix:=ch;
  SelNr:=0;
  IsSel:=false;
end;


constructor TOptions.InitSel(ch:char);
begin
  new(Items,Init(10,5));
  Prefix:=ch;
  SelNr:=0;
  IsSel:=true;
end;


destructor  TOptions.Done;
begin
  dispose(Items,Done);
end;


procedure TOptions.AddItem(const name,param:string);
begin
  Items^.Insert(NewOptionItem(name,Param));
end;


function TOptions.ItemCount:integer;
begin
  ItemCount:=Items^.Count;
end;


function TOptions.ItemName(index:integer):string;
var
  P : POptionItem;
begin
  P:=Items^.At(Index);
  if assigned(P) then
   ItemName:=P^.Name
  else
   ItemName:='';
end;


function TOptions.ItemParam(index:integer):string;
var
  P : POptionItem;
begin
  P:=Items^.At(Index);
  if assigned(P) then
   ItemParam:='-'+Prefix+P^.Param
  else
   ItemParam:='';
end;


function TOptions.ItemIsSet(index:integer):boolean;
var
  P : POptionItem;
begin
  if not IsSel then
   begin
     P:=Items^.At(Index);
     if assigned(P) then
      ItemIsSet:=P^.IsSet[OptionMode]
     else
      ItemIsSet:=false;
   end
  else
   ItemIsSet:=false;
end;


procedure TOptions.ItemSet(index:integer;b:boolean);
var
  P : POptionItem;
begin
  if not IsSel then
   begin
     P:=Items^.At(Index);
     if assigned(P) then
      P^.IsSet[OptionMode]:=b;
   end;
end;


function TOptions.GetCurrSel:integer;
begin
  if IsSel then
   GetCurrSel:=SelNr
  else
   GetCurrSel:=-1;
end;


procedure TOptions.SetCurrSel(index:integer);
begin
  if IsSel then
   SelNr:=index;
end;


procedure TOptions.WriteItemsCfg;
var
  Pref : char;

  procedure writeitem(P:POptionItem);{$ifndef FPC}far;{$endif}
  begin
    if (P^.Param<>'') and (P^.IsSet[OptionMode]) then
      Writeln(CfgFile,'-'+Pref+P^.Param)
  end;

begin
  Pref:=Prefix;
  if IsSel then
   begin
     writeln(CfgFile,ItemParam(SelNr));
   end
  else
   begin
     Items^.ForEach(@writeitem);
   end;
end;


function TOptions.ReadItemsCfg(const s:string):boolean;
var
  FoundP : POptionItem;

  function checkitem(P:POptionItem):boolean;{$ifndef FPC}far;{$endif}
  begin
    CheckItem:=(P^.Param=s);
  end;

begin
  FoundP:=Items^.FirstThat(@checkitem);
  if assigned(FoundP) then
   begin
     if IsSel then
      SelNr:=Items^.IndexOf(FoundP)
     else
      FoundP^.IsSet[OptionMode]:=true;
     ReadItemsCfg:=true;
   end
  else
   ReadItemsCfg:=false;
end;


{*****************************************************************************
                                    Others
*****************************************************************************}

function GetConditionalDefines: string;
begin
  GetConditionalDefines:=CondDefs[OptionMode];
end;


procedure SetConditionalDefines(const Defs: string);
begin
  CondDefs[OptionMode]:=Defs;
end;


procedure WriteConditionalDefines;
var
  s,s1 : string;
  i : integer;
begin
  s:=CondDefs[OptionMode];
  repeat
    i:=pos(' ',s);
    if i=0 then
     i:=255;
    s1:=Copy(s,1,i-1);
    if s1<>'' then
     writeln(CfgFile,'-d'+s1);
    Delete(s,1,i);
  until s='';
end;


procedure ReadConditionalDefines(const s:string);
begin
  CondDefs[OptionMode]:=CondDefs[OptionMode]+s;
end;


{*****************************************************************************
                                 Read / Write
*****************************************************************************}

procedure WriteOptions(const fn:string);
begin
{ create the switches }
  assign(CfgFile,fn);
  {$I-}
   rewrite(CfgFile);
  {$I+}
  if ioresult<>0 then
   exit;
  writeln(CfgFile,'# Automaticly created file, don''t edit.');
  TargetOptions^.WriteItemsCfg;
  VerboseOptions^.WriteItemsCfg;
  SyntaxOptions^.WriteItemsCfg;
  CodegenOptions^.WriteItemsCfg;
  OptimizationOptions^.WriteItemsCfg;
  ProcessorOptions^.WriteItemsCfg;
  AsmReaderOptions^.WriteItemsCfg;
  WriteConditionalDefines;
  close(CfgFile);
end;


procedure ReadOptions(const fn:string);
var
  c : char;
  s : string;
begin
  assign(CfgFile,fn);
  {$I-}
   reset(CfgFile);
  {$I+}
  if ioresult<>0 then
   exit;
  while not eof(CfgFile) do
   begin
     readln(CfgFile,s);
     if (length(s)>2) and (s[1]='-') then
      begin
        c:=s[2];
        Delete(s,1,2);
        case c of
         'd' : ReadConditionalDefines(s);
         'S' : SyntaxOptions^.ReadItemsCfg(s);
         'T' : TargetOptions^.ReadItemsCfg(s);
         'R' : AsmReaderOptions^.ReadItemsCfg(s);
         'C' : CodegenOptions^.ReadItemsCfg(s);
         'v' : VerboseOptions^.ReadItemsCfg(s);
         'O' : begin
                 if not OptimizationOptions^.ReadItemsCfg(s) then
                  ProcessorOptions^.ReadItemsCfg(s);
               end;
        end;
      end;
   end;
  close(CfgFile);
end;



{*****************************************************************************
                                 Initialize
*****************************************************************************}

procedure InitOptions;
begin
  New(SyntaxOptions,InitToggle('S'));
  with SyntaxOptions^ do
   begin
     AddItem('~D~elphi 2 extensions on','2');
     AddItem('~C~-like operators','c');
     AddItem('S~t~op after first error','e');
     AddItem('Allo~w~ LABEL and GOTO','g');
     AddItem('C++ styled ~i~nline','i');
     AddItem('Global C ~m~acros','m');
     AddItem('TP/BP ~7~.0 compatibility','o');
     AddItem('Del~p~hi compatibility','d');
     AddItem('A~l~low STATIC in objects','s');
   end;
  New(VerboseOptions,InitToggle('v'));
  with VerboseOptions^ do
   begin
     AddItem('~W~arnings','w');
     AddItem('~N~otes','n');
     AddItem('~H~ints','h');
     AddItem('General ~I~nfo','i');
     AddItem('~U~sed,tried info','ut');
     AddItem('~A~ll','a');
     AddItem('Show all ~P~rocedures if error','b');
   end;
  New(CodegenOptions,InitToggle('C'));
  with CodegenOptions^ do
   begin
     AddItem('~R~ange checking','r');
     AddItem('~S~tack checking','t');
     AddItem('~I~/O checking','i');
     AddItem('Integer ~o~verflow checking','o');
   end;
  New(OptimizationOptions,InitToggle('O'));
  with OptimizationOptions^ do
   begin
     AddItem('Generate ~s~maller code','g');
     AddItem('Generate ~f~aster code','G');
     AddItem('Use register-~v~ariables','r');
     AddItem('~U~ncertain optimizations','u');
     AddItem('Level ~1~ optimizations','1');
     AddItem('Level ~2~ optimizations','2');
   end;
  New(ProcessorOptions,InitSel('O'));
  with ProcessorOptions^ do
   begin
     AddItem('i~3~86/i486','p1');
     AddItem('Pentium/PentiumMM~X~ (tm)','p2');
     AddItem('P~P~ro/PII/c6x86/K6 (tm)','p3');
   end;
  New(TargetOptions,InitSel('T'));
  with TargetOptions^ do
   begin
     AddItem('DOS (GO32V~1~)','go32v1');
     AddItem('~D~OS (GO32V2)','go32v2');
     AddItem('~L~inux','linux');
     AddItem('~O~S/2','os2');
     AddItem('~W~IN32','win32');
   end;
  New(AsmReaderOptions,InitSel('R'));
  with AsmReaderOptions^ do
   begin
     AddItem('No preprocessin~g~','direct');
     AddItem('~A~T&T style assembler','att');
     AddItem('Int~e~l style assembler','intel');
   end;
end;


procedure DoneOptions;
begin
  dispose(SyntaxOptions,Done);
  dispose(VerboseOptions,Done);
  dispose(CodegenOptions,Done);
  dispose(OptimizationOptions,Done);
  dispose(ProcessorOptions,Done);
  dispose(TargetOptions,Done);
  dispose(AsmReaderOptions,Done);
end;


end.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.1  1998/12/22 10:39:40  peter
    + options are now written/read
    + find and replace routines

}
