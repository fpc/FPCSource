{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Compiler switches routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPSwitches;

interface

uses
  Objects,
  Systems,
  FPConst;

const
     MinStackSize    = 1024;
     MaxStackSize    = 67107840;
     MinHeapSize     = 1024;
     MaxHeapSize     = 67107840;

type
    TSwitchMode = (om_Normal,om_Debug,om_Release);

    TSwitchItemTyp = (ot_Select,ot_Boolean,ot_String,ot_Longint);

    PSwitchItem = ^TSwitchItem;
    TSwitchItem = object(TObject)
      Typ       : TSwitchItemTyp;
      Name      : string[30];
      Param     : string[10];
      constructor Init(const n,p:string);
      function  NeedParam:boolean;virtual;
      function  ParamValue:string;virtual;
      procedure Reset;virtual;
    end;

    PSelectItem = ^TSelectItem;
    TSelectItem = object(TSwitchItem)
      constructor Init(const n,p:string);
    end;

    PBooleanItem = ^TBooleanItem;
    TBooleanItem = object(TSwitchItem)
      IsSet : array[TSwitchMode] of boolean;
      constructor Init(const n,p:string);
      function  NeedParam:boolean;virtual;
      procedure Reset;virtual;
    end;

    PStringItem = ^TStringItem;
    TStringItem = object(TSwitchItem)
      Str : array[TSwitchMode] of string;
      multiple : boolean;
      constructor Init(const n,p:string;mult:boolean);
      function  NeedParam:boolean;virtual;
      function  ParamValue:string;virtual;
      procedure Reset;virtual;
    end;

    PLongintItem = ^TLongintItem;
    TLongintItem = object(TSwitchItem)
      Val : array[TSwitchMode] of longint;
      constructor Init(const n,p:string);
      function  NeedParam:boolean;virtual;
      function  ParamValue:string;virtual;
      procedure Reset;virtual;
    end;

    PSwitches = ^TSwitches;
    TSwitches = object
      constructor Init(ch:char);
      constructor InitSelect(ch:char);
      destructor  Done;
      { general items }
      function  ItemCount:integer;
      function  ItemName(index:integer):string;
      function  ItemParam(index:integer):string;
      { type specific }
      procedure AddSelectItem(const name,param:string);
      procedure AddBooleanItem(const name,param:string);
      procedure AddLongintItem(const name,param:string);
      procedure AddStringItem(const name,param:string;mult:boolean);
      function  GetCurrSel:integer;
      function  GetBooleanItem(index:integer):boolean;
      function  GetLongintItem(index:integer):longint;
      function  GetStringItem(index:integer):string;
      procedure SetCurrSel(index:integer);
      procedure SetBooleanItem(index:integer;b:boolean);
      procedure SetLongintItem(index:integer;l:longint);
      procedure SetStringItem(index:integer;const s:string);
      { read / write to cfgfile which must be open }
      procedure WriteItemsCfg;
      function  ReadItemsCfg(const s:string):boolean;
    private
      IsSel  : boolean;
      Prefix : char;
      SelNr  : integer;
      Items  : PCollection;
    end;

const
    SwitchesMode : TSwitchMode = om_Normal;

    SwitchesModeName : array[TSwitchMode] of string[10]=
      ('~N~ormal','~D~ebug','~R~elease');
    SwitchesModeStr : array[TSwitchMode] of string[8]=
      ('NORMAL','DEBUG','RELEASE');

var
    SyntaxSwitches,
    VerboseSwitches,
    CodegenSwitches,
    OptimizationSwitches,
    ProcessorSwitches,
    AsmReaderSwitches,
    TargetSwitches,
    ConditionalSwitches,
    MemorySwitches,
    DirectorySwitches : PSwitches;

{ write/read the Switches to ppc.cfg file }
procedure WriteSwitches(const fn:string);
procedure ReadSwitches(const fn:string);

{ initialize }
procedure InitSwitches;
procedure DoneSwitches;


implementation

uses
  Dos,
  GlobType,Tokens,Compiler,
  FPUtils,FPVars;

var
  CfgFile : text;

{*****************************************************************************
                                TSwitchItem
*****************************************************************************}

constructor TSwitchItem.Init(const n,p:string);
begin
  Inherited Init;
  Name:=n;
  Param:=p;
end;


function TSwitchItem.NeedParam:boolean;
begin
  NeedParam:=false;
end;


function TSwitchItem.ParamValue:string;
begin
  ParamValue:='';
end;


procedure TSwitchItem.Reset;
begin
end;


{*****************************************************************************
                                TSelectItem
*****************************************************************************}

constructor TSelectItem.Init(const n,p:string);
begin
  Inherited Init(n,p);
  Typ:=ot_Select;
end;


{*****************************************************************************
                               TBooleanItem
*****************************************************************************}

constructor TBooleanItem.Init(const n,p:string);
begin
  Inherited Init(n,p);
  Typ:=ot_Boolean;
  Reset;
end;


function TBooleanItem.NeedParam:boolean;
begin
  NeedParam:=IsSet[SwitchesMode];
end;


procedure TBooleanItem.Reset;
begin
  FillChar(IsSet,sizeof(IsSet),0);
end;


{*****************************************************************************
                                TStringItem
*****************************************************************************}

constructor TStringItem.Init(const n,p:string;mult:boolean);
begin
  Inherited Init(n,p);
  Typ:=ot_String;
  Multiple:=mult;
  Reset;
end;


function TStringItem.NeedParam:boolean;
begin
  NeedParam:=(Str[SwitchesMode]<>'');
end;


function TStringItem.ParamValue:string;
begin
  ParamValue:=Str[SwitchesMode];
end;


procedure TStringItem.Reset;
begin
  FillChar(Str,sizeof(Str),0);
end;


{*****************************************************************************
                               TLongintItem
*****************************************************************************}

constructor TLongintItem.Init(const n,p:string);
begin
  Inherited Init(n,p);
  Typ:=ot_Longint;
  Reset;
end;


function TLongintItem.NeedParam:boolean;
begin
  NeedParam:=(Val[SwitchesMode]<>0);
end;


function TLongintItem.ParamValue:string;
var
  s : string;
begin
  Str(Val[SwitchesMode],s);
  ParamValue:=s;
end;


procedure TLongintItem.Reset;
begin
  FillChar(Val,sizeof(Val),0);
end;


{*****************************************************************************
                                   TSwitch
*****************************************************************************}

constructor TSwitches.Init(ch:char);
begin
  new(Items,Init(10,5));
  Prefix:=ch;
  SelNr:=0;
  IsSel:=false;
end;


constructor TSwitches.InitSelect(ch:char);
begin
  new(Items,Init(10,5));
  Prefix:=ch;
  SelNr:=0;
  IsSel:=true;
end;


destructor  TSwitches.Done;
begin
  dispose(Items,Done);
end;


procedure TSwitches.AddSelectItem(const name,param:string);
begin
  Items^.Insert(New(PSelectItem,Init(name,Param)));
end;


procedure TSwitches.AddBooleanItem(const name,param:string);
begin
  Items^.Insert(New(PBooleanItem,Init(name,Param)));
end;


procedure TSwitches.AddLongintItem(const name,param:string);
begin
  Items^.Insert(New(PLongintItem,Init(name,Param)));
end;


procedure TSwitches.AddStringItem(const name,param:string;mult:boolean);
begin
  Items^.Insert(New(PStringItem,Init(name,Param,mult)));
end;


function TSwitches.ItemCount:integer;
begin
  ItemCount:=Items^.Count;
end;


function TSwitches.ItemName(index:integer):string;
var
  P : PSwitchItem;
begin
  P:=Items^.At(Index);
  if assigned(P) then
   ItemName:=P^.Name
  else
   ItemName:='';
end;


function TSwitches.ItemParam(index:integer):string;
var
  P : PSwitchItem;
begin
  P:=Items^.At(Index);
  if assigned(P) then
   ItemParam:='-'+Prefix+P^.Param
  else
   ItemParam:='';
end;


function TSwitches.GetBooleanItem(index:integer):boolean;
var
  P : PBooleanItem;
begin
  P:=Items^.At(Index);
  if assigned(P) and (P^.Typ=ot_boolean) then
   GetBooleanItem:=P^.IsSet[SwitchesMode]
  else
   GetBooleanItem:=false;
end;


function TSwitches.GetLongintItem(index:integer):longint;
var
  P : PLongintItem;
begin
  P:=Items^.At(Index);
  if assigned(P) and (P^.Typ=ot_longint) then
   GetLongintItem:=P^.Val[SwitchesMode]
  else
   GetLongintItem:=0;
end;


function TSwitches.GetStringItem(index:integer):string;
var
  P : PStringItem;
begin
  P:=Items^.At(Index);
  if assigned(P) and (P^.Typ=ot_string) then
   GetStringItem:=P^.Str[SwitchesMode]
  else
   GetStringItem:='';
end;


procedure TSwitches.SetBooleanItem(index:integer;b:boolean);
var
  P : PBooleanItem;
begin
  P:=Items^.At(Index);
  if assigned(P) and (P^.Typ=ot_boolean) then
   P^.IsSet[SwitchesMode]:=b;
end;


procedure TSwitches.SetLongintItem(index:integer;l:longint);
var
  P : PLongintItem;
begin
  P:=Items^.At(Index);
  if assigned(P) and (P^.Typ=ot_longint) then
   P^.Val[SwitchesMode]:=l;
end;


procedure TSwitches.SetStringItem(index:integer;const s:string);
var
  P : PStringItem;
begin
  P:=Items^.At(Index);
  if assigned(P) and (P^.Typ=ot_string) then
   P^.Str[SwitchesMode]:=s;
end;


function TSwitches.GetCurrSel:integer;
begin
  if IsSel then
   GetCurrSel:=SelNr
  else
   GetCurrSel:=-1;
end;


procedure TSwitches.SetCurrSel(index:integer);
begin
  if IsSel then
   SelNr:=index;
end;


procedure TSwitches.WriteItemsCfg;
var
  Pref : char;

  procedure writeitem(P:PSwitchItem);{$ifndef FPC}far;{$endif}
  var
    s,s1 : string;
    i : integer;
  begin
    if P^.NeedParam then
     begin
       if (P^.Typ=ot_string) and (PStringItem(P)^.Multiple) then
        begin
          s:=PStringItem(P)^.Str[SwitchesMode];
          repeat
            i:=pos(';',s);
            if i=0 then
             i:=255;
            s1:=Copy(s,1,i-1);
            if s1<>'' then
             writeln(CfgFile,' -'+Pref+P^.Param+s1);
            Delete(s,1,i);
          until s='';
        end
       else
        Writeln(CfgFile,' -'+Pref+P^.Param+P^.ParamValue);
     end;
  end;

begin
  Pref:=Prefix;
  if IsSel then
    writeln(CfgFile,' '+ItemParam(SelNr))
  else
    Items^.ForEach(@writeitem);
end;


function TSwitches.ReadItemsCfg(const s:string):boolean;

  function checkitem(P:PSwitchItem):boolean;{$ifndef FPC}far;{$endif}
  begin
    CheckItem:=(P^.Param=Copy(s,1,length(P^.Param)));
  end;

var
  FoundP : PSwitchItem;
  code : integer;
begin
  FoundP:=Items^.FirstThat(@checkitem);
  if assigned(FoundP) then
   begin
     case FoundP^.Typ of
      ot_Select  : SelNr:=Items^.IndexOf(FoundP);
      ot_Boolean : PBooleanItem(FoundP)^.IsSet[SwitchesMode]:=true;
      ot_String  : begin
                     if (PStringItem(FoundP)^.Multiple) and (PStringItem(FoundP)^.Str[SwitchesMode]<>'') then
                      PStringItem(FoundP)^.Str[SwitchesMode]:=PStringItem(FoundP)^.Str[SwitchesMode]+';'+
                        Copy(s,length(FoundP^.Param)+1,255)
                     else
                      PStringItem(FoundP)^.Str[SwitchesMode]:=Copy(s,length(FoundP^.Param)+1,255);
                   end;
      ot_Longint : Val(Copy(s,length(FoundP^.Param)+1,255),PLongintItem(FoundP)^.Val[SwitchesMode],code);
     end;
     ReadItemsCfg:=true;
   end
  else
   ReadItemsCfg:=false;
end;


{*****************************************************************************
                                 Read / Write
*****************************************************************************}

procedure WriteSwitches(const fn:string);
var
  OldSwitchesMode : TSwitchMode;
begin
{ create the switches }
  assign(CfgFile,fn);
  {$I-}
   rewrite(CfgFile);
  {$I+}
  if ioresult<>0 then
   exit;
  writeln(CfgFile,'# Automaticly created file, don''t edit.');
  OldSwitchesMode:=SwitchesMode;
  for SwitchesMode:=low(TSwitchMode) to high(TSwitchMode) do
   begin
     Writeln(CfgFile,'#IFDEF '+SwitchesModeStr[SwitchesMode]);
     TargetSwitches^.WriteItemsCfg;
     VerboseSwitches^.WriteItemsCfg;
     SyntaxSwitches^.WriteItemsCfg;
     CodegenSwitches^.WriteItemsCfg;
     OptimizationSwitches^.WriteItemsCfg;
     ProcessorSwitches^.WriteItemsCfg;
     AsmReaderSwitches^.WriteItemsCfg;
     DirectorySwitches^.WriteItemsCfg;
     MemorySwitches^.WriteItemsCfg;
     ConditionalSwitches^.WriteItemsCfg;
     Writeln(CfgFile,'#ENDIF');
     Writeln(CfgFile,'');
   end;
  close(CfgFile);
  SwitchesMode:=OldSwitchesMode;
end;


procedure ReadSwitches(const fn:string);
var
  c : char;
  s : string;
  OldSwitchesMode,i : TSwitchMode;
begin
  assign(CfgFile,fn);
  {$I-}
   reset(CfgFile);
  {$I+}
  if ioresult<>0 then
   exit;
  OldSwitchesMode:=SwitchesMode;
  SwitchesMode:=om_Normal;
  while not eof(CfgFile) do
   begin
     readln(CfgFile,s);
     s:=LTrim(s);
     if (length(s)>2) and (s[1]='-') then
      begin
        c:=s[2];
        Delete(s,1,2);
        case c of
         'd' : ConditionalSwitches^.ReadItemsCfg(s);
         'S' : SyntaxSwitches^.ReadItemsCfg(s);
         'F' : DirectorySwitches^.ReadItemsCfg(s);
         'T' : TargetSwitches^.ReadItemsCfg(s);
         'R' : AsmReaderSwitches^.ReadItemsCfg(s);
         'C' : CodegenSwitches^.ReadItemsCfg(s);
         'v' : VerboseSwitches^.ReadItemsCfg(s);
         'O' : begin
                 if not OptimizationSwitches^.ReadItemsCfg(s) then
                  ProcessorSwitches^.ReadItemsCfg(s);
               end;
        end;
      end
     else
      if (Copy(s,1,7)='#IFDEF ') then
       begin
         Delete(s,1,7);
         for i:=low(TSwitchMode) to high(TSwitchMode) do
          if s=SwitchesModeStr[i] then
           begin
             SwitchesMode:=i;
             break;
           end;
       end;
   end;
  close(CfgFile);
  SwitchesMode:=OldSwitchesMode;
end;



{*****************************************************************************
                                 Initialize
*****************************************************************************}

procedure InitSwitches;
begin
  New(SyntaxSwitches,Init('S'));
  with SyntaxSwitches^ do
   begin
     AddBooleanItem('~D~elphi 2 extensions on','2');
     AddBooleanItem('~C~-like operators','c');
     AddBooleanItem('S~t~op after first error','e');
     AddBooleanItem('Allo~w~ LABEL and GOTO','g');
     AddBooleanItem('C++ styled ~i~nline','i');
     AddBooleanItem('Global C ~m~acros','m');
     AddBooleanItem('TP/BP ~7~.0 compatibility','o');
     AddBooleanItem('Del~p~hi compatibility','d');
     AddBooleanItem('A~l~low STATIC in objects','s');
   end;
  New(VerboseSwitches,Init('v'));
  with VerboseSwitches^ do
   begin
     AddBooleanItem('~W~arnings','w');
     AddBooleanItem('~N~otes','n');
     AddBooleanItem('~H~ints','h');
     AddBooleanItem('General ~I~nfo','i');
     AddBooleanItem('~U~sed,tried info','ut');
     AddBooleanItem('~A~ll','a');
     AddBooleanItem('Show all ~P~rocedures if error','b');
   end;
  New(CodegenSwitches,Init('C'));
  with CodegenSwitches^ do
   begin
     AddBooleanItem('~R~ange checking','r');
     AddBooleanItem('~S~tack checking','t');
     AddBooleanItem('~I~/O checking','i');
     AddBooleanItem('Integer ~o~verflow checking','o');
   end;
  New(OptimizationSwitches,Init('O'));
  with OptimizationSwitches^ do
   begin
     AddBooleanItem('Generate ~s~maller code','g');
     AddBooleanItem('Generate ~f~aster code','G');
     AddBooleanItem('Use register-~v~ariables','r');
     AddBooleanItem('~U~ncertain optimizations','u');
     AddBooleanItem('Level ~1~ optimizations','1');
     AddBooleanItem('Level ~2~ optimizations','2');
   end;
  New(ProcessorSwitches,InitSelect('O'));
  with ProcessorSwitches^ do
   begin
     AddSelectItem('i~3~86/i486','p1');
     AddSelectItem('Pentium/PentiumMM~X~ (tm)','p2');
     AddSelectItem('P~P~ro/PII/c6x86/K6 (tm)','p3');
   end;
  New(TargetSwitches,InitSelect('T'));
  with TargetSwitches^ do
   begin
     AddSelectItem('DOS (GO32V~1~)','go32v1');
     AddSelectItem('~D~OS (GO32V2)','go32v2');
     AddSelectItem('~L~inux','linux');
     AddSelectItem('~O~S/2','os2');
     AddSelectItem('~W~IN32','win32');
   end;
  New(AsmReaderSwitches,InitSelect('R'));
  with AsmReaderSwitches^ do
   begin
     AddSelectItem('No preprocessin~g~','direct');
     AddSelectItem('~A~T&T style assembler','att');
     AddSelectItem('Int~e~l style assembler','intel');
   end;
  New(ConditionalSwitches,Init('d'));
  with ConditionalSwitches^ do
   begin
     AddStringItem('Conditio~n~al defines','',true);
   end;
  New(MemorySwitches,Init('C'));
  with MemorySwitches^ do
   begin
     AddLongintItem('~S~tack size','s');
     AddLongintItem('~H~eap size','h');
   end;
  New(DirectorySwitches,Init('F'));
  with DirectorySwitches^ do
   begin
     AddStringItem('~U~nit directories','u',true);
     AddStringItem('~I~nclude directories','i',true);
     AddStringItem('~L~ibrary directories','l',true);
     AddStringItem('~O~bject directories','o',true);
   end;
  SwitchesPath:=LocateFile(SwitchesName);
  if SwitchesPath='' then
    SwitchesPath:=SwitchesName;
  SwitchesPath:=FExpand(SwitchesPath);
end;


procedure DoneSwitches;
begin
  dispose(SyntaxSwitches,Done);
  dispose(VerboseSwitches,Done);
  dispose(CodegenSwitches,Done);
  dispose(OptimizationSwitches,Done);
  dispose(ProcessorSwitches,Done);
  dispose(TargetSwitches,Done);
  dispose(AsmReaderSwitches,Done);
  dispose(ConditionalSwitches,Done);
  dispose(MemorySwitches,Done);
  dispose(DirectorySwitches,Done);
end;


end.
{
  $Log$
  Revision 1.1  1998-12-28 15:47:52  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}
