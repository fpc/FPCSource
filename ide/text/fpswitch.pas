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
unit FPSwitch;

interface

uses
  Objects,
  Systems,
  FPConst;

const
     MinMemSize      = 1024;     { min. local heap and stack size }
     MaxMemSize      = 67107840; { max. local heap and stack size }

type
    TSwitchMode = (om_Normal,om_Debug,om_Release);

    TSwitchItemTyp = (ot_Select,ot_Boolean,ot_String,ot_Longint);

    PSwitchItem = ^TSwitchItem;
    TSwitchItem = object(TObject)
      Typ       : TSwitchItemTyp;
      Name      : string[50];
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
      function  GetCurrSelParam : String;
      function  GetBooleanItem(index:integer):boolean;
      function  GetLongintItem(index:integer):longint;
      function  GetStringItem(index:integer):string;
      procedure SetCurrSel(index:integer);
      function  SetCurrSelParam(const s : String) : boolean;
      procedure SetBooleanItem(index:integer;b:boolean);
      procedure SetLongintItem(index:integer;l:longint);
      procedure SetStringItem(index:integer;const s:string);
      { read / write to cfgfile which must be open }
      procedure WriteItemsCfg;
      function  ReadItemsCfg(const s:string):boolean;
    private
      IsSel  : boolean;
      Prefix : char;
      SelNr  : array[TSwitchMode] of integer;
      Items  : PCollection;
    end;

const
    SwitchesMode : TSwitchMode = om_Normal;

    SwitchesModeName : array[TSwitchMode] of string[10]=
      ('~N~ormal','~D~ebug','~R~elease');
    SwitchesModeStr : array[TSwitchMode] of string[8]=
      ('NORMAL','DEBUG','RELEASE');
    CustomArg : array[TSwitchMode] of string{$ifndef FPC}[128]{$endif}=
      ('','','');

var
    LibLinkerSwitches,
    DebugInfoSwitches,
    ProfileInfoSwitches,
    {MemorySizeSwitches, doubled !! }
    SyntaxSwitches,
    VerboseSwitches,
    CodegenSwitches,
    OptimizationSwitches,
    OptimizingGoalSwitches,
    ProcessorSwitches,
    AsmReaderSwitches,
    AsmInfoSwitches,
    AsmOutputSwitches,
    TargetSwitches,
    ConditionalSwitches,
    MemorySwitches,
    BrowserSwitches,
    DirectorySwitches : PSwitches;

{ write/read the Switches to ppc.cfg file }
procedure WriteSwitches(const fn:string);
procedure ReadSwitches(const fn:string);

{ initialize }
procedure InitSwitches;
procedure SetDefaultSwitches;
procedure DoneSwitches;
function  GetSourceDirectories : string;


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
  FillChar(SelNr,SizeOf(SelNr),#0);
  IsSel:=false;
end;


constructor TSwitches.InitSelect(ch:char);
begin
  new(Items,Init(10,5));
  Prefix:=ch;
  FillChar(SelNr,SizeOf(SelNr),#0);
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
   GetCurrSel:=SelNr[SwitchesMode]
  else
   GetCurrSel:=-1;
end;


function  TSwitches.GetCurrSelParam : String;
begin
  if IsSel then
    GetCurrSelParam:=PSwitchItem(Items^.At(SelNr[SwitchesMode]))^.Param
  else
    GetCurrSelParam:='';
end;

procedure TSwitches.SetCurrSel(index:integer);
begin
  if IsSel then
   SelNr[SwitchesMode]:=index;
end;

function  TSwitches.SetCurrSelParam(const s : String) : boolean;
  function checkitem(P:PSwitchItem):boolean;{$ifndef FPC}far;{$endif}
  begin
    { empty items are not equivalent to others !! }
    CheckItem:=((S='') and (P^.Param='')) or
               ((Length(S)>0) and (P^.Param=s));
  end;

var
  FoundP : PSwitchItem;
begin
  FoundP:=Items^.FirstThat(@CheckItem);
  if Assigned(FoundP) then
    begin
      SetCurrSelParam:=true;
      SelNr[SwitchesMode]:=Items^.IndexOf(FoundP);
    end
  else
    SetCurrSelParam:=false;
end;


procedure TSwitches.WriteItemsCfg;
var
  Pref : char;

  procedure writeitem(P:PSwitchItem);{$ifndef FPC}far;{$endif}
  var
    s,s1 : string;
    i,j : integer;
  begin
    if P^.NeedParam then
     begin
       if (P^.Typ=ot_string) and (PStringItem(P)^.Multiple) then
         begin
           s:=PStringItem(P)^.Str[SwitchesMode];
           repeat
             i:=pos(';',s);
             j:=pos(' ',s);
             if i=0 then
              i:=256;
             if (j>0) and (j<i) then
               i:=j;
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
    writeln(CfgFile,' '+ItemParam(SelNr[SwitchesMode]))
  else
    Items^.ForEach(@writeitem);
end;

procedure WriteCustom;
var
  s : string;
  i : longint;
begin
  s:=CustomArg[SwitchesMode];
  While s<>'' do
    begin
       i:=pos(' ',s);
       if i=0 then i:=256;
       writeln(CfgFile,' '+Copy(s,1,i-1));
       if i=256 then
         s:=''
       else
         s:=copy(s,i+1,255);
    end;
end;


function TSwitches.ReadItemsCfg(const s:string):boolean;

  function checkitem(P:PSwitchItem):boolean;{$ifndef FPC}far;{$endif}
  begin
    { empty items are not equivalent to others !! }
    CheckItem:=((S='') and (P^.Param='')) or
               ((Length(S)>0) and (P^.Param=Copy(s,1,length(P^.Param))));
  end;

var
  FoundP : PSwitchItem;
  code : integer;
begin
  FoundP:=Items^.FirstThat(@checkitem);
  if assigned(FoundP) then
   begin
     case FoundP^.Typ of
      ot_Select  : SelNr[SwitchesMode]:=Items^.IndexOf(FoundP);
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
     OptimizingGoalSwitches^.WriteItemsCfg;
     ProcessorSwitches^.WriteItemsCfg;
     AsmReaderSwitches^.WriteItemsCfg;
     AsmInfoSwitches^.WriteItemsCfg;
     AsmOutputSwitches^.WriteItemsCfg;
     DirectorySwitches^.WriteItemsCfg;
     MemorySwitches^.WriteItemsCfg;
     ConditionalSwitches^.WriteItemsCfg;
     LibLinkerSwitches^.WriteItemsCfg;
     DebugInfoSwitches^.WriteItemsCfg;
     ProfileInfoSwitches^.WriteItemsCfg;
     BrowserSwitches^.WriteItemsCfg;
     {MemorySizeSwitches^.WriteItemsCfg;}
     WriteCustom;
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
  res : boolean;
  OldSwitchesMode,i : TSwitchMode;
begin
  assign(CfgFile,fn);
  {$I-}
   reset(CfgFile);
  {$I+}
  if ioresult<>0 then
   begin
     SetDefaultSwitches;
     exit;
   end;
  OldSwitchesMode:=SwitchesMode;
  SwitchesMode:=om_Normal;
  while not eof(CfgFile) do
   begin
     readln(CfgFile,s);
     s:=LTrim(s);
     if (length(s)>=2) and (s[1]='-') then
      begin
      c:=s[2];
      res:=false;
      Delete(s,1,2);
      case c of
       'a' : res:=AsmInfoSwitches^.ReadItemsCfg(s);
       'A' : res:=AsmOutputSwitches^.ReadItemsCfg(s);
       'b' : res:=BrowserSwitches^.ReadItemsCfg(s);
       'C' : begin
               res:=CodegenSwitches^.ReadItemsCfg(s);
               if not res then
                 res:=MemorySwitches^.ReadItemsCfg(s);
             end;
       'd' : res:=ConditionalSwitches^.ReadItemsCfg(s);
       'F' : res:=DirectorySwitches^.ReadItemsCfg(s);
       'g' : res:=DebugInfoSwitches^.ReadItemsCfg(s);
       'O' : begin
               res:=true;
               if not OptimizationSwitches^.ReadItemsCfg(s) then
                 if not ProcessorSwitches^.ReadItemsCfg(s) then
                   res:=OptimizingGoalSwitches^.ReadItemsCfg(s);
             end;
       'p' : res:=ProfileInfoSwitches^.ReadItemsCfg(s);
       'R' : res:=AsmReaderSwitches^.ReadItemsCfg(s);
       'S' : res:=SyntaxSwitches^.ReadItemsCfg(s);
       'T' : res:=TargetSwitches^.ReadItemsCfg(s);
       'v' : res:=VerboseSwitches^.ReadItemsCfg(s);
       'X' : res:=LibLinkerSwitches^.ReadItemsCfg(s);
       end;
      { keep all others as a string }
      if not res then
       CustomArg[SwitchesMode]:=CustomArg[SwitchesMode]+' -'+c+s;
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
       end
      else;
   end;
  close(CfgFile);
  SwitchesMode:=OldSwitchesMode;
end;


function  GetSourceDirectories : string;
var
  P : PStringItem;
  S : String;
  c : char;
  function checkitem(P:PSwitchItem):boolean;{$ifndef FPC}far;{$endif}
  begin
    CheckItem:=(P^.Typ=ot_string) and (P^.Param=c);
  end;
begin
  GetSourceDirectories:='';
  c:='u';
  P:=DirectorySwitches^.Items^.FirstThat(@CheckItem);
  S:='';
  if assigned(P) then
    S:=P^.Str[SwitchesMode];
  c:='i';
  P:=DirectorySwitches^.Items^.FirstThat(@CheckItem);
  if assigned(P) then
    S:=P^.Str[SwitchesMode]+';'+S;
  if S='' then
    GetSourceDirectories:=SourceDirs+';'
  else
    GetSourceDirectories:=SourceDirs+';'+S+';';
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
     AddBooleanItem('N~o~tes','n');
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
  New(OptimizingGoalSwitches,InitSelect('O'));
  with OptimizingGoalSwitches^ do
    begin
       AddSelectItem('Generate ~f~aster code','G');
       AddSelectItem('Generate s~m~aller code','g');
    end;
  New(OptimizationSwitches,Init('O'));
  with OptimizationSwitches^ do
   begin
     AddBooleanItem('Use regis~t~er-variables','r');
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
     AddSelectItem('~D~irect assembler','direct');
     AddSelectItem('~A~T&T style assembler','att');
     AddSelectItem('~I~ntel style assembler','intel');
   end;
  New(AsmInfoSwitches,Init('a'));
  with AsmInfoSwitches^ do
   begin
     AddBooleanItem('~L~ist source','l');
     AddBooleanItem('list ~r~egister allocation','r');
     AddBooleanItem('list ~t~emp allocation','t');
   end;
  New(AsmOutputSwitches,InitSelect('A'));
  with AsmOutputSwitches^ do
   begin
     AddSelectItem('Use ~G~NU as','as');
     AddSelectItem('Use ~N~ASM coff','nasmcoff');
     AddSelectItem('Use NASM ~e~lf','nasmelf');
     AddSelectItem('Use NASM ~o~bj','nasmobj');
     AddSelectItem('Use ~M~ASM','masm');
     AddSelectItem('Use ~T~ASM','tasm');
     AddSelectItem('Use ~c~off','coff');
     AddSelectItem('Use ~p~ecoff','pecoff');
   end;
  New(BrowserSwitches,InitSelect('b'));
  with BrowserSwitches^ do
   begin
     AddSelectItem('N~o~ browser','-');
     AddSelectItem('Only Glob~a~l browser','+');
     AddSelectItem('~L~ocal and global browser','l');
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
     AddStringItem('~E~XE & PPU directories','E',true);
   end;

  New(LibLinkerSwitches,InitSelect('X'));
  with LibLinkerSwitches^ do
   begin
     AddSelectItem('~D~ynamic libraries','D');
     AddSelectItem('~S~tatic libraries','S');
   end;
  New(DebugInfoSwitches,InitSelect('g'));
  with DebugInfoSwitches^ do
   begin
     AddSelectItem('~S~trip all debug symbols from executable','-');
     AddSelectItem('Generate ~d~ebug symbol information','');
     { AddSelectItem('Generate ~d~bx symbol information','d');
       does not work anyhow (PM) }
   end;
  New(ProfileInfoSwitches,InitSelect('p'));
  with ProfileInfoSwitches^ do
   begin
     AddSelectItem('~N~o profile information','-');
     AddSelectItem('Generate profile code for g~p~rof','g');
   end;
  {New(MemorySizeSwitches,Init('C'));
  with MemorySizeSwitches^ do
   begin
     AddLongIntItem('~S~tack size','s');
     AddLongIntItem('Local ~h~eap size','h');
   end;}
  SwitchesPath:=LocateFile(SwitchesName);
  if SwitchesPath='' then
    SwitchesPath:=SwitchesName;
  SwitchesPath:=FExpand(SwitchesPath);

end;

procedure SetDefaultSwitches;
var
   i,OldSwitchesMode : TSwitchMode;

begin
  { setup some useful defaults }
  OldSwitchesMode:=SwitchesMode;
  for i:=low(TSwitchMode) to high(TSwitchMode) do
    begin
       SwitchesMode:=i;
       { default is Pentium }
       ProcessorSwitches^.SetCurrSel(1);
       { AT&T reader }
       AsmReaderSwitches^.SetCurrSel(1);
       { 128k stack }
       MemorySwitches^.SetLongintItem(0,65536*2);
       { 2 MB heap }
       MemorySwitches^.SetLongintItem(1,1024*1024*2);
       { goto/lable allowed }
       SyntaxSwitches^.SetBooleanItem(3,true);
       case i of
          om_debug:
            begin
               { debugging info on }
               DebugInfoSwitches^.SetCurrSel(1);
               { range checking }
               CodegenSwitches^.SetBooleanItem(0,true);
               { io checking }
               CodegenSwitches^.SetBooleanItem(2,true);
               { overflow checking }
               CodegenSwitches^.SetBooleanItem(3,true);
            end;
          om_normal:
            begin
               OptimizationSwitches^.SetBooleanItem(2,true);
            end;
          om_release:
            begin
               OptimizationSwitches^.SetBooleanItem(2,true);
               OptimizationSwitches^.SetBooleanItem(3,true);
            end;
       end;
       { set appriopriate default target }
{$ifdef go32v2}
       TargetSwitches^.SetCurrSel(1);
{$endif}
{$ifdef linux}
       TargetSwitches^.SetCurrSel(2);
{$endif}
{$ifdef win32}
       TargetSwitches^.SetCurrSel(4);
{$endif}
{$ifdef os2}
       TargetSwitches^.SetCurrSel(3);
{$endif}
    end;
  SwitchesMode:=OldSwitchesMode;
end;

procedure DoneSwitches;
begin
  dispose(SyntaxSwitches,Done);
  dispose(VerboseSwitches,Done);
  dispose(CodegenSwitches,Done);
  dispose(OptimizationSwitches,Done);
  dispose(OptimizingGoalSwitches,Done);
  dispose(ProcessorSwitches,Done);
  dispose(BrowserSwitches,Done);
  dispose(TargetSwitches,Done);
  dispose(AsmReaderSwitches,Done);
  dispose(ConditionalSwitches,Done);
  dispose(MemorySwitches,Done);
  {dispose(MemorySizeSwitches,Done);}
  dispose(DirectorySwitches,Done);
  dispose(DebugInfoSwitches,Done);
  dispose(LibLinkerSwitches,Done);
  dispose(ProfileInfoSwitches,Done);

end;


end.
{
  $Log$
  Revision 1.19  2000-03-07 22:52:50  pierre
   + Assembler tab in Options|Compiler

  Revision 1.18  2000/03/07 21:17:29  pierre
   +ASMInfoSwitches AsmOutputSwitches

  Revision 1.17  2000/02/04 14:34:47  pierre
  readme.txt

  Revision 1.16  2000/02/04 00:05:20  pierre
   * -Fi must also be used for GetSourceDirectories

  Revision 1.15  2000/01/10 15:52:53  pierre
    * use default command line switches only if fp.cfg not found

  Revision 1.14  1999/10/14 14:22:23  florian
    * if no ini file is found the ide uses some useful defaults

  Revision 1.13  1999/04/29 09:36:12  peter
    * fixed hotkeys with Compiler switches
    * fixed compiler status dialog
    * Run shows again the output

  Revision 1.12  1999/03/23 15:11:34  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.11  1999/03/12 01:14:01  peter
    * flag if trytoopen should look for other extensions
    + browser tab in the tools-compiler

  Revision 1.10  1999/02/16 12:46:38  pierre
   * String items can also be separated by spaces

  Revision 1.9  1999/02/10 09:45:55  pierre
    * MemorySizeSwitches Removed (was duplicate of MemorySwitches !)
    * Added missing disposes at exit

  Revision 1.8  1999/02/08 17:38:52  pierre
   + added CustomArg

  Revision 1.7  1999/02/06 00:07:48  florian
    * speed/size optimization is now a radio button

  Revision 1.6  1999/02/05 13:51:44  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.5  1999/02/05 12:12:00  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.4  1999/02/04 13:32:10  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.3  1999/01/12 14:29:39  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.2  1999/01/04 11:49:50  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:52  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

}