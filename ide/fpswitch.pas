{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Berczi Gabor

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
  WUtils,
  FPConst;

const
     MinMemSize      = 1024;     { min. local heap and stack size }
     MaxMemSize      = 67107840; { max. local heap and stack size }

type
    TParamID =
      (idNone,idAlign,idRangeChecks,idStackChecks,idIOChecks,
       idOverflowChecks,idAsmDirect,idAsmATT,idAsmIntel,idAsmMot,
       idSymInfNone,idSymInfGlobalOnly,idSymInfGlobalLocal,
       idStackSize,idHeapSize,idStrictVarStrings,idExtendedSyntax,
       idMMXOps,idTypedAddress,idPackRecords,idPackEnum,idStackFrames,
       idReferenceInfo,idDebugInfo,idBoolEval,
       idLongString,idTypeInfo);

    TSwitchMode = (om_Normal,om_Debug,om_Release);

    TSwitchItemTyp = (ot_Select,ot_Boolean,ot_String,ot_Longint);

    PSwitchItem = ^TSwitchItem;
    TSwitchItem = object(TObject)
      Typ       : TSwitchItemTyp;
      Name      : string[50];
      Param     : string[10];
      ParamID   : TParamID;
      constructor Init(const n,p:string; AID: TParamID);
      function  NeedParam:boolean;virtual;
      function  ParamValue:string;virtual;
      function  ParamValueBool(SM: TSwitchMode):boolean;virtual;
      function  GetSwitchStr(SM: TSwitchMode): string; virtual;
      function  GetNumberStr(SM: TSwitchMode): string; virtual;
      function  GetOptionStr(SM: TSwitchMode): string; virtual;
      procedure Reset;virtual;
    end;

    PSelectItem = ^TSelectItem;
    TSelectItem = object(TSwitchItem)
      IsDefault : boolean;
      constructor Init(const n,p:string; AID: TParamID);
      { Select to avoid anything in config file }
      constructor InitDefault(const n:string);
    end;

    PBooleanItem = ^TBooleanItem;
    TBooleanItem = object(TSwitchItem)
      IsSet : array[TSwitchMode] of boolean;
      constructor Init(const n,p:string; AID: TParamID);
      function  NeedParam:boolean;virtual;
      procedure Reset;virtual;
      function  GetSwitchStr(SM: TSwitchMode): string; virtual;
      function  ParamValueBool(SM: TSwitchMode):boolean;virtual;
    end;

    PStringItem = ^TStringItem;
    TStringItem = object(TSwitchItem)
      Str : array[TSwitchMode] of string;
      multiple : boolean;
      constructor Init(const n,p:string;AID: TParamID; mult:boolean);
      function  NeedParam:boolean;virtual;
      function  ParamValue:string;virtual;
      procedure Reset;virtual;
    end;

    PLongintItem = ^TLongintItem;
    TLongintItem = object(TSwitchItem)
      Val : array[TSwitchMode] of longint;
      constructor Init(const n,p:string; AID: TParamID);
      function  NeedParam:boolean;virtual;
      function  ParamValue:string;virtual;
      function  GetNumberStr(SM: TSwitchMode): string; virtual;
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
      procedure AddSelectItem(const name,param:string; AID: TParamID);
      procedure AddDefaultSelect(const name:string);
      procedure AddBooleanItem(const name,param:string; AID: TParamID);
      procedure AddLongintItem(const name,param:string; AID: TParamID);
      procedure AddStringItem(const name,param:string;AID: TParamID;mult:boolean);
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
    OtherLinkerSwitches,
    DebugInfoSwitches,
    LinkAfterSwitches,
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

{ write/read the Switches to fpc.cfg file }
procedure WriteSwitches(const fn:string);
procedure ReadSwitches(const fn:string);

{ initialize }
procedure InitSwitches;
procedure SetDefaultSwitches;
procedure DoneSwitches;
function  GetSourceDirectories : string;

procedure GetCompilerOptionLines(C: PUnsortedStringCollection);

implementation

uses
  Dos,
  GlobType,
  FPString,FPVars,FPUtils;

var
  CfgFile : text;

{*****************************************************************************
            TSwitchItem
*****************************************************************************}

constructor TSwitchItem.Init(const n,p:string; AID: TParamID);
begin
  Inherited Init;
  Name:=n;
  Param:=p;
  ParamID:=AID;
end;


function TSwitchItem.NeedParam:boolean;
begin
  NeedParam:=false;
end;


function TSwitchItem.ParamValue:string;
begin
  ParamValue:='';
end;

function TSwitchItem.ParamValueBool(SM: TSwitchMode):boolean;
begin
  Abstract;
  ParamValueBool:=false;
end;

function TSwitchItem.GetSwitchStr(SM: TSwitchMode): string;
begin
  Abstract;
  GetSwitchStr:='';
end;

function TSwitchItem.GetNumberStr(SM: TSwitchMode): string;
begin
  Abstract;
  GetNumberStr:='';
end;

function TSwitchItem.GetOptionStr(SM: TSwitchMode): string;
begin
  Abstract;
  GetOptionStr:='';
end;

procedure TSwitchItem.Reset;
begin
end;


{*****************************************************************************
            TSelectItem
*****************************************************************************}

constructor TSelectItem.Init(const n,p:string; AID: TParamID);
begin
  Inherited Init(n,p,AID);
  Typ:=ot_Select;
  IsDefault:=false;
end;

constructor TSelectItem.InitDefault(const n:string);
begin
  Inherited Init(n,'',idNone);
  Typ:=ot_Select;
  IsDefault:=true;
end;

{*****************************************************************************
                TBooleanItem
*****************************************************************************}

constructor TBooleanItem.Init(const n,p:string; AID: TParamID);
begin
  Inherited Init(n,p,AID);
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

function TBooleanItem.ParamValueBool(SM: TSwitchMode):boolean;
begin
  ParamValueBool:=IsSet[SM];
end;

function TBooleanItem.GetSwitchStr(SM: TSwitchMode): string;
begin
  GetSwitchStr:=BoolToStr(IsSet[SM],'+','-');
end;


{*****************************************************************************
            TStringItem
*****************************************************************************}

constructor TStringItem.Init(const n,p:string; AID: TParamID; mult:boolean);
begin
  Inherited Init(n,p,AID);
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

constructor TLongintItem.Init(const n,p:string; AID: TParamID);
begin
  Inherited Init(n,p,AID);
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

function TLongintItem.GetNumberStr(SM: TSwitchMode): string;
begin
  GetNumberStr:=IntToStr(Val[SM]);
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


procedure TSwitches.AddSelectItem(const name,param:string; AID: TParamID);
begin
  Items^.Insert(New(PSelectItem,Init(name,Param,AID)));
end;


procedure TSwitches.AddDefaultSelect(const name:string);
begin
  Items^.Insert(New(PSelectItem,InitDefault(name)));
end;


procedure TSwitches.AddBooleanItem(const name,param:string; AID: TParamID);
begin
  Items^.Insert(New(PBooleanItem,Init(name,Param,AID)));
end;


procedure TSwitches.AddLongintItem(const name,param:string; AID: TParamID);
begin
  Items^.Insert(New(PLongintItem,Init(name,Param,AID)));
end;


procedure TSwitches.AddStringItem(const name,param:string;AID: TParamID;mult:boolean);
begin
  Items^.Insert(New(PStringItem,Init(name,Param,AID,mult)));
end;


function TSwitches.ItemCount:integer;
begin
  ItemCount:=Items^.Count;
end;


function TSwitches.ItemName(index:integer):string;
var
  P : PSwitchItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) then
   ItemName:=P^.Name
  else
   ItemName:='';
end;


function TSwitches.ItemParam(index:integer):string;
var
  P : PSwitchItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) then
   ItemParam:='-'+Prefix+P^.Param
  else
   ItemParam:='';
end;


function TSwitches.GetBooleanItem(index:integer):boolean;
var
  P : PBooleanItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) and (P^.Typ=ot_boolean) then
   GetBooleanItem:=P^.IsSet[SwitchesMode]
  else
   GetBooleanItem:=false;
end;


function TSwitches.GetLongintItem(index:integer):longint;
var
  P : PLongintItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) and (P^.Typ=ot_longint) then
   GetLongintItem:=P^.Val[SwitchesMode]
  else
   GetLongintItem:=0;
end;


function TSwitches.GetStringItem(index:integer):string;
var
  P : PStringItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) and (P^.Typ=ot_string) then
   GetStringItem:=P^.Str[SwitchesMode]
  else
   GetStringItem:='';
end;


procedure TSwitches.SetBooleanItem(index:integer;b:boolean);
var
  P : PBooleanItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) and (P^.Typ=ot_boolean) then
   P^.IsSet[SwitchesMode]:=b;
end;


procedure TSwitches.SetLongintItem(index:integer;l:longint);
var
  P : PLongintItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
  if assigned(P) and (P^.Typ=ot_longint) then
   P^.Val[SwitchesMode]:=l;
end;


procedure TSwitches.SetStringItem(index:integer;const s:string);
var
  P : PStringItem;
begin
  if index<ItemCount then
    P:=Items^.At(Index)
  else
    P:=nil;
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
         if P^.Param<>'/' then
           Writeln(CfgFile,' -'+Pref+P^.Param+P^.ParamValue);
     end;
  end;

var
  P : PSelectItem;
begin
  Pref:=Prefix;
  if IsSel then
    begin
      P:=Items^.At(SelNr[SwitchesMode]);
      if not P^.IsDefault then
        writeln(CfgFile,' '+ItemParam(SelNr[SwitchesMode]));
    end
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
    { but -dGDB didn't work because of this PM }
    CheckItem:=((P^.Param='') and ((S='') or (P^.typ=ot_String))) or
               ((Length(P^.Param)>0) and (P^.Param=Copy(s,1,length(P^.Param))));
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
  writeln(CfgFile,'# '+msg_automaticallycreateddontedit);
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
     OtherLinkerSwitches^.WriteItemsCfg;
     DebugInfoSwitches^.WriteItemsCfg;
     ProfileInfoSwitches^.WriteItemsCfg;
     LinkAfterSwitches^.WriteItemsCfg;
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
       's' : res:=LinkAfterSwitches^.ReadItemsCfg(s);
       'R' : res:=AsmReaderSwitches^.ReadItemsCfg(s);
       'S' : res:=SyntaxSwitches^.ReadItemsCfg(s);
       'T' : res:=TargetSwitches^.ReadItemsCfg(s);
       'v' : res:=VerboseSwitches^.ReadItemsCfg(s);
       'X' : begin
               res:=LibLinkerSwitches^.ReadItemsCfg(s);
               if not res then
                 res:=OtherLinkerSwitches^.ReadItemsCfg(s);
             end;
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
     AddBooleanItem(opt_delphi2extensions,'2',idNone);
     AddBooleanItem(opt_clikeoperators,'c',idNone);
     AddBooleanItem(opt_stopafterfirsterror,'e',idNone);
     AddBooleanItem(opt_allowlabelandgoto,'g',idNone);
     AddBooleanItem(opt_cplusplusstyledinline,'i',idNone);
     AddBooleanItem(opt_globalcmacros,'m',idNone);
     AddBooleanItem(opt_tp7compatibility,'o',idNone);
     AddBooleanItem(opt_delphicompatibility,'d',idNone);
     AddBooleanItem(opt_allowstaticinobjects,'s',idNone);
     { Useless as they are not passed to the compiler PM
     AddBooleanItem(opt_strictvarstrings,'/',idStrictVarStrings);
     AddBooleanItem(opt_extendedsyntax,'/',idExtendedSyntax);
     AddBooleanItem(opt_allowmmxoperations,'/',idMMXOps);  }
   end;
  New(VerboseSwitches,Init('v'));
  with VerboseSwitches^ do
   begin
     AddBooleanItem(opt_warnings,'w',idNone);
     AddBooleanItem(opt_notes,'n',idNone);
     AddBooleanItem(opt_hints,'h',idNone);
     AddBooleanItem(opt_generalinfo,'i',idNone);
     AddBooleanItem(opt_usedtriedinfo,'ut',idNone);
     AddBooleanItem(opt_all,'a',idNone);
     AddBooleanItem(opt_showallprocsonerror,'b',idNone);
   end;
  New(CodegenSwitches,Init('C'));
  with CodegenSwitches^ do
   begin
     AddBooleanItem(opt_rangechecking,'r',idRangeChecks);
     AddBooleanItem(opt_stackchecking,'t',idStackChecks);
     AddBooleanItem(opt_iochecking,'i',idIOChecks);
     AddBooleanItem(opt_overflowchecking,'o',idOverflowChecks);
   end;
  New(OptimizingGoalSwitches,InitSelect('O'));
  with OptimizingGoalSwitches^ do
    begin
       AddSelectItem(opt_generatefastercode,'G',idNone);
       AddSelectItem(opt_generatesmallercode,'g',idNone);
    end;
  New(OptimizationSwitches,Init('O'));
  with OptimizationSwitches^ do
   begin
{$ifdef I386}
     AddBooleanItem(opt_useregistervariables,'r',idNone);
     AddBooleanItem(opt_uncertainoptimizations,'u',idNone);
     AddBooleanItem(opt_level1optimizations,'1',idNone);
     AddBooleanItem(opt_level2optimizations,'2',idNone);
{$else not I386}
 {$ifdef m68k}
     AddBooleanItem(opt_level1optimizations,'a',idNone);
     AddBooleanItem(opt_useregistervariables,'x',idNone);
 {$endif m68k}
{$endif I386}
   end;
  New(ProcessorSwitches,InitSelect('O'));
  with ProcessorSwitches^ do
   begin
{$ifdef I386}
     AddSelectItem(opt_i386486,'p1',idNone);
     AddSelectItem(opt_pentiumandmmx,'p2',idNone);
     AddSelectItem(opt_pentiumpro,'p3',idNone);
{$else not I386}
 {$ifdef m68k}
     AddSelectItem(opt_m68000,'',idNone);
     AddSelectItem(opt_m68020,'2',idNone);
 {$endif m68k}
{$endif not I386}
   end;
  New(TargetSwitches,InitSelect('T'));
  with TargetSwitches^ do
   begin
{$ifdef I386}
     {AddSelectItem('DOS (GO32V~1~)','go32v1',idNone);}
     AddSelectItem('~D~OS (GO32V2)','go32v2',idNone);
     AddSelectItem('~F~reeBSD','freebsd',idNone);
     AddSelectItem('~L~inux','linux',idNone);
     AddSelectItem('~N~etBSD','netbsd',idNone);
     AddSelectItem('~O~S/2','os2',idNone);
     AddSelectItem('~W~IN32','win32',idNone);
     AddSelectItem('N~e~tWare','netware',idNone);
{$endif I386}
{$ifdef M68K}
     AddSelectItem('~A~miga','amiga',idNone);
     AddSelectItem('A~t~ari','atari',idNone);
     AddSelectItem('~L~inux','linux',idNone);
     AddSelectItem('~N~etBSD','netbsd',idNone);
     AddSelectItem('~P~alm OS','palmos',idNone);
     {AddSelectItem('~M~ac OS','macos',idNone); }
{$endif M68K}
   end;
  New(AsmReaderSwitches,InitSelect('R'));
  with AsmReaderSwitches^ do
   begin
{$ifdef I386}
     AddSelectItem(opt_directassembler,'direct',idAsmDirect);
     AddSelectItem(opt_attassembler,'att',idAsmATT);
     AddSelectItem(opt_intelassembler,'intel',idAsmIntel);
{$endif I386}
{$ifdef M68K}
     AddSelectItem(opt_motassembler,'mot',idAsmDirect);
{$endif M68K}
   end;
  New(AsmInfoSwitches,Init('a'));
  with AsmInfoSwitches^ do
   begin
     AddBooleanItem(opt_listsource,'l',idNone);
     AddBooleanItem(opt_listregisterallocation,'r',idNone);
     AddBooleanItem(opt_listtempallocation,'t',idNone);
   end;
  New(AsmOutputSwitches,InitSelect('A'));
  with AsmOutputSwitches^ do
   begin
     AddDefaultSelect(opt_usedefaultas);
{$ifdef I386}
     AddSelectItem(opt_usegnuas,'as',idNone);
     AddSelectItem(opt_usenasmcoff,'nasmcoff',idNone);
     AddSelectItem(opt_usenasmelf,'nasmelf',idNone);
     AddSelectItem(opt_usenasmobj,'nasmobj',idNone);
     AddSelectItem(opt_usemasm,'masm',idNone);
     AddSelectItem(opt_usetasm,'tasm',idNone);
     AddSelectItem(opt_usecoff,'coff',idNone);
     AddSelectItem(opt_usepecoff,'pecoff',idNone);
{$endif I386}
   end;
  New(BrowserSwitches,InitSelect('b'));
  with BrowserSwitches^ do
   begin
     AddSelectItem(opt_nobrowser,'-',idSymInfNone);
     AddSelectItem(opt_globalonlybrowser,'+',idSymInfGlobalOnly);
     AddSelectItem(opt_localglobalbrowser,'l',idSymInfGlobalLocal);
   end;
  New(ConditionalSwitches,Init('d'));
  with ConditionalSwitches^ do
   begin
     AddStringItem(opt_conditionaldefines,'',idNone,true);
   end;
  New(MemorySwitches,Init('C'));
  with MemorySwitches^ do
   begin
     AddLongintItem(opt_stacksize,'s',idStackSize);
     AddLongintItem(opt_heapsize,'h',idHeapSize);
   end;
  New(DirectorySwitches,Init('F'));
  with DirectorySwitches^ do
   begin
     AddStringItem(opt_unitdirectories,'u',idNone,true);
     AddStringItem(opt_includedirectories,'i',idNone,true);
     AddStringItem(opt_librarydirectories,'l',idNone,true);
     AddStringItem(opt_objectdirectories,'o',idNone,true);
     AddStringItem(opt_exeppudirectories,'E',idNone,true);
   end;

  New(LibLinkerSwitches,InitSelect('X'));
  with LibLinkerSwitches^ do
   begin
     AddDefaultSelect(opt_librariesdefault);
     AddSelectItem(opt_dynamiclibraries,'D',idNone);
     AddSelectItem(opt_staticlibraries,'S',idNone);
     AddSelectItem(opt_smartlibraries,'X',idNone);
   end;
  New(OtherLinkerSwitches,Init('X'));
  with OtherLinkerSwitches^ do
   begin
     AddBooleanItem(opt_stripalldebugsymbols,'s',idNone);
     AddBooleanItem(opt_forcestaticlibs,'t',idNone);
   end;
  New(DebugInfoSwitches,InitSelect('g'));
  with DebugInfoSwitches^ do
   begin
     AddSelectItem(opt_nogendebugsymbolinfo,'-',idNone);
     AddSelectItem(opt_gendebugsymbolinfo,'',idNone);
     AddSelectItem(opt_gensymbolandbacktraceinfo,'l',idNone);
     { AddSelectItem('Generate ~d~bx symbol information','d');
       does not work anyhow (PM) }
   end;
  New(LinkAfterSwitches,Init('s'));
  LinkAfterSwitches^.AddBooleanItem(opt_linkafter,'',idNone);
  New(ProfileInfoSwitches,InitSelect('p'));
  with ProfileInfoSwitches^ do
   begin
     AddSelectItem(opt_noprofileinfo,'-',idNone);
     AddSelectItem(opt_gprofinfo,'g',idNone);
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
{$ifdef i386}
       { default is Pentium }
       ProcessorSwitches^.SetCurrSel(1);
       { AT&T reader }
       AsmReaderSwitches^.SetCurrSel(1);
{$endif i386}
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
{$ifdef freebsd}
       TargetSwitches^.SetCurrSel(2);
{$endif}
{$ifdef linux}
{$ifdef i386}
       TargetSwitches^.SetCurrSel(3);
{$endif i386}
{$ifdef m68k}
       TargetSwitches^.SetCurrSel(2);
{$endif m68k}
{$endif linux}
{$ifdef os2}
       TargetSwitches^.SetCurrSel(4);
{$endif}
{$ifdef win32}
       TargetSwitches^.SetCurrSel(5);
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
  dispose(AsmOutputSwitches,Done);
  dispose(AsmInfoSwitches,Done);
  dispose(ConditionalSwitches,Done);
  dispose(MemorySwitches,Done);
  {dispose(MemorySizeSwitches,Done);}
  dispose(DirectorySwitches,Done);
  dispose(DebugInfoSwitches,Done);
  dispose(LibLinkerSwitches,Done);
  dispose(LinkAfterSwitches,Done);
  dispose(OtherLinkerSwitches,Done);
  dispose(ProfileInfoSwitches,Done);
end;

procedure GetCompilerOptionLines(C: PUnsortedStringCollection);
procedure AddLine(const S: string);
begin
  C^.Insert(NewStr(S));
end;
procedure ConstructSwitchModeDirectives(SM: TSwitchMode; const IfDefSym: string);
var SwitchParams: PStringCollection;
    MiscParams  : PStringCollection;
procedure AddSwitch(const S: string);
begin
  SwitchParams^.Insert(NewStr(S));
end;
procedure AddParam(const S: string);
begin
  MiscParams^.Insert(NewStr(S));
end;
procedure EnumSwitches(P: PSwitches);
procedure HandleSwitch(P: PSwitchItem); {$ifndef FPC}far;{$endif}
begin
  case P^.ParamID of
{    idAlign :}
    idRangeChecks    : AddSwitch('R'+P^.GetSwitchStr(SM));
    idStackChecks    : AddSwitch('S'+P^.GetSwitchStr(SM));
    idIOChecks       : AddSwitch('I'+P^.GetSwitchStr(SM));
    idOverflowChecks : AddSwitch('Q'+P^.GetSwitchStr(SM));
{    idAsmDirect      : if P^.GetParamValueBool[SM] then AddParam('ASMMODE DIRECT');
    idAsmATT         : if P^.GetParamValueBool[SM] then AddParam('ASMMODE ATT');
    idAsmIntel       : if P^.GetParamValueBool[SM] then AddParam('ASMMODE INTEL');
    idAsmMot         : if P^.GetParamValueBool[SM] then AddParam('ASMMODE MOT');}
{    idSymInfNone     : ;
    idSymInfGlobalOnly:;
    idSymInfGlobalLocal:if P^.ParamValueBool(SM) then AddSwitch('L+');}
{    idStackSize
    idHeapSize}
    idStrictVarStrings: AddSwitch('V'+P^.GetSwitchStr(SM));
    idExtendedSyntax  : AddSwitch('X'+P^.GetSwitchStr(SM));
    idMMXOps          : if P^.ParamValueBool(SM) then AddParam('MMX');
    idTypedAddress    : AddSwitch('T'+P^.GetSwitchStr(SM));
{    idPackRecords
    idPackEnum}
    idStackFrames     : AddSwitch('W'+P^.GetSwitchStr(SM));
    idReferenceInfo   : AddSwitch('Y'+P^.GetSwitchStr(SM));
    idDebugInfo       : AddSwitch('D'+P^.GetSwitchStr(SM));
    idBoolEval        : AddSwitch('B'+P^.GetSwitchStr(SM));
    idLongString      : AddSwitch('H'+P^.GetSwitchStr(SM));
    idTypeInfo        : AddSwitch('M'+P^.GetSwitchStr(SM));
   end;
end;
begin
  P^.Items^.ForEach(@HandleSwitch);
end;
var I: integer;
    S: string;
begin
  AddLine('{$IFDEF '+IfDefSym+'}');
  New(SwitchParams, Init(10,10));
  New(MiscParams, Init(10,10));
  EnumSwitches(LibLinkerSwitches);
  EnumSwitches(OtherLinkerSwitches);
  EnumSwitches(DebugInfoSwitches);
  EnumSwitches(ProfileInfoSwitches);
  EnumSwitches(SyntaxSwitches);
  EnumSwitches(VerboseSwitches);
  EnumSwitches(CodegenSwitches);
  EnumSwitches(OptimizationSwitches);
  EnumSwitches(OptimizingGoalSwitches);
  EnumSwitches(ProcessorSwitches);
  EnumSwitches(AsmReaderSwitches);
  EnumSwitches(AsmInfoSwitches);
  EnumSwitches(AsmOutputSwitches);
  EnumSwitches(TargetSwitches);
  EnumSwitches(ConditionalSwitches);
  EnumSwitches(MemorySwitches);
  EnumSwitches(BrowserSwitches);
  EnumSwitches(DirectorySwitches);
  S:='';
  for I:=0 to SwitchParams^.Count-1 do
  begin
    if I=0 then S:='{$' else S:=S+',';
    S:=S+PString(SwitchParams^.At(I))^;
  end;
  if S<>'' then S:=S+'}';
  if S<>'' then AddLine('  '+S);
  for I:=0 to MiscParams^.Count-1 do
    AddLine('  {$'+PString(MiscParams^.At(I))^+'}');
  Dispose(SwitchParams, Done); Dispose(MiscParams, Done);
  AddLine('{$ENDIF '+IfDefSym+'}');
end;
var SM: TSwitchMode;
begin
  for SM:=Low(TSwitchMode) to High(TSwitchMode) do
    ConstructSwitchModeDirectives(SM,SwitchesModeStr[SM]);
end;


end.
{
  $Log$
  Revision 1.7  2002-04-10 22:41:05  pierre
   * remove switches that have no command line option

  Revision 1.6  2002/03/16 14:53:23  armin
  + add netware target

  Revision 1.5  2002/02/20 15:06:52  pierre
   avoid to insert options in .cfg file if the option has no command line

  Revision 1.4  2001/11/24 02:06:43  carl
  * Renamed ppc.cfg -> fpc.cfg

  Revision 1.3  2001/08/29 23:31:27  pierre
   * fix some m68k specific options

  Revision 1.2  2001/08/07 21:27:34  pierre
   * avoid RTE 211 on At method with wrong index

  Revision 1.1  2001/08/04 11:30:24  peter
    * ide works now with both compiler versions

  Revision 1.1.2.11  2001/08/03 13:07:47  pierre
   * avoid crashes on m68k cpus

  Revision 1.1.2.10  2001/08/02 14:22:10  pierre
   * add some m68k specific switches for compiler

  Revision 1.1.2.9  2001/03/16 17:46:56  pierre
   * add some missing dispose

  Revision 1.1.2.8  2001/02/13 11:48:26  pierre
   + Strip debug nifo switch added

  Revision 1.1.2.7  2000/11/28 10:55:13  pierre
   + add freebsd target

  Revision 1.1.2.6  2000/11/14 17:40:44  pierre
   + External linking now optional

  Revision 1.1.2.5  2000/11/14 09:40:35  marco
   * Third batch renamefest

  Revision 1.1.2.4  2000/10/26 10:17:10  pierre
   * fix reading of -dGDB switch in cfg file

  Revision 1.1.2.3  2000/08/04 14:05:19  michael
  * Fixes from Gabor:
   [*] the IDE now doesn't disable Compile|Make & Build when all windows
       are closed, but there's still a primary file set
       (set bug 1059 to fixed!)

   [*] the IDE didn't read some compiler options correctly back from the
       FP.CFG file, for ex. the linker options. Now it read everything
       correctly, and also automatically handles smartlinking option synch-
       ronization.
       (set bug 1048 to fixed!)

  Revision 1.1.2.2  2000/07/15 21:38:47  pierre
   + Add default selection that does not write anything to config file
   + Add smart link

  Revision 1.1.2.1  2000/07/15 21:30:06  pierre
  * Wrong commit text

  Revision 1.1  2000/07/13 09:48:36  michael
  + Initial import

  Revision 1.23  2000/06/22 09:07:12  pierre
   * Gabor changes: see fixes.txt

  Revision 1.22  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.21  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.20  2000/03/08 16:51:50  pierre
   + -gl option support

  Revision 1.19  2000/03/07 22:52:50  pierre
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
