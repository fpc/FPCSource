{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018-2021 by the Free Pascal development team
    Original author: Michael van Canneyt

    New common base for all pascal code generator units in fcl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit pascodegen;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

const
  DefaultDottedDefine = 'FPC_DOTTEDUNITS';

Type
  TCodegenLogType = (cltInfo,cltWarning);
  TCodegenLogTypes = Set of TCodegenLogType;
  TCodeGeneratorLogEvent = Procedure (Sender : TObject; LogType : TCodegenLogType; Const Msg : String) of object;
  TCodeSection = (csUnknown, csConst, csType, csVar, csResourcestring, csDeclaration);
  TDottedUnitsSupport = (dusNone,  // Do not enabled support dotted units
                         dusUses,  // Split uses in dotted and non-dotted
                         dusFull   // Split uses in dotted and non-dotted, allow unitname to be dotted as well
                         );

  { TPascalCodeGenerator }

  TPascalCodeGenerator = Class(TComponent)
  Private
    FAddTimeStamp: Boolean;
    FDottedDefine: String;
    FDottedExtraUnits: String;
    FDottedUnitsSupport: TDottedUnitsSupport;
    FExtraUnits: String;
    FKeywordPrefix: String;
    FKeywordSuffix: String;
    FLicenseText: TStrings;
    FOnLog: TCodeGeneratorLogEvent;
    FOutputUnitName: String;
    FSource : TStrings;
    Findent : String;
    FSections : Array of TCodeSection;
    FSectionCount : Integer;
    FSwitches: TStrings;
    function GetDottedDefine: String;
    function GetSection: TCodeSection;
    procedure SetLicenseText(AValue: TStrings);
    procedure SetSection(AValue: TCodeSection);
    procedure SetSwitches(AValue: TStrings);
  Protected
    // Source manipulation
    Procedure DoLog(Const Msg : String; AType : TCodegenLogType = cltInfo);
    Procedure DoLog(Const Fmt : String; Args : Array of const; AType : TCodegenLogType = cltInfo);
    Function BaseUnits : String; virtual;
    Function DottedBaseUnits : String; virtual;
  Public
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // Emit section type word
    Procedure EnsureSection(aSection : TCodeSection);
    Procedure PushSection(ASection : TCodeSection = csUnknown);
    Function PopSection : TCodeSection;
    Procedure CreateHeader; virtual;
    Procedure CreateUnitClause; virtual;
    Procedure Indent;
    Procedure Undent;
    Function IsKeyWord (Const S : String) : Boolean; virtual;
    Function EscapeKeyWord(Const S : String; ForceAmpersand : Boolean = false) : String; virtual;
    Function MakePascalString(const S: String; AddQuotes: Boolean=False): String;
    Function PrettyPrint(Const S: string): String;
    Procedure AddLn(Const aLine: string);
    Procedure AddLn(Const TheLines : array of string);
    Procedure AddLn(Const TheLines : TStrings);
    Procedure AddLn(Const Fmt: string; Args : Array of const);
    Procedure Comment(Const AComment : String; Curly : Boolean = False);
    Procedure Comment(Const AComment : Array of String);
    Procedure Comment(Const AComment : TStrings);
    Procedure ClassComment(Const AClassName: String); virtual;
    Procedure ClassHeader(Const AClassName: String); deprecated 'use ClassComment instead';
    Procedure SimpleMethodBody(Lines: Array of string); virtual;
    procedure SaveToStream(const AStream: TStream);
    Procedure SaveToFile(Const AFileName : string);
    Property Source : TStrings Read FSource; // output
    Property CurrentSection : TCodeSection Read GetSection Write SetSection;
  Published
    Property OutputUnitName : String Read FOutputUnitName Write FOutputUnitName;
    Property ExtraUnits : String Read FExtraUnits Write FExtraUnits;
    Property DottedExtraUnits : String Read FDottedExtraUnits Write FDottedExtraUnits;
    Property DottedDefine : String Read GetDottedDefine Write FDottedDefine;
    Property DottedUnitsSupport : TDottedUnitsSupport Read FDottedUnitsSupport Write FDottedUnitsSupport;
    Property LicenseText : TStrings Read FLicenseText Write SetLicenseText;
    Property Switches : TStrings Read FSwitches Write SetSwitches;
    Property OnLog : TCodeGeneratorLogEvent Read FOnLog Write FOnlog;
    Property AddTimeStamp : Boolean Read FAddTimeStamp Write FAddTimeStamp;
    Property KeywordSuffix : String Read FKeywordSuffix Write FKeywordSuffix;
    Property KeywordPrefix : String Read FKeywordPrefix Write FKeywordPrefix;
  end;

implementation

{ TPascalCodeGenerator }

procedure TPascalCodeGenerator.Indent;
begin
  FIndent:=FIndent+StringOfChar(' ',2);
end;

procedure TPascalCodeGenerator.Undent;

Var
  L : Integer;
begin
  L:=Length(Findent);
  if L>0  then
    FIndent:=Copy(FIndent,1,L-2)
end;

function TPascalCodeGenerator.IsKeyWord(const S: String): Boolean;

Const
   KW=';absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'+
       'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'+
       'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'+
       'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'+
       'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'+
       'as;class;dispinterface;except;exports;finalization;finally;initialization;'+
       'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
       'private;published;length;setlength;';

begin
  Result:=Pos(';'+lowercase(S)+';',KW)<>0;
end;

function TPascalCodeGenerator.EscapeKeyWord(const S: String; ForceAmpersand : Boolean = false): String;
begin
  Result:=S;
  if IsKeyWord(S) then
    if ForceAmpersand then
      Result:='&'+Result
    else
      Result:=KeywordPrefix+Result+KeywordSuffix
end;

procedure TPascalCodeGenerator.AddLn(const aLine: string);

begin
  FSource.Add(FIndent+aLine);
end;

procedure TPascalCodeGenerator.AddLn(const TheLines: array of string);

Var
  S : String;

begin
  For s in TheLines do
    Addln(S);
end;

procedure TPascalCodeGenerator.AddLn(const TheLines: TStrings);
Var
  S : String;

begin
  For s in TheLines do
    Addln(S);
end;

procedure TPascalCodeGenerator.AddLn(const Fmt: string; Args: array of const);
begin
  AddLn(Format(Fmt,Args));
end;

procedure TPascalCodeGenerator.Comment(const AComment: String; Curly: Boolean);
begin
  if Curly then
    AddLn('{ '+AComment+' }')
  else
    AddLn('// '+AComment);
end;

procedure TPascalCodeGenerator.Comment(const AComment: array of String);
begin
  AddLn('{');
  Indent;
  AddLn(AComment);
  Undent;
  AddLn('}');
end;

procedure TPascalCodeGenerator.Comment(const AComment: TStrings);
begin
  AddLn('{');
  Indent;
  AddLn(AComment);
  Undent;
  AddLn('}');
end;

constructor TPascalCodeGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSource:=TStringList.Create;
  FLicenseText:=TstringList.Create;
  FSwitches:=TStringList.Create;
  FSwitches.Add('MODE ObjFPC');
  FSwitches.Add('H+');
  FDottedDefine:=DefaultDottedDefine;
  SetLength(FSections,0);
  FSectionCount:=0;
  PushSection(csUnknown);
  FKeywordPrefix:='&';
end;

destructor TPascalCodeGenerator.Destroy;
begin
  FreeAndNil(FSwitches);
  FreeAndNil(FLicenseText);
  FreeAndNil(FSource);
  inherited Destroy;
end;

procedure TPascalCodeGenerator.EnsureSection(aSection: TCodeSection);

Const
  SectionKeyWords : Array[TCodeSection] of string
    = ('', 'Const', 'Type', 'Var', 'Resourcestring', '');

begin
  If CurrentSection<>aSection then
    begin
    CurrentSection:=aSection;
    if SectionKeyWords[CurrentSection]<>'' then
      AddLn(SectionKeyWords[CurrentSection]);
    end;
end;

procedure TPascalCodeGenerator.PushSection(ASection : TCodeSection = csUnknown);
begin
  if FSectionCount=Length(FSections) then
    SetLength(FSections,FSectionCount+10);
  FSections[FSectionCount]:=ASection;
  Inc(FSectionCount);
end;

function TPascalCodeGenerator.PopSection: TCodeSection;
begin
  if FSectionCount=0 then
    Result:=csUnknown
  else
    begin
    Dec(FSectionCount);
    Result:=FSections[FSectionCount];
    end;

end;

procedure TPascalCodeGenerator.SaveToStream(const AStream : TStream);

begin
  FSource.SaveToStream(AStream)
end;

procedure TPascalCodeGenerator.SaveToFile(const AFileName: string);

Var
  F : TFileStream;
  B : Boolean;

begin
  B:=False;
  F:=Nil;
  try
    B:=(Source.Count=0) and (OutputUnitName='');
    if B then
      OutputUnitname:=ChangeFileExt(ExtractFileName(AFileName),'');
    F:=TFileStream.Create(aFilename,fmCreate);
    SaveToStream(F);
  finally
    F.Free;
    if B then
      OutputUnitName:='';
  end;
end;

procedure TPascalCodeGenerator.SetSection(AValue: TCodeSection);
begin
  if GetSection=AValue then
     Exit;
  FSections[FSectionCount-1]:=AValue;
end;

procedure TPascalCodeGenerator.SetSwitches(AValue: TStrings);
begin
  if FSwitches=AValue then Exit;
  FSwitches.Assign(AValue);
end;

function TPascalCodeGenerator.GetSection: TCodeSection;
begin
  Result:=FSections[FSectionCount-1];
end;

function TPascalCodeGenerator.GetDottedDefine: String;
begin
  Result:=FDottedDefine;
  if Result='' then
    Result:=DefaultDottedDefine;
end;

procedure TPascalCodeGenerator.SetLicenseText(AValue: TStrings);
begin
  if FLicenseText=AValue then Exit;
  FLicenseText.Assign(AValue);
end;

procedure TPascalCodeGenerator.DoLog(const Msg: String; AType: TCodegenLogType);
begin
  If Assigned(FOnLog) then
    FOnLog(Self,Atype,Msg);
end;

procedure TPascalCodeGenerator.DoLog(const Fmt: String; Args: array of const;
  AType: TCodegenLogType);
begin
  DoLog(Format(Fmt,Args),AType);
end;

procedure TPascalCodeGenerator.CreateHeader;

  Function Combine(B,S : String) : string;

  begin
    Result:=S;
    if (B<>'') then
      if (S<>'') then
        begin
        if (B[Length(B)]<>',') then
          B:=B+',';
        Result:=B+S;
        end
      else
        Result:=B;
  end;

Var
  S : String;

begin
  if LicenseText.Count>0 then
    Comment(LicenseText);
  if AddTimeStamp then
    Comment('Generated on: '+DateTimeToStr(Now));
  For S in Switches do
    addln('{$%s}',[S]);
  addln('');
  addln('interface');
  addln('');

  if DottedUnitsSupport<>dusNone then
    begin
    addln('{$IFDEF %s}',[DottedDefine]);
    addln('uses %s;',[Combine(DottedBaseUnits,DottedExtraUnits)]);
    addln('{$ELSE %s}',[DottedDefine]);
    end;
  addln('uses %s;',[Combine(BaseUnits,ExtraUnits)]);
  if DottedUnitsSupport<>dusNone then
    addln('{$ENDIF %s}',[DottedDefine]);
  addln('');
end;

procedure TPascalCodeGenerator.CreateUnitClause;
begin
  if DottedUnitsSupport=dusFull then
    addln('{$IFNDEF %s}',[DottedDefine]);
  AddLn('Unit %s;',[OutputUnitName]);
  if DottedUnitsSupport=dusFull then
    addln('{$ENDIF %s}',[DottedDefine]);
  AddLn('');
end;

procedure TPascalCodeGenerator.SimpleMethodBody(Lines: array of string);

Var
   S : String;

begin
  AddLn('');
  AddLn('begin');
  Indent;
  For S in Lines do
    AddLn(S);
  Undent;
  AddLn('end;');
  AddLn('');
end;

function TPascalCodeGenerator.BaseUnits: String;
begin
  Result:='';
end;

function TPascalCodeGenerator.DottedBaseUnits: String;
begin
  Result:='';
end;

function TPascalCodeGenerator.MakePascalString(const S: String; AddQuotes: Boolean
  ): String;

begin
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
  if AddQuotes then
    Result:=''''+Result+'''';
end;

function TPascalCodeGenerator.PrettyPrint(const S: string): String;

begin
  If (S='') then
    Result:=''
  else
    Result:=Upcase(S[1])+Copy(S,2,Length(S)-1);
end;

procedure TPascalCodeGenerator.ClassComment(const AClassName: String);

begin
  AddLn('');
  AddLn('{ '+StringOfChar('-',68));
  AddLn('  '+AClassName);
  AddLn('  '+StringOfChar('-',68)+'}');
  AddLn('');
end;

procedure TPascalCodeGenerator.ClassHeader(const AClassName: String);
begin
  ClassComment(AClassName);
end;

end.

