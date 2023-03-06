unit rewritemakefile;

{$mode ObjFPC}{$H+}

interface

uses strutils, regexpr, sysutils, classes, types, prefixer;

Type

  { TRewriteMakeFile }
  TMakeFileToolLogEvent = procedure(Sender : TObject; EventType : TEventType; Const Msg : String) of object;

  TRewriteMakeFile = Class(TComponent)
  private
    FAliasesFileName: String;
    FNew,
    FCommon,
    FNames,
    FSkip,
    FAliases : TStrings;
    FOnLog: TMakeFileToolLogEvent;
    FCommonUnitsFileName: String;
    FSkipUnitsFileName: String;

    procedure SetAliasesFileName(AValue: String);
    procedure SetCommonUnitsFileName(AValue: String);
    procedure SetSkipUnitsFileName(AValue: String);
  Protected
    procedure DoMsg(const aFmt: String; const aArgs: array of const;
      EventType: TEventType=etInfo); overload;
    procedure DoMsg(const aMessage: String; EventType: TEventType=etInfo); overload;

    procedure LoadNames(const aFileName: String; aAliases, aNames: TStrings); virtual;
    procedure addRecipe(aRecipe: TStrings); virtual;
    function GetNextLine(aLines: TStrings; var I: integer; var aLine: String): Boolean;
    function GetDottedUnitSrc(const aUnit,DottedUnit, aExt : String) : String; virtual;
  Public
    class function ExtractSourceExt(aLine, aUnit: string): String;
    class function ReplaceSourceFile(aLine, aUnit: string): String;
    class function CheckContinue(aLine: String): Boolean;
    class function CorrectSpaces(S: String; aIndent: String): string;
    class procedure FixTabs(sl: TStrings);
    class function IsRule(aLine: String): Boolean;
    class function ReplaceUnits(const aLine: string; aUnitNames: TStrings): String;
    class function ReplaceWord(aLine, aName, aFull: String): String;
    class function StripMacroChars(S: String): String;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure HandleMakeFile(aFileName: string);
    Property AliasesFileName : String Read FAliasesFileName Write SetAliasesFileName;
    Property CommonUnitsFileName : String Read FCommonUnitsFileName Write SetCommonUnitsFileName;
    Property SkipUnitsFileName : String Read FSkipUnitsFileName Write SetSkipUnitsFileName;
    Property OnLog : TMakeFileToolLogEvent Read FOnLog Write FOnLog;
  end;

Implementation

class function TRewriteMakeFile.ReplaceWord(aLine, aName, aFull: String): String;

var
  RE : TRegExpr;

begin
  RE:=TRegExpr.Create('\b'+aName+'\b');
  try
    RE.ModifierI:=True;
    Result:=RE.Replace(aLine,aFull);
//    Writeln(aLine,': ',aName,' -> ',aFull,' = ',Result);
  finally
    RE.Free;
  end;
end;


class function TRewriteMakeFile.ReplaceUnits(const aLine: string; aUnitNames: TStrings): String;

Var
  res,aName,aFull : String;

begin
  Res:=aLine;
  for aName in aUnitNames do
    begin
    aFull:='$('+UpperCase(aName)+'UNIT)';
    Res:=ReplaceWord(Res,aName,aFull);
    end;
  Result:=Res;
end;

procedure TRewriteMakeFile.SetAliasesFileName(AValue: String);
begin
  if FAliasesFileName=AValue then Exit;
  FAliasesFileName:=AValue;
  if aValue='' then
    FAliases.Clear;
  FNames.Clear;
  LoadNames(aValue,FAliases,FNames);
end;

procedure TRewriteMakeFile.SetCommonUnitsFileName(AValue: String);
begin
  if FCommonUnitsFileName=AValue then Exit;
  FCommonUnitsFileName:=AValue;
  if aValue='' then
    FCommon.Clear
  else
    FCommon.LoadFromFile(aValue);
end;

procedure TRewriteMakeFile.SetSkipUnitsFileName(AValue: String);
begin
  if FSkipUnitsFileName=AValue then Exit;
  FSkipUnitsFileName:=AValue;
  if aValue='' then
    FSkip.Clear
  else
    FSkip.LoadFromFile(aValue);
end;

procedure TRewriteMakeFile.DoMsg(const aFmt: String;
  const aArgs: array of const; EventType: TEventType);

begin
  DoMsg(Format(aFmt,aArgs),EventType);
end;

procedure TRewriteMakeFile.DoMsg(const aMessage: String; EventType: TEventType);
begin
  if assigned(OnLog) then
    OnLog(Self,EventType, aMessage);
end;

procedure TRewriteMakeFile.LoadNames(const aFileName: String; aAliases,
  aNames: TStrings);

var
  I : integer;
  N,V : String;

begin
  aAliases.LoadFromFile(aFileName);
  for I:=0 to aAliases.Count-1 do
    begin
    aAliases.GetNameValue(I,N,V);
    if SameText(N,'unixcp') then
      Writeln('Aloha');
    aAliases[I]:=N+'='+TPrefixer.ApplyAliasRule(N,V);
    aNames.Add(N);
    end;
end;

class function TRewriteMakeFile.CheckContinue(aLine: String): Boolean;

Var
  L : Integer;

begin
  L:=Length(aLine);
  Result:=(L>0) and (aLine[L]='\');
end;

class function TRewriteMakeFile.IsRule(aLine: String): Boolean;

begin
  Result:=(aLine='') or (aLine[1]=#9)
end;

function TRewriteMakeFile.GetNextLine(aLines: TStrings; var I: integer;
  var aLine: String): Boolean;

begin
  Result:=I<aLines.Count-1;
  if Result then
    begin
    Inc(I);
    aLine:=aLines[i];
    end;
end;

function TRewriteMakeFile.GetDottedUnitSrc(const aUnit, DottedUnit, aExt: String
  ): String;
begin
  if FCommon.IndexOf(aUnit)=-1 then
    Result:='$(NSOSDIR)/'
  else
    Result:='$(NSINC)/';
  Result:=Result+DottedUnit+aExt
end;

class function TRewriteMakeFile.ReplaceSourceFile(aLine, aUnit: string): String;

  Procedure AddToResult(S: String);

  begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+S;
  end;

  Function ReplacePath(S : String) : String;

  begin
    if pos(aUnit+'.pp',S)>0 then
      Result:='$<'
    else if pos(aUnit+'.pas',S)>0 then
      Result:='$<'
    else
      Result:=S;
  end;

Var
  a : Array of string;
  S : String;

begin
  Result:='';
  A:=SplitString(aLine,' ');
  For S in a do
    begin
    if Pos(aUnit,S)=0 then
      AddToResult(S)
    else
      AddToResult(ReplacePath(S));
    end;
end;

class function TRewriteMakeFile.StripMacroChars(S: String): String;

begin
  if Pos('$(',S)=0 then
    Result:=S
  else
    begin
    Result:=StringReplace(S,'$(','',[]);
    Result:=StringReplace(Result,')','',[]);
    end;
end;

constructor TRewriteMakeFile.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAliases:=TStringList.Create;
  FNames:=TStringList.Create;
  FCommon:=TStringList.Create;
  FNew:=TStringList.Create;
  FSkip:=TStringList.Create;
end;

destructor TRewriteMakeFile.Destroy;
begin
  FreeAndNil(FSkip);
  FreeAndNil(FNew);
  FreeAndNil(FAliases);
  FreeAndNil(FNames);
  FreeAndNil(FCommon);
  inherited Destroy;
end;

class function TRewriteMakeFile.CorrectSpaces(S: String; aIndent: String
  ): string;

var
  len,aCount : Integer;

begin
  aCount:=0;
  len:=Length(aIndent);
  While (aCount<Length(S)) and (S[aCount+1]=' ') do
    inc(aCount);
  Result:=S;
  if aCount<Len then
    Result:=Copy(aIndent,1,Len-aCount)+Result
  else if aCount>Len then
    Delete(Result,1,aCount-Len);
end;

class function TRewriteMakeFile.ExtractSourceExt(aLine, aUnit: string): String;

Var
  a : Array of string;
  S : String;

begin
  Result:='';
  A:=SplitString(aLine,' ');
  For S in a do
    begin
    if Pos(aUnit,S)<>0 then
      begin
      Result:=ExtractFileExt(S);
      exit;
      end;
    end;

end;

procedure TRewriteMakeFile.addRecipe(aRecipe: TStrings);

var
  aTarget, aCompileLine, aExt, aUnit, aDottedUnit, aCasedUnit,  aLine,aDeps,aUpper,aIndent,UnitDeps : String;
  P,NameIdx,Idx,I,iRules : Integer;

begin
  aLine:=aRecipe[0];
  P:=Pos('$(PPUEXT)',aLine);
  aUnit:=Trim(Copy(aLine,1,P-1));
  if FSkip.IndexOf(aUnit)<>-1 then
    begin
    DoMsg('Skipping unit "%s"',[aUnit],etWarning);
    Exit;
    end;
  NameIdx:=FNames.IndexOf(aUnit);
  if NameIdx<>-1 then
    FNames.Delete(NameIdx);

  P:=Pos(':',aLine);
  aTarget:=Copy(aLine,1,P);
  aDeps:=Copy(aLine,P+1);
  aUpper:=UpperCase(aUnit);
  UnitDeps:=StripMacroChars(aUpper)+'_DEPS';
  FNew.Add('');
  aExt:=ExtractSourceExt(aDeps,aUnit);
  aDeps:=UNITDEPS+'='+Trim(ReplaceUnits(aDeps,FNames));
  if aDeps[Length(aDeps)]<>'\' then
    aDeps:=aDeps+'\';
  FNew.Add(aDeps);
  aIndent:=StringOfChar(' ',Length(UnitDeps)+1);
  i:=0;
  While CheckContinue(aLine) and GetNextLine(aRecipe,I,aLine) do
    begin
    if aExt='' then
      aExt:=ExtractSourceExt(aDeps,aUnit);
    aDeps:=CorrectSpaces(ReplaceUnits(aLine,FNames),aIndent);
    if aDeps[Length(aDeps)]<>'\' then
      aDeps:=aDeps+'\';
    FNew.Add(aDeps);
    end;
  FNew.Add(aIndent+'$('+UnitDeps+'_OS) '+'$('+UnitDeps+'_CPU)');
  FNew.Add('');
  FNew.Add(aTarget+' $('+UnitDeps+')');
  iRules:=I;
  While GetNextLine(aRecipe,I,aLine) and IsRule(aLine) do
    begin
    aCompileLine:=ReplaceSourceFile(aLine,aUnit);
    FNew.Add(ReplaceUnits(aCompileLine,FNames));
    end;
  Idx:=FAliases.IndexOfName(aUnit);
  if Idx<>-1 then
    begin
    FAliases.GetNameValue(Idx,aCasedUnit,aDottedunit);
    aTarget:=StringReplace(aTarget,aUnit,aDottedUnit,[]);
    FNew.Add('');
    FNew.Add(aTarget+' '+GetDottedUnitSrc(aUnit,aDottedUnit,aExt)+' $('+UnitDeps+')');
    I:=iRules;
    While GetNextLine(aRecipe,I,aLine) and IsRule(aLine) do
      begin
      aCompileLine:=ReplaceSourceFile(aLine,aUnit);
      FNew.Add(ReplaceUnits(aCompileLine,FNames));
      end;
    end;
  if NameIdx<>-1 then
    FNames.Insert(NameIdx,aUnit);
end;

class procedure TRewriteMakeFile.FixTabs(sl:TStrings);
var
  i,j,k : integer;
  s,s2  : string;
  isContinue : Boolean;
begin
  isContinue:=False;
  i:=0;
  while (i<sl.Count) do
    begin
    s:=sl[i];
    if Not IsContinue then
      begin
      if (s<>'') and (s[1] in [' ',#9]) then
        begin
        k:=0;
        j:=0;
        repeat
          inc(j);
          case s[j] of
            ' ' :
              inc(k);
            #9 :
              k:=(k+7) and not(7);
          else
              break;
          end;
        until (j=length(s));
        if k>7 then
          begin
          s2:='';
          Delete(s,1,j-1);
          while (k>7) do
           begin
           s2:=s2+#9;
           dec(k,8);
           end;
          while (k>0) do
           begin
           s2:=s2+' ';
           dec(k);
           end;
          sl[i]:=s2+s;
          end;
        end;
      end;
    IsContinue:=(S<>'') and (S[Length(S)]='\');
    inc(i);
    end;
end;



procedure TRewriteMakeFile.HandleMakeFile(aFileName: string);

Var
  aMakeFile : TStrings;

  Function DoGetNextLine(var I : integer; var aLine : String) : Boolean;

  begin
    Result:=GetNextLine(aMakeFile,I,aLine);
  end;

var
  i,P : Integer;
  aRecipe : TStrings;
  aSection,aLine : String;

begin
  aLine:='';
  aRecipe:=Nil;
  aMakeFile:=TStringList.Create;
  try
    aRecipe:=TstringList.Create;
    aMakeFile.LoadFromFile(aFileName);
    FixTabs(aMakeFile);
    I:=-1;
    While DoGetNextLine(I,aLine) do
      begin
      aLine:=aMakefile[I];
      if Copy(aLine,1,1)='[' then
        aSection:=LowerCase(Copy(aLine,2,Length(aLine)-2));
      Case asection of
      'rules',
      'prerules' :
        begin
        P:=Pos('$(PPUEXT)',aLine);
        if (P>0) and (Pos(':',aLine)>P) then
          begin
          aRecipe.Clear;
          aRecipe.Add(aLine);
          // Add continuation lines
          While CheckContinue(aLine) and DoGetNextLine(I,aLine) do
            aRecipe.Add(aLine);
          // Add compiler rules
          While DoGetNextLine(I,aLine) and (IsRule(aLine)) do
            aRecipe.add(aLine);
          addRecipe(aRecipe);
          Dec(I); // Go back to previous line.
          end
        else
          FNew.Add(aLine);
        end;
      'shared',
      'target':
        begin
        P:=Pos('=',aLine);
        if (P>0) and (IndexText(Trim(Copy(aLine,1,P-1)),['units','implicitunits','libunits'])<>-1)  then
          begin
          FNew.Add(ReplaceUnits(aLine,FNames));
          While CheckContinue(aLine) and DoGetNextLine(I,aLine) do
            FNew.Add(ReplaceUnits(aLine,FNames));
          end
        else
          FNew.Add(aLine);
        end;
      else
        FNew.Add(aLine);
      end;
      end;
//      ReplaceUnits(aMakefile[I],aNames);
    FNew.SaveToFile(aFileName+'.new');
  finally
    aMakeFile.Free;
    aRecipe.Free;
  end;
end;


end.

