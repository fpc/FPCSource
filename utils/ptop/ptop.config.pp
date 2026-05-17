unit ptop.config;

{$modeswitch out}
{$modeswitch result}
{$modeswitch exceptions}
{$h+}

interface

uses
    {$ifdef FPC_DOTTEDUNITS}
    system.classes,
    {$else}
    classes,
    {$endif}
    ptop.types
    ;

{ To create options and defaults }
procedure CreateOptionsTable(out Option: OptionTable);
procedure SetKeywordTerminators(var Option: OptionTable);
procedure SetDefaultIndents(var Option: OptionTable);
Procedure SetDefaultConfigs(var Option : OptionTable);

{ To read an existing file or to generate a new file }
function ReadConfigFrom(S: TStream; var Option: OptionTable): boolean;
procedure GenerateNewConfig(S: TStream);

implementation

uses
  {$ifdef FPC_DOTTEDUNITS}
  system.typinfo,
  system.sysutils,
  {$else}
  typinfo,
  sysutils,
  {$endif}
  ptop.strutils
  ;

resourcestring
  E_UNKNOWN_KEYWORD = 'Unknown keyword: %s';
  E_UNKNOWN_OPTION  = 'Unknown option: %s';
  E_NO_TARGET       = 'No Pascal keyword specified';
  E_UNKNOWN_INDENT  = 'Unknown indent: %s';
  E_NO_VALUE        = 'No setting for this keyword';
  S_PROCESSED_CFG   = 'Processed config file: read %d lines';


Procedure CreateOptionsTable(Out Option : OptionTable);

Var Sym : KeySymbol;
    T : TTokenScope;

begin
  FOR sym := endsym TO othersym DO
    For T:=Low(TTokenScope) to High(TTokenScope) do
      begin
      NEW(option[T,sym]);
      option[T,sym]^.selected := [];
      option[T,sym]^.dindsym := [];
      option[T,sym]^.terminators := []
      END;
end;

Procedure SetKeywordTerminators(Var Option : OptionTable);

Var
  T : TTokenScope;

begin
  For T:=Low(TTokenScope) to High(TTokenScope) do
    begin
    option[t,casesym]^.terminators    := [ofsym];
    option[t,casevarsym]^.terminators := [ofsym];
    option[t,forsym]^.terminators     := [dosym];
    option[t,whilesym]^.terminators   := [dosym];
    option[t,withsym]^.terminators    := [dosym];
    option[t,ifsym]^.terminators      := [thensym];
    option[t,untilsym]^.terminators   := [endsym, untilsym, elsesym, semicolon];
    option[t,becomes]^.terminators    := [endsym, untilsym, elsesym, semicolon];
    option[t,openparen]^.terminators  := [closeparen];
    option[t,usessym]^.terminators    := [semicolon];
    end;
end;

Procedure SetDefaultIndents (Var Option : OptionTable);

Var
  T : TTokenScope;

begin
  For T := Low(TTokenScope) to High(TTokenScope) do
    begin
    option[t,recordsym]^.dindsym    := [endsym];
    option[t,funcsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
    option[t,procsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
    option[t,constsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
    option[t,typesym]^.dindsym      := [labelsym, constsym, typesym, varsym];
    option[t,varsym]^.dindsym       := [labelsym, constsym, typesym, varsym];
    option[t,beginsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
    option[t,publicsym]^.dindsym    := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,privatesym]^.dindsym   := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,protectedsym]^.dindsym := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,publishedsym]^.dindsym := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,finallysym]^.dindsym   := [trysym];
    option[t,exceptsym]^.dindsym    := [trysym];
    option[t,elsesym]^.dindsym      := [ifsym, thensym, elsesym];
    option[t,untilsym]^.dindsym     := [ifsym, thensym, elsesym, forsym, whilesym, withsym, colon, equals];
    option[t,endsym]^.dindsym       := [ifsym, thensym, elsesym, forsym, whilesym,
                                        withsym, casevarsym, colon, equals, recordsym,
                                        trysym,classsym,objectsym,protectedsym,privatesym,
                                        publicsym,publishedsym,finallysym,exceptsym];
    option[t,semicolon]^.dindsym    := [ifsym, thensym, elsesym, forsym,
                                        whilesym, withsym, colon, equals];
    option[t,implementationsym]^.dindsym    := [labelsym, varsym, typesym, constsym,
                                      endsym,propertysym];
    end;
end;

Procedure SetDefaultConfigs(Var Option : OptionTable);

Var
  T : TTokenScope;

begin
  For T:=Low(TTokenScope) to High(TTokenScope) do
    begin
    option[t,progsym]^.selected         := [capital,blinbefore, spaft];
    option[t,unitsym]^.selected         := [capital,blinbefore, spaft];
    option[t,librarysym]^.selected      := [capital,blinbefore, spaft];
    option[t,funcsym]^.selected         := [capital,blinbefore, dindonkey, spaft];
    option[t,procsym]^.selected         := [capital,blinbefore, dindonkey, spaft];
    option[t,labelsym]^.selected        := [capital,blinbefore, spaft, inbytab];
    option[t,constsym]^.selected        := [capital,blinbefore, dindonkey, spaft, inbytab];
    option[t,typesym]^.selected         := [capital,blinbefore, dindonkey, spaft, inbytab];
    option[t,varsym]^.selected          := [capital,blinbefore, dindonkey, spaft, inbytab];
    option[t,beginsym]^.selected        := [capital,dindonkey, crbefore, crafter, inbytab];
    option[t,repeatsym]^.selected       := [capital,inbytab, crafter];
    option[t,recordsym]^.selected       := [capital,inbyIndent, crafter];
    option[t,objectsym]^.selected       := [capital,inbyIndent];
    option[t,classsym]^.selected        := [capital,inbyIndent];
    option[t,publicsym]^.selected       := [capital,crbefore, dindonkey, spaft,inbytab];
    option[t,publishedsym]^.selected    := [capital,crbefore, dindonkey, spaft,inbytab];
    option[t,protectedsym]^.selected    := [capital,crbefore, dindonkey, spaft,inbytab];
    option[t,privatesym]^.selected      := [capital,crbefore, dindonkey, spaft,inbytab];
    option[t,trysym]^.Selected          := [capital,crbefore,crafter,inbytab];
    option[t,finallysym]^.selected      := [capital,crbefore,dindent,crafter,inbytab];
    option[t,exceptsym]^.selected       := [capital,crbefore,dindent,crafter,inbytab];
    option[t,casesym]^.selected         := [capital,spaft, inbytab, gobsym, crafter];
    option[t,casevarsym]^.selected      := [capital,spaft, inbytab, gobsym, crafter];
    option[t,ofsym]^.selected           := [capital,crsupp, spbef, spaft];
    option[t,forsym]^.selected          := [capital,spaft, inbytab, gobsym, crafter];
    option[t,whilesym]^.selected        := [capital,spaft, inbytab, gobsym, crafter];
    option[t,withsym]^.selected         := [capital,spaft, inbytab, gobsym, crafter];
    option[t,dosym]^.selected           := [capital,crsupp, spbef];
    option[t,ifsym]^.selected           := [capital,spaft, inbytab, gobsym];
    option[t,implementationsym]^.selected := [capital,blinbefore,crafter,dindonkey];
    option[t,interfacesym]^.selected    := [capital,blinbefore,crafter];
    option[t,usessym]^.selected         := [capital,blinbefore,spaft];
    option[t,thensym]^.selected         := [capital];
    option[t,elsesym]^.selected         := [capital,crbefore, dindonkey, inbytab];
    option[t,endsym]^.selected          := [capital,crbefore, crafter,dindonkey,dindent];
    option[t,untilsym]^.selected        := [capital,crbefore, dindonkey, dindent, spaft,
                                            gobsym, crafter];
    option[t,becomes]^.selected         := [capital,spbef, spaft, gobsym];
    option[t,Delphicomment]^.Selected   := [crafter];
    option[t,opencomment]^.selected     := [capital,crsupp];
    option[t,closecomment]^.selected    := [capital,crsupp];
    option[t,semicolon]^.selected       := [capital,crsupp, dindonkey, crafter];
    option[t,colon]^.selected           := [capital,inbytab];
    option[t,equals]^.selected          := [capital,spbef, spaft, inbytab];
    option[t,openparen]^.selected       := [capital,gobsym];
    option[t,period]^.selected          := [capital,crsupp];
    end;
  option[tsInterface,funcsym]^.selected         := [capital, dindonkey, spaft];
  option[tsInterface,procsym]^.selected         := [capital, dindonkey, spaft];
end;

Procedure GenerateNewConfig(S : TStream);
Var TheKey,TheIndent : KeySymbol;
    TheOpt : Options;
    Written : Boolean;
    Option : OptionTable;

begin
  CreateOptionsTable(option);
  SetDefaultConfigs(option);
  SetDefaultIndents(option);
  SetKeywordTerminators(option);

  For TheKey := Firstkey to Lastkey do
    begin
    { Write options }
    WriteString (S, GetEnumName(EntryTableTypInfo, Ord(TheKey))+'=');
    Written:=False;
    for TheOpt:=FirstOpt to LastOpt do
      If TheOpt in Option[tsInterface,TheKey]^.Selected then
        begin
        if written then
           WriteString (S,',')
        else
           Written:=True;
        writeString (S, GetEnumName(OptionTypInfo, Ord(TheOpt)));
        end;
    WriteCr (S);
    { Write de-indent keysyms, if any }
    If Option[tsInterface,TheKey]^.dindsym<>[] then
      begin
      WriteString (S,'['+GetEnumName(EntryTableTypInfo, Ord(TheKey))+']=');
      Written:=False;
      For TheIndent:=FirstKey to lastkey do
      If TheIndent in Option[tsInterface,TheKey]^.dindsym then
        begin
        if written then
           WriteString (S,',')
        else
           Written:=True;
        WriteString (S, GetEnumName(EntryTableTypInfo, Ord(Theindent)));
        end;
      WriteCr (S);
      end;
    end;
end;

Function ReadConfigFrom(S: TStream; var Option: OptionTable) : Boolean;
Type TLineType = (ltNormal,ltIndent,ltGobble);

Var
  TheKey : KeySymbol;
  Found : Boolean;
  L : TStringList;
  LT : TLineType;
  Name, Value: string;
  I : Longint;

  Procedure SetOption(TheKey : KeySymbol; Var OptionList : String);
  Var TheOpt  : Options;
      Found : Boolean;
      K : longint;
      opt : string;
      TS : TTokenScope;

  begin
    Repeat
      K:=pos(',',optionlist);
      If k>0 then
        begin
        opt:=Copy(OptionList,1,k-1);
        strip(opt);
        Delete(OptionList,1,k);
        end
      else
        opt:=OptionList;

      If Length(Opt)>0 then
        begin
        Found := GetEnumValue(OptionTypInfo,opt) <> -1;
        If not found then
          LogWithLocation(i + 1,
            length(L.Names[i]) + 1 + length(L.Values[L.Names[i]]),
            Format(E_UNKNOWN_OPTION, [L.Values[L.Names[i]]]),
            L.Strings[i])
        else
          For TS:=Low(TTokenScope) to High(TTokenScope) do
            Option[TS,TheKey]^.Selected:=Option[TS,TheKey]^.Selected+[TheOpt];
        end;
    until k=0;
  end;

  Function GetKeySimList(Const aType : String; Var OptionList : String) : keysymset;

  Var
      TheIndent : Keysymbol;
      Found : Boolean;
      K : longint;
      opt : string;

  begin
    Result:=[];
    Repeat
      K:=pos(',',optionlist);
      If k>0 then
        begin
        opt:=Copy(OptionList,1,k-1);
        strip(opt);
        Delete(OptionList,1,k);
        end
      else
        opt:=OptionList;
      If Length(Opt)>0 then
        begin
        Found:=GetEnumValue(EntryTableTypInfo,opt)<>-1;
        If not found then
          begin
          Verbose (Format(E_UNKNOWN_INDENT, [aType]));
          exit;
          end;
        Include(Result,Theindent);
        end;
    until k=0;
  end;

  Procedure SetIndent(TheKey : KeySymbol; Var OptionList : String);
  Var
    TS : TTokenScope;
    Syms : KeySymSet;
  begin
    Syms:=GetKeySimList('indent',OptionList);
    For TS:=Low(TTokenScope) to High(TTokenScope) do
      With Option[TS,TheKey]^ do
         dindsym:=dindsym+Syms;
  end;

  Procedure SetGobble(TheKey : KeySymbol; Var OptionList : String);

  Var
    TS : TTokenScope;
    Syms : KeySymSet;

  begin
    Syms:=GetKeySimList('gobble',OptionList);
    For TS:=Low(TTokenScope) to High(TTokenScope) do
      With Option[TS,TheKey]^ do
         Terminators:=Terminators+Syms;
  end;

  Function CheckLineType (var Name : String) : TLineType;

  begin
    If (Name[1]='[') and (Name[Length(Name)]=']') then
     begin
     Name:=Copy(Name,2,Length(Name)-2);
     Result:=ltIndent
     end
   else If (Name[1]='<') and (Name[Length(Name)]='>') then
     begin
     Name:=Copy(Name,2,Length(Name)-2);
     Result:=ltgobble
     end
   else
     Result:=ltNormal;
  end;

begin
  Result := false;
  L := TStringList.Create;
  Try
    L.LoadFromStream(S);
    For I := 0 to L.Count - 1 do
    begin
      if L.Strings[I][1] = '#' then continue;

      Name := LowerCase(L.Names[i]);
      Value := LowerCase(L.Values[Name]);

      { Strip comments }
      If pos('#', Name) <> 0 then
        Name := Copy(Name, 1, Pos('#', Name) - 1);

      If pos('#', Value) <> 0 then
        Value := Copy(Value, 1, Pos('#', Value) - 1);

      If (Length(Name) = 0) and (Length(Value) = 0) then
        continue
      Else if Length(Name) = 0 then
      begin
        LogWithLocation(I + 1, 1, E_NO_TARGET, L.Strings[i]);
        continue;
      end
      Else if Length(Value) = 0 then
      begin
        LogWithLocation(I + 1, Length(Name) + 1, E_NO_VALUE, L.Strings[i]);
        continue;
      end;

      Strip(Name);
      Strip(Value);
      LT := CheckLineType(Name);
      Found := GetEnumValue(EntryTableTypInfo, Name) <> -1;

      If not found then
        LogWithLocation (
          I + 1, Length(Name) + 1,
          Format(E_UNKNOWN_KEYWORD, [Name]),
          L.Strings[I]
        )
      else
        Case LT of
          ltIndent: SetIndent(TheKey, Value);
          ltNormal: SetOption(TheKey, Value);
          ltGobble: SetGobble(TheKey, Value);
        end;
      end;
  Finally
    L.Free;
  end;

  Verbose (S_PROCESSED_CFG);
  Result:=true;
end;

end.
