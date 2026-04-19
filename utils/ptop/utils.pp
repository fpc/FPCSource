Procedure Strip (Var S : String);

Const WhiteSpace = [#32, #9, #10, #13];

Var I,J : Longint;

begin
  If length(s)=0 then exit;
  I:=1;
  While (S[I] in whitespace) and (I<Length(S)) do inc(i);
  J:=length(S);
  While (S[J] in whitespace) and (J>1) do dec(j);
  If I<=J then
    S:=Copy(S,i,j-i+1)
  else
    S:='';
end;

Procedure ClassID(Value: Token;
                  lngth: INTEGER;
                  VAR idtype: keysymbol;
                  VAR IsKeyWord: BOOLEAN);
  { Classify an identifier.  We are only interested
    in it if it is a keyword, so we use the hash table. }
  VAR
    Keyvalue: String[MAXKEYLENGTH];
    Sym : keysymbol;

  BEGIN
    IF lngth > MAXKEYLENGTH THEN BEGIN
      idtype := othersym;
      IsKeyWord := FALSE
    END
    ELSE
      BEGIN
      IsKeyWord := FALSE;
      KeyValue:= UpperCase(Value);
      sym:=endsym;
      While (Not IsKeyword) and (sym<=lastformatsym) DO
         begin
         iskeyword:=(KeyValue=Keyword[sym]);
         if not iskeyword then
           Sym:=Succ(sym);
         end;
      if IsKeyWord then
        idtype:=sym
      ELSE
        idtype := othersym;
      END
  END; { of ClassID }

{ ---------------------------------------------------------------------
    Functions to create options and set defaults.
  ---------------------------------------------------------------------}

Procedure CreateOptions (Out Option : OptionTable);

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

Procedure SetTerminators(Var Option : OptionTable);

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
  For T:=Low(TTokenScope) to High(TTokenScope) do
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
    option[t,exceptsym]^.dindsym   := [trysym];
    option[t,elsesym]^.dindsym      := [ifsym, thensym, elsesym];
    option[t,untilsym]^.dindsym     := [ifsym, thensym, elsesym, forsym, whilesym,
                                      withsym, colon, equals];
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

Procedure SetDefaults (Var Option : OptionTable);

{ Sets default values for the formatting rules. }

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

{ ---------------------------------------------------------------------
    Stream handling routines
  ---------------------------------------------------------------------}
{$push}{$warn 5036 off} // C seems to be not initialized

Function ReadChar (S : TStream) : Char;
Var C : Char;

begin
  repeat
    if S.Position=S.Size then
      C:=#0
    else
      S.Read(C,1);
  Until (C<>#13);
  ReadChar:=C;
end;

Function EoSLn (S : TStream) : Char;

Const WhiteSpace = [' ', #9, #13 ];

Var C : Char;

begin
  Repeat
    if S.Position = S.Size then
      C:=#0
    else
      S.Read(C,1);
  Until (Not (C in WhiteSpace)) or ((C=#10));
  EoSln:=C;
end;

{$pop} // warning code 5036

Function ReadString (S: TStream): String;

Var
  I : Byte;
  Count : Integer;

begin
  Result:='';
  I:=0;
  Repeat
    If ((I+1)>Length(Result)) then
      SetLength(Result,Length(Result)+255);
    Count:=S.Read(Result[I+1],1);
    If Count>0 then
      Inc(I);
  until (Result[I]=#10) or (Count=0);
  If Result[i]=#10 Then Dec(I);
  If Result[I]=#13 then Dec(I);
  SetLength(Result,I);
end;

Procedure WriteString (S : TStream; ST : String); inline;
begin
  S.Write(St[1],length(St));
end;

Procedure WriteAnsiString (S : TStream; ST : AnsiString); inline;
begin
  S.Write(St[1],length(St));
end;

Procedure WriteCR (S: TStream); inline;
begin
  WriteString(S,LineEnding);
end;

Procedure WriteLnString (S : TStream; ST : String); inline;
begin
  WriteString(S,ST);
  WriteCR(S);
end;

Procedure GenerateCfgFile(S : TStream);

Var TheKey,TheIndent : KeySymbol;
    TheOpt : Options;
    Written : Boolean;
    Option : OptionTable;

begin
  CreateOptions(option);
  SetDefaults(option);
  SetDefaultIndents(option);
  For TheKey:=Firstkey to lastkey do
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

Function trimMiddle ( a:ansistring; lnght: integer; size: integer):string;
var
    half:Integer;
begin
    if lnght > size then
    begin
      half := (size - 3) div 2;
      trimMiddle := copy(a,1,half) + '...' + copy(a,lnght-half+1,half);
    end
    else
      trimMiddle := a;
end;