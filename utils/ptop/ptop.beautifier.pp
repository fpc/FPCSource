{$I ptop.beautifier.inc}

Implementation

uses
  {$ifdef FPC_DOTTEDUNITS}
  system.typinfo,
  fcl.eventlog,
  {$else}
  typinfo,
  eventlog,
  {$endif}
  ptop.config,
  ptop.strutils
  ;

resourcestring
  S_PRETTY_PRINTED  = 'Read %d lines of input, %d lines written.';

VAR
  sets   : tableptr;
  dblch  : dblcharset;
  logger : TEventLog;

{ Read the next character and classify it }
Procedure TPrettyPrinter.GetChar;
VAR  Ch: CHAR;
BEGIN
  currchar := nextchar;
  WITH nextchar DO
    begin
    Ch:=ReadChar(Ins);
    If Ch=#0 then
      BEGIN
      name := filemark;
      Value := Blank
      END
    ELSE If (Ch=#10) THEN
      BEGIN
      name := endofline;
      Value := Ch;
      Inc(inlines);
      END
    ELSE
      BEGIN
        Value := Ch;
        case Ch of
          'a'..'z', 'A'..'Z', '_': name := letter;
          '0'..'9': name := digit;
          '''': name := quote;
          #13, ' ', #9: name := space;
          else name := otherchar;
        end;
      END
    end;
END; { of GetChar }


{ Store a character in the current symbol }
Procedure TPrettyPrinter.StoreNextChar(
    VAR lngth: INTEGER; VAR Value: Token);
  BEGIN
    GetChar;
    IF lngth < MAXSYMBOLSIZE THEN BEGIN {XXX - should there be a limit at all?}
      Inc(lngth);
      setlength(Value,lngth);
      Value[lngth] := currchar.Value;
    END;
  END; { of StoreNextChar }


Procedure TPrettyPrinter.SkipBlanks(out spacesbefore, crsbefore: INTEGER);
  { Count the spaces between symbols }
  BEGIN
    spacesbefore := 0;
    crsbefore := 0;
    WHILE nextchar.name IN [space, endofline] DO BEGIN
      GetChar;
      CASE currchar.name OF
        space:      Inc(spacesbefore);
        endofline:  BEGIN
                      Inc(crsbefore);
                      spacesbefore := 0;
                    END;
      END;  {case}
    END;
  END; { of SkipBlanks }


Procedure TPrettyPrinter.GetComment(sym: symbolinfo);
  { Process comments using brace notation }
  BEGIN
    sym^.name := opencomment;
    WHILE NOT ((currchar.Value = '}')
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF currchar.Value = '}' THEN sym^.name := closecomment;
  END; { of GetComment }

Procedure TPrettyPrinter.GetDoubleComment(sym: symbolinfo);
  { Process comments using parenthesis notation }
  BEGIN
    sym^.name := dopencomment;
    WHILE NOT (((currchar.Value = '*') AND (nextchar.Value = ')'))
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF (currchar.Value = '*') AND (nextchar.Value = ')') THEN BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      sym^.name := dclosecomment;
    END;
  END; { of GetDoubleComment }

Procedure TPrettyPrinter.GetDelphiComment(sym: symbolinfo);
  { Process comments using either brace or parenthesis notation }
  BEGIN
    sym^.name := Delphicomment;
    WHILE NOT ((nextchar.name = endofline) OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);

  END; { of GetDelphiComment }

Procedure TPrettyPrinter.GetIdentifier(sym: symbolinfo);
  { Read an identifier and classify it }
  BEGIN
    WHILE nextchar.name IN [letter, digit] DO
      StoreNextChar(sym^.length, sym^.Value);
    ClassID(sym^.Value, sym^.length, sym^.name, sym^.IsKeyWord);
    IF sym^.name IN [recordsym, objectsym,classsym, casesym, endsym] THEN
      begin
      if sym^.name=implementationsym then
        FTokenScope:=tsImplementation;
      if sym^.name in [recordsym,objectsym,classsym] then
        LastStruct:=sym^.name;
      CASE sym^.name OF
        RecordSym : Inc(RecordLevel);
        ClassSym : ClassSeen:=True;
        objectsym : begin
                    if (PreviousSymbol=Ofsym) then
                      sym^.name:=ofobjectsym
                    else
                      ObjectSeen:=True;
                    end;
        casesym   : IF (RecordLevel>0) and (LastStruct=recordsym) THEN sym^.name := casevarsym;
        endsym    : If (LastStruct=recordsym) then
                      Dec(Recordlevel);
                    else
                      begin
                      ClassSeen:=False;
                      ObjectSeen:=False;
                      end
      END;  {case}
      end;
     If (PreviousSymbol=ClassSym) and (sym^.Name=ofsym) then
       ClassSeen:=False;
     PreviousSymbol:=sym^.Name;
  END; { of GetIdentifier }


{ Read a number and store it as a string }
Procedure TPrettyPrinter.GetNumber(sym: symbolinfo);
  BEGIN
    WHILE nextchar.name = digit DO StoreNextChar(sym^.length, sym^.Value);
    sym^.name := othersym;
  END; { of GetNumber }


PROCEDURE TPrettyPrinter.GetCharLiteral(sym: symbolinfo);
  { Read a quoted string }
  BEGIN
    WHILE nextchar.name = quote DO BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      WHILE NOT (nextchar.name IN [quote, endofline, filemark]) DO
        StoreNextChar(sym^.length, sym^.Value);
      IF nextchar.name = quote THEN StoreNextChar(sym^.length, sym^.Value);
    END;
    sym^.name := othersym;
  END; { of GetCharLiteral }


FUNCTION TPrettyPrinter.char_Type: keysymbol;

  { Classify a character pair }

  VAR
    NextTwoChars: SpecialChar;
    Hit: BOOLEAN;
    thischar: keysymbol;
  BEGIN
    NextTwoChars[1] := currchar.Value;
    NextTwoChars[2] := nextchar.Value;
    thischar := becomes;
    Hit := FALSE;
    WHILE NOT (Hit OR (thischar = opencomment)) DO BEGIN
      IF NextTwoChars = DblChar[thischar] THEN Hit := TRUE
      ELSE Inc(thischar);
    END;
    IF NOT Hit THEN BEGIN
      thischar := opencomment;
      WHILE NOT (Hit OR (PRED(thischar) = period)) DO BEGIN
        IF currchar.Value = SglChar[thischar] THEN Hit := TRUE
        ELSE Inc(thischar);
      END;
    END;
    IF Hit THEN char_Type := thischar
    ELSE char_Type := othersym;
  END; { of char_Type }


Procedure TPrettyPrinter.GetSpecialChar(sym: symbolinfo);
   { Read special characters }
  BEGIN
    StoreNextChar(sym^.length, sym^.Value);
    sym^.name := char_Type;
    IF sym^.name IN dblch THEN StoreNextChar(sym^.length, sym^.Value)
  END; { of GetSpecialChar }


Procedure TPrettyPrinter.GetNextSymbol(sym: symbolinfo);
  { Read a symbol using the appropriate procedure }
  BEGIN
    CASE nextchar.name OF
      letter:     GetIdentifier(sym);
      digit:      GetNumber(sym);
      quote:      GetCharLiteral(sym);
      otherchar:  BEGIN
                    GetSpecialChar(sym);
                    IF sym^.name = opencomment THEN GetComment(sym)
                    else IF sym^.name = dopencomment THEN GetDoubleComment(sym)
                    else IF sym^.name= DelphiComment then GetDelphiComment(Sym)
                  END;
      filemark:   sym^.name := endoffile;
      ELSE {:} {Turbo}
        WRITELN('Unknown character type: ', ORD(nextchar.name));
    END;  {case}
  END; { of GetNextSymbol }


Procedure TprettyPrinter.GetSymbol;
{ Store the next symbol in NEXTSYM }
  VAR
    dummy: symbolinfo;
  BEGIN
    dummy := currsym;
    currsym := nextsym;
    nextsym := dummy;
    SkipBlanks(nextsym^.spacesbefore, nextsym^.crsbefore);
    nextsym^.length := 0;
    nextsym^.IsKeyWord := FALSE;
    IF currsym^.name = opencomment THEN GetComment(nextsym)
    ELSE IF currsym^.name = dopencomment THEN GetDoubleComment(nextsym)
    ELSE GetNextSymbol(nextsym);
  END;  {of GetSymbol}


Procedure TprettyPrinter.PopStack(Out indentsymbol: keysymbol;
                                  Out prevmargin: INTEGER);
  { Manage stack of indentation symbols and margins }
  BEGIN
    IF top > 0 THEN BEGIN
      indentsymbol := stack[top].indentsymbol;
      prevmargin := stack[top].prevmargin;
      Dec(top);
    END
    ELSE BEGIN
      indentsymbol := othersym;
      prevmargin := 0;
    END;
  END; { of PopStack }


Procedure TPrettyPrinter.PushStack(indentsymbol: keysymbol;
                                   prevmargin: INTEGER );
  BEGIN
    Inc(top);
    stack[top].indentsymbol := indentsymbol;
    stack[top].prevmargin := prevmargin;
  END; { of PushStack }


Procedure TPrettyPrinter.WriteCRs(numberofcrs: INTEGER);
  VAR
    i: INTEGER;
  BEGIN
    IF numberofcrs > 0 THEN BEGIN
      FOR i := 1 TO numberofcrs DO
        WriteCr(OutS);
      Inc(outlines,numberofcrs);
      Currlinepos := 0;
      FirstWordStackPos:=-1;
    END;
  END; { of WriteCRs }


Procedure TPrettyPrinter.InsertCR;
  BEGIN
    IF currsym^.crsbefore = 0 THEN BEGIN
      WriteCRs(1);
      currsym^.spacesbefore := 0;
    END;
  END; { of InsertCR }


Procedure TPrettyPrinter.InsertBlankLine;
BEGIN
  IF currsym^.crsbefore = 0 THEN
    BEGIN
    IF currlinepos = 0 THEN
      WriteCRs(1)
    ELSE
      WriteCRs(2);
      currsym^.spacesbefore := 0;
    END
  ELSE
    IF currsym^.crsbefore = 1 THEN
      IF currlinepos > 0 THEN
        begin
        WriteCRs(1);
        currsym^.spacesbefore := 0;
        end;
END; { of InsertBlankLine }


Procedure TPrettyPrinter.LShiftOn(dindsym: keysymset);
  { Move margin left according to stack configuration and current symbol }
  VAR
    indentsymbol: keysymbol;
    prevmargin: INTEGER;
  BEGIN
{$ifdef DEBUG_PTOP}
    Write('LShiftOn ',EntryNames[currsym^.name],' : ',FirstWordPos,'/',CurrMargin);
{$endif DEBUG_PTOP}
    IF top > 0 THEN BEGIN
      REPEAT
        PopStack(indentsymbol, prevmargin);
        IF indentsymbol IN dindsym THEN currmargin := prevmargin;
      UNTIL NOT (indentsymbol IN dindsym) OR (top = 0);
      IF NOT (indentsymbol IN dindsym) THEN
        PushStack(indentsymbol, prevmargin);
    END;
{$ifdef DEBUG_PTOP}
    Writeln('-> ',CurrMargin);
{$endif DEBUG_PTOP}
  END; { of LShiftOn }


Procedure TprettyPrinter.LShift;
{ Move margin left according to stack top }
  VAR
    indentsymbol: keysymbol;
    prevmargin: INTEGER;
  BEGIN
{$ifdef DEBUG_PTOP}
    Write('LShift ',EntryNames[currsym^.name],' : ',FirstWordPos,'/',CurrMargin);
{$endif DEBUG_PTOP}
    IF top > 0 THEN BEGIN
      PopStack(indentsymbol, prevmargin);
      currmargin := prevmargin;
(* maybe PopStack(indentsymbol,currmargin); *)
    END;
{$ifdef DEBUG_PTOP}
    Writeln('-> ',CurrMargin);
{$endif DEBUG_PTOP}
  END; { of LShift }

Procedure TprettyPrinter.RShift(currmsym: keysymbol);
  { Move right, stacking margin positions }
  BEGIN
{$ifdef DEBUG_PTOP}
    Write('RShift ',EntryNames[currmsym],' : ',FirstWordPos,'/',Currmargin);
{$endif DEBUG_PTOP}
    IF top < MAXSTACKSIZE THEN PushStack(currmsym, currmargin);
    IF startpos > currmargin THEN currmargin := startpos;
    Inc(currmargin,INDENT);
{$ifdef DEBUG_PTOP}
    Writeln(' -> ',Currmargin)
{$endif DEBUG_PTOP}
  END; { of RShift }

Procedure TprettyPrinter.RShiftIndent{$ifdef DEBUG_PTOP}(currmsym: keysymbol){$endif debug};
  { Move right, stacking margin positions }
  BEGIN
{$ifdef DEBUG_PTOP}
    Write('RShiftIndent ',EntryNames[currmsym],' : ',FirstWordPos,'/',Currmargin);
{$endif DEBUG_PTOP}
    If (FirstWordStackPos>=0) then
      Top:=FirstWordStackPos
    else
      Top:=0;
{$ifdef DEBUG_PTOP}
    If (Top>0) then
      Write(' Stackpos ',Top,' Item: ',EntryNames[Stack[Top].IndentSymbol],' Pos: ',Stack[Top].Prevmargin)
    else
      Write(' no item on stack');
{$endif DEBUG_PTOP}
    IF top < MAXSTACKSIZE THEN PushStack(othersym, FirstWordPos);
//    IF top < MAXSTACKSIZE THEN PushStack(currmsym, currmargin);
    CurrMargin:=FirstWordPos+Indent;
{$ifdef DEBUG_PTOP}
    Writeln(' -> ',Currmargin)
{$endif DEBUG_PTOP}
  END; { of RShift }


Procedure TPrettyPrinter.InsertSpace(VAR symbol: symbolinfo);
  { Insert space if room on line }
  BEGIN
    IF currlinepos < LineSize THEN BEGIN
      WriteString(OutS, Blank);
      Inc(currlinepos);
      IF (symbol^.crsbefore = 0) AND (symbol^.spacesbefore > 0)
      THEN Dec(symbol^.spacesbefore);
    END;
  END; { of InsertSpace }


Procedure TPrettyPrinter.MoveLinePos(newlinepos: INTEGER);
  { Insert spaces until correct line position reached }
  VAR  i: INTEGER;
  BEGIN
    FOR i := SUCC(currlinepos) TO newlinepos DO
      WriteString(OutS, Blank);
    currlinepos := newlinepos;
  END; { of MoveLinePos }


Procedure TPrettyPrinter.PrintSymbol;

  BEGIN
    IF (currsym^.IsKeyWord) then
      begin
      If upper in sets^.selected Then
        WriteString (OutS,UpperCase(currsym^.value))
      else if lower in sets^.selected then
        WriteString (OutS,LowerCase(currsym^.value))
      else if capital in sets^.selected then
        begin
        WriteString(OutS,UpCase(CurrSym^.Value[1]));
        WriteString(OutS,LowerCase(Copy(CurrSym^.Value,2,MAXSYMBOLSIZE)));{XXX - ?should it be length?}
        end
      else
        WriteString(OutS,Currsym^.Value);
      end
    ELSE
      WriteAnsiString(OutS, currsym^.Value);
    startpos := currlinepos;
    Inc(currlinepos,currsym^.length);
    if (FirstWordStackPos=-1) then
      begin
      FirstWordPos:=startpos;
      FirstWordStackPos:=Top;
{$ifdef DEBUG_PTOP}
      write('First word : ',currlinepos,': ',currsym^.value);
      If (FirstWordStackPos>0) then
        writeln(' [Stack: ',FirstWordStackPos,' Item: "',EntryNames[Stack[FirstWordStackPos].IndentSymbol],'" Pos: ',Stack[FirstWordStackPos].Prevmargin,']')
      else
        Writeln(' No stack')
{$endif DEBUG_PTOP}
      end;
  END; { of PrintSymbol }


Procedure TPrettyPrinter.PPSymbol;
{ Find position for symbol and then print it }
  VAR  newlinepos: INTEGER;
  BEGIN
    WriteCRs(currsym^.crsbefore);
    IF ((currLinePos<>0) and (currlinepos + currsym^.spacesbefore > currmargin)) OR
       (currsym^.name IN [opencomment, closecomment,dopencomment, dclosecomment])
    THEN
      newlinepos := currlinepos + currsym^.spacesbefore
    ELSE
      newlinepos := currmargin;
    IF newlinepos + currsym^.length > LINESIZE THEN
      BEGIN {XXX - this needs to be cleaned for case of long symbol values}
      WriteCRs(1);
      IF currmargin + currsym^.length <= LINESIZE THEN
        newlinepos := currmargin
      ELSE IF currsym^.length < LINESIZE THEN
        newlinepos := LINESIZE - currsym^.length
      ELSE
        newlinepos := 0;
      END;
    MoveLinePos(newlinepos);
    PrintSymbol;
  END; { of PPSymbol }


Procedure TPrettyPrinter.Gobble(terminators: keysymset);
  { Print symbols which follow a formatting symbol but which do not
    affect layout }
  BEGIN
{$ifdef DEBUG_PTOP}
    Inc(GobbleLevel);
    Writeln('Gobble start ',GobbleLevel,' : ',EntryNames[currsym^.name]);
{$endif DEBUG_PTOP}
    IF top < MAXSTACKSIZE THEN PushStack(currsym^.name, currmargin);
    currmargin := currlinepos;
    WHILE NOT ((nextsym^.name IN terminators)
    OR (nextsym^.name = endoffile)) DO BEGIN
      GetSymbol;
      PPSymbol;
    END;
    LShift;
{$ifdef DEBUG_PTOP}
    Writeln('Gobble end ',gobblelevel,' : ',EntryNames[nextsym^.name],' ',nextsym^.name in terminators );
    Dec(GobbleLevel);
{$endif DEBUG_PTOP}
  END; { of Gobble }

Function TPrettyPrinter.PrettyPrint : Boolean;

Begin
  PrettyPrint:=False;
  If Not Assigned(Ins) or Not Assigned(OutS) then
    exit;
  If Not Assigned(CfgS) then
    begin
    SetDefaultConfigs(Option);
    SetDefaultIndents(Option);
    end
  else
    ReadConfigFrom(CfgS, Option);
  { Initialize variables }
  top := 0;
  currlinepos := 0;
  currmargin := 0;
  inlines := 0;
  outlines := 0;
  CrPending := FALSE;
  FirstWordStackPos:=-1;
  RecordLevel := 0;
  GetChar;
  NEW(currsym);
  NEW(nextsym);
  GetSymbol;
  WHILE nextsym^.name <> endoffile DO BEGIN
    GetSymbol;
{$ifdef DEBUG_PTOP}
    Writeln('line in-'+IntToStr(inlines)+' out-'+IntToStr(outlines)+
            ' symbol "'+EntryNames[currsym^.name]+'" = "'+
            trimMiddle(currsym^.value,length(currsym^.value),MAXSHOWSIZE)+'"');
{$endif DEBUG_PTOP}
    sets := option[FTokenScope,currsym^.name];
    IF (CrPending AND NOT (crsupp IN sets^.selected))
    OR (crbefore IN sets^.selected) THEN BEGIN
      InsertCR;
      CrPending := FALSE
    END;
    IF blinbefore IN sets^.selected THEN BEGIN
      InsertBlankLine;
      CrPending := FALSE
    END;
    IF dindonkey IN sets^.selected THEN
      LShiftOn(sets^.dindsym);
    IF dindent IN sets^.selected THEN
      LShift;
    IF spbef IN sets^.selected THEN InsertSpace(currsym);
    PPSymbol;
    IF spaft IN sets^.selected THEN InsertSpace(nextsym);
    IF inbytab IN sets^.selected THEN
      RShift(currsym^.name)
    else IF inbyindent IN sets^.selected THEN
      RShiftIndent{$ifdef DEBUG_PTOP}(currsym^.name){$endif debug};
    IF gobsym IN sets^.selected THEN Gobble(sets^.terminators);
    IF crafter IN sets^.selected THEN CrPending := TRUE
  END;
  IF CrPending THEN WriteCRs(1);
  Verbose(Format(S_PRETTY_PRINTED, [inlines, outlines]));
  PrettyPrint:=True;
end;

Constructor TPrettyPrinter.Create;

Begin
  Indent:=DefIndent;
  LineSize:=DefLineSize;
  CreateOptionsTable(Option);
  SetKeywordTerminators(Option);
  InS:=Nil;
  OutS:=Nil;
  CfgS:=Nil;
End;

initialization
  dblch := [becomes, notequal, lessorequal, greaterorequal,
            plusequals, minusequals, divideequals, timesequals, exponential,
            opencomment];
  logger := TEventLog.Create(nil);
  logger.AppendContent := true;
  logger.LogType := ltStdOut;
finalization
  logger.free;
end.
