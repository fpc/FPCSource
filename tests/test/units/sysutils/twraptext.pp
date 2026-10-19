{ Tests SysUtils.WrapText: fixed cases, a reference implementation on random input, and invariants }
program twraptext;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  cBreakChars: TSysCharSet = [' ','-'];
  cQuotes: TSysCharSet = ['''','"'];
  cMaxReported = 20;

var
  Failures: Integer = 0;
  Checks: Integer = 0;

// Returns aValue with control characters written as #nn
function Repr(const aValue: String): String;

var
  I: Integer;

begin
  Result:='';
  for I:=1 to Length(aValue) do
    if aValue[I]<' ' then
      Result:=Result+'#'+IntToStr(Ord(aValue[I]))
    else
      Result:=Result+aValue[I];
end;


// Records a failure with its description
procedure Fail(const aMsg: String);

begin
  Inc(Failures);
  if Failures<=cMaxReported then
    Writeln('FAIL: ',aMsg);
end;


// Compares WrapText(aLine,aBreakStr,aBreakChars,aMaxCol) with aExpected
procedure Check(const aName, aLine, aBreakStr: String; const aBreakChars: TSysCharSet; aMaxCol: Integer; const aExpected: String);

var
  lResult: String;

begin
  Inc(Checks);
  lResult:=WrapText(aLine,aBreakStr,aBreakChars,aMaxCol);
  if lResult<>aExpected then
    Fail(aName+': WrapText("'+Repr(aLine)+'","'+Repr(aBreakStr)+'",'+IntToStr(aMaxCol)+') = "'
      +Repr(lResult)+'", expected "'+Repr(aExpected)+'"');
end;


// Checks with BreakStr '|' and break characters space and hyphen
procedure CheckBar(const aName, aLine: String; aMaxCol: Integer; const aExpected: String);

begin
  Check(aName,aLine,'|',cBreakChars,aMaxCol,aExpected);
end;


// Wraps aLine as specified: cut after the last break character within aMaxCol columns, else after the
// first one beyond it; a break character in column 1, or inside quotes, is no break point;
// an existing aBreakStr ends the line without adding one
function RefWrap(const aLine, aBreakStr: String; const aBreakChars: TSysCharSet; aMaxCol: Integer): String;

var
  lStart, lPos, lCol, lLen, lLast, lCut: Integer;
  lQuote, lChar: AnsiChar;
  lExisting: Boolean;

begin
  Result:='';
  lLen:=Length(aLine);
  lStart:=1;
  while lStart<=lLen do
    begin
    lLast:=0;
    lCut:=0;
    lQuote:=#0;
    lExisting:=False;
    lPos:=lStart;
    while (lPos<=lLen) and (lCut=0) do
      begin
      lChar:=aLine[lPos];
      lCol:=lPos-lStart+1;
      if lQuote=#0 then
        begin
        if lChar in cQuotes then
          begin
          lQuote:=lChar;
          Inc(lPos);
          Continue;
          end;
        end
      else if lChar=lQuote then
        lQuote:=#0
      else
        begin
        Inc(lPos);
        Continue;
        end;
      if (aBreakStr<>'') and (Copy(aLine,lPos,Length(aBreakStr))=aBreakStr) then
        begin
        lCut:=lPos+Length(aBreakStr)-1;
        lExisting:=True;
        end
      else if (lCol>aMaxCol) and (lLast>0) then
        lCut:=lLast
      else if (lCol>1) and (lChar in aBreakChars) then
        begin
        if lCol<=aMaxCol then
          lLast:=lPos
        else
          lCut:=lPos;
        end;
      Inc(lPos);
      end;
    if lCut=0 then
      lCut:=lLen;
    Result:=Result+Copy(aLine,lStart,lCut-lStart+1);
    if (lCut<lLen) and not lExisting then
      Result:=Result+aBreakStr;
    lStart:=lCut+1;
    end;
end;


// Returns a random string of up to aMaxLen characters taken from aAlphabet
function RandomLine(const aAlphabet: String; aMaxLen: Integer): String;

var
  I, lLen: Integer;

begin
  lLen:=Random(aMaxLen+1);
  SetLength(Result,lLen);
  for I:=1 to lLen do
    Result[I]:=aAlphabet[1+Random(Length(aAlphabet))];
end;


// Returns True if aValue contains a character of aChars in positions aFrom to aTo
function HasBreakChar(const aValue: String; aFrom, aTo: Integer; const aChars: TSysCharSet): Boolean;

var
  I: Integer;

begin
  Result:=False;
  for I:=aFrom to aTo do
    if (I>=1) and (I<=Length(aValue)) and (aValue[I] in aChars) then
      Exit(True);
end;


// Checks the structure of the result for a line without quotes or '|'
procedure CheckInvariants(const aLine: String; aMaxCol: Integer);

var
  lResult, lSeg, lRest, lName: String;
  lParts: TStringArray;
  I, lLen: Integer;

begin
  Inc(Checks);
  lName:='invariant "'+aLine+'" '+IntToStr(aMaxCol);
  lResult:=WrapText(aLine,'|',cBreakChars,aMaxCol);
  if StringReplace(lResult,'|','',[rfReplaceAll])<>aLine then
    begin
    Fail(lName+': removing breaks does not give the input: "'+lResult+'"');
    Exit;
    end;
  if aLine='' then
    Exit;
  lParts:=lResult.Split(['|']);
  for I:=0 to High(lParts) do
    begin
    lSeg:=lParts[I];
    lLen:=Length(lSeg);
    if lLen=0 then
      begin
      Fail(lName+': empty line in "'+lResult+'"');
      Exit;
      end;
    if (lLen>1) and HasBreakChar(lSeg,2,lLen-1,cBreakChars) and (lLen>aMaxCol) then
      Fail(lName+': line "'+lSeg+'" exceeds MaxCol although it has an earlier break point, in "'+lResult+'"');
    if I<High(lParts) then
      begin
      if not (lSeg[lLen] in cBreakChars) or (lLen=1) then
        Fail(lName+': line "'+lSeg+'" does not end after a break character, in "'+lResult+'"');
      lRest:=String.Join('',Copy(lParts,I+1,Length(lParts)));
      if (lLen<=aMaxCol) and ((lLen+Length(lRest)<=aMaxCol) or HasBreakChar(lRest,1,aMaxCol-lLen,cBreakChars)) then
        Fail(lName+': line "'+lSeg+'" is not cut at the last break point within MaxCol, in "'+lResult+'"');
      end;
    end;
end;


// Fixed cases with known results
procedure TestFixed;

const
  cZ = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz';

var
  lBefore, lExpected: String;

begin
  lBefore:='This is quite a long string, at least 50 chars long, I am going to sleep '+cZ+#0+sLineBreak;
  lExpected:='This is '+sLineBreak+'quite a '+sLineBreak+'long '+sLineBreak+'string, '+sLineBreak
    +'at least '+sLineBreak+'50 chars '+sLineBreak+'long, I '+sLineBreak+'am going '+sLineBreak
    +'to sleep '+sLineBreak+cZ+#0+sLineBreak;
  Inc(Checks);
  if WrapText(lBefore,10)<>lExpected then
    Fail('issue 39624 example: "'+Repr(WrapText(lBefore,10))+'"');

  // Lengths around MaxCol
  CheckBar('empty line','',10,'');
  CheckBar('shorter than MaxCol','aaa bbb',10,'aaa bbb');
  CheckBar('exactly MaxCol','aaaa bbbbb',10,'aaaa bbbbb');
  CheckBar('MaxCol plus one','aaaa bbbbbb',10,'aaaa |bbbbbb');
  CheckBar('break char in column MaxCol','aaaaaaaaa bbb',10,'aaaaaaaaa |bbb');
  CheckBar('break char in column MaxCol+1','aaaaaaaaaa bbb',10,'aaaaaaaaaa |bbb');
  CheckBar('break char beyond MaxCol only','aaaaaaaaaaaa bbb',10,'aaaaaaaaaaaa |bbb');
  CheckBar('last of several break chars','aa bb cc dd ee',10,'aa bb cc |dd ee');
  CheckBar('next break char beyond MaxCol','aaaa bbbbb cc',10,'aaaa |bbbbb cc');
  CheckBar('many short words','aa bb cc dd ee ff gg',5,'aa |bb |cc |dd |ee |ff gg');
  CheckBar('many short words, last cut','aa bb cc dd ee ff ggg',5,'aa |bb |cc |dd |ee |ff |ggg');
  CheckBar('hyphen','aa-bbbb cccc',10,'aa-bbbb |cccc');
  CheckBar('hyphen and space','aaaa-bbbb cccc-dddd',10,'aaaa-bbbb |cccc-dddd');

  // Words longer than MaxCol are not split
  CheckBar('single long word','aaaaaaaaaaaaaaa',10,'aaaaaaaaaaaaaaa');
  CheckBar('long word first','aaaaaaaaaaaaaaa bb',10,'aaaaaaaaaaaaaaa |bb');
  CheckBar('long word in the middle','aaaa bbbbbbbbbbbb cc',10,'aaaa |bbbbbbbbbbbb |cc');
  CheckBar('long word last','aaaa bbbbbbbbbbbbbbb',10,'aaaa |bbbbbbbbbbbbbbb');
  CheckBar('two long words','aaaaaaaaaaaa bbbbbbbbbbbb',10,'aaaaaaaaaaaa |bbbbbbbbbbbb');

  // Break characters at the start of a line
  CheckBar('leading space','  bbbbbbbbbbbbbbbb cc',10,'  |bbbbbbbbbbbbbbbb |cc');
  CheckBar('single leading space',' bbbbbbbbbbbbbbbb cc',10,' bbbbbbbbbbbbbbbb |cc');
  CheckBar('double space at the cut','aaaaaaaaa  bbbbbbbbbbbbb',10,'aaaaaaaaa | bbbbbbbbbbbbb');
  CheckBar('only spaces','    ',2,'  |  ');
  CheckBar('single space',' ',1,' ');

  // Break characters at the end
  CheckBar('trailing space','aaaa bbbbbb ',10,'aaaa |bbbbbb ');
  CheckBar('long word with trailing space','aaaaaaaaaaaa ',10,'aaaaaaaaaaaa ');
  CheckBar('trailing space at MaxCol','aaaaaaaaa ',10,'aaaaaaaaa ');

  // Small and invalid MaxCol
  CheckBar('MaxCol 1','a b c',1,'a |b |c');
  CheckBar('MaxCol 1, longer words','aa bb',1,'aa |bb');
  CheckBar('MaxCol 0','aa bb cc',0,'aa |bb |cc');
  CheckBar('MaxCol 0, leading space',' aa bb',0,' aa |bb');
  CheckBar('negative MaxCol','aa bb cc',-5,'aa |bb |cc');

  // Quotes
  CheckBar('break chars inside quotes','aa ''b c d e f'' gg',10,'aa |''b c d e f'' |gg');
  CheckBar('double quotes','aa "b c d e f" gg',10,'aa |"b c d e f" |gg');
  CheckBar('quote within MaxCol','''a b'' cc dddddd',10,'''a b'' cc |dddddd');
  CheckBar('unterminated quote','aa ''b c d e f g h',10,'aa ''b c d e f g h');
  CheckBar('other quote inside quote','aa "b '' c" dddddd eeee',10,'aa |"b '' c" |dddddd |eeee');
  CheckBar('break string inside quotes','aa ''b|c'' dd',10,'aa ''b|c'' |dd');

  // Existing break strings
  CheckBar('existing break','aaaa|bbbbb ccccc',10,'aaaa|bbbbb |ccccc');
  CheckBar('existing break resets column','aaaaaaaa|bbbbbbb cc',10,'aaaaaaaa|bbbbbbb cc');
  CheckBar('existing break, then wrap','aaaaaaaa|bbbbbbbb cc',10,'aaaaaaaa|bbbbbbbb |cc');
  CheckBar('existing break in column MaxCol+1','aaaa bbbbb|cc',10,'aaaa bbbbb|cc');
  CheckBar('existing break after MaxCol','aaaa bbbbbbb|cc',10,'aaaa |bbbbbbb|cc');
  CheckBar('existing break at the end','aaaa|',10,'aaaa|');
  CheckBar('existing break at the start','|aaaa',10,'|aaaa');
  CheckBar('consecutive existing breaks','aa||bb',10,'aa||bb');
  Check('two-character break string','aaaa bbbbbb cc',#13#10,cBreakChars,10,'aaaa '#13#10'bbbbbb cc');
  Check('existing two-character break','aaaa'#13#10'bbbbbbb cc',#13#10,cBreakChars,10,'aaaa'#13#10'bbbbbbb cc');
  Check('first character of break string alone','aaaa'#13'bbbbbbbb cc',#13#10,cBreakChars,10,'aaaa'#13'bbbbbbbb '#13#10'cc');

  // Empty break string and break characters
  Check('empty break string','aaaa bbbbbb cc','',cBreakChars,10,'aaaa bbbbbb cc');
  Check('empty break string with #0','ab'#0'cd ef','',cBreakChars,3,'ab'#0'cd ef');
  Check('no break characters','aaaa bbbbbb cc','|',[],10,'aaaa bbbbbb cc');
  Check('no break characters, existing break','aaaa|bbbbbb cc','|',[],5,'aaaa|bbbbbb cc');

  // Default overload: line break, space, hyphen and tab
  Inc(Checks);
  if WrapText('aaaa'#9'bbbbbb cc',10)<>'aaaa'#9+sLineBreak+'bbbbbb cc' then
    Fail('default overload with tab: "'+Repr(WrapText('aaaa'#9'bbbbbb cc',10))+'"');
  Inc(Checks);
  if WrapText('aaaa-bbbbbb cc',10)<>'aaaa-'+sLineBreak+'bbbbbb cc' then
    Fail('default overload with hyphen: "'+Repr(WrapText('aaaa-bbbbbb cc',10))+'"');
end;


// Compares WrapText with RefWrap on random lines
procedure TestReference;

const
  cAlphabet = 'aaaaab  --''"|';

var
  I, lMaxCol: Integer;
  lLine, lBreakStr: String;

begin
  for I:=1 to 20000 do
    begin
    lLine:=RandomLine(cAlphabet,40);
    lMaxCol:=Random(14)-1;
    if Odd(I) then
      lBreakStr:='|'
    else
      lBreakStr:='a|';
    Check('reference',lLine,lBreakStr,cBreakChars,lMaxCol,RefWrap(lLine,lBreakStr,cBreakChars,lMaxCol));
    end;
end;


// Checks the invariants on random lines without quotes
procedure TestInvariants;

var
  I: Integer;

begin
  for I:=1 to 20000 do
    CheckInvariants(RandomLine('aaaab  -',50),1+Random(15));
end;


begin
  RandSeed:=39624;
  TestFixed;
  TestReference;
  TestInvariants;
  if Failures>0 then
    begin
    Writeln(Failures,' of ',Checks,' checks failed');
    Halt(1);
    end;
  Writeln('ok, ',Checks,' checks');
end.
