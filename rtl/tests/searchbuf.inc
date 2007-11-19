type
  TEqualFunction = function (const a,b : char) : boolean;

function EqualWithCase (const a,b : char) : boolean;
begin
  result := (a = b);
end;

function EqualWithoutCase (const a,b : char) : boolean;
begin
  result := (lowerCase(a) = lowerCase(b));
end;

function IsWholeWord (bufstart, bufend, wordstart, wordend : pchar) : boolean;
begin
            // Check start
  result := ((wordstart = bufstart) or ((wordstart-1)^ in worddelimiters)) and
            // Check end
            ((wordend = bufend) or ((wordend+1)^ in worddelimiters));
end;

function SearchDown(buf,aStart,endchar:pchar; SearchString:string;
    equal : TEqualFunction; WholeWords:boolean) : pchar;
var Found : boolean;
    s, c : pchar;
begin
  result := aStart;
  Found := false;
  while not Found and (result <= endchar) do
    begin
    // Search first letter
    while (result <= endchar) and not Equal(result^,SearchString[1]) do
      inc (result);
    // Check if following is searchstring
    c := result;
    s := @(Searchstring[1]);
    Found := true;
    while (c <= endchar) and (s^ <> #0) and Found do
      begin
      Found := Equal(c^, s^);
      inc (c);
      inc (s);
      end;
    if s^ <> #0 then
      Found := false;
    // Check if it is a word
    if Found and WholeWords then
      Found := IsWholeWord(buf,endchar,result,c-1);
    if not found then
      inc (result);
    end;
  if not Found then
    result := nil;
end;

Function SearchUp(buf,aStart,endchar:pchar; SearchString:string;
    equal : TEqualFunction; WholeWords:boolean) : pchar;
var Found : boolean;
    s, c, l : pchar;
begin
  aStart := buf;
  Found := false;
  l := @(SearchString[length(SearchString)]);
  while not Found and (result >= buf) do
    begin
    // Search last letter
    while (result >= buf) and not Equal(result^,l^) do
      dec (result);
    // Check if before is searchstring
    c := result;
    s := l;
    Found := true;
    while (c >= buf) and (s >= @SearchString[1]) and Found do
      begin
      Found := Equal(c^, s^);
      dec (c);
      dec (s);
      end;
    if (s >= @(SearchString[1])) then
      Found := false;
    // Check if it is a word
    if Found and WholeWords then
      Found := IsWholeWord(buf,endchar,c+1,result);
    if found then
      result := c+1
    else
      dec (result);
    end;
  if not Found then
    result := nil;
end;

//function SearchDown(buf,aStart,endchar:pchar; SearchString:string; equal : TEqualFunction; WholeWords:boolean) : pchar;
function SearchBuf(Buf: PChar;BufLen: Integer;SelStart: Integer;SelLength: Integer;
    SearchString: String;Options: TStringSearchOptions):PChar;
var
  equal : TEqualFunction;
begin
  {$ifdef noexception}
  if SearchString = '' then
    result := nil
  else
    begin
  {$endif}
    if soMatchCase in Options then
      Equal := @EqualWithCase
    else
      Equal := @EqualWithoutCase;
    if soDown in Options then
      SearchDown(buf,buf+SelStart+SelLength,Buf+(BufLen-1), SearchString, Equal, (soWholeWord in Options))
    else
      SearchUp(buf,buf+SelStart+SelLength,Buf+(BufLen-1), SearchString, Equal, (soWholeWord in Options))
    {$ifdef noexception}
    end;
    {$endif}
end;

