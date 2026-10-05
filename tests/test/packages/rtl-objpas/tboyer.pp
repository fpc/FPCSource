{$mode objfpc}

uses
  SysUtils, StrUtils;
const
  result1 : array of SizeInt = (1, 4, 7, 10, 13, 16);
  result2 : array of SizeInt = (7, 9);
  result3 : array of SizeInt = (1, 10);
  IdeoSpace2 = #$E3#$80#$80#$E3#$80#$80;
var
  a : array of SizeInt;
  i : LongInt;

{ Halts with aCode+1, aCode+2 or aCode+3 when the case insensitive search of
  aPattern in aSource does not find all matches at result3 }
procedure CheckInsensitive(const aSource, aPattern: string; aCode: LongInt);

var
  lMatches : array of SizeInt;
  l : LongInt;

begin
  if not FindMatchesBoyerMooreCaseInSensitive(aSource,aPattern,lMatches,true) then
    halt(aCode+1);
  if Length(lMatches)<>Length(result3) then
    halt(aCode+2);
  for l:=Low(lMatches) to High(lMatches) do
    if lMatches[l]<>result3[l] then
      halt(aCode+3);
end;


begin
  if FindMatchesBoyerMooreCaseSensitive('abcabcabcabcabcabcab','abcab',a,false) then
    begin
      if Length(a)<>1 then
        halt(2);
      if a[0]<>result1[0] then
        halt(3);
    end
  else
    halt(1);

  if FindMatchesBoyerMooreCaseSensitive('abcabcabcabcabcabcab','abcab',a,true) then
    begin
      if Length(a)<>Length(result1) then
        halt(12);
      for i:=Low(a) to High(a) do
        if a[i]<>result1[i] then
          halt(13);
    end
  else
    halt(11);

  if FindMatchesBoyerMooreCaseInSensitive('abcabcabcabcabcabcab','abcab',a,false) then
    begin
      if Length(a)<>1 then
        halt(22);
      if a[0]<>result1[0] then
        halt(23);
    end
  else
    halt(21);

  if FindMatchesBoyerMooreCaseInSensitive('abcabcabcabcabcabcab','abcab',a,true) then
    begin
      if Length(a)<>Length(result1) then
        halt(32);
      for i:=Low(a) to High(a) do
        if a[i]<>result1[i] then
          halt(33);
    end
  else
    halt(31);

  if FindMatchesBoyerMooreCaseInSensitive('abcabcabcAbcabcAbcab','abcaB',a,false) then
    begin
      if Length(a)<>1 then
        halt(42);
      if a[0]<>result1[0] then
        halt(43);
    end
  else
    halt(41);

  if FindMatchesBoyerMooreCaseInSensitive('abcabCabcAbcabcABcab','abcaB',a,true) then
    begin
      if Length(a)<>Length(result1) then
        halt(52);
      for i:=Low(a) to High(a) do
        if a[i]<>result1[i] then
          halt(53);
    end
  else
    halt(51);

  if FindMatchesBoyerMooreCaseInSensitive('hello hehehe','hehe',a,true) then
    begin
      if Length(a)<>Length(result2) then
        halt(62);
      for i:=Low(a) to High(a) do
        if a[i]<>result2[i] then
          halt(63);
    end
  else
    halt(61);

  { issue #32770 }
  CheckInsensitive('abbabbcdeabbabb','abbabb',70);
  CheckInsensitive('ABBABBcdeABBABB','abbabb',80);
  CheckInsensitive('abbabbcdeabbabb','abbABB',90);
  CheckInsensitive(IdeoSpace2+#$E7#$A9#$BA+IdeoSpace2,IdeoSpace2,100);
  if StringReplace(IdeoSpace2+#$E7#$A9#$BA+IdeoSpace2,IdeoSpace2,'',[rfReplaceAll,rfIgnoreCase],sraBoyerMoore)<>#$E7#$A9#$BA then
    halt(111);
  if StringReplace('abbabbcdeABBABB','abbabb','',[rfReplaceAll,rfIgnoreCase],sraBoyerMoore)<>'cde' then
    halt(112);

  writeln('ok');
end.
