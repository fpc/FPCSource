uses SysUtils, System.RegularExpressionsCore;

Const
  Rec1 = 'Name:"John" Surname:"Doe" Email:"john@example.com"';
  Rec2 = 'Name:"Jane" Surname:"Dolina" Email:"jane@doe.com"';

procedure DumpMatch(aIndex : Integer; aRegex : TPerlRegex);

var
  I : integer;

begin
  Writeln('Match ',aIndex,':');
  Writeln('  offset: ',aRegex.MatchedOffset);
  Writeln('  text: ',aRegex.MatchedText);
  Writeln('  Group count: ',aRegex.GroupCount);
  For I:=1 to aRegex.GroupCount do
    Writeln('  [',IntToStr(I),'] : ',aRegex.Groups[I]);
  Writeln('  Named group count: ',aRegex.NameCount);
  For I:=0 to aRegex.GroupCount-1 do
    With aRegex do
      Writeln('  [',IntToStr(I),'] name: "',Names[I],'", value: "',NamedGroups[Names[i]],'"');
end;

var
  RegEx : TPerlRegex;
  aIndex : integer;

begin
  Regex:=TPerlRegex.Create;
  Regex.Subject:=Rec1+#10+Rec2;
  Regex.RegEx:='Name:"(?<Name>[\w]+?)".*?Surname:"(?<Surname>[\w]+?)".*?Email:"(?<Email>\b[\w.%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}\b)"';
  if not Regex.Match then
    Writeln('No match found')
  else
    begin
    aIndex:=1;
    DumpMatch(aIndex,Regex);
    While Regex.MatchAgain do
      begin
      Inc(aIndex);
      DumpMatch(aIndex,Regex);
      end;
    end;
end.