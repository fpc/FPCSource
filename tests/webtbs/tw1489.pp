{$ifdef fpc}{$mode delphi}{$endif}

uses classes;

var
  StrList : TStringList;
  x                             : Integer;
  Para : string;
begin
  Para:='Hello,"This","i"s",a,"Test for"," TStringList"';
  StrList := TStringList.Create;

  writeln('Parameter: '+Para);
  StrList.CommaText := Para;
  writeln('Strings:');
  for x := 0 to StrList.Count-1 do
    writeln(StrList.Strings[x]);
  writeln('CommaText: '+StrList.CommaText);
  if StrList.CommaText<>'Hello,This,i,"s""",a,"Test for"," TStringList"' then
   begin
     writeln('ERROR!');
     halt(1);
   end;
  StrList.Free;
end.
