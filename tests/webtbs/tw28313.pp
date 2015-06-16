program project1;

{$mode delphi}{$H+}

uses
  Classes;

var
   mode:integer;
   tabs:TStrings;
begin
   tabs:=TStringList.Create;
   mode:=0;
   try
    if not (Mode in [0..Tabs.Count-1]) then exit;
   finally
     tabs.Free;
   end;
end.

