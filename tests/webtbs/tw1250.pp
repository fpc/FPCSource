{$mode objfpc}

uses classes;

var list : TStringList;

begin
  list := TStringList.Create;
  try
    list.commatext := '"0","6","-1"';
    writeln ('---');
    writeln (list.text);
    writeln ('---');
  finally
    list.Free;
    writeln ('Freeing list');
  end;
end.
