{$ifdef fpc}{$mode objfpc}{$endif}

uses classes, sysutils;

var list : TStringList;

begin
  list := TStringList.Create;
  try
    try
      list.commatext := '"OK"';
      writeln ('---');
      writeln (list.text);
      writeln ('---');
    except
      on e:exception do
          begin
            writeln('Exception: '+e.message);
            halt(1);
          end;
    end;
    try
      //Failed
      list.commatext := '';
      writeln ('---');
      writeln (list.text);
      writeln ('---');
    except
      on e:exception do
          begin
            writeln('Exception: '+e.message);
            halt(1);
          end;
    end;
  finally
    list.Free;
    writeln ('Freeing list');
  end;
end.
