{$ifdef fpc}{$mode objfpc}{$h+}{$endif}
uses
  Variants, SysUtils;

var
  v: Variant;
  code: integer;

begin
  code := 0;
  v := 10;
  try
    writeln(Format('%s', [v]));
  except
    ShowException(exceptObject, exceptAddr);
    code := code or 1;
  end;  
  v := 'foo';
  try
    writeln(Format('%s', [v]));
  except
    ShowException(exceptObject, exceptAddr);
    code := code or 2;
  end;  
  v := 1.5;
  try
    writeln(Format('%s', [v]));
  except
    ShowException(exceptObject, exceptAddr);
    code := code or 4;
  end;
  Halt(code);
end.

