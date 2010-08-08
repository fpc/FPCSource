{$mode objfpc}

Program BCDTest;

Uses SysUtils;

var
  gotexcept: boolean;
Begin
    WriteLn (BCDToInt ($1234)); { should retuen 1234 }
    if (BCDToInt ($1234)) <> 1234 then
      halt(1);

    gotexcept:=false;
    try
      WriteLn (BCDToInt ($A0));   { Invalid value }
    except
      gotexcept:=true;
    end;
    if not gotexcept then
      halt(1);

    WriteLn (BCDToInt ($7D));   { should return -7 }
    if (BCDToInt ($7D)) <> -7 then
      halt(2);
End.
