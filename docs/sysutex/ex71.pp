Program example71;

{$mode objfpc}

{ This program demonstrates the Format function }

Uses sysutils;

Var P : Pointer;
    fmt,S : string;

Procedure TestInteger;

begin
  Try
    Fmt:='[%d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%%]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    fmt:='[%.4d]';S:=Format (fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10.4d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10.4d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10d]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10.4d]';S:=Format (fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*d]';S:=Format (fmt,[4,5,10]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestHexaDecimal;

begin
  try
    Fmt:='[%x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10.4x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10.4x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10x]';S:=Format (Fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10.4x]';S:=Format (fmt,[10]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*x]';S:=Format (fmt,[4,5,10]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestPointer;

begin
  P:=Pointer(1234567);
  try
    Fmt:='[0x%p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%10p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%10.4p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%0:p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%0:10p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%0:10.4p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%0:-10p]';S:=Format (Fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[0x%0:-10.4p]';S:=Format (fmt,[P]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*p]';S:=Format (fmt,[4,5,P]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestString;

begin
  try
    Fmt:='[%s]';S:=Format(fmt,['This is a string']);Writeln(fmt:12,'=> ',s);
    fmt:='[%0:s]';s:=Format(fmt,['This is a string']);Writeln(fmt:12,'=> ',s);
    fmt:='[%0:18s]';s:=Format(fmt,['This is a string']);Writeln(fmt:12,'=> ',s);
    fmt:='[%0:-18s]';s:=Format(fmt,['This is a string']);Writeln(fmt:12,'=> ',s);
    fmt:='[%0:18.12s]';s:=Format(fmt,['This is a string']);Writeln(fmt:12,'=> ',s);
    fmt:='[%-*.*s]';s:=Format(fmt,[18,12,'This is a string']);Writeln(fmt:12,'=> ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestExponential;

begin
  Try
    Fmt:='[%e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10.4e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10.4e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10e]';S:=Format (Fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10.4e]';S:=Format (fmt,[1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*e]';S:=Format (fmt,[4,5,1.234]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestNegativeExponential;

begin
  Try
    Fmt:='[%e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10.4e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10.4e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10e]';S:=Format (Fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10.4e]';S:=Format (fmt,[-1.234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*e]';S:=Format (fmt,[4,5,-1.234]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestSmallExponential;

begin
  Try
    Fmt:='[%e]';S:=Format (Fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10e]';S:=Format (Fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10.4e]';S:=Format (Fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:e]';S:=Format (Fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10e]';S:=Format (Fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10.4e]';S:=Format (Fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10e]';S:=Format (Fmt,[0.0123]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10.4e]';S:=Format (fmt,[0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*e]';S:=Format (fmt,[4,5,0.01234]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

Procedure TestSmallNegExponential;

begin
  Try
    Fmt:='[%e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%10.4e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:10.4e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10e]';S:=Format (Fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%0:-10.4e]';S:=Format (fmt,[-0.01234]);writeln(Fmt:12,' => ',s);
    Fmt:='[%-*.*e]';S:=Format (fmt,[4,5,-0.01234]);writeln(Fmt:12,' => ',s);
  except
    On E : Exception do
      begin
      Writeln ('Exception caught : ',E.Message);
      end;
  end;
  writeln ('Press enter');
  readln;
end;

begin
  TestInteger;
  TestHexadecimal;
  TestPointer;
  TestExponential;
  TestNegativeExponential;
  TestSmallExponential;
  TestSmallNegExponential;
  teststring;
end.