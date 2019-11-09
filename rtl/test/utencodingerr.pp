unit utencodingerr;

{$mode delphi}
{$H+}

interface

uses
  SysUtils;

implementation

uses punit, utrtl;

Procedure DumpException(E : Exception);

begin
  If ShowDebugOutput then
    Writeln(E.ClassName, ' ', E.Message);
end;

Function encodingerrors : AnsiString;

var
  S: String;
  Bytes: TBytes;
  
begin
  Result:='';
  S := '';
  Bytes:=Nil;
  SetLength(Bytes, 0);
  try
    // invalid source array?
    TEncoding.UTF8.GetBytes(S, 1, -1, Bytes, 0);
    Exit('Error on 1');
  except on E: Exception do
    DumpException(E);
  end;
  S := 'Test';
  try
    // delphi raises a message "Invalid source array" while the problem is in
    // destination array in real
    TEncoding.UTF8.GetBytes(S, 0, 2, Bytes, 0);
    Exit('Error on 2');
  except on E: Exception do
    DumpException(E);
  end;
  SetLength(Bytes, 1);
  try
    // invalid count
    TEncoding.UTF8.GetBytes(S, 5, 2, Bytes, 0);
    Exit('Error on 3');
  except on E: Exception do
    DumpException(E);
  end;
  try
    // character index out of bounds
    TEncoding.UTF8.GetBytes(S, 0, 2, Bytes, 0);
    Exit('Error on 4');
  except on E: Exception do
    DumpException(E);
  end;
  try
    // invalid destination index
    TEncoding.UTF8.GetBytes(S, 1, 2, Bytes, -1);
    Exit('Error on 5');
  except on E: Exception do
    DumpException(E);
  end;
end;

initialization
  SysUtilsTest('utencodingerr',@encodingerrors);  
end.
