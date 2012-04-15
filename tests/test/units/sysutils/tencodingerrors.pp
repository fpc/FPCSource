program tencodingerrors;

{$mode delphi}{$H+}

uses
  SysUtils;

var
  S: String;
  Bytes: TBytes;
begin
  S := '';
  SetLength(Bytes, 0);
  try
    // invalid source array?
    TEncoding.UTF8.GetBytes(S, 1, -1, Bytes, 0);
    halt(1);
  except on E: Exception do
    WriteLn(E.ClassName, ' ', E.Message);
  end;
  S := 'Test';
  try
    // delphi raises a message "Invalid source array" while the problem is in
    // destination array in real
    TEncoding.UTF8.GetBytes(S, 0, 2, Bytes, 0);
    halt(2);
  except on E: Exception do
    WriteLn(E.ClassName, ' ', E.Message);
  end;
  SetLength(Bytes, 1);
  try
    // invalid count
    TEncoding.UTF8.GetBytes(S, 5, 2, Bytes, 0);
    halt(3);
  except on E: Exception do
    WriteLn(E.ClassName, ' ', E.Message);
  end;
  try
    // character index out of bounds
    TEncoding.UTF8.GetBytes(S, 0, 2, Bytes, 0);
    halt(4);
  except on E: Exception do
    WriteLn(E.ClassName, ' ', E.Message);
  end;
  try
    // invalid destination index
    TEncoding.UTF8.GetBytes(S, 1, 2, Bytes, -1);
    halt(5);
  except on E: Exception do
    WriteLn(E.ClassName, ' ', E.Message);
  end;
end.