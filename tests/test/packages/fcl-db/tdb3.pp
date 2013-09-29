program tdb3;
// Test for bug 12875
{$mode objfpc}{$H+}

uses
  Classes, SysUtils, dbf, db;

var
  Db1: TDbf;
  DateFloat: Double;
  DateStr: String;
begin
  Db1 := TDbf.Create(nil);
  Db1.FilePathFull := ExtractFilePath(ParamStr(0));
  Db1.TableName := 'testdatestr.dbf';
  if not FileExists(Db1.TableName) then
  begin
    Db1.FieldDefs.Add('DateField', ftDate);
    Db1.CreateTable;
  end;

  Db1.Open;
  Db1.Append;
  Db1.Post;

  DateStr :=  Db1.FieldByName('DateField').AsString;
  DateFloat := Db1.FieldByName('DateField').AsFloat;
  if DateFloat<>0 then Halt(1);
  WriteLn('DateStr: "',  DateStr, '" DateFloat: ', DateFloat);

  Db1.Edit;
  Db1.FieldByName('DateField').AsDateTime := Date;
  Db1.Post;
  DateStr :=  Db1.FieldByName('DateField').AsString;
  DateFloat := Db1.FieldByName('DateField').AsFloat;
  WriteLn('DateStr: "',  DateStr, '" DateFloat: ', DateFloat);

  Db1.Edit;
  Db1.FieldByName('DateField').AsString := '';
  Db1.Post;
  DateStr :=  Db1.FieldByName('DateField').AsString;
  DateFloat := Db1.FieldByName('DateField').AsFloat;
  WriteLn('DateStr: "',  DateStr, '" DateFloat: ', DateFloat);
  if not Db1.FieldByName('DateField').IsNull then Halt(1);
  if DateFloat<>0 then Halt(1);

  Db1.Free;
  DeleteFile('testdatestr.dbf');
end.


