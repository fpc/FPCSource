program demostorage;

{$mode objfpc}
{$h+}

uses wasm.storage.objects;

procedure DumpKeys(aStorage : TWasmStorage);

var
  I,lCount : Integer;
  lKey,lValue : UTF8String;

begin
  lCount:=aStorage.Count;
  Writeln('Have ',lCount,' keys');
  For I:=0 to lCount-1 do
    begin
    lKey:=aStorage.Keys[i];
    lValue:=aStorage.Items[lKey];
    Writeln('[',I,'] ',lKey,' = "',lValue,'"');
    end;
end;

var
  lStorage : TWasmLocalStorage;
  lKey,lValue : UTF8String;

begin
  lStorage:=TWasmLocalStorage.Create;
  Writeln('Existing keys at start: ');
  DumpKeys(lStorage);
  Writeln('Setting key "xyz" to value "123"');
  lStorage.Items['xyz']:='123';
  Writeln('Retrieving value of key "xyz" : "',lStorage.Items['xyz'],'"');
  Writeln('Deleting key "xyz"');
  lStorage.Remove('xyz');
  Writeln('Retrieving value of key "xyz" (again): "',lStorage.Items['xyz'],'"');
  Writeln('Existing keys at end: ');
  DumpKeys(lStorage);
end.

