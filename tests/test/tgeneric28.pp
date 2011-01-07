{ %norun }
program tgeneric28;
{$mode delphi}

// check delphi generic syntax

type
  TGenericClass<T> = class
    F: T;
  end;

  TGenericRecord<T> = record
    F: T;
  end;

  TGenericArray<T> = array of T;

var
  ClassSpecialize: TGenericClass<Integer>;
  RecordSpecialize: TGenericRecord<Integer>;
  ArraySpecialize: TGenericArray<Integer>;
begin
end.

