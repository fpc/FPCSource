program tb9000;

{$mode delphi}

uses
  Classes,
  Generics.Collections;

var
  List: TList<Integer>;
  ObjList: TObjectList<TObject>;
begin
  List := TList<Integer>.Create;
  try
    List.AddRange([0, 1, 0, 2, 0, 3, 3, 0, 2, 0, 1, 0]);
    if List.Count <> 12 then Halt(1);

    // remove 2
    List.Pack(
      function(const L, R: Integer): Boolean
      begin
        Result := L = 2;
      end
    );
    if List.Count <> 10 then Halt(2);

    // remove zeros
    List.Pack;
    if List.Count <> 4 then Halt(3);
  finally
    List.Free;
  end;

  ObjList := TObjectList<TObject>.Create;
  try
    ObjList.Add(TObject.Create);
    ObjList.Add(nil);
    ObjList.Add(TStream.Create);
    ObjList.Add(nil);
    ObjList.Add(TObject.Create);
    ObjList.Add(nil);
    ObjList.Add(TStream.Create);
    if ObjList.Count <> 7 then Halt(4);

    // remove nil
    ObjList.Pack;
    if ObjList.Count <> 4 then Halt(5);

    // remove all TStream
    ObjList.Pack(
      function(const L, R: TObject): Boolean
      begin
        Result := L is TStream;
      end);
    if ObjList.Count <> 2 then Halt(6);

  finally
    ObjList.Free;
  end;
end.
