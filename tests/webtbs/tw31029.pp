program tw31029;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

uses
  TypInfo,
  variants;

type
  TBytes = array of Byte;

  {$M+}
  TMyObject = class
  private
    FDynArr1: TBytes;
    FDynArr2: TBytes;
    FDynArr3: TBytes;
    FDynArr4: TBytes;
    function GetDynArr2: TBytes;
    function GetDynArr3(AIndex: Integer): TBytes;
    procedure SetDynArr2(AValue: TBytes);
    procedure SetDynArr3(AIndex: Integer; AValue: TBytes);
  protected
    procedure CheckIndex(AIndex: Integer); inline;
    function GetDynArr4: TBytes; virtual;
    procedure SetDynArr4(AValue: TBytes); virtual;
  published
    property DynArr1: TBytes read FDynArr1 write FDynArr1;
    property DynArr2: TBytes read GetDynArr2 write SetDynArr2;
    property DynArr3: TBytes index 1 read GetDynArr3 write SetDynArr3;
    property DynArr4: TBytes read GetDynArr4 write SetDynArr4;
  end;
  {$M-}

  function TMyObject.GetDynArr2: TBytes;
  begin
    Result := FDynArr2;
  end;

  procedure TMyObject.SetDynArr2(AValue: TBytes);
  begin
    FDynArr2 := AValue;
  end;

  function TMyObject.GetDynArr3(AIndex: Integer): TBytes;
  begin
    Result := FDynArr3;
    CheckIndex(AIndex);
  end;

  procedure TMyObject.SetDynArr3(AIndex: Integer; AValue: TBytes);
  begin
    FDynArr3 := AValue;
    CheckIndex(AIndex);
  end;

  function TMyObject.GetDynArr4: TBytes;
  begin
    Result := FDynArr4;
  end;

  procedure TMyObject.SetDynArr4(AValue: TBytes);
  begin
    FDynArr4 := AValue;
  end;

  procedure TMyObject.CheckIndex(AIndex: Integer);
  begin
    if AIndex <> 1 then begin
      Writeln('Invalid property index: ', AIndex);
      Halt(1);
    end;
  end;

  procedure CheckArr(const A1, A2: TBytes; const AMsg: string; ACode: LongInt); inline;
  begin
    //Writeln(HexStr(Pointer(A1)), ' ', HexStr(Pointer(A2)));
    if A1 <> A2 then begin
      Writeln(AMsg);
      Halt(ACode);
    end;
  end;

  procedure CheckArrContents(const A1, A2: TBytes; const AMsg: string; ACode: LongInt);
  var
    valid: Boolean;
    i: LongInt;
  begin
    valid := True;
    if Length(A1) <> Length(A2) then
      valid := False;
    if valid then begin
      for i := Low(A1) to High(A1) do begin
        if A1[i] <> A2[i] then begin
          valid := False;
          Break;
        end;
      end;
    end;
    if not valid then begin
      Writeln(AMsg);
      Halt(ACode);
    end;
  end;

var
  VMyObject: TMyObject;
  VDynArr1, VDynArr2, VDynArr3, VDynArr4: TBytes;
  V: Variant;
begin
  VMyObject := TMyObject.Create;
  try
    { direct use of SetDynArrayProp }

    VMyObject.DynArr1 := nil;
    VDynArr1 := TBytes.Create(65, 66, 64);
    SetDynArrayProp(VMyObject, 'DynArr1', Pointer(VDynArr1));
    CheckArr(VMyObject.DynArr1, VDynArr1,
      'SetDynArrayProp: VMyObject.DynArr1 <> VDynArr1', 2);
    VMyObject.DynArr1 := TBytes.Create(65, 66, 64);
    VDynArr1 := GetDynArrayProp(VMyObject, 'DynArr1');
    CheckArr(VMyObject.DynArr1, VDynArr1,
      'GetDynArrayProp: VMyObject.DynArr1 <> VDynArr1', 3);

    VMyObject.DynArr2 := nil;
    VDynArr2 := TBytes.Create(65, 66, 64);
    SetDynArrayProp(VMyObject, 'DynArr2', Pointer(VDynArr2));
    CheckArr(VMyObject.DynArr2, VDynArr2,
      'SetDynArrayProp: VMyObject.DynArr2 <> VDynArr2', 4);
    VMyObject.DynArr2 := TBytes.Create(65, 66, 64);
    VDynArr2 := GetDynArrayProp(VMyObject, 'DynArr2');
    CheckArr(VMyObject.DynArr2, VDynArr2,
      'GetDynArrayProp: VMyObject.DynArr2 <> VDynArr2', 5);

    VMyObject.DynArr3 := nil;
    VDynArr3 := TBytes.Create(65, 66, 64);
    SetDynArrayProp(VMyObject, 'DynArr3', Pointer(VDynArr3));
    CheckArr(VMyObject.DynArr3, VDynArr3,
      'SetDynArrayProp: VMyObject.DynArr3 <> VDynArr3', 6);
    VMyObject.DynArr3 := TBytes.Create(65, 66, 64);
    VDynArr3 := GetDynArrayProp(VMyObject, 'DynArr3');
    CheckArr(VMyObject.DynArr3, VDynArr3,
      'GetDynArrayProp: VMyObject.DynArr3 <> VDynArr3', 7);

    VMyObject.DynArr4 := nil;
    VDynArr4 := TBytes.Create(65, 66, 64);
    SetDynArrayProp(VMyObject, 'DynArr4', Pointer(VDynArr4));
    CheckArr(VMyObject.DynArr4, VDynArr4,
      'SetDynArrayProp: VMyObject.DynArr4 <> VDynArr4', 8);
    VMyObject.DynArr4 := TBytes.Create(65, 66, 64);
    VDynArr4 := GetDynArrayProp(VMyObject, 'DynArr4');
    CheckArr(VMyObject.DynArr4, VDynArr4,
      'GetDynArrayProp: VMyObject.DynArr4 <> VDynArr4', 9);

    { indirect use through a variant (a single test should be enough) }
    VMyObject.DynArr1 := nil;
    VDynArr1 := TBytes.Create(65, 66, 64);
    V := Null;
    DynArrayToVariant(V, Pointer(VDynArr1), TypeInfo(VDynArr1));
    SetPropValue(VMyObject, 'DynArr1', V);
    CheckArrContents(VMyObject.DynArr1, VDynArr1,
      'SetPropValue: VMyObject.DynArr1 <> VDynArr1', 10);
    VMyObject.DynArr1 := TBytes.Create(65, 66, 64);
    V := GetPropValue(VMyObject, 'DynArr1');
    VDynArr1 := nil;
    DynArrayFromVariant(Pointer(VDynArr1), V, TypeInfo(VDynArr1));
    CheckArrContents(VMyObject.DynArr1, VDynArr1,
      'GetPropValue: VMyObject.DynArr1 <> VDynArr1', 10);

    WriteLn('All tests OK');
  finally
    VMyObject.Free;
  end;
end.

