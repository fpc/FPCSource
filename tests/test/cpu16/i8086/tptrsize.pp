{ %cpu=i8086 }

procedure CheckSize(ActualSize, ExpectedSize: Integer);
begin
  Writeln(ActualSize);
  if ActualSize <> ExpectedSize then
  begin
    Writeln('Error! Expected: ', ExpectedSize);
    Halt(1);
  end;
end;

type
  PNearInteger = ^Integer; near;
  PNearCsInteger = ^Integer; near 'CS';
  PNearDsInteger = ^Integer; near 'DS';
  PNearSsInteger = ^Integer; near 'SS';
  PNearEsInteger = ^Integer; near 'ES';
  PNearFsInteger = ^Integer; near 'FS';
  PNearGsInteger = ^Integer; near 'GS';
  PFarInteger = ^Integer; far;
  PHugeInteger = ^Integer; huge;
begin
  Write('SizeOf(PNearInteger) = ');
  CheckSize(SizeOf(PNearInteger), 2);
  Write('SizeOf(PNearCsInteger) = ');
  CheckSize(SizeOf(PNearCsInteger), 2);
  Write('SizeOf(PNearDsInteger) = ');
  CheckSize(SizeOf(PNearDsInteger), 2);
  Write('SizeOf(PNearSsInteger) = ');
  CheckSize(SizeOf(PNearSsInteger), 2);
  Write('SizeOf(PNearEsInteger) = ');
  CheckSize(SizeOf(PNearEsInteger), 2);
  Write('SizeOf(PNearFsInteger) = ');
  CheckSize(SizeOf(PNearFsInteger), 2);
  Write('SizeOf(PNearGsInteger) = ');
  CheckSize(SizeOf(PNearGsInteger), 2);
  Write('SizeOf(PFarInteger) = ');
  CheckSize(SizeOf(PFarInteger), 4);
  Write('SizeOf(PHugeInteger) = ');
  CheckSize(SizeOf(PHugeInteger), 4);

  Write('SizeOf(NearPointer) = ');
  CheckSize(SizeOf(NearPointer), 2);
  Write('SizeOf(NearCsPointer) = ');
  CheckSize(SizeOf(NearCsPointer), 2);
  Write('SizeOf(NearDsPointer) = ');
  CheckSize(SizeOf(NearDsPointer), 2);
  Write('SizeOf(NearSsPointer) = ');
  CheckSize(SizeOf(NearSsPointer), 2);
  Write('SizeOf(NearEsPointer) = ');
  CheckSize(SizeOf(NearEsPointer), 2);
  Write('SizeOf(NearFsPointer) = ');
  CheckSize(SizeOf(NearFsPointer), 2);
  Write('SizeOf(NearGsPointer) = ');
  CheckSize(SizeOf(NearGsPointer), 2);
  Write('SizeOf(FarPointer) = ');
  CheckSize(SizeOf(FarPointer), 4);
  Write('SizeOf(HugePointer) = ');
  CheckSize(SizeOf(HugePointer), 4);

  Writeln('Success!!!');
end.
