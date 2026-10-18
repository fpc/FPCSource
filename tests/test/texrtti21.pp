program texrtti21;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

{ The ResultLocs of an interface, a class and a record method gives the address
  of the entry's slot that holds the result's locations, also for a class method
  without any parameter }

uses
  TypInfo;

{$RTTI EXPLICIT METHODS([vcPublic])}

type
  {$push}
  {$M+}
  ITest = interface
    function F(a: Byte): Byte;
  end;
  {$pop}

  TTest = class
  public
    function F(a: Byte): Byte;
    class function S: Byte; static;
  end;

  TRec = record
  public
    function F(a: Byte): Byte;
  end;

function TTest.F(a: Byte): Byte;
begin
  Result := a;
end;

class function TTest.S: Byte;
begin
  Result := 0;
end;

function TRec.F(a: Byte): Byte;
begin
  Result := a;
end;

procedure Fail(const aMsg: String);
begin
  Writeln(aMsg);
  Halt(1);
end;

{ the slot is the last field of the entry, and the Byte result comes back in one
  integer register; WebAssembly returns every value on its evaluation stack,
  which the location describes as a reference }
procedure CheckLocs(const aWhat: String; aLocs: PPParameterLocations; aTail: Pointer);
begin
  if not Assigned(aLocs) then
    Fail(aWhat + ': no result locations');
  if PByte(aLocs) + SizeOf(PParameterLocations) <> PByte(aTail) then
    Fail(aWhat + ': the result locations are not the last field of the entry');
  if not Assigned(aLocs^) then
    Fail(aWhat + ': the result locations are nil');
  if aLocs^^.Count <> 1 then
    Fail(aWhat + ': expected one result location');
{$ifdef cpuwasm32}
  if not aLocs^^.Location[0]^.Reference then
    Fail(aWhat + ': expected the result on the evaluation stack');
{$else}
  if aLocs^^.Location[0]^.Reference or (aLocs^^.Location[0]^.RegType <> TRegisterType.Int) then
    Fail(aWhat + ': expected the result in an integer register');
{$endif}
end;

var
  intftable: PIntfMethodTable;
  intfentry: PIntfMethodEntry;
  classtable: PVmtMethodExTable;
  classentry: PVmtMethodExEntry;
  rectable: PRecordMethodTable;
  recentry: PRecMethodExEntry;
  i, found: LongInt;
begin
  intftable := PInterfaceData(GetTypeData(TypeInfo(ITest)))^.MethodTable;
  found := 0;
  for i := 0 to intftable^.Count - 1 do begin
    intfentry := intftable^.Method[i];
    if intfentry^.Name = 'F' then begin
      CheckLocs('ITest.F', intfentry^.ResultLocs, intfentry^.Tail);
      Inc(found);
    end;
  end;
  if found <> 1 then
    Fail('ITest.F not found');

  classtable := PClassData(GetTypeData(TypeInfo(TTest)))^.ExMethodTable;
  found := 0;
  for i := 0 to classtable^.Count - 1 do begin
    classentry := classtable^.Method[i];
    if classentry^.Name = 'F' then begin
      CheckLocs('TTest.F', classentry^.ResultLocs, classentry^.Tail);
      Inc(found);
    end else if classentry^.Name = 'S' then begin
      if classentry^.ParamCount <> 0 then
        Fail('TTest.S: expected no parameters');
      CheckLocs('TTest.S', classentry^.ResultLocs, classentry^.Tail);
      Inc(found);
    end;
  end;
  if found <> 2 then
    Fail('TTest.F or TTest.S not found');

  rectable := PRecordData(GetTypeData(TypeInfo(TRec)))^.MethodTable;
  found := 0;
  for i := 0 to rectable^.Count - 1 do begin
    recentry := rectable^.Method[i];
    if recentry^.Name = 'F' then begin
      CheckLocs('TRec.F', recentry^.ResultLocs, recentry^.Tail);
      Inc(found);
    end;
  end;
  if found <> 1 then
    Fail('TRec.F not found');

  Writeln('ok');
end.
