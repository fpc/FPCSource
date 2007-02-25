{

    Test program for the CacheCls unit
    Copyright (C) 2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program CacheTest;

{$MODE objfpc}

uses Strings, CacheCls;

type
  TCacheTester = class
  private
    TestCache: TCache;
    function TestCacheIsDataEqual(ACache: TCache; AData1, AData2: Pointer): Boolean;
    procedure TestCacheFreeSlot(ACache: TCache; SlotIndex: Integer);
  protected
    procedure AddString(const s: PChar);
    procedure DumpCache;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;



function TCacheTester.TestCacheIsDataEqual(ACache: TCache;
  AData1, AData2: Pointer): Boolean;
begin
  if (not Assigned(AData1)) or (not Assigned(AData2)) then
    Result := (not Assigned(AData1)) and (not Assigned(AData2))
  else
    Result := StrComp(PChar(AData1), PChar(AData2)) = 0;
end;

procedure TCacheTester.TestCacheFreeSlot(ACache: TCache; SlotIndex: Integer);
var
  p: PChar;
begin
  Write('  Cache slot #', SlotIndex, ' has been freed (content: ');
  p := PChar(ACache.Slots[SlotIndex]^.Data);
  if Assigned(p) then
    WriteLn('"', p, '")')
  else
    WriteLn('nil)');
end;

procedure TCacheTester.AddString(const s: PChar);
var
  i: Integer;
begin
  WriteLn('Adding string "', s, '"...');
  i := TestCache.Add(Pointer(s));
  WriteLn('string got cache index #', i);
  WriteLn('New cache state:');
  DumpCache;
  WriteLn;
end;

procedure TCacheTester.DumpCache;
var
  Slot, PrevSlot: PCacheSlot;
begin
  Slot := TestCache.MRUSlot;
  PrevSlot := nil;
  while Assigned(Slot) do
  begin
    Write('  Slot #', Slot^.Index, '  ');
    if Assigned(Slot^.Data) then
      Write('"', PChar(Slot^.Data), '"')
    else
      Write('nil');
    if Slot^.Prev <> PrevSlot then
    begin
      Write('  Slot^.Prev is invalid! (');
      if Assigned(Slot^.Prev) then
        Write('points to #', Slot^.Prev^.Index)
      else
        Write('nil');
      Write(')');
    end;
    WriteLn;
    PrevSlot := Slot;
    Slot := Slot^.Next;
  end;
end;

constructor TCacheTester.Create;
begin
  inherited Create;
  TestCache := TCache.Create(4);
  TestCache.OnIsDataEqual := @TestCacheIsDataEqual;
  TestCache.OnFreeSlot := @TestCacheFreeSlot;

  WriteLn('Initial cache state:');
  DumpCache;
  WriteLn;
end;

destructor TCacheTester.Destroy;
begin
  TestCache.Free;
  inherited Destroy;
end;

procedure TCacheTester.Run;
begin
  AddString('1st');
  AddString('2nd');
  AddString('3rd');
  AddString('4th');
  AddString('5th');
  AddString('3rd');
  AddString('2nd');
  WriteLn('Setting slot count to 2...');
  TestCache.SlotCount := 2;
  WriteLn('Cache state after resize:');
  DumpCache;
  WriteLn;
  AddString('4th');
  WriteLn('Setting slot count to 6...');
  TestCache.SlotCount := 6;
  WriteLn('Cache state after resize:');
  DumpCache;
  WriteLn;
  AddString('5th');
  AddString('6th');
  AddString('7th');
  AddString('8th');
end;


var
  CacheTester: TCacheTester;

begin
  CacheTester := TCacheTester.Create;
  CacheTester.Run;
  CacheTester.Free;
end.
