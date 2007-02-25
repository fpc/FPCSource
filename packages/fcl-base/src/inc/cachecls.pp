{

    Generic cache class for FCL
    Copyright (C) 2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit CacheCls;

interface

uses SysUtils;

resourcestring
  SInvalidIndex = 'Invalid index %i';

type

{ TCache }

  TCache = class;

  ECacheError = class(Exception);

  { All slots are contained both in an array and in a double-linked list.
    * The array, which doesn't need any additional memory, can be used for fast
      sequential access; its indices can be exported to the user of the cache.
    * The linked list is used for on-the-fly reordering of the elements, so
      that the elements are MRU-sorted: The most recently used element is the
      head, the last recently used element is the tail of the list. We need a
      double-linked list: When the MRU value of an element changes, we will
      have to walk the list reversed, but when we are searching for an entry,
      we will search starting from the head. }

  PCacheSlot = ^TCacheSlot;
  TCacheSlot = record
    Prev, Next: PCacheSlot;     // -> double-linked list
    Data: Pointer;              // The custom data associated with this element
    Index: Integer;             // The array index of this slot
  end;

  PCacheSlotArray = ^TCacheSlotArray;
  TCacheSlotArray = array[0..MaxInt div SizeOf(TCacheSlot) - 1] of TCacheSlot;

  TOnIsDataEqual = function(ACache: TCache;
    AData1, AData2: Pointer): Boolean of object;
  TOnFreeSlot = procedure(ACache: TCache; SlotIndex: Integer) of object;


  { TCache implements a generic cache class.
    If you use the "Add" method and not only "AddNew", you will have to set
    the "OnIsDataEqual" property to your own compare function! }

  TCache = class
  private
    FOnIsDataEqual: TOnIsDataEqual;
    FOnFreeSlot: TOnFreeSlot;
    function GetData(SlotIndex: Integer): Pointer;
    function GetSlot(SlotIndex: Integer): PCacheSlot;
    procedure SetData(SlotIndex: Integer; AData: Pointer);
    procedure SetMRUSlot(ASlot: PCacheSlot);
    procedure SetSlotCount(ACount: Integer);
  protected
    FSlotCount: Integer;        // Number of cache elements
    FSlots: PCacheSlotArray;
    FMRUSlot,                   // First slot in MRU-sorted list
      FLRUSlot: PCacheSlot;     // Last slot in MRU-sorted list
  public
    constructor Create(ASlotCount: Integer);
    destructor Destroy; override;

    function Add(AData: Pointer): Integer;              // Checks for duplicates
    function AddNew(AData: Pointer): Integer;           // No duplicate checks
    function FindSlot(AData: Pointer): PCacheSlot;      // nil => not found
    function IndexOf(AData: Pointer): Integer;          // -1 => not found
    procedure Remove(AData: Pointer);

    // Accesses to the "Data" array will be reflected by the MRU list!
    property Data[SlotIndex: Integer]: Pointer read GetData write SetData;
    property MRUSlot: PCacheSlot read FMRUSlot write SetMRUSlot;
    property LRUSlot: PCacheSlot read FLRUSlot;
    property SlotCount: Integer read FSlotCount write SetSlotCount;
    property Slots[SlotIndex: Integer]: PCacheSlot read GetSlot;

    property OnIsDataEqual: TOnIsDataEqual
      read FOnIsDataEqual write FOnIsDataEqual;
    { OnFreeSlot is called when a slot is being released. This can only happen
      during Add or AddNew, when there is no more free slot available. }
    property OnFreeSlot: TOnFreeSlot read FOnFreeSlot write FOnFreeSlot;
  end;


implementation


{ TCache }

function TCache.GetData(SlotIndex: Integer): Pointer;
begin
  if (SlotIndex < 0) or (SlotIndex >= SlotCount) then
    raise ECacheError.CreateFmt(SInvalidIndex, [SlotIndex]);
  MRUSlot := @FSlots^[SlotIndex];
  Result := MRUSlot^.Data;
end;

function TCache.GetSlot(SlotIndex: Integer): PCacheSlot;
begin
  if (SlotIndex < 0) or (SlotIndex >= SlotCount) then
    raise ECacheError.CreateFmt(SInvalidIndex, [SlotIndex]);
  Result := @FSlots^[SlotIndex];
end;

procedure TCache.SetData(SlotIndex: Integer; AData: Pointer);
begin
  if (SlotIndex < 0) or (SlotIndex >= FSlotCount) then
    raise ECacheError.CreateFmt(SInvalidIndex, [SlotIndex]);
  MRUSlot := @FSlots^[SlotIndex];
  MRUSlot^.Data := AData;
end;

procedure TCache.SetMRUSlot(ASlot: PCacheSlot);
begin
  if ASlot <> FMRUSlot then
  begin
    // Unchain ASlot
    if Assigned(ASlot^.Prev) then
      ASlot^.Prev^.Next := ASlot^.Next;
    if Assigned(ASlot^.Next) then
      ASlot^.Next^.Prev := ASlot^.Prev;

    if ASlot = FLRUSlot then
      FLRUSlot := ASlot^.Prev;

    // Make ASlot the head of the double-linked list
    ASlot^.Prev := nil;
    ASlot^.Next := FMRUSlot;
    FMRUSlot^.Prev := ASlot;
    FMRUSlot := ASlot;
    if not Assigned(FMRUSlot^.Next) then
      FLRUSlot := FMRUSlot;
  end;
end;

procedure TCache.SetSlotCount(ACount: Integer);
var
  Slot: PCacheSlot;
  i: Integer;
begin
  if ACount <> SlotCount then
  begin
    if ACount < SlotCount then
    begin
      // Remove slots

      if Assigned(OnFreeSlot) then
        for i := ACount to SlotCount - 1 do
          OnFreeSlot(Self, i);

      while (MRUSlot^.Index >= ACount) and Assigned(MRUSlot^.Next) do
        FMRUSlot := MRUSlot^.Next;
      MRUSlot^.Prev := nil;

      while (LRUSlot^.Index >= ACount) and Assigned(LRUSlot^.Prev) do
        FLRUSlot := LRUSlot^.Prev;
      LRUSlot^.Next := nil;

      Slot := MRUSlot^.Next;
      while Assigned(Slot) do
      begin
        if Slot^.Index >= ACount then
        begin
          Slot^.Prev^.Next := Slot^.Next;
          if Assigned(Slot^.Next) then
            Slot^.Next^.Prev := Slot^.Prev;
        end;
        Slot := Slot^.Next;
      end;

      ReallocMem(FSlots, ACount * SizeOf(TCacheSlot));
    end else
    begin
      // Add new slots
      ReallocMem(FSlots, ACount * SizeOf(TCacheSlot));
      for i := SlotCount to ACount - 1 do
        with FSlots^[i] do
        begin
          Prev := @FSlots^[i + 1];
          Next := @FSlots^[i - 1];
          Data := nil;
          Index := i;
        end;
      LRUSlot^.Next := @FSlots^[ACount - 1];
      FSlots^[ACount - 1].Prev := LRUSlot;
      FLRUSlot := @FSlots^[SlotCount];
      FLRUSlot^.Next := nil;
    end;
    FSlotCount := ACount;
  end;
end;

constructor TCache.Create(ASlotCount: Integer);
var
  i: Integer;
begin
  inherited Create;
  FSlotCount := ASlotCount;

  if FSlotCount = 0 then
    exit;

  { Allocate the slots and initialize the double-linked list.
    Note: The list is set up so that the last recently used
    slot is the first slot! }

  GetMem(FSlots, FSlotCount * SizeOf(TCacheSlot));

  FMRUSlot := @FSlots^[FSlotCount - 1];
  FLRUSlot := @FSlots^[0];

  with FSlots^[0] do
  begin
    if FSlotCount > 1 then
      Prev := @FSlots^[1]
    else
      Prev := nil;
    Next := nil;
    Data := nil;
    Index := 0;
  end;

  for i := 1 to FSlotCount - 2 do
    with FSlots^[i] do
    begin
      Next := @FSlots^[i - 1];
      Prev := @FSlots^[i + 1];
      Data := nil;
      Index := i;
    end;

  with FSlots^[FSlotCount - 1] do
  begin
    Prev := nil;
    if FSlotCount > 1 then
      Next := @FSlots^[FSlotCount - 2];
    Data := nil;
    Index := FSlotCount - 1;
  end;
end;

destructor TCache.Destroy;
begin
  FreeMem(FSlots);
  inherited Destroy;
end;

function TCache.Add(AData: Pointer): Integer;
var
  Slot: PCacheSlot;
begin
  Slot := FindSlot(AData);
  if Assigned(Slot) then
  begin
    MRUSlot := Slot;
    Result := Slot^.Index;
  end else
    Result := AddNew(AData);
end;

function TCache.AddNew(AData: Pointer): Integer;
begin
  if Assigned(OnFreeSlot) then
    OnFreeSlot(Self, LRUSlot^.Index);
  MRUSlot := LRUSlot;
  MRUSlot^.Data := AData;
  Result := MRUSlot^.Index;
end;

function TCache.FindSlot(AData: Pointer): PCacheSlot;
begin
  ASSERT((SlotCount = 0) or Assigned(OnIsDataEqual));
  Result := MRUSlot;
  while Assigned(Result) do
  begin
    if OnIsDataEqual(Self, Result^.Data, AData) then
      exit;
    Result := Result^.Next;
  end;
end;

function TCache.IndexOf(AData: Pointer): Integer;
var
  Slot: PCacheSlot;
begin
  ASSERT((SlotCount = 0) or Assigned(OnIsDataEqual));
  Slot := MRUSlot;
  while Assigned(Slot) do
  begin
    if OnIsDataEqual(Self, Slot^.Data, AData) then
    begin
      Result := Slot^.Index;
      exit;
    end;
    Slot := Slot^.Next;
  end;
  indexof := -1;
end;

procedure TCache.Remove(AData: Pointer);
var
  Slot: PCacheSlot;
begin
  Slot := FindSlot(AData);
  if Assigned(Slot) then
    Slot^.Data := nil;
end;


end.
