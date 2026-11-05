{ %CPU=x86_64 }
{ %OPT=-O3 }
program tw41867;

{ The constant 0 assigned to Length_ and Budget in two sibling branches is loaded
  once into a register that both of a block's CMOVs read. The conversion of the
  second CMOV into a MOV removed that load, so the first CMOV read whatever the
  register held: on x86_64-win64, the 5000 that Clobber left in RCX }

{$mode delphi}
{$asmmode intel}

type
  TValue = Int64;

  PState = ^TState;
  TState = record
    Active: Boolean;
    Kind: Byte;
    Capacity: TValue;
    Size: TValue;
    Window: TValue;
    Pending: TValue;
    Ticks: TValue;
    Sequence: TValue;
  end;

var
  Sink: TValue;
  States: array[0..3] of TState;

{ Small enough for the loop in Service to end if Length_ receives it }
procedure Clobber; assembler; nostackframe; {$ifdef unix}sysv_abi_default;{$endif}
asm
  mov rcx,5000
  mov r8,rcx
  mov r9,rcx
  mov r10,rcx
  mov r11,rcx
end;

function Emit(const aIndex: TValue; const aSize: TValue): TValue;
begin
  Sink := Sink + aIndex + aSize;
  Clobber;
  Result := Sink and 3;
end;

function Receive(const aBudget: TValue): TValue;
begin
  if aBudget <= 0 then
    Result := 0
  else if aBudget > 1400 then
    Result := 1400
  else
    Result := aBudget;
end;

function Service(const aIndex: TValue; const aService: Boolean): TValue;
var
  State: PState;
  Length_, Budget, Sent, Limit, Offset: TValue;
begin
  Result := -999;
  State := @States[aIndex];
  if State^.Active then
    case State^.Kind of
      1, 2, 3, 4:
        begin
          Length_ := 0;
          if aService then
            begin
              if (State^.Pending > 0) and (State^.Kind <> 3) then
                begin
                  Sent := State^.Pending shr 1;
                  if Sent > 0 then
                    begin
                      State^.Pending := State^.Pending - Sent;
                      Sink := Sink + Emit(aIndex, 0);
                    end;
                end;

              if State^.Size >= State^.Capacity then
                begin
                  Length_ := 0;
                  Budget := 0;
                end
              else if State^.Size >= State^.Window then
                begin
                  Length_ := 0;
                  Budget := 0;
                end
              else
                begin
                  Budget := State^.Capacity - State^.Size;
                  if Budget > (State^.Window - State^.Size) then
                    Budget := State^.Window - State^.Size;
                  Length_ := Receive(Budget);
                end;

              if Length_ > 0 then
                begin
                  State^.Size := State^.Size + Length_;
                  Offset := 0;
                  Limit := 1400;
                  while Offset < Length_ do
                    begin
                      if (Length_ - Offset) > Limit then
                        Offset := Offset + Limit
                      else
                        Offset := Length_;
                      State^.Sequence := State^.Sequence + Offset;
                    end;
                  State^.Ticks := 0;
                end;

              if Budget < 0 then
                Sink := Sink + 1;
            end;
          Result := Length_;
        end;
    end;
end;

procedure Prepare(const aIndex: TValue; const aCapacity, aSize, aWindow, aPending: TValue);
begin
  FillChar(States[aIndex], SizeOf(TState), #0);
  States[aIndex].Active := True;
  States[aIndex].Kind := 1;
  States[aIndex].Capacity := aCapacity;
  States[aIndex].Size := aSize;
  States[aIndex].Window := aWindow;
  States[aIndex].Pending := aPending;
end;

var
  Got: TValue;

begin
  Prepare(0, 100, 200, 300, 64);
  Got := Service(0, True);
  if Got <> 0 then
    begin
      WriteLn('Size >= Capacity: got ', Got, ', expected 0');
      Halt(1);
    end;

  Prepare(1, 300, 200, 100, 64);
  Got := Service(1, True);
  if Got <> 0 then
    begin
      WriteLn('Size >= Window: got ', Got, ', expected 0');
      Halt(2);
    end;

  WriteLn('ok');
end.
