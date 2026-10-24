const _COP0_STATUS_IEc = 1 shl  0; // Current interrupt enable

function _cop0_GetSTATUS: dword; assembler;
asm
  mfc0 $t0, $STATUS
  sw $t0, result
end;

procedure _cop0_SetSTATUS(value: dword); assembler;
asm
  mtc0 $a0, $STATUS
end;


procedure _enableInterrupts;
begin
  _cop0_setSTATUS(_cop0_getSTATUS or _COP0_STATUS_IEc);
end;


function _disableInterrupts: boolean;
var
  sr : dword;

begin
  sr:= _cop0_getSTATUS;
  _cop0_setSTATUS(sr and not _COP0_STATUS_IEc);
  result:= (sr and _COP0_STATUS_IEc) <> 0;
end;


const
  StackMarker = DWord($A5A5A5A5);
  UnmarkedMargin = PtrUInt(4096);
  SafetyMargin = SizeUInt(16384);
  MinimumReservation = SizeUInt(32768);

var
  watermarkBegin, watermarkEnd: PtrUInt;
  originalWatermarkEnd: PtrUInt = 0;
  peakObserved: SizeUInt = 0;
  borrowedEnd: PtrUInt = 0;
  stackLengthBeforeBorrow: SizeUInt = 0;
  stackBottomBeforeBorrow: PtrUInt = 0;

procedure InitStackWatermark;
var
  stackPointer, stackTop: PtrUInt;
  p: PDWord;
begin
  if borrowedEnd <> 0 then Exit;
  watermarkBegin := (PtrUInt(StackBottom) + 3) and not PtrUInt(3);
  watermarkEnd := watermarkBegin;
  stackPointer := CurrentSP;
  stackTop := PtrUInt(StackBottom) + StackLength;
  if stackPointer > stackTop then stackPointer := stackTop;
  if (stackPointer <= watermarkBegin) or
     (stackPointer - watermarkBegin <= UnmarkedMargin) then Exit;
  watermarkEnd := (stackPointer - UnmarkedMargin) and not PtrUInt(3);
  { Reinitialization may run from a shallower call frame. Keep the first
    watermark's upper boundary instead of painting nearer the live stack. }
  if originalWatermarkEnd = 0 then originalWatermarkEnd := watermarkEnd;
  if watermarkEnd > originalWatermarkEnd then watermarkEnd := originalWatermarkEnd;
  if watermarkEnd <= watermarkBegin then begin
    watermarkEnd := watermarkBegin;
    Exit;
  end;
  p := PDWord(watermarkBegin);
  { Stay below the live stack frame. No calls inside the marking loop. }
  while PtrUInt(p) < watermarkEnd do begin
    p^ := StackMarker;
    Inc(p);
  end;
end;

function SampleStackUsage(var used: SizeUInt): Boolean;
var
  p: PDWord;
  untouched: PtrUInt;
begin
  Result := False;
  used := 0;
  if (watermarkEnd <= watermarkBegin) or
     (watermarkBegin <> ((PtrUInt(StackBottom) + 3) and not PtrUInt(3))) then Exit;
  if borrowedEnd <> 0 then p := PDWord(borrowedEnd)
  else p := PDWord(watermarkBegin);
  while PtrUInt(p) < watermarkEnd do begin
    if p^ <> StackMarker then Break;
    Inc(p);
  end;
  untouched := PtrUInt(p) - PtrUInt(StackBottom);
  used := StackLength - untouched;
  if used > peakObserved then peakObserved := used;
  { With no intact marker at the bottom, the peak cannot be bounded. }
  if borrowedEnd <> 0 then Result := PtrUInt(p) > borrowedEnd
  else Result := PtrUInt(p) > watermarkBegin;
end;

function BorrowStackBuffer(bytes: SizeUInt): Pointer;
var
  used, required, loadingReserve, originalLength: SizeUInt;
  originalBottom: PtrUInt;
  interruptsEnabled, changed: Boolean;
begin
  Result := nil;
  if (borrowedEnd <> 0) or (bytes = 0) or
     (bytes > High(SizeUInt) - 3) then Exit;
  bytes := (bytes + 3) and not SizeUInt(3);
  if not SampleStackUsage(used) then Exit;
  if peakObserved > High(SizeUInt) - SafetyMargin then Exit;
  loadingReserve := peakObserved + SafetyMargin;
  if loadingReserve < MinimumReservation then loadingReserve := MinimumReservation;
  if bytes > High(SizeUInt) - loadingReserve - 63 then Exit;
  required := (bytes + loadingReserve + 63) and not SizeUInt(63);
  originalLength := StackLength;
  originalBottom := PtrUInt(StackBottom);
  { Fit the reservation to call-stack space plus this buffer, even when
    the default reservation is larger than the combined requirement. }
  if required <> StackLength then begin
    interruptsEnabled := _disableInterrupts;
    changed := SetStackLength(required);
    if changed then InitStackWatermark;
    if interruptsEnabled then _enableInterrupts;
    if not changed then Exit;
  end;
  if (watermarkEnd <= watermarkBegin) or
     (bytes > watermarkEnd - watermarkBegin) then Exit;
  borrowedEnd := watermarkBegin + bytes;
  stackLengthBeforeBorrow := originalLength;
  stackBottomBeforeBorrow := originalBottom;
  Result := Pointer(watermarkBegin);

//  WriteLn('STACK model source: address=', watermarkBegin, ' bytes=', bytes, ' call-stack reserve=', loadingReserve, ' total=', StackLength);

end;

procedure ReleaseStackBuffer;
var
  used: SizeUInt;
  releasedBegin, releasedEnd, clearEnd: PtrUInt;
  p: PDWord;
  interruptsEnabled, restored: Boolean;
begin
  if borrowedEnd = 0 then Exit;
  { Sample ABOVE the source before restoring its overwritten watermark.
    This retains loading's stack peak without counting file bytes as stack. }
  if not SampleStackUsage(used) then begin
    WriteLn('STACK model source guard overwritten');
    Halt;
  end;
  releasedBegin := watermarkBegin;
  releasedEnd := borrowedEnd;
  borrowedEnd := 0;
//  WriteLn('STACK restore request: bytes=', stackLengthBeforeBorrow, ' original bottom=$', PrintHexValue(stackBottomBeforeBorrow));
  interruptsEnabled := _disableInterrupts;
  restored := SetStackLength(stackLengthBeforeBorrow);
  clearEnd := releasedBegin;
  if restored then begin
    { A large source can temporarily extend below the original stack.
      These bytes return to free heap capacity, so remove stale markers
      there before marking only the restored stack reservation. }
    clearEnd := PtrUInt(StackBottom);
    if clearEnd > releasedEnd then clearEnd := releasedEnd;
    if clearEnd < releasedBegin then clearEnd := releasedBegin;
    p := PDWord(releasedBegin);
    while PtrUInt(p) < clearEnd do begin
      p^ := 0;
      Inc(p);
    end;
    InitStackWatermark;
  end else begin
    { Keep the current watermark usable if the RTL rejects restoration. }
    p := PDWord(releasedBegin);
    while PtrUInt(p) < releasedEnd do begin
      p^ := StackMarker;
      Inc(p);
    end;
  end;
  if interruptsEnabled then _enableInterrupts;
  if restored then begin
  {
    if clearEnd > releasedBegin then WriteLn('STACK cleared released source: [$', PrintHexValue(releasedBegin), ', $', PrintHexValue(clearEnd), ') bytes=', clearEnd - releasedBegin);
      WriteLn('STACK restored after loading: reserved=', StackLength, ' bottom=$', PrintHexValue(PtrUInt(StackBottom)), ' top=$', PrintHexValue(PtrUInt(StackBottom) + StackLength));
      WriteLn('STACK remarked: [$', PrintHexValue(watermarkBegin), ', $', PrintHexValue(watermarkEnd), ') bytes=', watermarkEnd - watermarkBegin);
    if watermarkEnd > watermarkBegin then WriteLn('STACK marker check: first=$', PrintHexValue(PDWord(watermarkBegin)^), ' last=$', PrintHexValue(PDWord(watermarkEnd - 4)^));
  }
    if (StackLength <> stackLengthBeforeBorrow) or
       (PtrUInt(StackBottom) <> stackBottomBeforeBorrow) then begin
      WriteLn('STACK restoration mismatch');
      Halt;
    end;
  end;
//  else WriteLn('STACK restore rejected by RTL: requested=', stackLengthBeforeBorrow, ' reserved=', StackLength);
  stackLengthBeforeBorrow := 0;
  stackBottomBeforeBorrow := 0;
end;

procedure ReportStackUsage(const stage: ShortString);
var
  used: SizeUInt;
begin
  if not SampleStackUsage(used) then begin
    WriteLn('STACK ', stage, ': watermark unavailable or exhausted');
    Exit;
  end;
  WriteLn('STACK ', stage, ': reserved=', StackLength,
          ' estimated peak=', peakObserved, ' untouched=', StackLength - used);
end;

procedure TuneStackReservation;
var
  used, requested, oldLength: SizeUInt;
  interruptsEnabled, changed: Boolean;
begin
  if borrowedEnd <> 0 then Exit;
  if not SampleStackUsage(used) then begin
    WriteLn('STACK resize skipped: watermark unavailable or exhausted');
    Exit;
  end;
  if peakObserved > High(SizeUInt) - SafetyMargin - 63 then Exit;
  requested := (peakObserved + SafetyMargin + 63) and not SizeUInt(63);
  if requested < MinimumReservation then requested := MinimumReservation;
  oldLength := StackLength;
  if requested = oldLength then begin
    WriteLn('STACK unchanged: reserved=', oldLength,
            ' measured peak=', peakObserved);
    Exit;
  end;

  WriteLn('STACK resize attempt: reserved=', oldLength,
          ' requested=', requested, ' measured peak=', peakObserved);

  interruptsEnabled := _disableInterrupts;
  changed := SetStackLength(requested);
  { Newly available heap RAM must never be scanned or marked afterward. }
  if changed then InitStackWatermark;
  if interruptsEnabled then _enableInterrupts;

  if changed then
    WriteLn('STACK resized: ', oldLength, ' -> ', StackLength,
            ' measured peak=', peakObserved, ' bottom=', PtrUInt(StackBottom))
  else
    WriteLn('STACK resize rejected by RTL: requested=', requested,
            ' reserved=', StackLength);
end;