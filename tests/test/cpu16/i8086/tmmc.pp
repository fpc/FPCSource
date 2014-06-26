{ %cpu=i8086 }

{ Memory layout test for the compact memory model.

  Note that this test is NOT compatible with Turbo Pascal 3, despite the fact
  that TP3 uses the compact memory model. The difference is that TP3 puts the
  heap before the stack (i.e. at lower addresses than the stack). FPC for i8086
  in the compact memory model follows TP7's large memory model data layout,
  which means stack goes before the heap. In practice, this shouldn't matter for
  most programs. }

{$IFNDEF FPC_MM_COMPACT}
  {$DEFINE SKIP_TEST}
{$ENDIF not FPC_MM_COMPACT}

{$IFDEF SKIP_TEST}
program tmml;
begin
  Writeln('Test compiled for the wrong memory model. Goodbye!');
end
{$ELSE SKIP_TEST}

program tmml;

var
  CS, DS, SS, HS: Word;
  HeapP: Pointer;
  ErrorsFound: Boolean;

procedure Error(const S: string);
begin
  Writeln('Error! ', S);
  ErrorsFound := True;
end;

var
  ProcVar: Procedure;
begin
  ErrorsFound := False;
  Writeln('SizeOf(Pointer)=', SizeOf(Pointer));
  if SizeOf(Pointer) <> 4 then
    Error('SizeOf(Pointer) <> 4');
  Writeln('SizeOf(ProcVar)=', SizeOf(ProcVar));
  if SizeOf(ProcVar) <> 2 then
    Error('SizeOf(ProcVar) <> 2');
  GetMem(HeapP, 5);
  CS := CSeg;
  DS := DSeg;
  SS := SSeg;
  HS := Seg(HeapP^);
  Writeln('CS=', CS);
  Writeln('DS=', DS);
  Writeln('SS=', SS);
  Writeln('Heap Seg=', HS);
  if not (CS < DS) then
    Error('CS >= DS');
  if not (DS < SS) then
    Error('DS >= SS');
  if not (SS < HS) then
    Error('SS >= HeapSeg');
  FreeMem(HeapP, 5);
  if ErrorsFound then
  begin
    Writeln('Errors found!');
    Halt(1);
  end
  else
    Writeln('Ok!');
end
{$ENDIF SKIP_TEST}
.
