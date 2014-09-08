{ %cpu=i8086 }

{ Memory layout test for the large memory model. This test is compatible with
  Turbo Pascal 7, because the large model is TP7's memory model. }

{$IFDEF FPC}
  {$IFNDEF FPC_MM_LARGE}
    {$DEFINE SKIP_TEST}
  {$ENDIF not FPC_MM_LARGE}
{$ENDIF FPC}

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
  HeapOrgSeg, HeapOrgOfs, HeapEndSeg, HeapEndOfs: Word;
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
  if SizeOf(ProcVar) <> 4 then
    Error('SizeOf(ProcVar) <> 4');
  GetMem(HeapP, 5);
  CS := CSeg;
  DS := DSeg;
  SS := SSeg;
  HS := Seg(HeapP^);
  HeapOrgSeg := Seg(HeapOrg^);
  HeapOrgOfs := Ofs(HeapOrg^);
  HeapEndSeg := Seg(HeapEnd^);
  HeapEndOfs := Ofs(HeapEnd^);
  Writeln('PrefixSeg=', PrefixSeg);
  Writeln('CS=', CS);
  Writeln('DS=', DS);
  Writeln('SS=', SS);
  Writeln('HeapOrg=', HeapOrgSeg, ':', HeapOrgOfs);
  Writeln('HeapEnd=', HeapEndSeg, ':', HeapEndOfs);
  Writeln('Heap Seg=', HS);
  if not (PrefixSeg < CS) then
    Error('PrefixSeg >= CS');
  if not (CS < DS) then
    Error('CS >= DS');
  if not (DS < SS) then
    Error('DS >= SS');
  if not (SS < HS) then
    Error('SS >= HeapSeg');
  if HeapOrgOfs <> 0 then
    Error('HeapOrg offset <> 0');
  if HeapEndOfs <> 0 then
    Error('HeapEnd offset <> 0');
  if (HeapOrgSeg - SS) <> 1024 then
    Error('HeapOrgSeg <> SS+1024 (16kb stack)');
  if (PrefixSeg + MemW[PrefixSeg-1:3]) <> HeapEndSeg then
    Error('HeapEnd segment <> end_of_current_program_MCB');
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