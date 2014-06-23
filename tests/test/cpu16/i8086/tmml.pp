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
  ErrorsFound: Boolean;

procedure Error(const S: string);
begin
  Writeln('Error! ', S);
  ErrorsFound := True;
end;

begin
  ErrorsFound := False;
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