{ %cpu=i8086 }

{ Memory layout test for the tiny memory model. }

{$IFNDEF FPC_MM_TINY}
  {$DEFINE SKIP_TEST}
{$ENDIF not FPC_MM_TINY}

{$IFDEF SKIP_TEST}
program tmml;
begin
  Writeln('Test compiled for the wrong memory model. Goodbye!');
end
{$ELSE SKIP_TEST}

program tmml;

var
  CS, DS, SS, HS: Word;
  CodeOfs, DataOfs, HeapOfs, StackOfs: Word;
  HeapP: Pointer;
  ErrorsFound: Boolean;

procedure Error(const S: string);
begin
  Writeln('Error! ', S);
  ErrorsFound := True;
end;

function GetStackOfs: Word;
var
  LocalVar: Integer;
begin
  GetStackOfs := Ofs(LocalVar);
end;

var
  ProcVar: Procedure;
begin
  ErrorsFound := False;
  Writeln('SizeOf(Pointer)=', SizeOf(Pointer));
  if SizeOf(Pointer) <> 2 then
    Error('SizeOf(Pointer) <> 2');
  Writeln('SizeOf(ProcVar)=', SizeOf(ProcVar));
  if SizeOf(ProcVar) <> 2 then
    Error('SizeOf(ProcVar) <> 2');
  GetMem(HeapP, 5);
  CS := CSeg;
  DS := DSeg;
  SS := SSeg;
  HS := Seg(HeapP^);
  CodeOfs := Word(@Error);
  DataOfs := Ofs(CS);
  HeapOfs := Ofs(HeapP^);
  StackOfs := GetStackOfs;
  Writeln('PrefixSeg=', PrefixSeg);
  Writeln('CS=', CS);
  Writeln('DS=', DS);
  Writeln('SS=', SS);
  Writeln('Heap Seg=', HS);
  Writeln('Code Ofs=', CodeOfs);
  Writeln('Data Ofs=', DataOfs);
  Writeln('Heap Ofs=', HeapOfs);
  Writeln('Stack Ofs=', StackOfs);
  if PrefixSeg = CS then
    Writeln('looks like we are a .COM file!')
  else
  begin
    Writeln('looks like we are an .EXE file!');
    if not (PrefixSeg < CS) then
      Error('PrefixSeg >= CS');
    if (CS - PrefixSeg) <> 16 then
      Error('(CS - PrefixSeg) <> 16');
  end;
  if not (CS = DS) then
    Error('CS <> DS');
  if not (DS = SS) then
    Error('DS <> SS');
  if not (SS = HS) then
    Error('SS <> HeapSeg');
  if not (CodeOfs < DataOfs) then
    Error('CodeOfs >= DataOfs');
  if not (DataOfs < HeapOfs) then
    Error('DataOfs >= HeapOfs');
  if not (HeapOfs < StackOfs) then
    Error('HeapOfs >= StackOfs');
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
