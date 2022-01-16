{ %cpu=i8086 }
program tsegofs1a;

{ Test for typed constants, initialized with ofs(x) or seg(x) }
{ This test is TP7 compatible }

{$ifdef FPC}
  {$mode TP}
{$endif FPC}

procedure proc1;
begin
end;

procedure proc2;
begin
end;

var
  staticvar1, staticvar2: byte;

const
  procofstable: array [0..1] of word = (Ofs(proc1), Ofs(proc2));
  varofstable: array [0..1] of word = (Ofs(staticvar1), Ofs(staticvar2));

{ in the case of initialized typed constants, seg(x) produces segment
  relocations, which make it impossible to create a .com file, so let's avoid
  them in the tiny memory model (you can still produce an .exe in this memory
  model, but it's usually used for .com files) }
{$ifndef FPC_MM_TINY}
  procsegtable: array [0..1] of word = (Seg(proc1), Seg(proc2));
  varsegtable: array [0..1] of word = (Seg(staticvar1), Seg(staticvar2));
{$endif FPC_MM_TINY}

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

begin
  if procofstable[0] <> Ofs(proc1) then
    Error;
  if procofstable[1] <> Ofs(proc2) then
    Error;
  if varofstable[0] <> Ofs(staticvar1) then
    Error;
  if varofstable[1] <> Ofs(staticvar2) then
    Error;
{$ifndef FPC_MM_TINY}
  if procsegtable[0] <> Seg(proc1) then
    Error;
  if procsegtable[1] <> Seg(proc2) then
    Error;
  if varsegtable[0] <> Seg(staticvar1) then
    Error;
  if varsegtable[1] <> Seg(staticvar2) then
    Error;
{$endif FPC_MM_TINY}
  Writeln('Ok!');
end.
