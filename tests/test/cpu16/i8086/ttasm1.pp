{ %target=msdos }

{ Test for linking a TASM external module under i8086-msdos }

{ This test is Borland Pascal 7 compatible }

program ttasm1;

{$ifdef FPC}
  {$ifdef FPC_MM_LARGE}
    {$define ENABLE_TEST}
  {$endif FPC_MM_LARGE}
{$else FPC}
  {$define ENABLE_TEST}
{$endif FPC}

{$ifdef ENABLE_TEST}
function IncDWord(a: longint): longint; far; external {$ifdef fpc}name 'INCDWORD'{$endif};

{$L ttasm1.obj}

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

begin
  if IncDWord(5)<>6 then
    Error;
  Writeln('Ok!');
end
{$else ENABLE_TEST}
begin
  Writeln('This test is for a different memory model.');
end
{$endif ENABLE_TEST}
.
