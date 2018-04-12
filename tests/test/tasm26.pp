{ %CPU=i8086,i386,x86_64 }

{$IFDEF FPC}
{$MODE TP}
{$ASMMODE INTEL}
{$PIC OFF}
{$ENDIF FPC}

program tasm26;

type
  t = record
    a: word;
  end;

var
  v: t;

procedure x; assembler;
asm
  mov ax, tasm26.v.a;
end;

begin
end.
