Program FontDemo;
{ FontDemo.pas, by Marco van de Voort (C) 2000-2001

Compiler: 1.0.5 or 1.1 after 20-01-2001
Target  : FreeBSD 4.x+ with 16x8 font. 3.x untested  (syscons driver)

Demonstrate font modification with the console driver "syscons".
This program doesn't work under X or over telnet.

The purpose of the program is to demonstrate the procedures that change the
font. The demonstration assume a 80x25 console. Framebuffer devices or 80x50
displays (80x50 use 8x8 fonts) require a trivial modification.

The example of mirroring is absurd, but is very visible, so good for
demonstration. The real use is to load the font, change a few characters
(linedrawing, (C) characters, force existance of umlaute or tremas for the
duration of the application.

Note that if you switch to a different vty while the font is mirrored, that
vty is also mirrored.

Root can restore the font via a network device with:

vidcontrol -f 8x16 "fontname in /usr/share/syscons/fonts"   < /dev/ttyv1

The program saves the font, and will terminate and restore the font when
SIGUSR2 is received, unless -n is specified.

killall -USR2 fontdemo

}


Uses Console,{$ifdef ver1_0}Linux{$else}Baseunix{$endif},GetOpts;

{$ifdef ver1_0}
 function fpnanosleep;
 begin
   nanosleep;
 end;
{$endif}

procedure MirrorFont8(var Data;Count:longint); assembler;
{Mirrors on a bit level "Count" bytes in typeless variable "Data"}

asm
         mov data,%esi
         movl Count,%edx
.LLoop1: movb (%esi),%bl
         movl $8,%ecx
.LLoop2: shr  $1,%bl
         rcl  $1,%al
         loop .LLoop2
         movb %al,(%esi)
         incl %esi
         decl %edx
         jne .LLoop1
end['EAX','EBX','ECX','EDX','ESI'];


procedure GoLeft(var Data;Count:longint;shcnt:longint); assembler;
{Mirrors on a bit level "Count" bytes in typeless variable "Data"}

asm
         mov data,%esi
         mov data,%edi
         mov shcnt,%ecx
         movl Count,%edx
         xorl %eax,%eax
.LLoop1: lodsb
         shl  %cl,%eax
         stosb
         incl %esi
         incl %edi
         decl %edx
         jne .LLoop1
end['EAX','EBX','ECX','EDX','ESI'];

procedure GoRight(var Data;Count:longint;shcnt:longint); assembler;
{Mirrors on a bit level "Count" bytes in typeless variable "Data"}

asm
         mov data,%esi
         mov data,%edi
         mov shcnt,%ecx
         movl Count,%edx
         xor %eax,%eax
.LLoop1: lodsb
         shr  %cl,%eax
         stosb
         incl %esi
         incl %edi
         decl %edx
         jne .LLoop1
end['EAX','EBX','ECX','EDX','ESI'];

procedure DoAlt(var Data;Count:longint;shcnt:longint;alt:integer); assembler;
{Mirrors on a bit level "Count" bytes in typeless variable "Data"}

asm
         mov alt,%ecx
         mov data,%esi
         mov data,%edi
         add %ecx,%esi
         add %ecx,%edi

         mov shcnt,%ecx
         movl Count,%edx
         xorl %eax,%eax
.LLoop1: lodsb
         mov %edx,%ebx
         and  $1,%ebx
         test %ebx,%ebx
         je   .Lgoleftalt1
         shl  %cl,%eax
         jmp  .Lgoleftalt2
.Lgoleftalt1:
         shr  %cl,%eax
.Lgoleftalt2:
         stosb
         incl %esi
         incl %edi
         decl %edx
         jne .LLoop1
end['EAX','EBX','ECX','EDX','ESI'];

procedure stripbits (var Data;Count:longint); assembler;
{ "Compresses" a byte. 76543210 -> x764310x where x=0 (but 0 was already
used to indicate bit number :-)

Needed for a rotating effect. (Character rotating round vertical axis)
Does this for "Count" bytes in "Data".
}

asm
         mov data,%esi
         movl Count,%edx
.LLoop1: movb (%esi),%cl
         and  $219,%ecx
         mov  %ecx,%eax
         mov  %ecx,%ebx
         and  $24,%eax
         and  $3,%bl
         shr  $1,%al
         or   %bl,%al
         shl  $1,%al
         mov  %ecx,%ebx
         and  $192,%bl
         shl  $1,%al
         or   %bl,%al
         shr  $1,%al
         movb %al,(%esi)
         incl %esi
         decl %edx
         jne .LLoop1
end['EAX','EBX','ECX','EDX','ESI'];

procedure silloute (var Data;Count:longint); assembler;
{Iterates through "Count" bytes of "Data" and sets a byte to $48 if it is
not zero. If you would rotate a character round vertical axis through 90
degrees, this is about how it looks like}

asm
         mov data,%esi
         movl Count,%edx
.LLoop1: movb (%esi),%al
         mov  $48,%ecx
         test %al,%al
         je   .Lfurther
         mov  %cl,%al
.Lfurther:
         movb %al,(%esi)
         incl %esi
         decl %edx
         jne .LLoop1
end['EAX','EBX','ECX','EDX','ESI'];

var Originalfont : Fnt16;         {Font on startup, to be saved for restore}
    StopIt       : BOOLEAN;       {Becomes TRUE when SIGUSR2 is received}
    RestoreOnExit : Boolean;      {Should font be restored on exit?}

procedure OkThatsEnough(sig:longint);cdecl;

begin
 StopIt:=TRUE;
end;

procedure dorotate;

{ The animation order of the 5 distinctive states, -> 8 changes is one
rotation}
Type RotStatesType   = array[0..7] of longint;

const RotStates : RotStatesType=(0,1,4,3,2,3,4,1);

{5 states:
- 0 is mirrored,
- 1  mirrored "compressed"
- 2 is normal,
- 3 normal "compressed",
- 4 "silloutte"}

var fnts    : array[0..4] of fnt16;
    I       : Longint;
    iin,oout: timespec;

begin
   iin.tv_nsec:=250000000;
   iin.tv_sec:=0;
   fnts[2]:=OriginalFont;
   fnts[0]:=fnts[2];                    // Keep a copy.
   MirrorFont8(fnts[0],sizeof(fnt16));  // Mirror every byte at bitlevel
   fnts[1]:=fnts[0];
   stripbits(fnts[1],sizeof(fnt16));
   fnts[3]:=fnts[2];
   stripbits(fnts[3],sizeof(fnt16));
   fnts[4]:=fnts[2];
   silloute(fnts[4],sizeof(fnt16));
   i:=4;
   Repeat
     PIO_FONT8x16(0,fnts[RotStates[I and 7]]);          // Activate the mirrored set
     fpnanosleep(@iin,@oout);
     inc(i);
   until StopIt;
 end;

procedure upanddown(Mini:BOOLEAN);

var
    fnts      : array[1..4] OF fnt16;
    inn,outn  : Timespec;
    i         : longint;
    Mask      : Longint;

begin
   fnts[2]:=OriginalFont;
   inn.tv_nsec:=50000000;
   inn.tv_sec:=0;
   fnts[4]:=fnts[2];   {Make three copies}
   fnts[1]:=fnts[2];
   fnts[3]:=fnts[2];

   {Move one of them one byte up in memory. Font is one bit lower}

   move (fnts[1],fnts[1].fnt8x16[1],SIZEOF(Fnt16)-1);

   {Move another of them one byte down in memory. Font is one bit higher}
   IF Mini THEN
    Begin
     Mask:=1;
     move (fnts[2].fnt8x16[1],fnts[2],SIZEOF(Fnt16)-1);
    end
   else
    begin
     move (fnts[3].fnt8x16[1],fnts[3],SIZEOF(Fnt16)-1);
     Mask:=3;
    end;

   Repeat
     fpnanosleep(@inn,@outn);
     pIO_FONT8x16(0,fnts[1 + (I and Mask)]);
     inc(I);
   until StopIt;
end;

procedure LeftAndRight;

var
    fnts      : array[1..4] OF fnt16;
    inn,outn  : Timespec;
    i         : longint;
    Mask      : Longint;

begin
   fnts[2]:=OriginalFont;
   inn.tv_nsec:=50000000;
   inn.tv_sec:=0;
   fnts[4]:=fnts[2];   {Make three copies}
   fnts[1]:=fnts[2];
   fnts[3]:=fnts[2];

   {Move one of them one byte up in memory. Font is one bit lower}

   Goright(Fnts[1],SIZEOF(FNT16),2);
   GoLeft( Fnts[3],SIZEOF(FNT16),2);
   Repeat
     fpnanosleep(@inn,@outn);
     pIO_FONT8x16(0,fnts[1 + (I and 3)]);
     inc(I);
   until StopIt;
end;

procedure doalternate;

var
    fnts      : array[0..5] OF fnt16;
    inn,outn  : Timespec;
    i         : longint;
    Mask      : Longint;

begin
   fnts[2]:=OriginalFont;
   inn.tv_nsec:=500000000;
   inn.tv_sec:=0;
   fnts[4]:=fnts[2];   {Make three copies}
   fnts[1]:=fnts[2];
   fnts[3]:=fnts[2];

   {Move one of them one byte up in memory. Font is one bit lower}
   doalt(fnts[1],SIZEOF(FNT16) div 2,2,1);
   doalt(fnts[3],SIZEOF(FNT16) div 2,2,0);
   Repeat
     fpnanosleep(@inn,@outn);
     writeln(1 + (I and 3));
     pIO_FONT8x16(0,fnts[1 + (I and 3)]);
     inc(I);
   until StopIt;
end;

procedure JustMirror;

var fnt : Fnt16;

begin
  fnt:=OriginalFont;
  MirrorFont8(fnt,sizeof(fnt16));
  pIO_FONT8x16(0,fnt);
  IF RestoreOnExit THEN
  Repeat
  until StopIt;
end;

var DoThis        : Longint;

    c             : Char;
begin
 DoThis:=0;
 RestoreOnExit := TRUE;
 if PhysicalConsole(0) then             // a vty?
  begin
   REPEAT
    c:=GetOpt('n012345');                       // Commandline processing
    IF c IN ['0'..'5'] Then
     DoThis:= ORD(c)-48;
    IF c='n' THEN
     RestoreOnExit:=FALSE;
   UNTIL C=EndOfOptions;

   StopIt:=false;                       // Turns true on signal USR2
   GIO_FONT8x16(0,OriginalFont);        // Get font from videocard.
   fpSignal(SIGUSR2,@OkThatsEnough);    // Install handler for sigusr2.

   CASE DoThis OF                       // Call the font routines
    0 : DoRotate;
    1 : UpAndDown(TRUE);
    2 : JustMirror;
    3 : UpAndDown(FALSE);
    4 : LeftAndRight;
    5 : doAlternate;
    END;

   IF RestoreOnExit THEN                // clean up if required.
    PIO_FONT8x16(0,OriginalFont);
  end;
end.
