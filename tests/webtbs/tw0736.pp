{ %CPU=i386 }
{$ifdef FPC}
{$ASMMODE INTEL}
{$INLINE ON}
{$endif FPC}

program test;

type
   tobj = object
     x : word;
     constructor init;
     procedure test;virtual;
     procedure testx;
     end;

constructor tobj.init;
begin
  x:=1;
end;

procedure tobj.testx;
begin
  asm
    mov ax,3
    mov edx,SELF
    mov word ptr[edx+x],ax
  end;
end;

procedure tobj.test;
var
  pattern: word;
  dummyval : word;

  function rotate: boolean; assembler; {$ifdef FPC}inline;{$endif FPC}
  asm
    mov al,0
    rol word ptr [pattern],1
    rcl al,1
  end;

{ this does still not work because
  it can only work as inline not as normal sub function
  because dummyval and pattern are not reachable !! PM
  function rotateb(dummy : byte) : boolean; assembler; inline;
  asm
    movzx byte ptr [dummy],ax
    mov ax,word ptr [dummyval]
    mov al,0
    rol word ptr [pattern],1
    rcl al,1
  end; }

var
  i : byte;

begin
  pattern:= $a0a0;
  for i:=1 to 16 do
   begin
     Write('obj pattern = ',
       {$ifdef FPC}
       hexstr(pattern,4),' ');
       {$else}
       pattern,' ');
       {$endif}
     if rotate then
       Writeln('bit found')
     else
       Writeln('no bit found');
   end;
end;

procedure changepattern;
var
  pattern: word;
  dummyval : word;

  function rotate: boolean; assembler; {$ifdef FPC}inline;{$endif FPC}
  asm
    mov al,0
    rol word ptr [pattern],1
    rcl al,1
  end;

{ this does still not work because
  it can only work as inline not as normal sub function
  because dummyval and pattern are not reachable !! PM
  function rotateb(dummy : byte) : boolean; assembler; inline;
  asm
    movzx byte ptr [dummy],ax
    mov ax,word ptr [dummyval]
    mov al,0
    rol word ptr [pattern],1
    rcl al,1
  end; }

var
  i : byte;

begin
  pattern:= $a0a0;
  for i:=1 to 16 do
   begin
     Write('pattern = ',
       {$ifdef FPC}
       hexstr(pattern,4),' ');
       {$else}
       pattern,' ');
       {$endif}
     if rotate then
       Writeln('bit found')
     else
       Writeln('no bit found');
   end;
end;

var

  t : tobj;
begin
  changepattern;
  t.init;
  t.test;
  t.testx;
  if t.x<>3 then
    begin
      Writeln('Unable to access object fields in assembler');
      Halt(1);
    end;
end.
