{ %CPU=i386 }
{ %version=1.1 }

{ Source provided for Free Pascal Bug Report 1157 }
{ Submitted by "Colin Goldie" on  2000-10-06 }
{ e-mail: Colin_G@Positek.com.au }
{$ifdef fpc}
{$mode delphi}

{$asmmode intel}
{$endif}

{ @Result in assembler functions should create only a temporary
  variable when it is used }

Function GetBLUEfromRGB( color : word ) : byte; assembler;
asm
    mov cx,color
    and cx,1fh
    mov @Result,cl
    mov ax,255
end;

{
Does something weird .. to the stack im guessing ... error 206 and 103
errors occur 'File not open' ...

However, if instead of using @Result , i chuck my return value into the
accumulator register , everything thing works hunky dory.
}

Function GetBLUEfromRGB2( color : word ) : byte; assembler;
asm
     mov cx,color
     and cx,1fh
     mov al,cl
end;

begin
  if GetBlueFromRGB2($fff)<>GetBlueFromRGB($fff) then
    begin
      Writeln('Error in assembler statement');
      Halt(1);
    end;
end.
