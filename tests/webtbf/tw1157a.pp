{ %cpu=i386 }
{ %FAIL }
{ Source provided for Free Pascal Bug Report 1157 }
{ Submitted by "Colin Goldie" on  2000-10-06 }
{ e-mail: Colin_G@Positek.com.au }

{$mode tp}

{$asmmode intel}

{ @Result in assembler functions where
  the function result is not on stack
  should be refused in Turbo Pascal mode }

Function GetBLUEfromRGB( color : word ) : byte; assembler;
asm
    mov cx,color
    and cx,0000000000011111b
    mov @Result,cl
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
     and cx,0000000000011111b
     mov al,cl
end;

begin
  if GetBlueFromRGB2($fff)<>GetBlueFromRGB($fff) then
    begin
      Writeln('Error in assembler statement');
      Halt(1);
    end;
end.
