{ %CPU=i386 }
{ %TARGET=go32v2,win32,linux }
{ %NOTE=This test requires an installed Nasm }

{ Old file: tbs0043.pp }
{  shows assembler nasm output fpu opcodes problem     OK 0.99.6 (PFV) }

{$ifdef Unix}
  {$output_format nasmelf}
{$endif}
{$ifdef go32v2}
  {$output_format nasmcoff}
{$endif}
{$ifdef win32}
  {$output_format nasmwin32}
{$endif}

{$asmmode att}

{ THE OUTPUT is incorrect but the }
{ parsing is correct.             }
{ under nasm output only.         }
{ works correctly under tasm/gas  }
{ other problems occur with other }
{ things in math.inc              }
{ pp -TDOS -Ratt -Anasm bug0043.pp }
    procedure frac;

      begin
         asm
            subl $16,%esp
            fnstcw -4(%ebp)
            fwait                    { unknown instruction }
            movw -4(%ebp),%cx
            orw $0x0c3f,%cx
            movw %cx,-8(%ebp)
            fldcw -8(%ebp)
            fwait                    { unknown instruction }
            fldl 8(%ebp)
            frndint
            fsubl 8(%ebp)
            fabsl
            fclex
            fldcw -4(%ebp)
            leave
            ret $8
         end ['ECX'];
      end;

Begin
end.
