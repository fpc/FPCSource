{ %cpu=i386 }
{ %opt=-Cg- }

{ Source provided for Free Pascal Bug Report 3863 }
{ Submitted by "Jernej" on  2005-04-01 }
{ e-mail: jernejcoder@gmail.com }

{$mode delphi}

function FormatBuf(var Buffer; BufLen: Cardinal; const Format;
 FmtLen: Cardinal; const Args: array of const): Cardinal;
asm
CMP     EAX,Args.Integer[-4] // -> "error building record offset"
CMP     [EBX+EAX*8].Byte[4],vtInteger // -> error: assembler error in operand
CMP     EBX,Args.Integer[-4] // -> "error building record offset"
MOVZX   EBX,[ESI].Byte[4] // -> error: assembler error in operand
JMP     @CvtVector.Pointer[EBX*4] // "error: assembler syntax error"
@CvtVector:
        DD      0
        DD      0
        DD      0
        DD      0
end;

begin
end.
