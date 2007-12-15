{ %cpu=i386 }
{ %OPT=-Cg- }
{ %version=1.1 }
{ Source provided for Free Pascal Bug Report 2668 }
{ Submitted by "Marco" on  2003-09-06 }
{ e-mail: marco+bugs@freepascal.org }

{$ifdef fpc}{$MODE DELPHI} {$ASMMODE INTEL} {$endif}

type
  TFloatingPointClass =
   (
    fpZero,     // zero
    fpNormal,   // normal finite <> 0
    fpDenormal, // denormalized finite
    fpInfinite, // infinite
    fpNaN,      // not a number
    fpInvalid   // unsupported floating point format
   );

const
  FPClasses: array [0..5] of TFloatingPointClass =
   (
    fpInvalid,
    fpNaN,
    fpNormal,
    fpInfinite,
    fpZero,
    fpDenormal
   );

asm
        MOV ECX,0
        MOV EDX,0
        MOVZX   EAX, TFloatingPointClass(FPClasses[EDX])
        MOVZX   EAX, TFloatingPointClass([ECX].FPClasses[EDX])
        LEA ECX,FPClasses
        MOVZX   EAX, TFloatingPointClass([ECX].1+2[EDX])
end.
