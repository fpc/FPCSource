{ %FAIL }
{ Old file: tbf0060.pp }
{  shows missing type checking for case statements      OK 0.99.1 (CEC) }

Program Test;

{ No errors -- problems is due to the fact that the rules for type
compatibility (p.47 language guide) -- are not respected, in other words
in case statements there is no type checking whatsoever in fpc!!
 I think that these are separate cases:
   1st case) s32bit,u32bit,u8bit,s8bit,s16bit,u16bit
   2nd case) uchar
   3rd case) bool8bit
These are not /should not be compatible with each other in a case
statement imho - CEC
}

var
 myvar:char;
Begin
 case myvar of
 1: ;
 #2: ;
 end;
end.
