UNIT {SPARC}System;
INTERFACE
CONST
  MaxInt=1 SHL 16 - 1;
	MaxCardinal=1 SHL 32 - 1;
TYPE
  Integer=-MaxInt-1..MaxInt;
	Cardinal=0..MaxCardinal;
VAR
  Input,Output:Text;
IMPLEMENTATION
END.
