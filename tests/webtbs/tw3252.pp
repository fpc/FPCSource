{ Source provided for Free Pascal Bug Report 3252 }
{ Submitted by "Tom Verhoeff" on  2004-08-16 }
{ e-mail: T.Verhoeff@tue.nl }
program ConstRange;
  { To demonstrate error with sqr in const expression }

const
  Max1 = 12; { program compiles fine when changing 12 to 11 }
  Max2 = Max1 * Max1; { this works fine }
  Max3 = sqr ( Max1 ); { this fails }

type
  Index = 0 .. Max3;

{ remainder not relevant, but included to have a complete program }

var
  i: Index;

begin
  i := 0
; writeln ( i )
end.
