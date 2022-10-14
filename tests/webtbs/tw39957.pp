{ %opt=-O4 -Oodeadstore -Oonoregvar }
var
	src: array[0 .. 9] of int32;
	srcp: pInt32;

begin
	srcp := pInt32(src);
	srcp[0] := 1;
end.
