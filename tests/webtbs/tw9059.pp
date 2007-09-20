{ %version=2.3 }
{ %opt=-Oodfa -vw -Sew}
program DoesNotSeemToBeInited;

{$goto on}

label 10, 20, 30;
var i: integer;
begin
        goto 20;
        10: begin i:= i + 1; goto 30 end;
        20: i:= 1;
        goto 10;
        30: writeln( 'i = ', i)
end.
