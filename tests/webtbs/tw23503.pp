{ %cpu=x86_64,i386 }
var c: 1..3;
begin
	c := 2;
	asm
		stc
	end;
 	case c of
		1: writeln('1');
        	2,3: writeln('2 or 3');
		otherwise
                  begin
                    writeln ('Bug!');
                    halt(1);
                  end;
	end;
end.
