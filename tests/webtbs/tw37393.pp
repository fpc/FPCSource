{ %OPT=-O2 }
program fx;
procedure rep9(a,b,c,d, e,f,g,h, i,j,k,l: single);
begin
	writeln(a, ' ', b, ' ', c, ' ', d);
	writeln(e, ' ', f, ' ', g, ' ', h);
	writeln(i, ' ', j, ' ', k, ' ', l);
    if (i<>9) or (j<>10) or (k<>11) or (l<>12) then
       halt(1);
end;
begin
	rep9(1,2,3,4,5,6,7,8,9,10,11,12);
    writeln('ok');
end.
