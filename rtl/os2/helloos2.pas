program helloos2;

var	a,b:^word;

begin
		writeln('Hallo Wereld.');
		if os_mode=osDOS then
			writeln('We draaien onder DOS.')
		else
			writeln('We draaien onder OS/2.');
		writeln('Vrij geheugen: ',memavail);
		writeln('Grootste blok: ',maxavail);
		writeln('Heapstart: ',longint(heaporg));
		writeln('Heapend: ',longint(heapend));
		getmem(a,1000);
		getmem(b,2000);
		a^:=2;
		b^:=10;
		freemem(a,1000);
		freemem(b,2000);
end.
