{$mode delphi}
program test;

const
	factor	:	double	=	1.0;


function CheckLongint(count,wcount:longint):double;
begin
	result:=(count+wcount)*factor;	
end;


function CheckCardinal(count,wcount:cardinal):double;
begin
	result:=(count+wcount)*factor;	
end;


begin 
	writeln('LONGINT:  ',CheckLongint(3,1));
	writeln('CARDINAL: ',CheckCardinal(3,1));
end.                     
