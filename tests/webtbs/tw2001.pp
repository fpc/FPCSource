{ %version=1.1}
{ Source provided for Free Pascal Bug Report 2001 }
{ Submitted by "Gene Hardesty" on  2002-06-09 }
{ e-mail: geneh@softhome.net }
program qwrdbug;
const
     terabytes=1024*1024*1024*1024;             // Compiler can't do math
     terabytes1:qword=1024*1024*1024*1024;      // Typing doesn't work
     terabytes2=1099511627776;                  // Thx to CALC for the number
var
     terabytes3:qword;
     terabytes4:int64;                          // Just for comparison

begin
     terabytes3:=qword (terabytes2);          // This doesn't work either.
     terabytes3:=qword (1099511627776);       // This doesn't work too.
     terabytes3:=1024*1024*1024*1024;
     terabytes4:=1024*1024*1024*1024;
     writeln (terabytes);                       // Displays 0
     writeln (terabytes1);                      // Also displays 0
     writeln (terabytes2:2);                  // The compiler treats it as a REAL
     writeln (terabytes3);                      // Displays 0
     writeln (terabytes4);                      // Also displays 0
end.
