{ Source provided for Free Pascal Bug Report 4253 }
{ Submitted by "Gerhard" on  2005-08-03 }
{ e-mail: gs@g--s.de }
program tirange ;

  var
//    Count : -high ( cardinal )..high ( cardinal ) ;
    Count : int64 ;
    long : longint ;

  begin
    count := -1 ;
    writeln ( count ) ;  // gives -1
    long := 1 ;
    count := -long ;
    writeln ( count ) ;  // gives 4294967295
   end.
