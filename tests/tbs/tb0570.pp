program rangtest ;

type
  trange = 0..2030 ;
  ytrange = 1990..2030 ;

CONST
  lrange =   low ( trange ) ;
  hrange =  high ( trange ) ;
  ylrange =  low ( ytrange ) ;
  yhrange = high ( ytrange ) ;

var
  bbb : trange ;
  kkk : longint ;
  xyzzy : array [ ytrange, 1..100 ] of
            record
              xyzp : longint ;
              xyzb : boolean ;
             end ;

begin       (*$r+,s+,o+*)
  bbb := 0 ;
  kkk := 1 ;
  IF ( bbb >= ylrange )                   //  this IFstatement can not be found in the assembler file
     AND ( bbb <= yhrange )         //  and the program stops with range error
    THEN begin                             //
      WITH xyzzy[bbb,kkk] DO
        BEGIN
          halt(1);
          xyzp := 2 ;
          xyzb := True ;
         END ;
     end
    else writeln ( 'out' ) ;
 end.
