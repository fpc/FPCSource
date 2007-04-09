{ Source provided for Free Pascal Bug Report 4229 }
{ Submitted by "Gerhard" on  2005-07-28 }
{ e-mail: gs@g--s.de }
unit tw4229 ;

interface

  type
    strobj = object
               bs : string ;
               ba : ansistring ;
              end ;

  operator := ( const a : ansistring ) z : strobj ;

implementation

  operator := ( const s : string ) z : strobj ;

    begin
      z.bs := s ;
     end ;

  operator := ( const a : ansistring ) z : strobj ;

    begin
      z.ba := a ;
     end ;

end.
