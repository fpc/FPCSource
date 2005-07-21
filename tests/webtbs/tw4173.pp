{ Source provided for Free Pascal Bug Report 4173 }
{ Submitted by "Gerhard" on  2005-07-11 }
{ e-mail: gs@g--s.de }
{
  operator with a redefine by ABSOLUTE on its result var
  produces the internal error 200110205;
  only when operator is defined in a unit.
}


{$inline on}

{$define nok} { if this is defined, the operator with problem is compiled }
{ $define ok}  { if this is defined, the operator without problem is compiled }

unit tw4173 ;

interface

  type
    tbcd = record something : integer end ;
    tbcdx = record something : integer end ;

{$ifdef nok}
  operator := ( const bcd : tbcd ) z : comp ; inline ;
{$endif}

{$ifdef ok}
  operator := ( const bcd : tbcdx ) z : comp ; inline ;
{$endif}

implementation

{$ifdef nok}
  operator := ( const bcd : tbcd ) z : comp ; inline ;

    var
      zz : int64 absolute z ;

    begin
     end ;
{$endif}

{$ifdef ok}
  operator := ( const bcd : tbcdx ) z : comp ; inline ;

    var
      zz : int64 ;
      zzz : comp absolute zz ;

    begin
      zz := 3 ;
      z := zzz ;
     end ;
{$endif}

end.
