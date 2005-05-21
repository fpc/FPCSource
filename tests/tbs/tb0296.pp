{ Old file: tbs0355.pp }
{  }

{MvdV; published in core.
    Element that is in the type zz too is not recognised as such.
    }

type xx=(notinsubset1,insubset1,insubset2,notinsubset2);
     zz=insubset1..insubset2;

     ll=record
         yy:zz;
         end;

const oo : array[0..1] of ll = (
                                  (yy:insubset1),
                                  (yy:insubset2));
begin
end.
