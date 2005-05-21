{ Source provided for Free Pascal Bug Report 2210 }
{ Submitted by "peter" on  2002-10-30 }
{ e-mail: peter@casltd.co.uk }

{$mode objfpc}
unit tw2210;
interface
implementation
type
tclass = class
            function blah (p:integer) : boolean;
         end;

function tclass.blah(p:integer):boolean;
begin
   blah := true;
end;

end.
