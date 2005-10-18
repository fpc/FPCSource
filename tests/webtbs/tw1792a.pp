uses
  math;
var
  s : string;
Begin
{$ifdef FPC_HAS_TYPE_EXTENDED}
 str(intpower(2,63),s);
 if s<>' 9.2233720368547758E+0018' then
   begin
     WriteLn(intpower(2,63));
     halt(1);
   end;
{$endif FPC_HAS_TYPE_EXTENDED}

{$ifdef FPC_HAS_TYPE_DOUBLE}
 str(double(intpower(2,63)),s);
{$ifdef FPC_HAS_TYPE_EXTENDED}
 if s<>' 9.223372036854776E+018' then
{$else FPC_HAS_TYPE_EXTENDED}
 if s<>' 9.22337203685478E+018' then
{$endif FPC_HAS_TYPE_EXTENDED}
   begin
     WriteLn(double(intpower(2,63)));
     halt(1);
   end;
{$endif FPC_HAS_TYPE_DOUBLE}
end.
