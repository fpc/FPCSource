{ bug for shrd assemblerreader }
begin
{$asmmode intel}
   asm
      SHRD [ESI-8], EAX, CL
   end;
end.
