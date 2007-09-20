{ %cpu=i386,x86_64}
{ %norun }

{$asmmode intel}

begin
  asm
    rcl ax,1
    rcr ax,1
  end
end.
