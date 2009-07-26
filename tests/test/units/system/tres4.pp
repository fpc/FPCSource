{$mode objfpc}

program tres4;


uses
  classes,


  sysutils;

{$R tres4.res}

function  testResource(aName : string): longword;
var
  ResourceStream: TResourceStream;
begin
  resourceStream:=nil;
  result:=0;
  try
   try
    resourceStream := TResourceStream.Create(HInstance, aname, RT_RCDATA);
    result:=resourcestream.size;
   except
   
    on EResNotFound do
      begin
        writeln(aname);
        halt(1)
      end;
   else
   end;
  finally
   freeandnil(ResourceStream);
  end;

end;

begin
  writeln('Resources test.');
  if (testresource('mdtytul100_png')<>50223) then
    halt(2);
  if (testresource('mdtoptyt_png') <>136303) then
    halt(3);
  writeln('Done.');
end.

