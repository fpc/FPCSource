{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 sysutils,classes;
type
 ttestclass = class(tcomponent)
  private
   frealprop: real;
  published
   property realprop: real read frealprop write frealprop;
 end;
var
 instance1,instance2: ttestclass;
 stream1,stream2: tmemorystream;
begin
 instance1:= ttestclass.create(nil);
 instance2:= ttestclass.create(nil);
 stream1:= tmemorystream.create;
 stream2:= tmemorystream.create;
 try
  instance1.realprop:= 1e100;
  stream1.writecomponent(instance1);
  stream1.position:= 0;
  objectbinarytotext(stream1,stream2);
  stream2.position:= 0;
  stream1.clear;
  objecttexttobinary(stream2,stream1);
  stream1.position:= 0;
  stream1.readcomponent(instance2);
  writeln('instance1: ',instance1.realprop,' instance2: ',instance2.realprop);
  if instance1.realprop<>instance2.realprop then
    halt(1);

  stream1.clear;
  stream2.clear;
  instance1.realprop:= 1;
  stream1.writecomponent(instance1);
  stream1.position:= 0;
  objectbinarytotext(stream1,stream2);
  stream2.position:= 0;
  stream1.clear;
  objecttexttobinary(stream2,stream1);
  stream1.position:= 0;
  stream1.readcomponent(instance2);
  writeln('instance1: ',instance1.realprop,' instance2: ',instance2.realprop);
  if instance1.realprop<>instance2.realprop then
    halt(1);
finally
  instance1.free;
  instance2.free;
  stream1.free;
  stream2.free;
 end;
end.
