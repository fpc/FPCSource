{ Source provided for Free Pascal Bug Report 5082 }
{ Submitted by "Martin Schreiber" on  2006-05-01 }
{ e-mail:  }
program storedfalse;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils,classes;

type
 ttestclass1 = class(tcomponent)
  private
   fprop1: real;
  public
   property prop1: real read fprop1 write fprop1 stored false;
 end;

 ttestclass2 = class(ttestclass1)
  published
   property prop1;
 end;

var
 testclass2: ttestclass2;
 stream1,stream2: tmemorystream;
 str1: string;

begin
 testclass2:= ttestclass2.create(nil);
 testclass2.prop1:= 1;
 stream1:= tmemorystream.create;
 try
  stream1.writecomponent(testclass2);
  stream2:= tmemorystream.create;
  try
   stream1.position:= 0;
   objectbinarytotext(stream1,stream2);
   stream2.position:= 0;
   setlength(str1,stream2.size);
   move(stream2.memory^,str1[1],length(str1));
   write(str1);
  finally
   stream2.free;
  end;
 finally
  stream1.free;
 end;
 if pos('prop1',str1)<>0 then
   begin
     writeln('error');
     halt(1);
   end;
end.
