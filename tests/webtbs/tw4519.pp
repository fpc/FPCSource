{ Source provided for Free Pascal Bug Report 4519 }
{ Submitted by "Martin Schreiber" on  2005-11-17 }
{ e-mail:  }
program project1;
{$mode objfpc}{$h+}
uses
 classes;

type
 ttestcomp = class(tcomponent)
  private
   fstringvar: string;
   procedure readstringvar(reader: treader);
   procedure writestringvar(writer: twriter);
  protected
   procedure defineproperties(filer: tfiler); override;
 end;

{ ttestcomp }

procedure ttestcomp.readstringvar(reader: treader);
begin
 fstringvar:= reader.readstring;
end;

procedure ttestcomp.writestringvar(writer: twriter);
begin
 writer.writestring(fstringvar);
end;

procedure ttestcomp.defineproperties(filer: tfiler);
begin
 filer.defineproperty('stringvar',@readstringvar,@writestringvar,true);
end;

var
 testcomp: ttestcomp;
 stream1,stream2: tmemorystream;

begin
 testcomp:= ttestcomp.create(nil);
 stream1:= tmemorystream.create;
 stream1.writecomponent(testcomp);
 stream1.position:= 0;
 stream2:= tmemorystream.create;
 objectbinarytotext(stream1,stream2); //<- AV
 stream1.free;
 stream2.free;
end.
