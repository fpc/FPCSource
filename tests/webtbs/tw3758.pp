{ Source provided for Free Pascal Bug Report 3758 }
{ Submitted by "Martin Schreiber" on  2005-03-07 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type

 ttestcomp = class(tcomponent)
  private
   fwstr: widestring;
  published
   property wstr: widestring read fwstr write fwstr;
  end;

var
 stream1,stream2: tmemorystream;
 f: tfilestream;
 testcomp1,testcomp2: ttestcomp;
 str1: widestring;

begin
 setlength(str1,2);
 str1[1]:= widechar($1f00);
 str1[2]:= widechar($203);
 stream1:= tmemorystream.create;
 stream2:= tmemorystream.create;
 testcomp1:= ttestcomp.create(nil);
 testcomp2:= ttestcomp.create(nil);
 try
  testcomp1.wstr:= str1;
  stream1.writecomponent(testcomp1);
  stream1.position:= 0;
  objectbinarytotext(stream1,stream2);
  stream1.clear;
  stream2.position:= 0;
  f:= tfilestream.create('test.txt',fmcreate);
  f.copyfrom(stream2,0);
  f.free;
  stream2.position:= 0;
  objecttexttobinary(stream2,stream1);
  stream1.position:= 0;
  stream1.readcomponent(testcomp2);
  if testcomp2.wstr = str1 then begin
   writeln('OK');
  end
  else begin
   writeln('Error');
  end;
 finally
  stream1.free;
  stream2.free;
  testcomp1.free;
  testcomp2.free;
  deletefile('test.txt');
 end;
end.
