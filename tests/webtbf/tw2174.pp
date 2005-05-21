{ %FAIL }
{ Source provided for Free Pascal Bug Report 2174 }
{ Submitted by "Maarten Bekers" on  2002-10-13 }
{ e-mail: fpc@elesoft.net }

{$mode delphi}

program test;

type StringArrayObj = Object
          constructor Init(AllocStrings: Longint);
          destructor Done;
     end; { StringArrayObj }

type pStringArrayObj = ^StringArrayObj;

constructor StringArrayObj.Init(AllocStrings: Longint);
begin
end; { constructor Init }

destructor StringArrayObj.Done;
begin
end; { destructor Done }

var
  oo2,oo3: ^stringarrayobj;

begin
{!!!}
new(oo2, init);
new(oo3, init);

writeln('one');
writeln('two!');
{!!!}
end.
