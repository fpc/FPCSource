{ %target=darwin }
{ %cpu=i386,powerpc,powerpc64,x86_64,arm }
{ %NEEDLIBRARY }
{ %fail }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

const
{$ifdef windows}
  libname='tobjcl1.dll';
{$else}
  libname='tobjcl1';
  {$linklib tobjcl1}
{$endif}

type
  MyHiddenObjcClass=objcclass external (NSObject)
  end;

var
  a: MyHiddenObjcClass;
begin
  a:=NSObject(MyHiddenObjcClass.alloc).init;
  a.release;
end.
