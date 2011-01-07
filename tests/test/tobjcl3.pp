{ %target=darwin }
{ %cpu=powerpc64,x86_64,arm }
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
  MyLibObjCClass = objcclass external (NSObject)
   public
    fa: byte;
    fb: byte;
    { this field is declared as private in the real class,
      and the non-fragile ABI should make sure that this
      gives a linker error }
    fc: byte;
    function publicfun: byte; message 'publicfun';
    function protectedfun: byte; message 'protectedfun';
    function privatefun: byte; message 'privatefun';
  end;

var
  a: MyLibObjCClass;
begin
  a:=NSObject(MyLibObjCClass.alloc).init;
  a.fc:=55;
end.
