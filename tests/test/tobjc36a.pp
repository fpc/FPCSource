{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %fail }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

type
  MyObject2 = objcclass(NSObject)
  end;

  MyCategory = objccategory(MyObject2)
    procedure extraproc(a: longint); message 'extraproc:';
  end;

  MyObject = objcclass(NSObject)
    // overrides extraproc added to NSObject
    procedure extraproc(a: longint); override; message 'extraproc:';
  end;

procedure MyCategory.extraproc(a: longint);
  begin
    if a<>1 then
      halt(1);
  end;

procedure MyObject.extraproc(a: longint);
  begin
    if a<>2 then
      halt(2);
  end;


var
  a: NSObject;
  b: MyObject;
begin
  a:=NSObject.alloc.init;
  a.extraproc(1);
  a.release;
  b:=MyObject.alloc.init;
  b.extraproc(2);
  b.release;
end.
