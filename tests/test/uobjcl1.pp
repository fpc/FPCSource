{$mode objfpc}
{$modeswitch objectivec1}

{ Written by Jonas Maebe in 2009, released into the public domain }

unit uobjcl1;

interface

type
  MyLibObjCClass = objcclass(NSObject)
   public
    fa: byte;
    function publicfun: byte; message 'publicfun';
   protected
    fb: byte;
    function protectedfun: byte; message 'protectedfun';
   private
    fc: byte;
    function privatefun: byte; message 'privatefun';
  end;
  
 implementation
 
function MyLibObjCClass.publicfun: byte;
  begin
    result:=fa;
  end;


function MyLibObjCClass.protectedfun: byte;
  begin
    result:=fb;
  end;


function MyLibObjCClass.privatefun: byte;
  begin
    result:=fc;
  end;

type
  MyHiddenObjcClass = objcclass(NSObject)
  end;


end.
