{ %target=darwin }
{ %cpu=powerpc,i386 }

{$mode objfpc}
{$modeswitch objectivec1}

unit uobjc7;

interface

type
  tobjcprot = objcprotocol
    procedure isrequired; message 'isrequired';
   optional
    procedure isoptional; message 'isoptional';
   required
    procedure alsorequired; message 'alsorequired';
  end;

implementation

end.
