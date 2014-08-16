{$namespace org.freepascal.test.lcproc}

unit tlowercaseproc;

{$mode delphi}
{$targetswitch lowercaseprocstart}

interface

procedure DoIt;

type
  tc = class
   procedure MethodName;
   class procedure ClassMethodName; static;
   procedure XToY; // should become xToY
   procedure PREFIXThingToTest; // should become prefixThingToTest
   procedure RC64Encode; // should become rc64Encode;
  end;

implementation

procedure DoIt;
var
  a: ansistringclass;
begin
  { this routine is declared with uppercase C at the start in the system unit,
    check that we don't lowercase this one as well }
  a:=AnsistringClass(AnsistringClass.CreateFromLiteralStringBytes('abcdef',DefaultSystemCodePage));
end;

procedure tc.MethodName;
begin
  doit;
  classmethodname;
end;

class procedure tc.ClassMethodName; static;
begin
  doit;
end;

procedure tc.xtoy;
begin
end;

procedure tc.PREFIXThingToTest;
begin
end;

procedure tc.RC64Encode;
begin
end;

end.
