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

end.
