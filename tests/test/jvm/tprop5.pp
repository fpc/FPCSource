{$mode delphi}
{$modeswitch unicodestrings}
{$namespace org.freepascal.test}

Unit tprop5;

interface

uses
 jdk15;

type
 TBaseClass = class
 private
   FLevel : integer;
   procedure SetLevel(value: integer); virtual;
 protected
   property Level: Integer read FLevel write SetLevel;
 end;

 TDerivedClass = class(TBaseClass)
 public
   property Level;
 end;

implementation

procedure TBaseClass.SetLevel(Value: integer);
begin
 FLevel := Value;
end;

end.
