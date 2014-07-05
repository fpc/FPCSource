{$mode delphi}
{$modeswitch unicodestrings}
{$namespace org.freepascal.test}

Unit tprop6;

interface

uses
 jdk15;

type
 TBaseClassProp6 = class
 private
   FLevel : integer;
   procedure SetLevel(value: integer); virtual;
 public
   property Level: Integer read FLevel write SetLevel;
 end;

 TDerivedClassProp6 = class(TBaseClassProp6)
 protected
   procedure SetLevel(value: integer); override;
 public
   property Level: Integer read FLevel write SetLevel;
 end;

implementation

procedure TBaseClassProp6.SetLevel(Value: integer);
begin
 FLevel := Value;
end;

procedure TDerivedClassProp6.SetLevel(Value: integer);
begin
 FLevel := Value+1;
end;

end.
