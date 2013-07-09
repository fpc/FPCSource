{ %NORUN }

program tw24651;

//{$mode delphi}{$H+}
{$modeswitch class}
{$A+}
{$B-}
{$I+}
{$X-}

uses
  Classes
  { you can add units after this };

type
  o_Class1 = class
    fString1 : string ;
    constructor Create ;
  end ;

  o_Class2 = class (o_Class1)
    fString2 : string ;
    constructor Create (aStr : string) ;
  end ;

constructor o_Class1.Create ;
//var t_o : pointer ;
begin
  {t_o := }inherited Create ;
  fString1 := 'test value'
end ;

constructor o_Class2.Create (aStr : string) ;
//var c_1 : pointer;
begin
  {c_1 := }inherited Create ;
  fstring2 := aStr
end ;

var
  C2 : o_Class2 ;

begin
  C2 := o_Class2.Create ('test param') ;
  WriteLn (C2.fString1)
end.
