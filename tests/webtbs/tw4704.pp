{ %OPT=-S2 }
{ Source provided for Free Pascal Bug Report 4704 }
{ Submitted by "Phil H." on  2006-01-17 }
{ e-mail: pjhess@purdue.edu }
program TestExcep;

uses
  SysUtils,
  Variants;

var
  AnInt : Integer;
  AVar  : Variant;
  
begin
  AVar := Null;
  try
    AnInt := AVar;
    halt(1);
    case AnInt of
      1 : ;
      end;
    
  except 
    on E: EVariantError do
      begin
      WriteLn('Handled EVariantError');
      WriteLn(E.ClassName);
      WriteLn(E.Message);
      if (E.Message = '') then
        halt(3);
      end;
    on E: Exception do
      begin
      WriteLn('Handled Exception');
      WriteLn(E.ClassName);
      WriteLn(E.Message);
      halt(2);
      end;
    end;
  
end.

