{$mode objfpc}
{$H+}
// Run with paramters "1 2 3 4 5"

{$MACRO ON}
{ $DEFINE stdcall:=register}

Uses
  SysUtils,
  Variants;

Type
  TMyClass = Class
    Function GetProperty(Dum: Variant): Variant; stdcall;
  End;

Var
  FUser: TMyClass;
  FI: Longint;
  Parameters: Array Of String;

Function TMyClass.GetProperty(Dum: Variant): Variant; stdcall;
Begin
  Result := '';
End;

procedure Display;
var
  FI: longint;
begin
  // Output content of the parameters buffer
  For FI := 0 to Length(Parameters) - 1 Do Write(Parameters[FI] + ' ');
  Writeln;
end;

Begin
  // Create class instance
  FUser := TMyClass.Create;

  // Fetch params to parameters structure
  SetLength(Parameters, System.ParamCount + 1);
  For FI := 0 to Length(Parameters) - 1 Do Parameters[FI] := System.ParamStr(FI);

  // Display parameters
  Display;

  // Process params
  For FI := 0 To Length(Parameters) - 1 do
  Begin
    // Get property
    FUser.GetProperty(Parameters[FI]);

    // Display parameters
    Display;
  End;
End.
