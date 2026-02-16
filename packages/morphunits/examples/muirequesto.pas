{
    Copyright (c) 2026 Karoly Balogh

    MUI MUI_RequestObjectA() example program
    Example program for Free Pascal's MorphOS bindings

    With thanks to Jacek Piszczek for his help.

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}
{ Needs MorphOS 3.19+ }

{ This program can be written in a more convenient way using the
  MUIHelper unit in ami-extras, this example demonstrates the direct
  MUI and BOOPSI API way, that isn't very well suited for Pascal,
  because it's very C-style macro and pointer heavy. }

program muirequesto;

uses
  intuition, mui, utility;

var
  stringobj: pObject_;
  result: DWord;
  name: PChar;

begin
  name:='';

  stringobj := MUI_NewObject(MUIC_String, [ MUIA_Frame, MUIV_Frame_String, TAG_END ]);
  DoMethod(stringobj, [ OM_RETAIN ]);
  result := MUI_RequestObjectA(nil, nil, 0, 'My Requester', 'OK|Cancel',
                               stringobj, 'Enter your name below:', nil);
  if (result = 1) then
    begin
      GetAttr(MUIA_String_Contents, stringobj, @name);
      writeln('Your name is: ',name);
    end;
  MUI_DisposeObject(stringobj);
end.
