{
 staticbug.pas

 With FPC 2.2.2:

 staticbug.lpr(24,31) Error: function header doesn't match the previous declaration "class TMyController.doClose(Pointer, Pointer, Pointer);CDecl"
}
program staticbug;

{$mode delphi}{$STATIC ON}

uses
  Classes, SysUtils;

type

  { TMyController }

  TMyController = class
  public
    class procedure doClose(_self: Pointer; _cmd: Pointer; sender: Pointer); cdecl; static;
  end;

class procedure TMyController.doClose(_self: Pointer; _cmd: Pointer; sender: Pointer); cdecl; static;
begin
end;

begin
end.
