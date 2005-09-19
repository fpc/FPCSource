{$mode delphi}

unit tw4350;

interface

uses
  Classes;

type
  TIdStackSocketHandle = integer ;// class end;
  TIdServeFile = function(ASocket: TIdStackSocketHandle; AFileName: string): cardinal;
  TIdStackBSDBase = Class
                      end;

var
  GServeFileProc: TIdServeFile = nil; // comment either, and the other line works?
  GBSDStack: TIdStackBSDBase = nil;

implementation

end.
