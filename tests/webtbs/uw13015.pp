unit uw13015; 

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes;
  
type
  TTestClass=class(TComponent)
  private
    fWStr:WideString;
  public
    constructor Create(AnOwner:TComponent);override;
    procedure DumpAndCheck;
  published
    property Wstr:WideString read fWStr write fWStr;
  end;

const
   {$ifdef fpc}
   ws:WideString=#$43f#$440#$438#$432#$435#$442', '#$43f#$440#$44B#$432#$456#$442#$430#$43d#$44c#$43d#$435' - pr'#$fc'fung spa'#$df' gut';
   {$else}
   ws:WideString='привет, прывітаньне - prufung spa'#$df' gut';
   {$endif}
   
   
procedure Register;

implementation
uses SysUtils;

constructor TTestClass.Create(AnOwner:TComponent);
begin
     inherited Create(AnOwner);
     fWStr:=ws;
end;


procedure TTestClass.DumpAndCheck;
var
   i,w:integer;
begin
     for i:=1 to length(fWstr) do
     begin
          w:=Word(fWstr[i]);
          write(format('%.04x ',[w]));
          if w<>word(ws[i]) then
            halt(1);
     end;
     writeln;
end;


procedure Register; 
begin
     RegisterComponents('tc',[TTestClass]);
end; 

end.
