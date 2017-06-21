program event;

{$MODE objfpc}{$H+}

uses
  SysUtils, ptc;

function ButtonState2Str(const bs: TPTCMouseButtonState): string;
var
  I: TPTCMouseButton;
begin
  Result := '';
  for I in TPTCMouseButton do
    if I in bs then
      WriteStr(Result, Result, ',', I);
  if Result = '' then
    Result := '[]'
  else
  begin
    Result[1] := '[';
    Result := Result + ']';
  end;
end;

function ModifierKeys2Str(const mk: TPTCModifierKeys): string;
var
  I: TPTCModifierKey;
begin
  Result := '';
  for I in TPTCModifierKey do
    if I in mk then
      WriteStr(Result, Result, ',', I);
  if Result = '' then
    Result := '[]'
  else
  begin
    Result[1] := '[';
    Result := Result + ']';
  end;
end;

var
  console: IPTCConsole;
  ev: IPTCEvent;
  Done: Boolean = False;
  RX, RY: Integer;
begin
  try
    try
      console := TPTCConsoleFactory.CreateNew;

      console.Option('intercept window close');
  //    console.Option('resizable window');

      console.Open('event test');

      repeat
        console.NextEvent(ev, True, PTCAnyEvent);
        if Supports(ev, IPTCMouseButtonEvent) then
          with ev as IPTCMouseButtonEvent do
            Writeln('IPTCMouseButtonEvent(X=', X, '; Y=', Y, '; DeltaX=', DeltaX,
            '; DeltaY=', DeltaY, '; ButtonState=', ButtonState2Str(ButtonState),
            '; Press=', Press, '; Release=', Release, '; Button=', Button, ')')
        else if Supports(ev, IPTCMouseEvent) then
          with ev as IPTCMouseEvent do
            Writeln('IPTCMouseEvent(X=', X, '; Y=', Y, '; DeltaX=', DeltaX,
            '; DeltaY=', DeltaY, '; ButtonState=', ButtonState2Str(ButtonState),
            ')')
        else if Supports(ev, IPTCKeyEvent) then
          with ev as IPTCKeyEvent do
            Writeln('IPTCKeyEvent(Code=', Code, '; Unicode=', Unicode, '; Press=',
              Press, '; Release=', Release, '; Alt=', Alt, '; Shift=', Shift,
              '; Control=', Control, '; ModifierKeys=',
              ModifierKeys2Str(ModifierKeys), ')')
        else if Supports(ev, IPTCResizeEvent) then
          with ev as IPTCResizeEvent do
            Writeln('IPTCResizeEvent(Width=', Width, '; Height=', Height, ')')
        else if Supports(ev, IPTCCloseEvent) then
          with ev as IPTCCloseEvent do
            Writeln('IPTCCloseEvent()')
        else
          Writeln('UNKNOWN EVENT TYPE');

        if Supports(ev, IPTCKeyEvent) then
          with ev as IPTCKeyEvent do
            if Press then
            begin
              case Code of
                PTCKEY_G:
                  console.Option('grab mouse');
                PTCKEY_U:
                  console.Option('ungrab mouse');
                PTCKEY_S:
                  console.Option('show cursor');
                PTCKEY_H:
                  console.Option('hide cursor');
                PTCKEY_R:
                  console.Option('relative mouse on');
                PTCKEY_A:
                  console.Option('relative mouse off');
                PTCKEY_M:
                  begin
                    RX := Random(console.Width);
                    RY := Random(console.Height);
                    Writeln('MoveMouseTo(', RX, ', ', RY, ')');
                    if not console.MoveMouseTo(RX, RY) then
                      writeln('MoveMouseTo FAILED (or is not supported by the console)');
                  end;
                PTCKEY_Q:
                  Done := True;
              end;
            end;
      until Done;
    finally
      if Assigned(console) then
        console.Close;
    end;
  except
    on error: TPTCError do
      error.Report;
  end;
end.
