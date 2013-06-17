{
 PTC OpenGL example for PTCPas
 Copyright (c) Nikolay Nikolov (nickysn@users.sourceforge.net)
 This source code is in the public domain
}

program PtcGLExample;

{$MODE objfpc}

uses
  ptc, gl, SysUtils;

var
  Console: IPTCConsole;
  Event: IPTCEvent;
  Done: Boolean = False;
begin
  try
    try
      { create console }
      Console := TPTCConsoleFactory.CreateNew;

      { tell PTC we want OpenGL }
      Console.OpenGL_Enabled := True;

      { enable OpenGL double buffering }
      Console.OpenGL_Attributes.DoubleBuffer := True;

      { open the console }
      Console.Open('PTC OpenGL example');

      glClearColor(0.0, 0.0, 0.0, 0.0);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

      { loop until the key 'q' is pressed }
      repeat
        { draw scene }
        glClear(GL_COLOR_BUFFER_BIT);

        glBegin(GL_POLYGON);
          glColor3f(1.0, 0.0, 0.0);
          glVertex3f(0.25, 0.25, 0.0);
          glColor3f(1.0, 1.0, 0.0);
          glVertex3f(0.75, 0.25, 0.0);
          glColor3f(0.5, 0.0, 1.0);
          glVertex3f(0.75, 0.75, 0.0);
          glColor3f(0.0, 1.0, 0.0);
          glVertex3f(0.25, 0.75, 0.0);
        glEnd;

        glFlush;

        { swap buffers }
        Console.OpenGL_SwapBuffers;

        { check for events }
        if Console.NextEvent(Event, False, PTCAnyEvent) then
        begin
          { handle keyboard events }
          if Supports(event, IPTCKeyEvent) and (event as IPTCKeyEvent).Press then
          begin
            case (event as IPTCKeyEvent).Code of
              PTCKEY_Q: Done := True;
            end;
          end;
        end;
      until Done;
    finally
      if Assigned(Console) then
        Console.Close;
    end;
  except
    on Error: TPTCError do
      { report error }
      Error.Report;
  end;
end.
