{
  $Id$

  GL unit creation tool for Linux dynamic version
  (c) 1999-2000 Sebastian Guenther, sg@freepascal.org
}


program c_linuxd;

{$MODE objfpc}
{$H+}

uses SysUtils, Classes, buildgl;

var
  f: Text;

procedure PrintInterface(lines: TStringList; var dest: Text);
var
  i: Integer;
begin
  for i := 0 to lines.Count - 1 do
    WriteLn(dest, lines.Strings[i]);
end;

procedure PrintProcDecls(procs: TStringList);
var
  i, j: Integer;
  s: String;
begin
  for i := 0 to procs.Count - 1 do
  begin
    s := procs.Strings[i];
    j := Pos('//', s);
    if (Length(s) = 0) or ((j > 0) and (Trim(s)[1] = '/')) then
      WriteLn(f, s)
    else if j = 0 then
      WriteLn(f, s, ' cdecl;')
    else
      WriteLn(f, TrimRight(Copy(s, 1, j - 1)), ' cdecl; ', Copy(s, j, Length(s)));
  end;
end;

procedure PrintProcLoaders(procs: TStringList; const libname: String);
var
  i, j: Integer;
  s: String;
begin
  for i := 0 to procs.Count - 1 do
  begin
    s := Trim(procs.Strings[i]);
    j := Pos(':', s);
    s := Trim(Copy(s, 1, j - 1));
    if (Length(s) = 0) or (Pos('//', s) > 0) then continue;
    WriteLn(f, '  ', s, ' := GetProc(', libname, ', ''', s, ''');');
  end;
end;

procedure PrintCVSLogSection;
begin
  WriteLn(f);
  WriteLn(f);
  WriteLn(f, '{');
  WriteLn(f, '  $', 'Log:$');  // this source file (c_linuxd.pp) is in CVS, too!
  WriteLn(f, '}');
end;

var
  DefGL, DefGLExt, DefGLU, DefGLX, DefGLUT: TDefReader;
  tpl: Text;
  s: String;
begin
  WriteLn('Template processor for OpenGL related Units');

  // Load definition files

  WriteLn('Loading definition files...');

  DefGL    := TDefReader.Create('gl.def');
  DefGLExt := TDefReader.Create('glext.def');
  DefGLU   := TDefReader.Create('glu.def');
  DefGLX   := TDefReader.Create('glx.def');
  DefGLUT  := TDefReader.Create('glut.def');


  // Build GL unit

  WriteLn('Generating GL unit for Linux...');

  Assign(f, '../linux/gl.pp');
  Rewrite(f);
  Assign(tpl, 'gl_linux.tpl');
  Reset(tpl);
  while not EOF(tpl) do
  begin
    ReadLn(tpl, s);
    if Copy(s, 1, 1) = '%' then
    begin
      if s = '%GLDecls' then
        PrintInterface(DefGL.InterfaceBlock, f)
      else if s = '%GLProcs1' then
        PrintProcDecls(DefGL.Procs)
      else if s = '%GLProcs2' then
        PrintProcLoaders(DefGL.Procs, 'libgl')
      else if s = '%GLExtDecls' then
        PrintInterface(DefGLExt.InterfaceBlock, f)
      else if s = '%GLExtProcs1' then
        PrintProcDecls(DefGLExt.Procs)
      else if s = '%GLExtProcs2' then
        PrintProcLoaders(DefGLExt.Procs, 'libgl')
      else if s = '%GLUDecls' then
        PrintInterface(DefGLU.InterfaceBlock, f)
      else if s = '%GLUProcs1' then
        PrintProcDecls(DefGLU.Procs)
      else if s = '%GLUProcs2' then
        PrintProcLoaders(DefGLU.Procs, 'libglu')
      else if s = '%GLXDecls' then
        PrintInterface(DefGLX.InterfaceBlock, f)
      else if s = '%GLXProcs1' then
        PrintProcDecls(DefGLX.Procs)
      else if s = '%GLXProcs2' then
        PrintProcLoaders(DefGLX.Procs, 'libglx')
      else
        WriteLn(f, '// ### c_linuxd: Don''t know what to insert here!: ', s);
    end else if Copy(s, 1, 1) <> '#' then
      WriteLn(f, s);
  end;
  PrintCVSLogSection;
  Close(f);


  // Build GLUT unit

  WriteLn('Generating GLUT unit for Linux...');

  Assign(f, '../linux/glut.pp');
  Rewrite(f);
  Assign(tpl, 'glut_linux.tpl');
  Reset(tpl);
  while not EOF(tpl) do
  begin
    ReadLn(tpl, s);
    if Copy(s, 1, 1) = '%' then
    begin
      if s = '%GLUTDecls' then
        PrintInterface(DefGLUT.InterfaceBlock, f)
      else if s = '%GLUTProcs1' then
        PrintProcDecls(DefGLUT.Procs)
      else if s = '%GLUTProcs2' then
        PrintProcLoaders(DefGLUT.Procs, 'libglut')
      else
        WriteLn(f, '// ### c_linuxd: Don''t know what to insert here!: ', s);
    end else if Copy(s, 1, 1) <> '#' then
      WriteLn(f, s);
  end;
  PrintCVSLogSection;
  Close(f);

  WriteLn('Done...');
end.


{
  $Log$
  Revision 1.2  2000-07-13 11:33:28  michael
  + removed logs
 
}
