{$ifdef Delphi}
const fpini : array[0..2] of string[240]=(
{$else Delphi}
const fpini : array[0..2,1..240] of char=(
{$endif Delphi}
  '[Compile]'#013#010+
  'CompileMode=DEBUG'#013#010+
  #013#010+
  '[Help]'#013#010+
  'Files="%basepath%\doc\fpctoc.htx"'#013#010+
  #013#010+
  '[Editor]'#013#010+
  'DefaultTabSize=8'#013#010+
  'DefaultFlags=20599'#013#010+
  'DefaultSaveExt=.pas'#013#010+
  'DefaultIndentSize=1'#013#010+
  #013#010+
  '[Highlight]'#013#010+
  'Exts="*.pas;*.pp;*.inc"'#013#010+
  'NeedsTabs="make*;make*.*"'#013#010+
  #013#010+
  '[Sou','rcePath]'#013#010+
  'SourceList=""'#013#010+
  #013#010+
  '[Mouse]'#013#010+
  'DoubleDelay=8'#013#010+
  'ReverseButtons=0'#013#010+
  'AltClickAction=6'#013#010+
  'CtrlClickAction=1'#013#010+
  #013#010+
  '[Search]'#013#010+
  'FindFlags=4'#013#010+
  #013#010+
  '[Breakpoints]'#013#010+
  'Count=0'#013#010+
  #013#010+
  '[Watches]'#013#010+
  'Count=0'#013#010+
  #013#010+
  '[Preferences]'#013#010+
  'DesktopFileFlags=209'#013#010+
  'CenterCurrentLineWhile','Debugging=1'#013#010+
  'AutoSaveFlags=7'#013#010+
  'MiscOptions=6'#013#010+
  'DesktopLocation=1'#013#010+
  #013#010+
  '[Misc]'#013#010+
  'ShowReadme=1'#013#010+
  #013#010+
  '[Files]'#013#010+
  'OpenExts="*.pas;*.pp;*.inc"'#013#010
);
