{$ifdef Delphi}
const fppkg : array[0..1] of string[240]=(
{$else Delphi}
const fppkg : array[0..1,1..240] of char=(
{$endif Delphi}
  '[Defaults]'#010+
  'ConfigVersion=4'#010+
  'LocalRepository=%LocalRepository%'#010+
  'BuildDir={LocalRepository}build/'#010+
  'ArchivesDir={LocalRepository}archives/'#010+
  'CompilerConfigDir=%CompilerConfigDir%'#010+
  'RemoteMirrors=http://www.freepascal.org/repository/mirrors.xml'#010+
  'Remote','Repository=auto'#010+
  'CompilerConfig=default'#010+
  'FPMakeCompilerConfig=default'#010+
  'Downloader=lnet'#010
);
