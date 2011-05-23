{$ifdef Delphi}
const fppkg : array[0..1] of string[240]=(
{$else Delphi}
const fppkg : array[0..1,1..240] of char=(
{$endif Delphi}
  '[Defaults]'#013#010+
  'ConfigVersion=4'#013#010+
  'LocalRepository=%LocalRepository%'#013#010+
  'BuildDir={LocalRepository}build/'#013#010+
  'ArchivesDir={LocalRepository}archives/'#013#010+
  'CompilerConfigDir=%CompilerConfigDir%'#013#010+
  'RemoteMirrors=http://www.freepascal.org/repository/mirrors.xml'#013,#010+
  'RemoteRepository=auto'#013#010+
  'CompilerConfig=default'#013#010+
  'FPMakeCompilerConfig=default'#013#010+
  'Downloader=lnet'#013#010
);
