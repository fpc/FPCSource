unit pkgmessages;

{$mode objfpc}{$H+}

interface


Resourcestring
  SErrInValidArgument        = 'Invalid command-line argument at position %d : %s';
  SErrNeedArgument           = 'Option at position %d (%s) needs an argument';
  SErrMissingConfig          = 'Missing configuration Makefile.fpc or fpmake.pp';
  SErrRunning                = 'The FPC make tool encountered the following error: %s';
  SErrFailedToCompileFPCMake = 'Could not compile fpmake driver program';
  SErrNoFTPDownload          = 'This binary has no support for FTP downloads.';
  SErrNoHTTPDownload         = 'This binary has no support for HTTP downloads.';
  SErrBackupFailed           = 'Backup of file "%s" to file "%s" failed.';
  SErrUnknownProtocol        = 'Unknown download protocol: "%s"';
  SErrNoSuchFile             = 'File "%s" does not exist.';
  SErrWGetDownloadFailed     = 'Download failed: wget reported exit status %d.';
  SErrDownloadFailed         = 'Download failed: %s';
  SErrInvalidVerbosity       = 'Invalid verbosity string: "%s"';
  SErrInvalidCommand         = 'Invalid command: %s';
  
  SErrHTTPGetFailed          = 'HTTP Download failed.';
  SErrLoginFailed            = 'FTP LOGIN command failed.';
  SErrCWDFailed              = 'FTP CWD "%s" command failed.';  
  SErrGETFailed              = 'FTP GET "%s" command failed.';
  SLogGeneratingFPMake       = 'Generating fpmake.pp';
  SLogCompilingFPMake        = 'Compiling fpmake.pp: ';
  SLogRunningFPMake          = 'Running fpmake';
  
implementation

end.

