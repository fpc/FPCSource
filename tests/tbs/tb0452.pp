{ %version=1.1 }
{ %target=linux }
unit tb0452;

{$mode delphi}

interface

function sem_open(__name: PChar; __oflag: Integer): Pointer; cdecl; varargs;

implementation

const
  libpthreadmodulename = 'libpthread.so';

function sem_open; external libpthreadmodulename name 'sem_open';

end.
