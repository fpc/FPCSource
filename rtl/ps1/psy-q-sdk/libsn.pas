// LIBSN.H
unit libsn;
interface
{
** FILESERVER FUNCTIONS:
**
** NOTE: For PCread and PCwrite do not load files by passing extreme
** values for count as you might on UNIX as this will cause the full
** amount specified to be transferred - the file will be padded to
** that length with zeroes which may over-write memory beyond the
** end of the file.
**
** If you are unsure of the length of a file which you are about
** to read into memory then perform a
** 	len = PClseek( fd, 0, 2);
** This will set len to the length of the file which you can then
** pass to a PCread() function call.
}

{
** re-initialise PC filing system, close open files etc
**
** passed: void
**
** return: error code (0 if no error)
}
function PCinit: longint; external;

{
** open a file on PC host
**
** passed:	PC file pathname, open mode, permission flags
**
** return:	file-handle or -1 if error
**
** note: perms should be zero (it is ignored)
**
** open mode:	0 => read only
** 		1 => write only
**		2 => read/write
}
function PCopen(name: pchar; flags, perms: longint): longint; external;

{
** create (and open) a file on PC host
**
** passed:	PC file pathname, open mode, permission flags
**
** return:	file-handle or -1 if error
**
** note: perms should be zero (it is ignored)
}
function PCcreat(name: pchar; perms: longint): longint; external;

{
** seek file pointer to new position in file
**
** passed: file-handle, seek offset, seek mode
**
** return: absolute value of new file pointer position
**
** (mode 0 = rel to start, mode 1 = rel to current fp, mode 2 = rel to end)
}
function PClseek(fd: longint; offset: longint; mode: longint): longint; external;

{
** read bytes from file on PC
**
** passed: file-handle, buffer address, count
**
** return: count of number of bytes actually read
**
** note: unlike assembler function this provides for full 32 bit count
}
function PCread(fd: longint; buff: pointer; len: longint): longint; external;

{
** write bytes to file on PC
**
** passed: file-handle, buffer address, count
**
** return: count of number of bytes actually written
**
** note: unlike assembler function this provides for full 32 bit count
}
function PCwrite(fd: longint; buff: pointer; len: longint): longint; external;

{
** close an open file on PC
**
** passed: file-handle
**
** return: negative if error
**
}
function PCclose(fd: longint): longint; external;

implementation

begin
end.