program grab_vcsa;

{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Daniël Mantione
     member of the Free Pascal development team.

    VCSA grabber program for Linux.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

uses baseunix,termio;

{This program helps the video unit to use /dev/vcsa* to provide high
 quality output.

 Normally, when you login, the owner of the tty you are running on,
 /dev/tty0-31 is set to your username. Weird enough, this is not done
 with the video buffer devices belonging to that tty, /dev/vcs0-31
 and /dev/vcsa0-31. This makes it impossible to do high quality text
 mode video output.

 This program, designed to be run suid root, reads the owner and mode
 of the tty, and assigns them to the corresponding vcs and vcsa device.}

{Security design:

 - It has been checked if the user can provide any input to the program.
   The only input in the program is stdinputhandle, which cannot be
   controlled by the user. The user has therefore no control in any way 
   about the code flow in the program; the code that is going to be
   executed is fixed no matter what a user does.

 - Outputs are the file permissions of /dev/vcs* and /dev/vcsa*. It has
   been considered if users could use the program this way to gain rights
   they should not have. By having access to /dev/vcs* and /dev/vcsa*
   the user can garble his own screen. This should not be a problem.

   After the user has logged out /dev/vcs* and /dev/vcsa* are automatically
   changed back to root. This removes the need for us to change permissions
   back again. If we would change permissions back, it would be useless, the
   user can kill the process before it changes back permissions.

 - Normal users cannot write to /dev/ and can therefore not use the program
   do change the permissions of other files than the actual devices.
}

const   result_success=0;
        result_not_on_console=1;
        result_stat_error=2;
        result_chown_error=3;
        result_chmod_error=4;
        result_not_owner_error=5;

var thistty:string;
    vcs,vcsa:string;
    ttystat:stat;

begin
  exitcode:=result_not_on_console;
  thistty:=ttyname(stdinputhandle);
  if isatty(stdinputhandle)=1 then
    begin
      { running on a tty, find out whether locally or remotely }
      if (length(thistty)>=9) and
         (copy(thistty,1,8)='/dev/tty') and
         (thistty[9] in ['0'..'9']) then
        begin
          {We are running on the Linux console}
          if fpstat(thistty,ttystat)<>0 then
            halt(result_stat_error);
          if ttystat.uid<>fpgetuid then
            halt(result_not_owner_error);
          vcs:='/dev/vcs'+copy(thistty,9,255);
          vcsa:='/dev/vcsa'+copy(thistty,9,255);
          
          {Change owner and group to that of /dev/tty??.}
          if fpchown(vcs,ttystat.uid,ttystat.gid)<>0 then
            halt(result_chown_error);
          if fpchown(vcsa,ttystat.uid,ttystat.gid)<>0 then
            halt(result_chown_error);

          {Change permissions to that of /dev/tty??.}
          if fpchmod(vcs,ttystat.mode)<>0 then
            halt(result_chmod_error);
          if fpchmod(vcsa,ttystat.mode)<>0 then
            halt(result_chmod_error);
          exitcode:=result_success;
        end;
    end;
end.
