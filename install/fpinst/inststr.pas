{
    $Id$
    This file is part Free Pascal
    Copyright (c) 2000 by Florian Klaempfl
    member of the Free Pascal development team

    This file contains the strings for the FPC install program

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$ifdef FPC}
{$MODE OBJFPC}
{$endif FPC}
unit inststr;

  interface

{$ifndef FPC}
    const
{$else FPC}
    resourcestring
{$endif FPC}
       dialog_language_title = 'Please choose your language';
       dialog_language_english = 'English';
       dialog_language_dutch = 'Dutch';
       dialog_language_french = 'French';
       dialog_language_russian = 'Russian';
       dialog_language_hungarian = 'Hungarian';
       dialog_language_spanish = 'Spanish';
       dialog_language_german = 'German';
       dialog_language_russian_win = 'Russian (Windows)';

       dialog_enddialog_title = 'Installation Successfull';

       dialog_unzipdialog_title = 'Extracting Packages';

       dialog_install_continue = '~C~ontinue';
       dialog_install_quit = '~Q~uit';
       dialog_install_basepath = '~B~ase path';
       dialog_install_config = 'Con~f~ig';
       dialog_install_createppccfg = 'create fpc.cfg';
       dialog_install_general = '~G~eneral';

       msg_nocomponents = 'No components selected.'#13#13'Abort installation?';
       msg_overwrite_cfg = 'Config %s already exists, continue writing default config?';
       msg_problems_writing_cfg = #3'A config not written.'#13#3'%s'#13#3'couldn''t be created';
       msg_problems_create_dir = 'A file with the name chosen as the installation '+
                                 'directory exists already. Cannot create this directory!';
       msg_no_components_selected = 'No components selected.'#13#13'Create a configfile ?';
       msg_select_dir = 'Please, choose the directory for installation first.';
       msg_no_components_found = 'No components found to install, aborting.';
       msg_install_dir_exists = 'The installation directory exists already. '+
                                'Do you want to continue ?';
       msg_install_cant_be_created = 'The installation directory %s couldn''t be created';
       msg_file_not_found = 'File %s not found!';
       msg_no_lfn = 'The operating system doesn''t support LFN (long file names),'+
                    ' so some packages won''t be installed';
       msg_corrupt_zip = 'File %s is probably corrupted!';
       msg_space_warning = 'There %s enough space on the target '+
                           'drive for all the selected components. Do you '+
                           'want to change the installation path?';
       msg_file_missing = 'File %s missing for the selected installation. '+
                          'Installation hasn''t been completed.';
       msg_extraction_error = 'Error (%s) while extracting. Disk full?'#13+
                              #13#3'Try again?';

       menu_install = 'Free Pascal Installer';

       str_requires_lfn = ' (requires LFN support)';
       str_checking_lfn = 'Checking lfn usage for ';
       str_invalid = ' [INVALID]';
       str_file = 'File: ';
       str_extend_path = 'Extend your PATH variable with ';
       str_ok = '~O~k';
       str_is_not = 'is not';
       str_might_not_be = 'might not be';
       str_to_compile = 'To compile files enter ';
       str_start_ide = 'To start the IDE (Integrated Development Environment) type ''fp'' at a command line prompt';
       str_libpath = 'and your LIBPATH with ';
       str_extend_libpath = 'Extend your LIBPATH with ';
       str_dll = 'dll';
       str_file2 = ' [file]';
       str_continue = '~C~ontinue';
       str_quit = '~Q~uit';

  implementation

end.
{
  $Log$
  Revision 1.5  2001-11-24 14:33:51  carl
  * ppc386.cfg -> fpc.cfg

  Revision 1.4  2000/10/11 15:57:47  peter
    * merged ide additions

  Revision 1.3  2000/09/22 12:15:49  florian
    + support of Russian (Windows)

  Revision 1.2  2000/09/22 11:07:51  florian
    + all language dependend strings are now resource strings
    + the -Fr switch is now set in the ppc386.cfg
}