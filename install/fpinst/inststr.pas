{$MODE OBJFPC}
unit inststr;

  interface

    resourcestring
       dialog_language_title = 'Please choose your language';
       dialog_language_english = 'English';
       dialog_language_dutch = 'Dutch';
       dialog_language_french = 'French';
       dialog_language_russian = 'Russian';
       dialog_language_hungarian = 'Hungarian';
       dialog_language_spanish = 'Spanish';
       dialog_language_german = 'German';

       dialog_enddialog_title = 'Installation Successfull';

       dialog_unzipdialog_title = 'Extracting Packages';

       msg_nocomponents = 'No components selected.'#13#13'Abort installation?';
       msg_overwrite_cfg = 'Config %s already exists, continue writing default config?';
       msg_problems_writing_cfg = #3'Default config not written.'#13#3'%s'#13#3'couldn''t be created';
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

       menu_install = 'Free Pascal Installer';

       str_requires_lfn = ' (requires LFN support)';
       str_checking_lfn = 'Checking lfn usage for ';
       str_invalid = ' [INVALID]';

  implementation

end.