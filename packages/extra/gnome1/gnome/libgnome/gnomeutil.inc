{$IfDef read_interface}

const
   PATH_SEP = '/';
   PATH_SEP_STR = '/';

function gnome_libdir_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_libdir_file';
function gnome_datadir_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_datadir_file';
function gnome_sound_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_sound_file';
function gnome_pixmap_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_pixmap_file';
function gnome_config_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_config_file';
function gnome_unconditional_libdir_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_unconditional_libdir_file';
function gnome_unconditional_datadir_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_unconditional_datadir_file';
function gnome_unconditional_sound_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_unconditional_sound_file';
function gnome_unconditional_pixmap_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_unconditional_pixmap_file';
function gnome_unconditional_config_file(filename:Pchar):pchar;cdecl;external libgnomedll name 'gnome_unconditional_config_file';

const
  G_FILE_TEST_EXISTS = ((1 shl 0) or (1 shl 1)) or (1 shl 2);
  G_FILE_TEST_ISFILE = 1 shl 0;
  G_FILE_TEST_ISLINK = 1 shl 1;
  G_FILE_TEST_ISDIR = 1 shl 2;

function g_file_test(filename:Pchar; test:longint):longint;cdecl;external libgnomedll name 'g_file_test';
function g_file_exists(filename:Pchar):longint;cdecl;external libgnomedll name 'g_file_exists';
function gnome_is_program_in_path(aprogram:Pgchar):Pgchar;cdecl;external libgnomedll name 'gnome_is_program_in_path';

function g_unix_error_string(error_num:longint):pchar;cdecl;external libgnomedll name 'g_unix_error_string';
function g_concat_dir_and_file(dir:Pchar; afile:Pchar):pchar;cdecl;external libgnomedll name 'g_concat_dir_and_file';

function g_extension_pointer(path:Pchar):pchar;cdecl;external libgnomedll name 'g_extension_pointer';
function g_copy_vector(vec:PPchar):Ppchar;cdecl;external libgnomedll name 'g_copy_vector';

function gnome_util_user_shell:pchar;cdecl;external libgnomedll name 'gnome_util_user_shell';
function g_is_image_filename(path:Pchar):gboolean;cdecl;external libgnomedll name 'g_is_image_filename';

//Function g_copy_strings(string1:Pgchar; args:array of const):Pgchar;
Function g_filename_index(path : Pgchar) : Pgchar;
Function g_filename_pointer(file_name:Pgchar):Pgchar;
Function g_flatten_vector(separator:Pgchar; str_array:PPgchar) : Pgchar;
Function gnome_util_user_home : Pgchar;
Function gnome_util_prepend_user_home(x : Pgchar) : Pgchar;
Function gnome_util_home_file(afile : Pgchar) : Pgchar;

{$EndIf read_interface}

{$Ifdef read_implementation}
{Function g_copy_strings(string1:Pgchar; args:array of const):Pgchar;
begin
  g_copy_strings := g_strconcat(string1, args);
end;}

Function g_filename_index(path : Pgchar) : Pgchar;
begin
  g_filename_index := Pointer(Longint(g_basename(path))-Longint(path));
end;

Function g_filename_pointer(file_name:Pgchar):Pgchar;
begin
  g_filename_pointer := g_basename(file_name);
end;

Function g_flatten_vector(separator:Pgchar; str_array:PPgchar) : Pgchar;
begin
  g_flatten_vector := g_strjoinv(separator, str_array);
end;

Function gnome_util_user_home : Pgchar;
begin
  gnome_util_user_home := g_get_home_dir;
end;

Function gnome_util_prepend_user_home(x : Pgchar) : PgChar;
begin
  gnome_util_prepend_user_home := g_concat_dir_and_file(gnome_util_user_home, x);
end;

Function gnome_util_home_file(afile : Pgchar) : Pgchar;
begin
 gnome_util_home_file := g_strconcat(gnome_util_user_home, ['/.gnome/', afile, nil]);
end;

{$Endif read_implementation}
