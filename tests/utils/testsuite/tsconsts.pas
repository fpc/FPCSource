unit tsconsts;

{$mode ObjFPC}

interface

const
  TestResultsTableName = 'TESTRESULTS';
  MaxLimit = 1000;

  TestsuiteURLPrefix='http://www.freepascal.org/testsuite/';
  TestsuiteBin='testsuite.cgi';
  ViewURL='http://svn.freepascal.org/cgi-bin/viewvc.cgi/';
  ViewRevURL='http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&amp;revision=';
  ViewGitHashURL='https://gitlab.com/freepascal.org/fpc/source/-/tree/';
  TestsSubDir='/tests/';
  DataBaseSubDir='/packages/fcl-db/tests/';


  faction_show_overview = 0;
  faction_show_run_results = 1;
  faction_show_run_pie = 2;
  faction_show_one_test = 3;
  faction_show_history = 4;
  faction_compare_with_previous = 5;
  faction_compare_with_next = 6;
  faction_compare2_with_previous = 7;
  faction_compare2_with_next = 8;
  faction_compare_both_with_previous = 9;
  faction_compare_both_with_next = 10;

Var
  SDetailsURL : string;

type
  known_versions = (
    ver_unknown,
    ver_1_0_10,
    ver_2_0_0,
    ver_2_0_1,
    ver_2_0_2,
    ver_2_0_3,
    ver_2_0_4,
    ver_2_0_5,
    ver_2_1_2,
    ver_2_1_4,
    ver_2_2_0,
    ver_2_2_1,
    ver_2_2_2,
    ver_2_2_3,
    ver_2_2_4,
    ver_2_2_5,
    ver_2_3_1,
    ver_2_4_0,
    ver_2_4_1,
    ver_2_4_2,
    ver_2_4_3,
    ver_2_4_4,
    ver_2_4_5,
    ver_2_5_1,
    ver_2_6_0,
    ver_2_6_1,
    ver_2_6_2,
    ver_2_6_3,
    ver_2_6_4,
    ver_2_6_5,
    ver_2_7_1,
    ver_3_0_0,
    ver_3_0_1,
    ver_3_0_2,
    ver_3_0_3,
    ver_3_0_4,
    ver_3_0_5,
    ver_3_1_1,
    ver_3_2_0,
    ver_3_2_1,
    ver_3_2_2,
    ver_3_2_3,
    ver_3_3_1);


const
  ver_trunk = high (known_versions);
  ver_string : array[known_versions] of string =
  (
   'unknown',
   '1.0.10',
   '2.0.0',
   '2.0.1',
   '2.0.2',
   '2.0.3',
   '2.0.4',
   '2.0.5',
   '2.1.2',
   '2.1.4',
   '2.2.0',
   '2.2.1',
   '2.2.2',
   '2.2.3',
   '2.2.4',
   '2.2.5',
   '2.3.1',
   '2.4.0',
   '2.4.1',
   '2.4.2',
   '2.4.3',
   '2.4.4',
   '2.4.5',
   '2.5.1',
   '2.6.0',
   '2.6.1',
   '2.6.2',
   '2.6.3',
   '2.6.4',
   '2.6.5',
   '2.7.1',
   '3.0.0',
   '3.0.1',
   '3.0.2',
   '3.0.3',
   '3.0.4',
   '3.0.5',
   '3.1.1',
   '3.2.0',
   '3.2.1',
   '3.2.2',
   '3.2.3',
   '3.3.1'
  );

  ver_branch : array [known_versions] of string =
  (
   '',
   '',
   'tags/release_2_0_0',
   'branches/fixes_2_0',
   'tags/release_2_0_2',
   'branches/fixes_2_0',
   'tags/release_2_0_4',
   'branches/fixes_2_0',
   'tags/release_2_1_2',
   'tags/release_2_1_4',
   'tags/release_2_2_0',
   'branches/fixes_2_2',
   'tags/release_2_2_2',
   'branches/fixes_2_2',
   'tags/release_2_2_4',
   'branches/fixes_2_2',
   'branches/fixes_2_2',
   'tags/release_2_4_0',
   'tags/release_2_4_0',
   'tags/release_2_4_2',
   'tags/release_2_4_2',
   'tags/release_2_4_4',
   'tags/release_2_4_4',
   'branches/fixes_2_4',
   'tags/release_2_6_0',
   'tags/release_2_6_0',
   'tags/release_2_6_2',
   'tags/release_2_6_2',
   'tags/release_2_6_4',
   'tags/release_2_6_4',
   'branches/fixes_2_6',
   'tags/release_3_0_0',
   'tags/release_3_0_0',
   'tags/release_3_0_2',
   'tags/release_3_0_2',
   'tags/release_3_0_4',
   'tags/release_3_0_4',
   'branches/fixes_3_0',
   'tags/release_3_2_0',
   'tags/release_3_2_0',
   'tags/release_3_2_2',
   'branches/fixes_3_2',
   'trunk'
  );

implementation

end.

