{
  $Id$
    This file is part of the Free Pascal test suite.
    Copyright (c) 2002 by the Free Pascal development team.

    This unit stores the strings used by
    dotest program, so that they can be also used by
    figest program.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit teststr;

interface

const
  failed_to_compile = 'Failed to compile ';
  success_compilation_failed = 'Success, compilation failed ';
  failed_compilation_successful = 'Failed, compilation successful ';
  successfully_compiled = 'Successfully compiled ';
  failed_to_run = 'Failed to run ';
  successfully_run = 'Successfully run ';
  skipping_graph_test = 'Skipping test because it uses graph ';
  skipping_interactive_test = 'Skipping test because it is interactive ';
  skipping_known_bug = 'Skipping test because it is a known bug ';
  skipping_compiler_version_too_low = 'Skipping test because compiler version too low ';
  skipping_other_cpu = 'Skipping test because for other cpu ';
  skipping_run_unit = 'Skipping test run because it is a unit ';
  skipping_run_test = 'Skipping run test ';

  line_separation = '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>';

  ResLogfile  : string[32] = 'log';

implementation

end.

{
  $Log$
  Revision 1.2  2002-11-13 15:26:24  pierre
   + digest program added

}
