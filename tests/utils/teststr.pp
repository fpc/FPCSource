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