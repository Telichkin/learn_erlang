{alias, test, "./test"}.
{logdir, "./test/logs"}.

{suites, test, all}.
{skip_cases, test, basic_SUITE, test_2, "Div by zero in test"}.