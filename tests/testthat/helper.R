# Suppress cli output in tests
options(cli.default_handler = function(...) NULL)
options(cli.verbosity = -1)
