# JSON-Schema testsuite.

The default tests test all parts.

However, it is possible to extend the testsuite by pointing the binary to
the test directory of the JSON-Schema.org testsuite.

You can obtain these by cloning the official testsuite:

[https://github.com/json-schema-org/JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)

git clone https://github.com/json-schema-org/JSON-Schema-Test-Suite.git

and then run the testsuite with the --test-dir argument:
```
./testschema --testdir=JSON-Schema-Test-Suite/tests/draft2020-12
```

