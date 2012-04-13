Concepts
--------

A Lux script may succeed or fail, meaning that the system under
test is either conforming to or diverging from the expected
behavior. A Lux script may also end with an **error**, if the
Lux engine encountered some problem, and could not determine
whether the system under test conforms or not.

A Lux script consists of a sequence of instructions, mostly `send`
and `expect` operations, read in sequence from top to bottom. The test
case **succeed**s if all statements in the script are executed (or if an
optional success criteria matched). The test case **fail**s if there
is a `expect` operation that does not match within a given time (or if
an optional failure criteria matches).

Each test case should not depend on other test cases being executed
before (for example preparing something) or after (to clean up).
The **cleanup** procedure is an integral part of each test case.

Each `send` and `expect` operation is directed to one particular shell
(the **active shell**). Input is sent to the **stdin** stream of the
shell. The **stdout** and **stderr** streams of the shell are combined
to one single output stream. A Lux script can start and control
many concurrent shells, but at any time point only one is the active
shell which can evaulate new Lux statements.

It is possible to reference **variables** in the `send`, `expect`,
`my`, `local`, `global` and `config` statements, using the standard
shell `$var` or `${var}` notation. Note that the substitution is
performed by the Lux engine and not by the process running within
the shell. The variables are initially set to all **environment
variables**. But new settings can be added by using `[my var=val]`,
`[local var=val]` and `[global var=val]`. Each such variable setting
overrides existing settings of the same variable.

If no variable substitution should take place, the dollar sign must be
*escape*d with yet another dollar sign (`$`). For example, if the
actual value of an environment variable named `var` should be read
from the Bourne shell, the `$var` string cannot be substituted to
another value by the Lux engine. The string `$var` must be sent
literally to the shell even if the Lux engine happens to have
have a variable registered with that name. In order to achieve this
the `$var` must be escaped as `$$var`.

The [regular expression][]s in `expect` statements may contain
**sub-patterns**. The values matching captured sub-patterns may be
accessed in the following variable assignments. For example if the
statement `?begin (.*) middle (.*) end` matches the actual shell output
`begin abc middle def end`. The captured sub-patterns can be accessed
as numbered variables: `[local foo=the vals are $1 and $2]` which will
assign the variable `foo` to the value "`the vals are abc and def`".
As in any language [regular expression][]s contains keywords. If the
output of a shell contains such a keyword and we want to match that,
the keyword must be escaped. Example of such keywords are any of
the characters `^$.?+*()[]{}|`.

The variable substitution mechanism makes it also possible to reuse
(parts of) generic scripts in several test cases. The (main) script
for these test cases may assign different values to variables and then
include a generic script that makes use of these variables.

See the documentation about [regular expression][]s in Erlang for
details about the regular expression dialect used in Lux. It is
an extended subset of [PCRE][PCRE].
