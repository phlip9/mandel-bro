"""This module implements the core Scheme interpreter functions, including the
eval/apply mutual recurrence, environment model, and read-eval-print loop.
"""

from scheme_primitives import *
from scheme_reader import *
from ucb import main, trace

def scheme_apply(procedure, args, env):
    """Apply Scheme PROCEDURE to argument values ARGS in environment ENV."""
    if isinstance(procedure, PrimitiveProcedure):
        return apply_primitive(procedure, args, env)
    elif isinstance(procedure, LambdaProcedure):
        frame = procedure.env.make_call_frame(procedure.formals, args)
        return scheme_eval(procedure.body, frame)
    elif isinstance(procedure, MuProcedure):
        frame = env.make_call_frame(procedure.formals, args)
        return scheme_eval(procedure.body, frame)
    else:
        raise SchemeError("Cannot call {0} with {1}".format(str(procedure), str(args)))

def pairs_to_list(pairs):
    """Converts a scheme list to a python list
    >>> pairs_to_list(Pair(1, Pair(2, nil)))
    [1, 2]
    """
    result = []
    for i in range(len(pairs)):
        result.append(pairs[i])
    return result

def apply_primitive(procedure, args, env):
    """Apply PrimitiveProcedure PROCEDURE to a Scheme list of ARGS in ENV.

    >>> env = create_global_frame()
    >>> plus = env.bindings["+"]
    >>> twos = Pair(2, Pair(2, nil))
    >>> apply_primitive(plus, twos, env)
    4
    >>> apply_primitive(plus, Pair(nil, nil), env)
    Traceback (most recent call last):
        ...
    scheme_primitives.SchemeError: operand 0 (()) is not a number
    """
    py_args = pairs_to_list(args)
    if procedure.use_env:
        py_args.append(env)
    try:
        return procedure.fn(*py_args)
    except TypeError:
        raise SchemeError("Cannot apply {0} to {1}".format(str(args), str(procedure)))


################
# Environments #
################

class Frame:
    """An environment frame binds Scheme symbols to Scheme values."""

    def __init__(self, parent):
        """An empty frame with a PARENT frame (that may be None)."""
        self.bindings = {}
        self.parent = parent

    def __repr__(self):
        if self.parent is None:
            return "<Global Frame>"
        else:
            s = sorted('{0}: {1}'.format(k,v) for k,v in self.bindings.items())
            return "<{{{0}}} -> {1}>".format(', '.join(s), repr(self.parent))

    def lookup(self, symbol):
        """Return the value bound to SYMBOL.  Errors if SYMBOL is not found."""
        if symbol in self.bindings:
            return self.bindings[symbol]
        elif self.parent != None:
            return self.parent.lookup(symbol)
        else:
            raise SchemeError("unknown identifier: {0}".format(str(symbol)))

    def set(self, symbol, value):
        if symbol in self.bindings:
            self.bindings[symbol] = value
        elif self.parent != None:
            self.parent.set(symbol, value)
        else:
            raise SchemeError("unknown identifier: {0}".format(str(symbol)))

    def global_frame(self):
        """The global environment at the root of the parent chain."""
        e = self
        while e.parent is not None:
            e = e.parent
        return e

    def make_call_frame(self, formals, vals):
        """Return a new local frame whose parent is SELF, in which the symbols
        in the Scheme formal parameter list FORMALS are bound to the Scheme
        values in the Scheme value list VALS. Raise an error if too many or too
        few arguments are given.

        >>> env = create_global_frame()
        >>> formals, vals = read_line("(a b c)"), read_line("(1 2 3)")
        >>> env.make_call_frame(formals, vals)
        <{a: 1, b: 2, c: 3} -> <Global Frame>>
        >>> formals, vals = read_line("(a b . c)"), read_line("(1 2 3 4 5)")
        >>> env.make_call_frame(formals, vals)
        <{a: 1, b: 2, c: (3 4 5)} -> <Global Frame>>
        """
        if scheme_dottedp(formals):
            frame = Frame(self)
            while True:
                if isinstance(formals, Pair):
                    formal, val = formals.first, vals.first
                    formals, vals = formals.second, vals.second
                    frame.bindings[formal] = val
                else:
                    frame.bindings[formals] = vals
                    return frame
        else:
            if len(formals) != len(vals):
                raise SchemeError('length of formals and values not equal\n' + repr(self))

            frame = Frame(self)
            for formal, val in zip(formals, vals):
                if not scheme_symbolp(formal):
                    raise SchemeError(str(formal) + ' is not a variable')
                frame.bindings[formal] = val
            return frame

    def define(self, sym, val):
        """Define Scheme symbol SYM to have value VAL in SELF."""
        self.bindings[sym] = val

class LambdaProcedure:
    """A procedure defined by a lambda expression or the complex define form."""

    def __init__(self, formals, body, env):
        """A procedure whose formal parameter list is FORMALS (a Scheme list),
        whose body is the single Scheme expression BODY, and whose parent
        environment is the Frame ENV.  A lambda expression containing multiple
        expressions, such as (lambda (x) (display x) (+ x 1)) can be handled by
        using (begin (display x) (+ x 1)) as the body."""
        self.formals = formals
        self.body = body
        self.env = env

    def __str__(self):
        return "(lambda {0} {1})".format(str(self.formals), str(self.body))

    def __repr__(self):
        args = (self.formals, self.body, self.env)
        return "LambdaProcedure({0}, {1}, {2})".format(*(repr(a) for a in args))

class MuProcedure:
    """A procedure defined by a mu expression, which has dynamic scope.
     _________________
    < Scheme is cool! >
     -----------------
            \   ^__^
             \  (oo)\_______
                (__)\       )\/\
                    ||----w |
                    ||     ||
    """

    def __init__(self, formals, body):
        """A procedure whose formal parameter list is FORMALS (a Scheme list),
        whose body is the single Scheme expression BODY.  A mu expression
        containing multiple expressions, such as (mu (x) (display x) (+ x 1))
        can be handled by using (begin (display x) (+ x 1)) as the body."""
        self.formals = formals
        self.body = body

    def __str__(self):
        return "(mu {0} {1})".format(str(self.formals), str(self.body))

    def __repr__(self):
        args = (self.formals, self.body)
        return "MuProcedure({0}, {1})".format(*(repr(a) for a in args))


#################
# Special forms #
#################

def do_lambda_form(vals, env):
    """Evaluate a lambda form with parameters VALS in environment ENV."""
    check_form(vals, 2)
    formals = vals.first
    check_formals(formals)
    vals = vals.second
    if len(vals) > 1:
        return LambdaProcedure(formals, begin(vals), env)
    else:
        return LambdaProcedure(formals, vals[0], env)


def do_mu_form(vals):
    """Evaluate a mu form with parameters VALS."""
    check_form(vals, 2)
    formals = vals[0]
    check_formals(formals)
    vals = vals.second
    if len(vals) > 1:
        return MuProcedure(formals, begin(vals))
    else:
        return MuProcedure(formals, vals[0])

def do_define_form(vals, env):
    """Evaluate a define form with parameters VALS in environment ENV."""
    check_form(vals, 2)
    target = vals[0]
    if scheme_symbolp(target):
        check_form(vals, 2, 2)
        env.define(target, scheme_eval(vals[1], env))
    elif isinstance(target, Pair):
        check_form(vals, 2)
        binding = target.first
        if not scheme_symbolp(binding):
            raise SchemeError(str(binding) + ' not a variable')
        formals = target.second
        check_formals(formals)
        body = vals.second
        if len(body) > 1:
            body = begin(body)
        else:
            body = body[0]
        val = LambdaProcedure(formals, body, env)
        env.define(binding, val)
    else:
        raise SchemeError("bad argument to define")
    return target

def do_quote_form(vals):
    """Evaluate a quote form with parameters VALS."""
    check_form(vals, 1, 1)
    return vals.first

def eval_bindings(bindings, env, recursive=False):
    if not scheme_listp(bindings):
        raise SchemeError("bad bindings list in let form")

    if recursive:
        env = env.make_call_frame(nil, nil)

    first = lambda x: x[0]
    names = bindings.map(first)

    def eval_second(binding):
        if len(binding) == 2:
            name = binding[0]
            evaled = scheme_eval(binding[1], env)
            if recursive:
                env.define(name, evaled)
            return evaled
        else:
            raise SchemeError('too many operands in binding: ' + str(binding))

    values = bindings.map(eval_second)

    return names, values

def do_normal_let(vals, env, recursive=False):
    bindings = vals[0]
    exprs = vals.second

    names, values = eval_bindings(bindings, env, recursive)
    new_env = env.make_call_frame(names, values)

    last = len(exprs) - 1
    for i in range(0, last):
        scheme_eval(exprs[i], new_env)

    return exprs[last], new_env

def do_named_let(vals, env):
    """
    A named let:
    (let fac ((n 10))
      (if (zero? n)
          1
          (* n (fac (- n 1)))))
    is equivalent to:
    (letrec ((fac (lambda (n)
                    (if (zero? n)
                        1
                        (* n (fac (- n 1)))))))
      (fac 10))
    """
    check_form(vals, 3)

    proc_id = vals[0]
    bindings = vals[1]
    exprs = vals.second.second

    names, values = eval_bindings(bindings, env, True)

    new_env = env.make_call_frame(nil, nil)
    inner_lambda = do_lambda_form(Pair(names, exprs), new_env)
    new_env.define(proc_id, inner_lambda)

    return quote(scheme_apply(inner_lambda, values, new_env)), new_env

def do_let_form(vals, env, recursive=False):
    """Evaluate a let form with parameters VALS in environment ENV."""
    check_form(vals, 2)

    if scheme_symbolp(vals[0]):
        return do_named_let(vals, env)
    else:
        return do_normal_let(vals, env, recursive)

def do_set_form(vals, env):
    check_form(vals, 2, 2)
    binding = vals[0]
    value = scheme_eval(vals[1], env)
    env.set(binding, value)

#########################
# Logical Special Forms #
#########################

def do_if_form(vals, env):
    """Evaluate if form with parameters VALS in environment ENV."""
    check_form(vals, 2, 3)
    cond = scheme_eval(vals[0], env)
    if scheme_true(cond):
        return vals[1]
    else:
        if vals.second.second is nil:
            return okay
        else:
            return vals[2]

def do_and_form(vals, env):
    """Evaluate short-circuited and with parameters VALS in environment ENV."""
    if len(vals) == 0:
        return True
    for i in range(len(vals)):
        val = vals[i]
        if i == len(vals) - 1:
            return val
        if scheme_false(scheme_eval(val, env)):
            return False

def quote(value):
    """Return a Scheme expression quoting the Scheme VALUE.

    >>> s = quote('hello')
    >>> print(s)
    (quote hello)
    >>> scheme_eval(s, Frame(None))  # "hello" is undefined in this frame.
    'hello'
    """
    return Pair("quote", Pair(value, nil))

def do_or_form(vals, env):
    """Evaluate short-circuited or with parameters VALS in environment ENV."""
    num_vals = len(vals)
    if num_vals == 0:
        return False
    for i, val in enumerate(vals):
        if i == num_vals - 1:
            return val
        evaluated = scheme_eval(val, env)
        if scheme_true(evaluated):
            return quote(evaluated)

def do_cond_form(vals, env):
    """Evaluate cond form with parameters VALS in environment ENV."""
    num_clauses = len(vals)
    for i, clause in enumerate(vals):
        check_form(clause, 1)
        last = i == num_clauses - 1
        if clause.first == "else":
            if not last:
                raise SchemeError("else must be last")
            test = True
            if clause.second is nil:
                raise SchemeError("badly formed else clause")
        else:
            test = scheme_eval(clause.first, env)
        if scheme_true(test):
            len_clause = len(clause)
            if last and len_clause == 1:
                    return quote(test)
            if len_clause > 2:
                return begin(clause.second)
            elif len_clause == 2:
                return clause[1]
            else:
                return quote(test)

    return okay

def begin(vals):
    check_form(vals, 1)
    return Pair('begin', vals)

def do_begin_form(vals, env):
    """Evaluate begin form with parameters VALS in environment ENV."""
    check_form(vals, 1)
    vals = list(vals)
    for val in vals[:-1]:
        scheme_eval(val, env)
    return vals[-1]

LOGIC_FORMS = {
        "and": do_and_form,
        "or": do_or_form,
        "if": do_if_form,
        "cond": do_cond_form,
        "begin": do_begin_form,
        }

# Utility methods for checking the structure of Scheme programs

def check_form(expr, min, max = None):
    """Check EXPR (default SELF.expr) is a proper list whose length is
    at least MIN and no more than MAX (default: no maximum). Raises
    a SchemeError if this is not the case."""
    if not scheme_listp(expr):
        raise SchemeError("badly formed expression: " + str(expr))
    length = len(expr)
    if length < min:
        raise SchemeError("too few operands in form")
    elif max is not None and length > max:
        raise SchemeError("too many operands in form")

def check_formals(formals):
    """Check that FORMALS is a valid parameter list, a Scheme list of symbols
    in which each symbol is distinct. Raise a SchemeError if the list of formals
    is not a well-formed list of symbols or if any symbol is repeated.

    >>> check_formals(read_line("(a b c)"))
    >>> check_formals(read_line("(a b c . d)"))
    >>> check_formals(read_line("5"))
    Traceback (most recent call last):
        ...
    scheme_primitives.SchemeError: ('%s not formed properly', 5)
    """
    uniqueValsList = []
    def check_unique(arg):
        if arg in uniqueValsList:
            raise SchemeError("formals need to be unique")
        if scheme_symbolp(arg) == False:
            raise SchemeError("%s not formed properly", formals)
        uniqueValsList.append(arg)

    if scheme_dottedp(formals) and formals is not nil:
        while True:
            if isinstance(formals, Pair):
                arg = formals.first
                formals = formals.second
                check_unique(arg)
            else:
                arg = formals
                check_unique(arg)
                return

    if scheme_listp(formals):
        for arg in formals:
            check_unique(arg)
    elif formals is not nil :
        raise SchemeError("%s not formed properly", formals)


def scheme_eval(expr, env):
    """Evaluate Scheme expression EXPR in environment ENV."""
    while True:
        if expr is None:
            raise SchemeError("Cannot evaluate an undefined expression.")

        # Evaluate Atoms
        if scheme_symbolp(expr):
            return env.lookup(expr)
        elif scheme_atomp(expr) or scheme_stringp(expr) or expr is okay:
            return expr
        elif scheme_vectorp(expr):
            raise SchemeError("cannot eval vector: " + str(expr))

        # All non-atomic expressions are lists.
        if not scheme_listp(expr):
            raise SchemeError("malformed list: {0}".format(str(expr)))
        first, rest = expr.first, expr.second

        # Evaluate Combinations
        if (scheme_symbolp(first) # first might be unhashable
            and first in LOGIC_FORMS):
            expr = LOGIC_FORMS[first](rest, env)
        elif first == "let":
            expr, env = do_let_form(rest, env)
        elif first == "letrec":
            expr, env = do_let_form(rest, env, True)
        elif first == "lambda":
            return do_lambda_form(rest, env)
        elif first == "define":
            return do_define_form(rest, env)
        elif first == "set!":
            return do_set_form(rest, env)
        elif first == "quote":
            return do_quote_form(rest)
        elif first == "mu":
            return do_mu_form(rest)
        else:
            procedure = scheme_eval(first, env)
            args = rest.map(lambda arg: scheme_eval(arg, env))

            if isinstance(procedure, PrimitiveProcedure):
                return apply_primitive(procedure, args, env)
            elif isinstance(procedure, LambdaProcedure):
                frame = procedure.env.make_call_frame(procedure.formals, args)
                expr, env = procedure.body, frame
            elif isinstance(procedure, MuProcedure):
                frame = env.make_call_frame(procedure.formals, args)
                expr, env = procedure.body, frame
            else:
                raise SchemeError("Cannot call {0}".format(str(procedure)))


################
# Input/Output #
################

def read_eval_print_loop(next_line, env, quiet=False, startup=False,
                         interactive=False, load_files=()):
    """Read and evaluate input until an end of file or keyboard interrupt."""
    if startup:
        for filename in load_files:
            scheme_load(filename, True, env)
    while True:
        try:
            src = next_line()
            while src.more_on_line:
                expression = scheme_read(src)
                result = scheme_eval(expression, env)
                if not quiet and result is not None:
                    print(result)
        except (SchemeError, SyntaxError, ValueError, RuntimeError) as err:
            if (isinstance(err, RuntimeError) and
                'maximum recursion depth exceeded' not in err.args[0]):
                raise
            print("Error:", err)
        except KeyboardInterrupt:  # <Control>-C
            if not startup:
                raise
            print("\nKeyboardInterrupt")
            if not interactive:
                return
        except EOFError:  # <Control>-D, etc.
            return


def scheme_load(*args):
    """Load a Scheme source file. ARGS should be of the form (SYM, ENV) or (SYM,
    QUIET, ENV). The file named SYM is loaded in environment ENV, with verbosity
    determined by QUIET (default true)."""
    if not (2 <= len(args) <= 3):
        vals = args[:-1]
        raise SchemeError("wrong number of arguments to load: {0}".format(vals))
    sym = args[0]
    quiet = args[1] if len(args) > 2 else True
    env = args[-1]
    if (scheme_stringp(sym)):
        sym = eval(sym)
    check_type(sym, scheme_symbolp, 0, "load")
    with scheme_open(sym) as infile:
        lines = infile.readlines()
    args = (lines, None) if quiet else (lines,)
    def next_line():
        return buffer_lines(*args)
    read_eval_print_loop(next_line, env.global_frame(), quiet=quiet)
    return okay

def scheme_open(filename):
    """If either FILENAME or FILENAME.scm is the name of a valid file,
    return a Python file opened to it. Otherwise, raise an error."""
    try:
        return open(filename)
    except IOError as exc:
        if filename.endswith('.scm'):
            raise SchemeError(str(exc))
    try:
        return open(filename + '.scm')
    except IOError as exc:
        raise SchemeError(str(exc))

def create_global_frame():
    """Initialize and return a single-frame environment with built-in names."""
    env = Frame(None)
    env.define("eval", PrimitiveProcedure(scheme_eval, True))
    env.define("apply", PrimitiveProcedure(scheme_apply, True))
    env.define("load", PrimitiveProcedure(scheme_load, True))
    add_primitives(env)
    return env

@main
def run(*argv):
    next_line = buffer_input
    interactive = True
    if argv:
        try:
            filename = argv[0]
            if filename == '-load':
                load_files = argv[1:]
            else:
                input_file = open(argv[0])
                lines = input_file.readlines()
                def next_line():
                    return buffer_lines(lines)
                interactive = False
        except IOError as err:
            print(err)
            sys.exit(1)
    read_eval_print_loop(next_line, create_global_frame(), startup=True,
                         interactive=interactive)
    tscheme_exitonclick()
