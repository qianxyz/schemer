import sys
import argparse
from collections import ChainMap
from typing import Iterator

# type aliases
Symbol = str              # A Scheme Symbol is implemented as a Python str
Number = int | float      # A Scheme Number is implemented as a Python int or float
Atom   = Symbol | Number  # A Scheme Atom is a Symbol or Number
List   = list             # A Scheme List is implemented as a Python list of Expressions
Exp    = Atom | List      # A Scheme expression is an Atom or List
Env    = ChainMap         # A Scheme environment is a chain of dictionaries


class Procedure:
    "A user-defined Scheme procedure."
    def __init__(self, params: List, body: Exp, env: Env):
        self.params, self.body, self.env = params, body, env
    def __call__(self, *args): 
        bindings = dict(zip(self.params, args))
        local_env = self.env.new_child(bindings)
        return eval(self.body, local_env)


def tokenize(source: str) -> list[str]:
    """Convert a Scheme source program into a list of tokens."""
    return source.replace("(", " ( ").replace(")", " ) ").split()


def parse(source: str) -> Iterator[Exp]:
    """Parse a source string into Scheme expressions."""
    tokens = tokenize(source)
    while tokens:
        yield parse_single_exp_from_tokens(tokens)


def parse_single_exp_from_tokens(tokens: list[str]) -> Exp:
    """Read a single Scheme expression from a list of tokens."""
    if not tokens:
        raise SyntaxError("Unexpected EOF")
    token = tokens.pop(0)
    if token == "(":
        l = []
        while True:
            if not tokens:
                raise SyntaxError("Unexpected EOF")
            if tokens[0] == ")":
                tokens.pop(0)  # pop off ")"
                return l
            l.append(parse_single_exp_from_tokens(tokens))
    elif token == ")":
        raise SyntaxError("Unexpected )")
    else:
        return atom(token)


def atom(token: str) -> Atom:
    """Convert a token into a Scheme Atom."""
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            if token == "#t": return True
            if token == "#f": return False
            return Symbol(token)


def standard_env() -> Env:
    """Create a standard Scheme environment."""

    def _car(x):
        if isinstance(x, List) and x: return x[0]
        raise ValueError("car is defined only for non-empty lists")

    def _cdr(x):
        if isinstance(x, List) and x: return x[1:]
        raise ValueError("cdr is defined only for non-empty lists")

    def _cons(x, y):
        if isinstance(y, List): return [x] + y
        raise ValueError("the second argument to cons must be a list")

    def _is_null(x):
        if isinstance(x, List): return x == []
        raise ValueError("null? is defined only for lists")

    def _is_eq(x, y):
        if isinstance(x, Symbol) and isinstance(y, Symbol): return x == y
        raise ValueError("the arguments to eq? must be non-numeric atoms")

    return Env({
        "car":   _car,
        "cdr":   _cdr,
        "cons":  _cons,
        "null?": _is_null,
        "atom?": lambda x: isinstance(x, Atom),
        "eq?":   _is_eq,
    })


def eval(x: Exp, env: Env) -> Exp:
    """Evaluate an expression in an environment."""
    if isinstance(x, Symbol):
        return env[x]
    elif isinstance(x, Number):
        return x
    op, *args = x
    if op == "quote":
        return args[0]
    elif op == "if":
        test, conseq, alt = args
        # in Scheme only #f is false
        exp = conseq if eval(test, env) is not False else alt
        return eval(exp, env)
    elif op == "define":
        symbol, exp = args
        env[symbol] = eval(exp, env)
    elif op == "lambda":
        params, body = args
        return Procedure(params, body, env)
    else:
        proc = eval(x[0], env)
        args = [eval(arg, env) for arg in x[1:]]
        return proc(*args)


def schemestr(exp: Exp):
    """Convert a Scheme expression to a printable Python string."""
    if isinstance(exp, List):
        return "(" + " ".join(schemestr(x) for x in exp) + ")"
    if exp is True: return "#t"
    if exp is False: return "#f"
    return str(exp)


def read_eval_print(source: str, env: Env):
    """Read source, eval, and print each result."""
    for exp in parse(source):
        val = eval(exp, env)
        if val is not None:
            print(schemestr(val))


def repl(env: Env, prompt="scheme> "):
    while True:
        try:
            read_eval_print(input(prompt), env)
        except EOFError: break
        except KeyboardInterrupt:
            print("\nKeyboardInterrupt")
            continue
        except Exception as e:
            print(f"Error: {e}")


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "file", nargs="?", type=argparse.FileType("r"),
        help="the Scheme source file to load",
    )
    parser.add_argument(
        "-i", "--repl", action="store_true",
        help="drop into REPL after running script",
    )
    return parser.parse_args()


def main():
    global_env = standard_env()

    args = parse_args()
    if args.file is not None:
        read_eval_print(args.file.read(), global_env)
        if args.repl:
            repl(global_env)
    elif sys.stdin.isatty() or args.repl:
        repl(global_env)
    else:
        read_eval_print(sys.stdin.read(), global_env)


if __name__ == "__main__":
    main()
