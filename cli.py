#!/usr/bin/env python3

import subprocess
import sys
import rich_click as click

EXECUTABLE_PATH = "/usr/local/bin/mycompiler_lib/mcompiler"
EXE_DEFAULT = "a.out"


class fmt(click.RichCommand):
    def format_usage(self, ctx, formatter):
        formatter.write_usage(
            prog=ctx.command_path,
            args="SOURCE [OPTIONS]",
            prefix="Usage: ",
        )


@click.command(cls=fmt)
@click.argument("source", type=click.Path(exists=True), required=False)
@click.option(
    "--tokens",
    type=click.Path(),
    is_flag=False,
    flag_value="",
    default=None,
    help="Output tokens to file",
)
@click.option(
    "--ast",
    type=click.Path(),
    is_flag=False,
    flag_value="",
    default=None,
    help="Output AST to file",
)
@click.option(
    "--symtab",
    type=click.Path(),
    is_flag=False,
    flag_value="",
    default=None,
    help="Output symbol table to file",
)
@click.option(
    "--ir",
    type=click.Path(),
    is_flag=False,
    flag_value="",
    default=None,
    help="Output IR to file",
)
@click.option(
    "--asm",
    type=click.Path(),
    is_flag=False,
    flag_value="",
    default=None,
    help="Output assembly to file",
)
@click.option(
    "--exe",
    type=click.Path(),
    is_flag=False,
    flag_value=EXE_DEFAULT,
    default=None,
    help="Output executable to file",
)
@click.option(
    "--json",
    type=click.Path(),
    is_flag=False,
    flag_value="",
    default=None,
    help="Export LSP data (symbols, AST, diagnostics) to JSON file",
)
@click.option(
    "--mute",
    is_flag=True,
    default=False,
    help="Suppress CLI output messages.",
)
@click.option(
    "--repl",
    is_flag=True,
    default=False,
    help="Start interactive REPL mode.",
)

def main(source, tokens, ast, symtab, ir, asm, exe, json, mute, repl):
    if repl:
        args = [EXECUTABLE_PATH, "--repl"]
        try:
            if not mute:
                click.secho("Starting REPL mode...", fg="cyan")
            subprocess.run(args, check=True)
        except subprocess.CalledProcessError as e:
            click.secho(
                f"REPL failed with return code {e.returncode}.", fg="red", err=True
            )
            sys.exit(e.returncode)
        return

    # Check if source is provided (required for non-REPL mode)
    if source is None:
        click.secho("Error: Source file is required when not using REPL mode.", fg="red", err=True)
        sys.exit(1)

    options = {
        "--tokens": tokens,
        "--ast": ast,
        "--symtab": symtab,
        "--ir": ir,
        "--asm": asm,
        "--exe": exe,
        "--json": json,
    }

    args = [EXECUTABLE_PATH, source]

    for flag, output in options.items():
        if output:
            args.extend([flag, output])
        elif output is not None:
            args.append(flag)

    if len(args) == 2:
        args.extend(["--exe", EXE_DEFAULT])

    try:
        if not mute:
            click.secho(f"Compiling '{' '.join(args)}'...", fg="cyan")
        subprocess.run(args, check=True)
        if not mute:
            click.secho("Compilation succeeded.", fg="green")
    except subprocess.CalledProcessError as e:
        click.secho(
            f"Compilation failed with return code {e.returncode}.", fg="red", err=True
        )
        sys.exit(e.returncode)


if __name__ == "__main__":
    main()
