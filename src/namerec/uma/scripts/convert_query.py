#!/usr/bin/env python3
"""Console script to convert between SQL and JSQL with auto-detection."""

import json
import sys
from typing import Annotated

import typer

from namerec.uma.jsql.converter import jsql_to_sql
from namerec.uma.jsql.converter import sql_to_jsql
from namerec.uma.jsql.exceptions import JSQLSyntaxError

app = typer.Typer(help='Convert between SQL and JSQL formats with automatic format detection.')


@app.command()
def convert(
    input_file: Annotated[
        typer.FileText | None,
        typer.Argument(help='Input file (defaults to stdin)'),
    ] = None,
    dialect: Annotated[
        str,
        typer.Option('--dialect', '-d', help='SQL dialect (generic, postgresql, mysql, sqlite, etc.)'),
    ] = 'generic',
    pretty: Annotated[
        bool,
        typer.Option('--pretty/--no-pretty', help='Pretty print output'),
    ] = True,
    output_format: Annotated[
        str | None,
        typer.Option(
            '--format',
            '-f',
            help='Force output format: sql or jsql (auto-detected by default)',
        ),
    ] = None,
) -> None:
    """
    Convert between SQL and JSQL formats.

    The script automatically detects the input format:
    - JSON input → assumed to be JSQL → converts to SQL
    - Text input → assumed to be SQL → converts to JSQL

    You can force the output format using --format option.

    Examples:

        # Auto-detect: JSON input → SQL output
        echo '{"from": "users", "select": [{"field": "id"}]}' | uv run convert-query

        # Auto-detect: SQL input → JSQL output
        echo "SELECT * FROM users" | uv run convert-query

        # Force specific output format
        echo "SELECT id FROM users" | uv run convert-query --format jsql

        # From file with specific dialect
        uv run convert-query input.json --dialect postgresql
    """
    try:
        # Read input
        input_text = input_file.read() if input_file else sys.stdin.read()

        input_text = input_text.strip()

        if not input_text:
            typer.echo('Error: No input provided', err=True)
            raise typer.Exit(1)

        # Auto-detect format or use forced format
        if output_format:
            # User forced output format
            if output_format.lower() == 'sql':
                # Convert JSQL to SQL
                result = _convert_jsql_to_sql(input_text, dialect, pretty)
            elif output_format.lower() == 'jsql':
                # Convert SQL to JSQL
                result = _convert_sql_to_jsql(input_text, dialect, pretty)
            else:
                typer.echo(f'Error: Unknown format "{output_format}". Use "sql" or "jsql".', err=True)
                raise typer.Exit(1)
        else:
            # Auto-detect based on input
            try:
                # Try to parse as JSON first
                json.loads(input_text)
                # It's JSON (JSQL) → convert to SQL
                result = _convert_jsql_to_sql(input_text, dialect, pretty)
            except json.JSONDecodeError:
                # Not JSON, assume it's SQL → convert to JSQL
                result = _convert_sql_to_jsql(input_text, dialect, pretty)

        typer.echo(result)

    except typer.Exit:
        raise
    except Exception as e:
        typer.echo(f'Error: {e}', err=True)
        raise typer.Exit(1)


def _convert_jsql_to_sql(input_text: str, dialect: str, pretty: bool) -> str:
    """Convert JSQL (JSON) to SQL."""
    try:
        jsql = json.loads(input_text)
    except json.JSONDecodeError as e:
        typer.echo(f'Error: Invalid JSON: {e}', err=True)
        raise typer.Exit(1)

    try:
        sql = jsql_to_sql(jsql, dialect=dialect)
    except JSQLSyntaxError as e:
        typer.echo(f'Error: Invalid JSQL: {e!s}', err=True)
        if hasattr(e, 'path') and e.path:
            typer.echo(f'  at path: {e.path}', err=True)
        raise typer.Exit(1)

    if not pretty:
        # Strip formatting for compact output
        return ' '.join(sql.split())

    return sql


def _convert_sql_to_jsql(input_text: str, dialect: str, pretty: bool) -> str:
    """Convert SQL to JSQL (JSON)."""
    try:
        jsql = sql_to_jsql(input_text, dialect=dialect)
    except JSQLSyntaxError as e:
        typer.echo(f'Error: Failed to parse SQL: {e!s}', err=True)
        raise typer.Exit(1)

    if pretty:
        return json.dumps(jsql, indent=2)

    return json.dumps(jsql)


def main() -> None:
    """Entry point for the script."""
    app()


if __name__ == '__main__':
    main()
