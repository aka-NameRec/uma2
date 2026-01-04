"""JSQL-specific exceptions."""


class JSQLSyntaxError(ValueError):
    """Raised when JSQL syntax is invalid."""

    def __init__(self, message: str, path: str | None = None) -> None:
        """
        Initialize JSQLSyntaxError.

        Args:
            message: Error message
            path: Path in JSQL structure where error occurred (e.g., 'where.conditions[0].op')
        """
        self.path = path
        if path:
            super().__init__(f'{message} (at {path})')
        else:
            super().__init__(message)


class JSQLExecutionError(RuntimeError):
    """Raised when JSQL query execution fails."""

    def __init__(self, message: str, query: dict | None = None, original_error: Exception | None = None) -> None:
        """
        Initialize JSQLExecutionError.

        Args:
            message: Error message
            query: Original JSQL query
            original_error: Original exception that caused the error
        """
        self.query = query
        self.original_error = original_error
        super().__init__(message)
