"""Custom exceptions for JSQL conversion operations."""

from namerec.uma.jsql.exceptions import JSQLSyntaxError

__all__ = [
    'InvalidExpressionError',
    'JSQLConversionError',
    'MissingFieldError',
    'UnknownOperatorError',
    'UnsupportedOperationError',
]


class JSQLConversionError(JSQLSyntaxError):
    """Base class for JSQL conversion errors."""



class UnknownOperatorError(JSQLConversionError):
    """Raised when an unknown operator is encountered."""

    def __init__(self, operator: str, path: str = '', supported: list[str] | None = None):
        """
        Initialize UnknownOperatorError.
        
        Args:
            operator: The unknown operator that was encountered
            path: Path to the problematic field in JSQL structure
            supported: List of supported operators
        """
        self.operator = operator
        self.supported = supported or []

        message = f"Unknown operator: '{operator}'"
        if self.supported:
            message += f". Supported: {', '.join(self.supported)}"

        super().__init__(message, path)


class InvalidExpressionError(JSQLConversionError):
    """Raised when expression structure is invalid."""

    def __init__(self, message: str, path: str = '', expression: dict | None = None):
        """
        Initialize InvalidExpressionError.
        
        Args:
            message: Description of what makes the expression invalid
            path: Path to the problematic field in JSQL structure
            expression: The invalid expression dictionary
        """
        self.expression = expression
        super().__init__(message, path)


class MissingFieldError(JSQLConversionError):
    """Raised when a required field is missing."""

    def __init__(self, field: str, path: str = '', context: str = ''):
        """
        Initialize MissingFieldError.
        
        Args:
            field: Name of the missing required field
            path: Path where the field was expected
            context: Additional context about why the field is required
        """
        self.field = field
        self.context = context

        message = f"Missing required field: '{field}'"
        if context:
            message += f' ({context})'

        super().__init__(message, path)


class UnsupportedOperationError(JSQLConversionError):
    """Raised when an unsupported operation is requested."""

    def __init__(self, operation: str, reason: str = '', path: str = ''):
        """
        Initialize UnsupportedOperationError.
        
        Args:
            operation: The unsupported operation
            reason: Why this operation is not supported
            path: Path to the problematic field
        """
        self.operation = operation
        self.reason = reason

        message = f"Unsupported operation: '{operation}'"
        if reason:
            message += f'. Reason: {reason}'

        super().__init__(message, path)
