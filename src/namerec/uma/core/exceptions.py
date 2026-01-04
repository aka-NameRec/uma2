"""UMA exception hierarchy."""


class UMAError(Exception):
    """Base exception for UMA errors."""

    def __init__(self, message: str, entity_name: str | None = None) -> None:
        """
        Initialize UMA exception.

        Args:
            message: Error message
            entity_name: Optional entity name context
        """
        self.entity_name = entity_name
        super().__init__(message)


class UMAAccessDeniedError(UMAError):
    """Access to entity/operation denied."""

    def __init__(
        self,
        entity_name: str,
        operation: str,
        message: str | None = None,
    ) -> None:
        """
        Initialize access denied error.

        Args:
            entity_name: Entity name
            operation: Operation name
            message: Optional custom message
        """
        self.operation = operation
        msg = message or f'Access denied for {operation} on {entity_name}'
        super().__init__(msg, entity_name)


class UMANotFoundError(UMAError):
    """Entity or record not found."""

    def __init__(self, entity_name: str, message: str | None = None) -> None:
        """
        Initialize not found error.

        Args:
            entity_name: Entity name
            message: Optional custom message
        """
        msg = message or f'Entity or record not found: {entity_name}'
        super().__init__(msg, entity_name)


class UMANotImplementedError(UMAError):
    """Operation not implemented for entity."""

    def __init__(
        self,
        entity_name: str,
        operation: str,
        message: str | None = None,
    ) -> None:
        """
        Initialize not implemented error.

        Args:
            entity_name: Entity name
            operation: Operation name
            message: Optional custom message
        """
        self.operation = operation
        msg = message or f'Operation {operation} not implemented for {entity_name}'
        super().__init__(msg, entity_name)


class UMAValidationError(UMAError):
    """Validation error."""

    def __init__(
        self,
        message: str,
        field_name: str | None = None,
        entity_name: str | None = None,
    ) -> None:
        """
        Initialize validation error.

        Args:
            message: Error message
            field_name: Optional field name
            entity_name: Optional entity name
        """
        self.field_name = field_name
        super().__init__(message, entity_name)
