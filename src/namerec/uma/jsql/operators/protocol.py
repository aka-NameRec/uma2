"""Protocol definition for operator handlers."""

from typing import Protocol

from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.jsql.types import JSQLExpression


class OperatorHandler(Protocol):
    """
    Protocol for operator handlers.
    
    Allows structural subtyping - any callable or class implementing
    this interface can be used as an operator handler.
    """
    
    async def __call__(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Handle operator condition.
        
        Args:
            condition_spec: JSQL condition specification
            
        Returns:
            SQLAlchemy clause element
        """
        ...
