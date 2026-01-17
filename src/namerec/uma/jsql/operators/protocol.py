"""Protocol definition for operator handlers."""

from typing import Protocol

from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.types import JSQLExpression


class OperatorHandler(Protocol):
    """
    Protocol for operator handlers.
    
    Allows structural subtyping - any callable or class implementing
    this interface can be used as an operator handler.
    
    Note: Actual implementations may have different signatures:
    - ComparisonOperatorHandler: (condition_spec, operator) -> ClauseElement
    - LogicalOperatorHandler: (condition_spec, operator) -> ClauseElement
    - SpecialOperatorHandler: (condition_spec, operator) -> ClauseElement
    - StringOperatorHandler: (condition_spec, operator, negate=False) -> ClauseElement
    """
    
    async def __call__(
        self,
        condition_spec: JSQLExpression,
        operator: JSQLOperator,
        **kwargs: object
    ) -> ClauseElement:
        """
        Handle operator condition.
        
        Args:
            condition_spec: JSQL condition specification
            operator: Operator to handle
            **kwargs: Additional operator-specific arguments (e.g., negate for StringOperatorHandler)
            
        Returns:
            SQLAlchemy clause element
        """
        ...
