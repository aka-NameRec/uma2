"""String operator handlers (LIKE, ILIKE, SIMILAR_TO, REGEXP, RLIKE)."""

from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.operators.protocol import OperatorHandler
from namerec.uma.jsql.types import JSQLExpression


class StringOperatorHandler:
    """Handler for string matching operators."""
    
    def __init__(self, parser: 'JSQLParser') -> None:
        """
        Initialize string operator handler.
        
        Args:
            parser: JSQLParser instance for building expressions
        """
        self.parser = parser
    
    async def __call__(
        self,
        condition_spec: JSQLExpression,
        operator: JSQLOperator,
        negate: bool = False
    ) -> ClauseElement:
        """
        Handle string matching operator.
        
        Args:
            condition_spec: JSQL condition specification
            operator: String operator (LIKE, ILIKE, SIMILAR_TO, REGEXP, RLIKE)
            negate: Whether to negate the result (for NOT operators)
            
        Returns:
            SQLAlchemy clause element
        """
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self.parser._build_expression(condition_spec[JSQLField.LEFT.value])
        left_type = self.parser._extract_expression_type(left)
        right = await self.parser._build_expression(condition_spec[JSQLField.RIGHT.value], expected_type=left_type)

        # Map operator to SQLAlchemy method
        if operator == JSQLOperator.LIKE:
            result = left.like(right)
        elif operator == JSQLOperator.ILIKE:
            result = left.ilike(right)
        elif operator == JSQLOperator.SIMILAR_TO:
            # SQLAlchemy doesn't have built-in SIMILAR TO, use op() method
            result = left.op('SIMILAR TO')(right)
        elif operator == JSQLOperator.REGEXP:
            # SQLAlchemy uses regexp_match or op() method depending on dialect
            result = left.op('REGEXP')(right)
        elif operator == JSQLOperator.RLIKE:
            # RLIKE is MySQL alias for REGEXP
            result = left.op('RLIKE')(right)
        else:
            raise JSQLSyntaxError(f'Unknown string operator: {operator.value}')

        return ~result if negate else result
