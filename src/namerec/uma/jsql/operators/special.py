"""Special operator handlers (IN, EXISTS, BETWEEN, IS NULL, etc.)."""

from sqlalchemy import exists
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.operators.protocol import OperatorHandler
from namerec.uma.jsql.types import JSQLExpression


class SpecialOperatorHandler:
    """Handler for special operators (IN, EXISTS, BETWEEN, IS NULL, etc.)."""
    
    def __init__(self, parser: 'JSQLParser') -> None:
        """
        Initialize special operator handler.
        
        Args:
            parser: JSQLParser instance for building expressions and queries
        """
        self.parser = parser
    
    async def __call__(self, condition_spec: JSQLExpression, operator: JSQLOperator) -> ClauseElement:
        """
        Handle special operator.
        
        Args:
            condition_spec: JSQL condition specification
            operator: Special operator
            
        Returns:
            SQLAlchemy clause element
        """
        if operator == JSQLOperator.IN:
            return await self._handle_in(condition_spec)
        elif operator == JSQLOperator.NOT_IN:
            return await self._handle_not_in(condition_spec)
        elif operator == JSQLOperator.EXISTS:
            return await self._handle_exists(condition_spec)
        elif operator == JSQLOperator.NOT_EXISTS:
            return await self._handle_not_exists(condition_spec)
        elif operator == JSQLOperator.IS_NULL:
            return await self._handle_is_null(condition_spec)
        elif operator == JSQLOperator.IS_NOT_NULL:
            return await self._handle_is_not_null(condition_spec)
        elif operator == JSQLOperator.BETWEEN:
            return await self._handle_between(condition_spec, negate=False)
        elif operator == JSQLOperator.NOT_BETWEEN:
            return await self._handle_between(condition_spec, negate=True)
        else:
            raise JSQLSyntaxError(f'Unknown special operator: {operator.value}')
    
    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self.parser._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self.parser._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self.parser._build_expression(right_spec)
        return left.in_(right)
    
    async def _handle_not_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT_IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT_IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self.parser._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self.parser._build_query(right_spec[JSQLField.SUBQUERY.value])
            return ~left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return ~left.in_(right_spec)

        # Expression
        right = await self.parser._build_expression(right_spec)
        return ~left.in_(right)
    
    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self.parser._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)
    
    async def _handle_not_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT_EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self.parser._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return ~exists(subquery)
    
    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self.parser._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)
    
    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self.parser._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)
    
    async def _handle_between(
        self,
        condition_spec: JSQLExpression,
        negate: bool = False
    ) -> ClauseElement:
        """
        Handle BETWEEN operator (with optional negation).
        
        Args:
            condition_spec: JSQL condition specification
            negate: Whether to negate the result (for NOT BETWEEN)
            
        Returns:
            SQLAlchemy clause element
        """
        operator = JSQLOperator.NOT_BETWEEN if negate else JSQLOperator.BETWEEN
        
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} must have "{JSQLField.EXPR.value}" field')
        if 'low' not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} must have "low" field')
        if 'high' not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} must have "high" field')

        expr = await self.parser._build_expression(condition_spec[JSQLField.EXPR.value])
        expr_type = self.parser._extract_expression_type(expr)
        low = await self.parser._build_expression(condition_spec['low'], expected_type=expr_type)
        high = await self.parser._build_expression(condition_spec['high'], expected_type=expr_type)
        
        result = expr.between(low, high)
        return ~result if negate else result
