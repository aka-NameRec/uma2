"""Logical operator handlers (AND, OR, NOT)."""

from typing import TYPE_CHECKING

from sqlalchemy import and_
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression

if TYPE_CHECKING:
    from namerec.uma.jsql.parser import JSQLParser


class LogicalOperatorHandler:
    """Handler for logical operators (AND, OR, NOT)."""

    def __init__(self, parser: 'JSQLParser') -> None:
        """
        Initialize logical operator handler.

        Args:
            parser: JSQLParser instance for building conditions
        """
        self.parser = parser

    async def __call__(self, condition_spec: JSQLExpression, operator: JSQLOperator) -> ClauseElement:
        """
        Handle logical operator.

        Args:
            condition_spec: JSQL condition specification
            operator: Logical operator (AND, OR, NOT)

        Returns:
            SQLAlchemy clause element
        """
        if operator == JSQLOperator.AND:
            return await self._handle_and(condition_spec)
        if operator == JSQLOperator.OR:
            return await self._handle_or(condition_spec)
        if operator == JSQLOperator.NOT:
            return await self._handle_not(condition_spec)
        raise JSQLSyntaxError(f'Unknown logical operator: {operator.value}')

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self.parser._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self.parser._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self.parser._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)
