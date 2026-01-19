"""Comparison operator handlers (=, !=, <, <=, >, >=)."""

from typing import TYPE_CHECKING

from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression

if TYPE_CHECKING:
    from namerec.uma.jsql.parser import JSQLParser


class ComparisonOperatorHandler:
    """Handler for comparison operators."""

    def __init__(self, parser: 'JSQLParser') -> None:
        """
        Initialize comparison operator handler.

        Args:
            parser: JSQLParser instance for building expressions
        """
        self.parser = parser

    async def __call__(self, condition_spec: JSQLExpression, operator: JSQLOperator) -> ClauseElement:
        """
        Handle comparison operator.

        Args:
            condition_spec: JSQL condition specification
            operator: Comparison operator

        Returns:
            SQLAlchemy clause element
        """
        if operator not in COMPARISON_OPERATORS:
            raise JSQLSyntaxError(f'Unknown comparison operator: {operator.value}')

        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.RIGHT.value}" field')

        left_spec = condition_spec[JSQLField.LEFT.value]
        right_spec = condition_spec[JSQLField.RIGHT.value]

        left_is_literal = JSQLField.VALUE.value in left_spec or JSQLField.PARAM.value in left_spec
        right_is_literal = JSQLField.VALUE.value in right_spec or JSQLField.PARAM.value in right_spec

        if JSQLField.FIELD.value in left_spec and right_is_literal:
            left = await self.parser._build_expression(left_spec)
            expected_type = getattr(left, 'type', None)
            right = await self.parser._build_expression(right_spec, expected_type=expected_type)
        elif JSQLField.FIELD.value in right_spec and left_is_literal:
            right = await self.parser._build_expression(right_spec)
            expected_type = getattr(right, 'type', None)
            left = await self.parser._build_expression(left_spec, expected_type=expected_type)
        else:
            left = await self.parser._build_expression(left_spec)
            right = await self.parser._build_expression(right_spec)

        # Use match-case for operator dispatch
        match operator:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE | JSQLOperator.NEQ_ISO:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {operator.value}')
