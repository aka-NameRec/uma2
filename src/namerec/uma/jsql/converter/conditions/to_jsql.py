"""Convert SQLGlot expressions to JSQL conditions.

This module handles the conversion of SQLGlot expression objects
to JSQL condition specifications. It supports all SQLGlot condition types
and converts them to the appropriate JSQL format.

Supported expression types:
- Logical: And, Or, Not
- Comparison: EQ, NEQ, GT, GTE, LT, LTE
- Special: In, Between, Is (for IS NULL)
- String: Like, ILike, SimilarTo, RegexpLike
- Existence: Exists, NotExists
"""

import logging
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_COMPARISON

logger = logging.getLogger(__name__)


def convert_sqlglot_select_to_jsql(select_expr: exp.Select) -> dict[str, Any]:
    """
    Convert SQLGlot Select expression to JSQL query dictionary.
    
    This is a helper function primarily used for converting subqueries
    in EXISTS and NOT EXISTS conditions. It converts a complete SELECT
    statement to its JSQL representation.
    
    Args:
        select_expr: SQLGlot Select expression object.
    
    Returns:
        JSQL query dictionary containing:
            - 'from': Table name or specification
            - 'select': List of selected fields (optional)
            - 'where': Condition specification (optional)
            - 'joins': List of join specifications (optional)
            - 'order_by': List of order specifications (optional)
            - 'limit': Limit value (optional)
            - 'offset': Offset value (optional)
            - 'group_by': List of group by fields (optional)
            - 'having': Having condition (optional)
    
    Example:
        >>> from sqlglot import parse_one
        >>> sql = "SELECT id FROM users WHERE age > 18"
        >>> select_expr = parse_one(sql)
        >>> jsql = convert_sqlglot_select_to_jsql(select_expr)
        >>> # Returns: {
        ... #     "from": "users",
        ... #     "select": [{"field": "id"}],
        ... #     "where": {"op": ">", "field": "age", "value": 18}
        ... # }
    """
    from namerec.uma.jsql.converter.joins import convert_join_to_jsql
    from namerec.uma.jsql.converter.sql_to_jsql import convert_order_to_jsql
    from namerec.uma.jsql.constants import JSQLField
    
    jsql: dict[str, Any] = {}
    
    # FROM clause
    if from_expr := select_expr.args.get('from_'):
        if isinstance(from_expr, exp.From):
            table = from_expr.this
            if isinstance(table, exp.Table):
                if table.alias:
                    jsql['from'] = {
                        'entity': table.name,
                        'alias': table.alias,
                    }
                else:
                    jsql['from'] = table.name
    
    # SELECT clause
    if select_expr.expressions:
        select_fields = []
        for expr in select_expr.expressions:
            field_dict = convert_expression_to_jsql(expr)
            select_fields.append(field_dict)
        if select_fields:
            jsql['select'] = select_fields
    
    # WHERE clause
    if select_expr.args.get('where'):
        where_expr = select_expr.args['where'].this
        where_jsql = convert_condition_to_jsql(where_expr)
        jsql['where'] = where_jsql
    
    # JOIN clauses
    if select_expr.args.get('joins'):
        joins = []
        for join in select_expr.args['joins']:
            join_dict = convert_join_to_jsql(join)
            joins.append(join_dict)
        if joins:
            jsql['joins'] = joins
    
    # ORDER BY clause
    if select_expr.args.get('order'):
        order_by = []
        for order_expr in select_expr.args['order'].expressions:
            order_dict = convert_order_to_jsql(order_expr)
            order_by.append(order_dict)
        if order_by:
            jsql['order_by'] = order_by
    
    # LIMIT clause
    if select_expr.args.get('limit'):
        limit_expr = select_expr.args['limit'].expression
        if isinstance(limit_expr, exp.Literal):
            jsql['limit'] = int(limit_expr.this)
    
    # OFFSET clause
    if select_expr.args.get('offset'):
        offset_expr = select_expr.args['offset'].expression
        if isinstance(offset_expr, exp.Literal):
            jsql['offset'] = int(offset_expr.this)
    
    # GROUP BY clause
    if select_expr.args.get('group'):
        group_by = []
        for group_expr in select_expr.args['group'].expressions:
            field_dict = convert_expression_to_jsql(group_expr)
            group_by.append(field_dict)
        if group_by:
            jsql['group_by'] = group_by
    
    # HAVING clause
    if select_expr.args.get('having'):
        having_expr = select_expr.args['having'].this
        having_jsql = convert_condition_to_jsql(having_expr)
        jsql['having'] = having_jsql
    
    return jsql


def convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """
    Convert SQLGlot condition expression to JSQL condition specification.

    This is the main entry point for converting SQLGlot expressions to JSQL.
    It handles all supported condition types and converts them to the
    appropriate JSQL format.

    Args:
        expr: SQLGlot expression object (e.g., exp.And, exp.EQ, exp.Like, etc.)

    Returns:
        JSQL condition specification dictionary with 'op' field and operator-specific fields.

    Raises:
        InvalidExpressionError: If condition structure is invalid (e.g., field references
            are required but not present).
        UnknownOperatorError: If comparison operator is not supported.
        UnsupportedOperationError: If condition type is not supported in JSQL.

    Example:
        >>> from sqlglot import parse_one
        >>> sql = "age >= 18 AND age < 65"
        >>> expr = parse_one(sql).args['where']
        >>> jsql = convert_condition_to_jsql(expr)
        >>> # Returns: {
        ... #     "op": "AND",
        ... #     "conditions": [
        ... #         {"op": ">=", "field": "age", "value": 18},
        ... #         {"op": "<", "field": "age", "value": 65}
        ... #     ]
        ... # }
    """
    # Handle AND/OR
    if isinstance(expr, exp.And):
        return {
            'op': JSQLOperator.AND.value,
            'conditions': [
                convert_condition_to_jsql(expr.left),
                convert_condition_to_jsql(expr.right),
            ],
        }

    if isinstance(expr, exp.Or):
        return {
            'op': JSQLOperator.OR.value,
            'conditions': [
                convert_condition_to_jsql(expr.left),
                convert_condition_to_jsql(expr.right),
            ],
        }

    # Handle ILIKE (case-insensitive LIKE) - this is a real sqlglot class
    if isinstance(expr, exp.ILike):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='ILIKE operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='ILIKE operator right side must be a string literal',
                path='right',
                expression=right
            )

        return {
            'field': left['field'],
            'op': JSQLOperator.ILIKE.value,
            'pattern': right['value'],
        }

    # Note: NOT ILIKE is handled below as exp.Not(exp.ILike(...))
    # Similar to NOT IN, NOT LIKE, etc.

    if isinstance(expr, exp.SimilarTo):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='SIMILAR TO operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='SIMILAR TO operator right side must be a string literal',
                path='right',
                expression=right
            )

        return {
            'field': left['field'],
            'op': JSQLOperator.SIMILAR_TO.value,
            'pattern': right['value'],
        }

    if isinstance(expr, exp.RegexpLike):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='REGEXP/RLIKE operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='REGEXP/RLIKE operator right side must be a string literal',
                path='right',
                expression=right
            )

        # Use REGEXP as default (RLIKE is MySQL alias)
        return {
            'field': left['field'],
            'op': JSQLOperator.REGEXP.value,
            'pattern': right['value'],
        }

    # Note: NOT BETWEEN is handled below as exp.Not(exp.Between(...))
    # Similar to NOT IN, NOT LIKE, etc.

    # Handle EXISTS (explicit, not wrapped)
    # Note: NOT EXISTS is handled below as exp.Not(exp.Exists(...))
    if isinstance(expr, exp.Exists):
        subquery_expr = expr.this
        return {
            'op': JSQLOperator.EXISTS.value,
            'subquery': convert_sqlglot_select_to_jsql(subquery_expr),
        }

    # Handle NOT - check for wrapped patterns
    if isinstance(expr, exp.Not):
        inner = expr.this
        
        # Handle NOT(IN(...)) pattern - convert to NOT IN
        if isinstance(inner, exp.In):
            left = convert_expression_to_jsql(inner.this)
            if 'field' not in left:
                raise InvalidExpressionError(
                    message='NOT IN operator left side must be a field reference',
                    path='left',
                    expression=left
                )
            
            values = []
            if isinstance(inner.expressions, list):
                for val_expr in inner.expressions:
                    val = convert_expression_to_jsql(val_expr)
                    if 'value' not in val:
                        raise InvalidExpressionError(
                            message='NOT IN operator values must be literals',
                            path='values',
                            expression=val
                        )
                    values.append(val['value'])
            
            return {
                'field': left['field'],
                'op': JSQLOperator.NOT_IN.value,
                'values': values,
            }
        
        # Handle NOT(LIKE(...)) pattern - convert to NOT LIKE
        if isinstance(inner, exp.Like):
            left = convert_expression_to_jsql(inner.this)
            right = convert_expression_to_jsql(inner.expression)
            
            if 'field' not in left:
                raise InvalidExpressionError(
                    message='NOT LIKE operator left side must be a field reference',
                    path='left',
                    expression=left
                )
            
            if 'value' not in right:
                raise InvalidExpressionError(
                    message='NOT LIKE operator right side must be a string literal',
                    path='right',
                    expression=right
                )
            
            return {
                'field': left['field'],
                'op': JSQLOperator.NOT_LIKE.value,
                'pattern': right['value'],
            }
        
        # Handle NOT(ILIKE(...)) pattern - convert to NOT ILIKE
        if isinstance(inner, exp.ILike):
            left = convert_expression_to_jsql(inner.this)
            right = convert_expression_to_jsql(inner.expression)
            
            if 'field' not in left:
                raise InvalidExpressionError(
                    message='NOT ILIKE operator left side must be a field reference',
                    path='left',
                    expression=left
                )
            
            if 'value' not in right:
                raise InvalidExpressionError(
                    message='NOT ILIKE operator right side must be a string literal',
                    path='right',
                    expression=right
                )
            
            return {
                'field': left['field'],
                'op': JSQLOperator.NOT_ILIKE.value,
                'pattern': right['value'],
            }
        
            # Handle NOT(BETWEEN(...)) pattern - convert to NOT BETWEEN
            if isinstance(inner, exp.Between):
                expr_jsql = convert_expression_to_jsql(inner.this)
                low_jsql = convert_expression_to_jsql(inner.args.get('low'))
                high_jsql = convert_expression_to_jsql(inner.args.get('high'))
            
            result = {
                'op': JSQLOperator.NOT_BETWEEN.value,
            }

            # Use 'expr' field (can be field or expression)
            if 'field' in expr_jsql:
                result['expr'] = {'field': expr_jsql['field']}
            elif 'value' in expr_jsql:
                result['expr'] = {'value': expr_jsql['value']}
            else:
                result['expr'] = expr_jsql
            
            if 'value' in low_jsql:
                result['low'] = {'value': low_jsql['value']}
            elif 'field' in low_jsql:
                result['low'] = {'field': low_jsql['field']}
            else:
                result['low'] = low_jsql
            
            if 'value' in high_jsql:
                result['high'] = {'value': high_jsql['value']}
            elif 'field' in high_jsql:
                result['high'] = {'field': high_jsql['field']}
            else:
                result['high'] = high_jsql
            
            return result
        
        # Handle NOT(EXISTS(...)) pattern - convert to NOT EXISTS
        if isinstance(inner, exp.Exists):
            # Extract subquery from EXISTS
            # Note: sqlglot's Exists has 'this' containing the subquery Select
            subquery_expr = inner.this
            # Convert sqlglot Select back to JSQL - this is complex, 
            # but we'll handle it similar to how we handle regular queries
            # For now, return NOT EXISTS with nested structure
            return {
                'op': JSQLOperator.NOT_EXISTS.value,
                'subquery': convert_sqlglot_select_to_jsql(subquery_expr),
            }
        
        # Generic NOT - fall back to NOT wrapper
        return {
            'op': JSQLOperator.NOT.value,
            'condition': convert_condition_to_jsql(expr.this),
        }

    # Handle comparison operators (use reverse mapping)
    if isinstance(expr, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE)):
        op = SQLGLOT_TO_COMPARISON.get(type(expr))
        if not op:
            raise UnknownOperatorError(
                operator=type(expr).__name__,
                path='',
                supported=list(SQLGLOT_TO_COMPARISON.values())
            )

        left = convert_expression_to_jsql(expr.left)
        right = convert_expression_to_jsql(expr.right)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='Comparison operator left side must be a field reference',
                path='left',
                expression=left
            )

        result: dict[str, Any] = {
            'field': left['field'],
            'op': op,
        }

        if 'value' in right:
            result['value'] = right['value']
        elif 'field' in right:
            result['right_field'] = right['field']
        elif 'from' in right and 'select' in right:
            # Right side is a subquery
            result['right'] = right
        else:
            raise InvalidExpressionError(
                message='Comparison operator right side must be a value, field reference, or subquery',
                path='right',
                expression=right
            )

        return result

    # Handle IN
    if isinstance(expr, exp.In):
        left = convert_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='IN operator left side must be a field reference',
                path='left',
                expression=left
            )

        # Check if IN has a subquery (stored in args['query'])
        if expr.args.get('query'):
            query_expr = expr.args['query']
            if isinstance(query_expr, exp.Subquery):
                # IN with subquery
                from namerec.uma.jsql.converter.conditions.to_jsql import convert_sqlglot_select_to_jsql
                # query_expr.this might be another Subquery (nested) or Select
                select_expr = query_expr.this
                if isinstance(select_expr, exp.Subquery):
                    # Unwrap nested subquery
                    select_expr = select_expr.this
                if isinstance(select_expr, exp.Select):
                    subquery_jsql = convert_sqlglot_select_to_jsql(select_expr)
                    return {
                        'field': left['field'],
                        'op': JSQLOperator.IN.value,
                        'right': subquery_jsql,
                    }
        
        # Also check expressions list for subquery (fallback)
        if isinstance(expr.expressions, list) and len(expr.expressions) == 1:
            first_expr = expr.expressions[0]
            if isinstance(first_expr, exp.Subquery):
                # IN with subquery
                from namerec.uma.jsql.converter.conditions.to_jsql import convert_sqlglot_select_to_jsql
                subquery_jsql = convert_sqlglot_select_to_jsql(first_expr.this)
                return {
                    'field': left['field'],
                    'op': JSQLOperator.IN.value,
                    'right': subquery_jsql,
                }

        # IN with value list
        values = []
        if isinstance(expr.expressions, list):
            for val_expr in expr.expressions:
                val = convert_expression_to_jsql(val_expr)
                if 'value' not in val:
                    raise InvalidExpressionError(
                        message='IN operator values must be literals',
                        path='values',
                        expression=val
                    )
                values.append(val['value'])

        return {
            'field': left['field'],
            'op': JSQLOperator.IN.value,
            'values': values,
        }

    # Handle BETWEEN
    if isinstance(expr, exp.Between):
        expr_jsql = convert_expression_to_jsql(expr.this)
        low_jsql = convert_expression_to_jsql(expr.args.get('low'))
        high_jsql = convert_expression_to_jsql(expr.args.get('high'))

        result = {
            'op': JSQLOperator.BETWEEN.value,
        }

        # Use 'expr' field (can be field or expression)
        if 'field' in expr_jsql:
            result['expr'] = {'field': expr_jsql['field']}
        elif 'value' in expr_jsql:
            result['expr'] = {'value': expr_jsql['value']}
        else:
            result['expr'] = expr_jsql

        if 'value' in low_jsql:
            result['low'] = {'value': low_jsql['value']}
        elif 'field' in low_jsql:
            result['low'] = {'field': low_jsql['field']}
        else:
            result['low'] = low_jsql

        if 'value' in high_jsql:
            result['high'] = {'value': high_jsql['value']}
        elif 'field' in high_jsql:
            result['high'] = {'field': high_jsql['field']}
        else:
            result['high'] = high_jsql

        return result

    # Handle LIKE
    if isinstance(expr, exp.Like):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='LIKE operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='LIKE operator right side must be a string literal',
                path='right',
                expression=right
            )

        return {
            'field': left['field'],
            'op': JSQLOperator.LIKE.value,
            'pattern': right['value'],
        }

    # Handle IS NULL / IS NOT NULL
    if isinstance(expr, exp.Is):
        left = convert_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='IS NULL operator left side must be a field reference',
                path='left',
                expression=left
            )

        if isinstance(expr.expression, exp.Null):
            return {
                'field': left['field'],
                'op': JSQLOperator.IS_NULL.value,
            }

        # IS NOT NULL is handled as Not(Is(...)) in sqlglot
        raise InvalidExpressionError(
            message='IS expression must be IS NULL',
            path='expression',
            expression={'type': type(expr.expression).__name__}
        )

    # Default: unsupported condition type
    raise UnsupportedOperationError(
        operation=f'Condition type: {type(expr).__name__}',
        reason='This condition type is not supported in JSQL conversion',
        path=''
    )
