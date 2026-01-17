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
from namerec.uma.jsql.converter.conditions.helpers import extract_in_operator_values
from namerec.uma.jsql.converter.conditions.helpers import extract_lower_like_pattern
from namerec.uma.jsql.converter.conditions.helpers import normalize_between_jsql_fields
from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_COMPARISON

# Forward imports to avoid circular dependencies
# These are used in convert_sqlglot_select_to_jsql and convert_condition_to_jsql
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from namerec.uma.jsql.converter.joins import convert_join_to_jsql
    from namerec.uma.jsql.converter.sql_to_jsql import convert_order_to_jsql

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
    # Lazy imports to avoid circular dependencies
    # Note: These imports are deferred because joins.py and sql_to_jsql.py
    # may import from this module, creating a circular dependency
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
        
        # Use left/right format (no longer using field/pattern format)
        return {
            'op': JSQLOperator.ILIKE.value,
            'left': left,
            'right': right,
        }

    # Note: NOT ILIKE is handled below as exp.Not(exp.ILike(...))
    # Similar to NOT IN, NOT LIKE, etc.

    if isinstance(expr, exp.SimilarTo):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)
        
        # Use left/right format (no longer using field/pattern format)
        return {
            'op': JSQLOperator.SIMILAR_TO.value,
            'left': left,
            'right': right,
        }

    if isinstance(expr, exp.RegexpLike):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)
        
        # Use left/right format (no longer using field/pattern format)
        # Use REGEXP as default (RLIKE is MySQL alias)
        return {
            'op': JSQLOperator.REGEXP.value,
            'left': left,
            'right': right,
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
            
            # Check if IN has a subquery
            if inner.args.get('query'):
                query_expr = inner.args['query']
                if isinstance(query_expr, exp.Subquery):
                    select_expr = query_expr.this
                    if isinstance(select_expr, exp.Subquery):
                        select_expr = select_expr.this
                    if isinstance(select_expr, exp.Select):
                        subquery_jsql = convert_sqlglot_select_to_jsql(select_expr)
                        return {
                            'op': JSQLOperator.NOT_IN.value,
                            'left': left,
                            'right': subquery_jsql,
                        }
            
            # IN with value list - use left/right format
            values = extract_in_operator_values(inner.expressions, 'NOT IN')
            return {
                'op': JSQLOperator.NOT_IN.value,
                'left': left,
                'right': {'values': values},
            }
        
        # Handle NOT(LIKE(...)) pattern - convert to NOT LIKE or NOT ILIKE
        if isinstance(inner, exp.Like):
            # Check for NOT LOWER(...) LIKE LOWER(...) pattern (SQLite converts NOT ILIKE to this)
            lower_result = extract_lower_like_pattern(inner)
            
            if lower_result:
                left, right = lower_result
                # Use left/right format (no longer using field/pattern format)
                # Convert NOT LOWER(...) LIKE LOWER(...) back to NOT ILIKE
                return {
                    'op': JSQLOperator.NOT_ILIKE.value,
                    'left': left,
                    'right': right,
                }
            
            # Regular NOT LIKE (not LOWER pattern)
            left = convert_expression_to_jsql(inner.this)
            right = convert_expression_to_jsql(inner.expression)
            
            # Use left/right format (no longer using field/pattern format)
            return {
                'op': JSQLOperator.NOT_LIKE.value,
                'left': left,
                'right': right,
            }
        
        # Handle NOT(ILIKE(...)) pattern - convert to NOT ILIKE
        if isinstance(inner, exp.ILike):
            left = convert_expression_to_jsql(inner.this)
            right = convert_expression_to_jsql(inner.expression)
            
            # Use left/right format (no longer using field/pattern format)
            return {
                'op': JSQLOperator.NOT_ILIKE.value,
                'left': left,
                'right': right,
            }
        
        # Handle NOT(BETWEEN(...)) pattern - convert to NOT BETWEEN
        if isinstance(inner, exp.Between):
            expr_jsql = convert_expression_to_jsql(inner.this)
            low_jsql = convert_expression_to_jsql(inner.args.get('low'))
            high_jsql = convert_expression_to_jsql(inner.args.get('high'))
            
            result = normalize_between_jsql_fields(expr_jsql, low_jsql, high_jsql)
            result['op'] = JSQLOperator.NOT_BETWEEN.value
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

        # Validate left side
        if 'field' not in left and 'func' not in left and 'op' not in left and 'from' not in left:
            raise InvalidExpressionError(
                message='Comparison operator left side must be a field reference, function call, expression, or subquery',
                path='left',
                expression=left
            )

        # Validate right side
        if 'value' not in right and 'field' not in right and 'func' not in right and 'from' not in right and 'op' not in right:
            raise InvalidExpressionError(
                message='Comparison operator right side must be a value, field reference, function call, expression, or subquery',
                path='right',
                expression=right
            )

        # Always use left/right format (no longer using field/value format)
        return {
            'op': op,
            'left': left,
            'right': right,
        }

    # Handle IN
    if isinstance(expr, exp.In):
        left = convert_expression_to_jsql(expr.this)

        # Check if IN has a subquery (stored in args['query'])
        if expr.args.get('query'):
            query_expr = expr.args['query']
            if isinstance(query_expr, exp.Subquery):
                # IN with subquery - use left/right format
                # query_expr.this might be another Subquery (nested) or Select
                select_expr = query_expr.this
                if isinstance(select_expr, exp.Subquery):
                    # Unwrap nested subquery
                    select_expr = select_expr.this
                if isinstance(select_expr, exp.Select):
                    subquery_jsql = convert_sqlglot_select_to_jsql(select_expr)
                    return {
                        'op': JSQLOperator.IN.value,
                        'left': left,
                        'right': subquery_jsql,
                    }
        
        # Also check expressions list for subquery (fallback)
        if isinstance(expr.expressions, list) and len(expr.expressions) == 1:
            first_expr = expr.expressions[0]
            if isinstance(first_expr, exp.Subquery):
                # IN with subquery - use left/right format
                subquery_jsql = convert_sqlglot_select_to_jsql(first_expr.this)
                return {
                    'op': JSQLOperator.IN.value,
                    'left': left,
                    'right': subquery_jsql,
                }

        # IN with value list - use left/right format
        values = extract_in_operator_values(expr.expressions, 'IN')

        return {
            'op': JSQLOperator.IN.value,
            'left': left,
            'right': {'values': values},
        }

    # Handle BETWEEN
    if isinstance(expr, exp.Between):
        expr_jsql = convert_expression_to_jsql(expr.this)
        low_jsql = convert_expression_to_jsql(expr.args.get('low'))
        high_jsql = convert_expression_to_jsql(expr.args.get('high'))
        
        result = normalize_between_jsql_fields(expr_jsql, low_jsql, high_jsql)
        result['op'] = JSQLOperator.BETWEEN.value
        return result

    # Handle LIKE
    if isinstance(expr, exp.Like):
        # Check for LOWER(...) LIKE LOWER(...) pattern (SQLite converts ILIKE to this)
        # This pattern should be converted back to ILIKE
        lower_result = extract_lower_like_pattern(expr)
        
        if lower_result:
            left, right = lower_result
            # Use left/right format (no longer using field/pattern format)
            # Convert LOWER(...) LIKE LOWER(...) back to ILIKE
            return {
                'op': JSQLOperator.ILIKE.value,
                'left': left,
                'right': right,
            }
        
        # Regular LIKE (not LOWER pattern)
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)
        
        # Use left/right format (no longer using field/pattern format)
        return {
            'op': JSQLOperator.LIKE.value,
            'left': left,
            'right': right,
        }

    # Handle IS NULL / IS NOT NULL
    if isinstance(expr, exp.Is):
        left = convert_expression_to_jsql(expr.this)

        if isinstance(expr.expression, exp.Null):
            # Use left/right format (no longer using field format)
            return {
                'op': JSQLOperator.IS_NULL.value,
                'left': left,
                'right': {'value': None},
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
