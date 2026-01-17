"""JOIN handling for JSQL <-> SQL conversion."""

import logging
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.converter.conditions import convert_condition_to_jsql
from namerec.uma.jsql.converter.conditions import jsql_condition_to_sqlglot
from namerec.uma.jsql.converter.operators import SQLGLOT_JOIN_SIDE_TO_TYPE

logger = logging.getLogger(__name__)


def jsql_join_to_sqlglot(join_spec: dict[str, Any]) -> dict[str, Any]:
    """
    Convert JSQL JOIN to sqlglot join components.

    Raises:
        InvalidExpressionError: If JOIN structure is invalid
        MissingFieldError: If required entity field is missing
    """
    if not isinstance(join_spec, dict):
        logger.debug('Invalid join_spec type, expected dict')
        raise InvalidExpressionError(
            message=f'JOIN spec must be dict, got {type(join_spec).__name__}',
            path='joins',
            expression={'type': type(join_spec).__name__, 'value': repr(join_spec)}
        )

    if 'entity' not in join_spec:
        logger.debug("Missing 'entity' in join_spec")
        raise MissingFieldError('entity', path='joins.entity', context='JOIN')

    entity = join_spec['entity']
    join_type = join_spec.get('type', JoinType.INNER.value).upper()
    logger.debug(f'Processing {join_type} JOIN to {entity}')

    table = exp.table_(entity)
    if alias := join_spec.get('alias'):
        table = exp.alias_(table, alias, table=True)

    on_cond = None
    if on_spec := join_spec.get('on'):
        on_cond = jsql_condition_to_sqlglot(on_spec)

    logger.debug(f'Built JOIN: {join_type} {entity}')
    return {
        'table': table,
        'on': on_cond,
        'type': join_type,
    }


def convert_join_to_jsql(join: exp.Join) -> dict[str, Any]:
    """
    Convert sqlglot JOIN to JSQL join.

    Raises:
        InvalidExpressionError: If JOIN structure is invalid
    """
    if not isinstance(join, exp.Join):
        raise InvalidExpressionError(
            message=f'Expected exp.Join, got {type(join).__name__}',
            path='joins',
            expression={'type': type(join).__name__}
        )

    # Get join type (use constant mapping)
    # Check for CROSS JOIN first (it uses 'kind' attribute, not 'side')
    if join.kind and join.kind.upper() == 'CROSS':
        join_type = JoinType.CROSS.value
    elif join.side:
        join_type = SQLGLOT_JOIN_SIDE_TO_TYPE.get(join.side, JoinType.INNER.value)
    else:
        join_type = JoinType.INNER.value

    # Get joined table
    table = join.this
    if not isinstance(table, exp.Table):
        raise InvalidExpressionError(
            message=f'JOIN table must be exp.Table, got {type(table).__name__}',
            path='joins.this',
            expression={'type': type(table).__name__}
        )

    result: dict[str, Any] = {
        'type': join_type,
        'entity': table.name,
    }

    if table.alias:
        result['alias'] = table.alias

    # Get ON condition (optional, but if present must be valid)
    # Note: sqlglot stores ON condition in join.args['on']
    # The structure: join.args['on'] is typically a wrapper, and the actual condition is in .this
    if join.args.get('on'):
        on_arg = join.args['on']

        # Check if on_arg itself is already a condition (EQ, AND, etc.) or a wrapper
        # If it's a condition type we can handle, use it directly
        if isinstance(on_arg, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE, exp.And, exp.Or, exp.Not, exp.In, exp.Like, exp.Is)):
            on_expr = on_arg
        elif hasattr(on_arg, 'this') and on_arg.this is not None:
            on_expr = on_arg.this
        else:
            on_expr = on_arg

        logger.debug(f'ON condition type: {type(on_expr).__name__}, value: {on_expr}')

        # Convert ON condition - will raise exception if invalid
        # ON condition must be a boolean expression (EQ, AND, OR, etc.), not a simple Column
        on_jsql = convert_condition_to_jsql(on_expr)
        result['on'] = on_jsql

    return result
