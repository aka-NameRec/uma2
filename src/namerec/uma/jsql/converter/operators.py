"""Operator mappings for JSQL <-> SQL conversion."""

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLOperator

# Operator mappings for JSQL -> SQL conversion
ARITHMETIC_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.ADD.value: exp.Add,
    JSQLOperator.SUB.value: exp.Sub,
    JSQLOperator.MUL.value: exp.Mul,
    JSQLOperator.DIV.value: exp.Div,
    # Note: || (CONCAT) is handled specially in expressions.py, not here
}

COMPARISON_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.EQ.value: exp.EQ,
    JSQLOperator.NE.value: exp.NEQ,
    JSQLOperator.NEQ_ISO.value: exp.NEQ,  # Maps to same as !=
    JSQLOperator.GT.value: exp.GT,
    JSQLOperator.GE.value: exp.GTE,
    JSQLOperator.LT.value: exp.LT,
    JSQLOperator.LE.value: exp.LTE,
}

LOGICAL_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.AND.value: exp.And,
    JSQLOperator.OR.value: exp.Or,
}

# Reverse mappings for SQL -> JSQL conversion
SQLGLOT_TO_ARITHMETIC: dict[type[exp.Expression], str] = {
    exp.Add: JSQLOperator.ADD.value,
    exp.Sub: JSQLOperator.SUB.value,
    exp.Mul: JSQLOperator.MUL.value,
    exp.Div: JSQLOperator.DIV.value,
}

SQLGLOT_TO_COMPARISON: dict[type[exp.Expression], str] = {
    exp.EQ: JSQLOperator.EQ.value,
    exp.NEQ: JSQLOperator.NE.value,
    exp.GT: JSQLOperator.GT.value,
    exp.GTE: JSQLOperator.GE.value,
    exp.LT: JSQLOperator.LT.value,
    exp.LTE: JSQLOperator.LE.value,
}

# Join type mapping for SQL -> JSQL
SQLGLOT_JOIN_SIDE_TO_TYPE: dict[str, str] = {
    'LEFT': JoinType.LEFT.value,
    'RIGHT': JoinType.RIGHT.value,
    'FULL': JoinType.FULL.value,
}
