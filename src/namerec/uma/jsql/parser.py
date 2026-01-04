"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import case
from sqlalchemy import cast
from sqlalchemy import exists
from sqlalchemy import false
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy import true
from sqlalchemy import type_coerce
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql import Join
from sqlalchemy.sql.expression import ClauseElement
from sqlalchemy.sql.expression import over

from namerec.uma.core.context import UMAContext
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if 'with' in jsql:
            await self._parse_ctes(jsql['with'])

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'with[{i}]'

            if 'name' not in cte_def:
                raise JSQLSyntaxError('CTE must have "name" field', path)

            if 'query' not in cte_def:
                raise JSQLSyntaxError('CTE must have "query" field', path)

            cte_name = cte_def['name']
            cte_query = await self._build_query(cte_def['query'])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if 'from' not in jsql:
            raise JSQLSyntaxError('Query must have "from" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql['from'])

        # Process JOINs first to register all table aliases
        if 'joins' in jsql:
            from_clause = await self._process_joins_for_aliases(jsql['joins'], from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(jsql.get('select', ['*']), from_clause)

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if 'where' in jsql:
            where_clause = await self._build_condition(jsql['where'])
            query = query.where(where_clause)

        # Add GROUP BY
        if 'group_by' in jsql:
            group_by_clauses = await self._build_group_by(jsql['group_by'])
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if 'having' in jsql:
            having_clause = await self._build_condition(jsql['having'])
            query = query.having(having_clause)

        # Add ORDER BY
        if 'order_by' in jsql:
            order_by_clauses = await self._build_order_by(jsql['order_by'])
            query = query.order_by(*order_by_clauses)

        # Add LIMIT
        if 'limit' in jsql:
            query = query.limit(jsql['limit'])

        # Add OFFSET
        if 'offset' in jsql:
            query = query.offset(jsql['offset'])

        return query

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                # Also register in table_aliases for column resolution
                self.table_aliases[from_spec] = self.ctes[from_spec]
                return self.ctes[from_spec]

            # Regular table
            entity_name = parse_entity_name(from_spec)
            if entity_name.entity not in self.context.metadata.tables:
                raise JSQLSyntaxError(f'Table "{from_spec}" not found')

            table = self.context.metadata.tables[entity_name.entity]
            # Register both by entity name and by from_spec (they might differ with namespace)
            self.table_aliases[entity_name.entity] = table
            if from_spec != entity_name.entity:
                self.table_aliases[from_spec] = table
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        if not select_spec:
            raise JSQLSyntaxError('SELECT clause cannot be empty')

        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'select[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Field with alias
            if 'field' in item_spec:
                column = await self._resolve_column(item_spec['field'], from_clause)
                if 'alias' in item_spec:
                    alias_name = item_spec['alias']
                    column = column.label(alias_name)
                    # Store alias for ORDER BY resolution
                    self.select_aliases[alias_name] = column
                items.append(column)
                continue

            # Function call
            if 'func' in item_spec:
                func_expr = await self._build_function(item_spec, path)
                if 'alias' in item_spec:
                    alias_name = item_spec['alias']
                    func_expr = func_expr.label(alias_name)
                    # Store alias for ORDER BY resolution
                    self.select_aliases[alias_name] = func_expr
                items.append(func_expr)
                continue

            # Expression
            if 'expr' in item_spec:
                expr = await self._build_expression(item_spec['expr'])
                if 'alias' in item_spec:
                    alias_name = item_spec['alias']
                    expr = expr.label(alias_name)
                    # Store alias for ORDER BY resolution
                    self.select_aliases[alias_name] = expr
                items.append(expr)
                continue

            # Window function
            if 'window_func' in item_spec:
                window_expr = await self._build_window_function(item_spec['window_func'], path)
                if 'alias' in item_spec:
                    alias_name = item_spec['alias']
                    window_expr = window_expr.label(alias_name)
                    # Store alias for ORDER BY resolution
                    self.select_aliases[alias_name] = window_expr
                items.append(window_expr)
                continue

            # Literal value
            if 'value' in item_spec:
                value_expr = literal(item_spec['value'])
                if 'alias' in item_spec:
                    value_expr = value_expr.label(item_spec['alias'])
                items.append(value_expr)
                continue

            raise JSQLSyntaxError('SELECT item must have one of: field, func, expr, window_func, value', path)

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables
            if table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'joins[{i}]'

            if 'table' not in join_spec:
                raise JSQLSyntaxError('JOIN must have "table" field', path)

            if 'on' not in join_spec:
                raise JSQLSyntaxError('JOIN must have "on" field', path)

            # Get join type (default INNER)
            join_type = join_spec.get('type', 'INNER').upper()
            if join_type not in ('INNER', 'LEFT', 'RIGHT', 'FULL', 'CROSS'):
                raise JSQLSyntaxError(f'Invalid join type: {join_type}', path)

            # Get table
            table_name = join_spec['table']

            # Check if it's a CTE
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            alias_name = join_spec.get('alias', table_name)
            if 'alias' in join_spec:
                table = table.alias(alias_name)

            # Register alias
            self.table_aliases[alias_name] = table

            # Build ON condition
            on_condition = await self._build_condition(join_spec['on'])

            # Apply join
            is_outer = join_type in ('LEFT', 'RIGHT', 'FULL')
            current_from = current_from.join(table, on_condition, isouter=is_outer)

        return current_from

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if 'op' not in condition_spec:
            raise JSQLSyntaxError('Condition must have "op" field')

        op = condition_spec['op'].upper()

        # Logical operators
        if op == 'AND':
            if 'conditions' not in condition_spec:
                raise JSQLSyntaxError('AND must have "conditions" field')
            conditions = [await self._build_condition(c) for c in condition_spec['conditions']]
            return and_(*conditions)

        if op == 'OR':
            if 'conditions' not in condition_spec:
                raise JSQLSyntaxError('OR must have "conditions" field')
            conditions = [await self._build_condition(c) for c in condition_spec['conditions']]
            return or_(*conditions)

        if op == 'NOT':
            if 'condition' not in condition_spec:
                raise JSQLSyntaxError('NOT must have "condition" field')
            condition = await self._build_condition(condition_spec['condition'])
            return not_(condition)

        # Comparison operators
        if op in ('=', '!=', '<', '<=', '>', '>='):
            left = await self._build_expression(condition_spec['left'])
            right = await self._build_expression(condition_spec['right'])

            return {
                '=': left == right,
                '!=': left != right,
                '<': left < right,
                '<=': left <= right,
                '>': left > right,
                '>=': left >= right,
            }[op]

        # IN operator
        if op == 'IN':
            left = await self._build_expression(condition_spec['left'])
            right_spec = condition_spec['right']

            # Subquery
            if isinstance(right_spec, dict) and 'subquery' in right_spec:
                subquery = await self._build_query(right_spec['subquery'])
                return left.in_(subquery)

            # List of values
            if isinstance(right_spec, list):
                return left.in_(right_spec)

            # Expression
            right = await self._build_expression(right_spec)
            return left.in_(right)

        # EXISTS operator
        if op == 'EXISTS':
            if 'subquery' not in condition_spec:
                raise JSQLSyntaxError('EXISTS must have "subquery" field')
            subquery = await self._build_query(condition_spec['subquery'])
            return exists(subquery)

        # IS NULL / IS NOT NULL
        if op == 'IS NULL':
            expr = await self._build_expression(condition_spec['expr'])
            return expr.is_(None)

        if op == 'IS NOT NULL':
            expr = await self._build_expression(condition_spec['expr'])
            return expr.isnot(None)

        # LIKE operator
        if op == 'LIKE':
            left = await self._build_expression(condition_spec['left'])
            right = await self._build_expression(condition_spec['right'])
            return left.like(right)

        raise JSQLSyntaxError(f'Unknown operator: {op}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if 'field' in expr_spec:
            # Use dummy from_clause since we have table_aliases
            return await self._resolve_column(expr_spec['field'], None)

        # Literal value
        if 'value' in expr_spec:
            return literal(expr_spec['value'])

        # Parameter
        if 'param' in expr_spec:
            param_name = expr_spec['param']
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if 'func' in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if 'op' in expr_spec:
            op = expr_spec['op'].upper()

            # Binary operators
            if op in ('+', '-', '*', '/', '%'):
                left = await self._build_expression(expr_spec['left'])
                right = await self._build_expression(expr_spec['right'])

                return {
                    '+': left + right,
                    '-': left - right,
                    '*': left * right,
                    '/': left / right,
                    '%': left % right,
                }[op]

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if 'subquery' in expr_spec:
            return await self._build_query(expr_spec['subquery'])

        raise JSQLSyntaxError('Invalid expression: must have field, value, param, func, or op')

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if 'func' not in func_spec:
            raise JSQLSyntaxError('Function must have "func" field', path)

        func_name = func_spec['func'].upper()
        args_spec = func_spec.get('args', [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function
        sql_func = getattr(func, func_name.lower(), None)
        if sql_func is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get('distinct', False):
            if args:
                args[0] = args[0].distinct()

        # Call function
        result = sql_func(*args)

        return result

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if 'func' not in window_spec:
            raise JSQLSyntaxError('Window function must have "func" field', path)

        func_name = window_spec['func'].upper()
        args_spec = window_spec.get('args', [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function
        sql_func = getattr(func, func_name.lower(), None)
        if sql_func is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause
        partition_by = None
        if 'partition_by' in window_spec:
            partition_by = []
            for part in window_spec['partition_by']:
                if isinstance(part, dict) and 'field' in part:
                    partition_by.append(await self._resolve_column(part['field'], None))
                else:
                    raise JSQLSyntaxError('partition_by must contain field references', path)

        order_by = None
        if 'order_by' in window_spec:
            order_by = await self._build_order_by(window_spec['order_by'])

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and 'field' in item:
                result.append(await self._resolve_column(item['field'], None))
            else:
                raise JSQLSyntaxError('GROUP BY item must be field name or {field: ...}')

        return result

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                # Check if it's a SELECT alias first
                if item in self.select_aliases:
                    column = self.select_aliases[item]
                else:
                    column = await self._resolve_column(item, None)
                result.append(column.asc())
            elif isinstance(item, dict):
                if 'field' not in item:
                    raise JSQLSyntaxError('ORDER BY item must have "field"')

                field_name = item['field']
                # Check if it's a SELECT alias first
                if field_name in self.select_aliases:
                    column = self.select_aliases[field_name]
                else:
                    column = await self._resolve_column(field_name, None)

                direction = item.get('direction', 'asc').lower()

                if direction == 'asc':
                    result.append(column.asc())
                elif direction == 'desc':
                    result.append(column.desc())
                else:
                    raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError('ORDER BY item must be field name or {field: ..., direction: ...}')

        return result
