"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from collections.abc import Mapping
from datetime import date
from datetime import datetime
from datetime import time
from datetime import timezone
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import Table
from sqlalchemy import bindparam
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement
from sqlalchemy.sql.sqltypes import Date
from sqlalchemy.sql.sqltypes import DateTime
from sqlalchemy.sql.sqltypes import TIMESTAMP

from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.alias_manager import AliasManager
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.operators.comparison import ComparisonOperatorHandler
from namerec.uma.jsql.operators.logical import LogicalOperatorHandler
from namerec.uma.jsql.operators.special import SpecialOperatorHandler
from namerec.uma.jsql.operators.string import StringOperatorHandler
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, namespace_configs: Mapping[str, NamespaceConfig]) -> None:
        """
        Initialize JSQL parser.

        Args:
            namespace_configs: Mapping of namespace names to configurations
        """
        self.namespace_configs = namespace_configs
        self.ctes: dict[str, Select] = {}  # CTEs by name (for SQLAlchemy queries)
        self.alias_manager = AliasManager()  # Manages table/column aliases
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases
        self._param_counter = 0  # Counter for generating unique parameter names
        # Map JSQL parameter names to bindparam objects for proper parameter mapping
        self._param_bindparams: dict[str, Any] = {}  # JSQL param name -> bindparam object
        # Cache for resolved columns: (field_spec, table_id) -> Column | ColumnElement
        # Uses table identifier (name/schema) instead of object id for stable caching
        self._column_cache: dict[tuple[str, str | None], Column | ColumnElement] = {}

        # Initialize operator handlers
        self._logical_handler = LogicalOperatorHandler(self)
        self._comparison_handler = ComparisonOperatorHandler(self)
        self._string_handler = StringOperatorHandler(self)
        self._special_handler = SpecialOperatorHandler(self)

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: lambda spec: self._logical_handler(spec, JSQLOperator.AND),
            JSQLOperator.OR: lambda spec: self._logical_handler(spec, JSQLOperator.OR),
            JSQLOperator.NOT: lambda spec: self._logical_handler(spec, JSQLOperator.NOT),
            JSQLOperator.IN: lambda spec: self._special_handler(spec, JSQLOperator.IN),
            JSQLOperator.NOT_IN: lambda spec: self._special_handler(spec, JSQLOperator.NOT_IN),
            JSQLOperator.BETWEEN: lambda spec: self._special_handler(spec, JSQLOperator.BETWEEN),
            JSQLOperator.NOT_BETWEEN: lambda spec: self._special_handler(spec, JSQLOperator.NOT_BETWEEN),
            JSQLOperator.EXISTS: lambda spec: self._special_handler(spec, JSQLOperator.EXISTS),
            JSQLOperator.NOT_EXISTS: lambda spec: self._special_handler(spec, JSQLOperator.NOT_EXISTS),
            JSQLOperator.IS_NULL: lambda spec: self._special_handler(spec, JSQLOperator.IS_NULL),
            JSQLOperator.IS_NOT_NULL: lambda spec: self._special_handler(spec, JSQLOperator.IS_NOT_NULL),
            JSQLOperator.LIKE: lambda spec: self._string_handler(spec, JSQLOperator.LIKE, False),
            JSQLOperator.NOT_LIKE: lambda spec: self._string_handler(spec, JSQLOperator.LIKE, True),
            JSQLOperator.ILIKE: lambda spec: self._string_handler(spec, JSQLOperator.ILIKE, False),
            JSQLOperator.NOT_ILIKE: lambda spec: self._string_handler(spec, JSQLOperator.ILIKE, True),
            JSQLOperator.SIMILAR_TO: lambda spec: self._string_handler(spec, JSQLOperator.SIMILAR_TO, False),
            JSQLOperator.REGEXP: lambda spec: self._string_handler(spec, JSQLOperator.REGEXP, False),
            JSQLOperator.RLIKE: lambda spec: self._string_handler(spec, JSQLOperator.RLIKE, False),
        }

    async def parse(
        self,
        jsql: JSQLQuery,
        params: dict[str, Any] | None = None,
    ) -> tuple[Select, NamespaceConfig, dict[str, str]]:
        """
        Parse JSQL query into SQLAlchemy Select statement and namespace config.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            Tuple of (SQLAlchemy Select statement, NamespaceConfig, param_mapping)
            param_mapping: Dictionary mapping JSQL parameter names to SQL parameter names

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
            ValueError: If namespace not found or no default available
        """
        self.params = params or {}
        # Reset state for each parse
        self._param_counter = 0  # Counter for generating unique parameter names
        self._param_bindparams = {}  # JSQL param name -> bindparam object
        self._column_cache = {}  # Cache for resolved columns
        self.ctes = {}  # CTEs by name (for SQLAlchemy queries)
        self.select_aliases = {}  # SELECT column aliases
        self.alias_manager = AliasManager()  # Reset alias manager for each parse

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        # Determine namespace from parsed query
        namespace = self._determine_namespace_from_parsed_query(jsql)

        # Get namespace config (already validated in get_namespace_config)
        config = self.get_namespace_config(namespace)

        # Build parameter mapping: JSQL param name -> SQL param name (bindparam.key)
        # Since we use key=param_name in bindparam(), the key should match the JSQL param name
        param_mapping = {jsql_name: bindparam_obj.key for jsql_name, bindparam_obj in self._param_bindparams.items()}

        return query, config, param_mapping

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            cte = cte_query.cte(name=cte_name)
            self.ctes[cte_name] = cte

            # Register CTE in AliasManager for column resolution
            self.alias_manager.register_cte(cte_name, cte)

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
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias and index its columns for fast lookup.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        # Delegate to AliasManager
        self.alias_manager.register_table(alias_name, table, *additional_aliases)

    async def _build_from(self, from_spec: str) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name (string)

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid or table not found
        """
        if not isinstance(from_spec, str):
            raise JSQLSyntaxError(
                f'FROM clause must be a string (table name), got {type(from_spec).__name__}. '
                'Subqueries and complex FROM specifications are not yet supported.'
            )

        # Check if it's a CTE
        if from_spec in self.ctes:
            cte = self.ctes[from_spec]
            self._register_table_alias(from_spec, cte)
            return cte

        # Regular table
        entity_name = parse_entity_name(from_spec)

        # Note: Table should be preloaded in executor before parsing
        # This ensures lazy loading happens before synchronous parsing
        table = await self._get_table_async(entity_name)

        # Register both by entity name and by from_spec (they might differ with namespace)
        self._register_table_alias(
            entity_name.entity,
            table,
            *([] if from_spec == entity_name.entity else [from_spec])
        )
        return table

    def get_namespace_config(self, namespace: str) -> NamespaceConfig:
        """
        Get namespace config by name.

        Args:
            namespace: Namespace name

        Returns:
            NamespaceConfig for the namespace

        Raises:
            ValueError: If namespace not found
        """
        if namespace not in self.namespace_configs:
            available = ', '.join(self.namespace_configs.keys())
            raise ValueError(f'Namespace "{namespace}" not found. Available: {available}')

        return self.namespace_configs[namespace]

    def _get_default_namespace(self) -> str | None:
        """
        Get default namespace if single namespace configured.

        Returns:
            Namespace name if single namespace, None otherwise
        """
        if len(self.namespace_configs) == 1:
            return next(iter(self.namespace_configs.keys()))
        return None

    def _determine_namespace_from_parsed_query(self, jsql: dict) -> str:
        """
        Determine namespace from parsed query.

        Namespace comes from:
        - FROM clause entity name
        - Or default namespace if single namespace configured

        Args:
            jsql: JSQL query dictionary

        Returns:
            Namespace name

        Raises:
            ValueError: If namespace cannot be determined
        """
        from_entity = jsql.get('from')
        if not from_entity:
            # No FROM - use default
            default_ns = self._get_default_namespace()
            if default_ns:
                return default_ns
            raise ValueError("JSQL must contain 'from' field")

        entity = parse_entity_name(from_entity if isinstance(from_entity, str) else '')

        # Use entity namespace or default
        if entity.namespace:
            return entity.namespace

        default_ns = self._get_default_namespace()
        if default_ns:
            return default_ns

        raise ValueError(
            'Namespace not specified and no default available. '
            'Use explicit namespace or configure single namespace.'
        )

    async def _get_table_async(self, entity_name: EntityName) -> Table:
        """
        Get table from metadata provider.

        Uses entity_name.namespace to determine which metadata provider to use.

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        namespace = entity_name.namespace
        if not namespace:
            # Resolve default namespace
            default_ns = self._get_default_namespace()
            if default_ns:
                namespace = default_ns
            else:
                raise ValueError(
                    'Namespace not specified and no default available. '
                    'Use explicit namespace or configure single namespace.'
                )

        config = self.get_namespace_config(namespace)

        try:
            return await config.metadata_provider.get_table(
                entity_name,
                config.engine,
                None,
            )
        except (UMANotFoundError, RuntimeError) as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _extract_expression_type(self, expr: ColumnElement) -> Any | None:
        """
        Extract type from SQLAlchemy expression.

        Args:
            expr: SQLAlchemy column element

        Returns:
            Expression type or None if not available
        """
        return getattr(expr, 'type', None)

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

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
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

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

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    def _get_table_identifier(self, from_clause: Table | None) -> str | None:
        """
        Extract stable table identifier for cache key.

        Uses table name and schema instead of object id() for stable caching
        that works across different queries and parser instances.

        Args:
            from_clause: SQLAlchemy Table object or None

        Returns:
            Table identifier string (e.g., "schema.table" or "table") or None
        """
        if from_clause is None:
            return None

        if isinstance(from_clause, Table):
            table_name = from_clause.name
            if from_clause.schema:
                return f'{from_clause.schema}.{table_name}'
            return table_name

        # For aliases or other types, use string representation
        # This is less ideal but maintains backward compatibility
        return str(from_clause)

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id') with caching.

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Create cache key: (field_spec, table_identifier)
        # Use table identifier (name/schema) instead of id() for stable caching
        # This allows cache to work across different queries for the same table
        table_id = self._get_table_identifier(from_clause)
        cache_key = (field_spec, table_id)

        # Check cache first
        if cache_key in self._column_cache:
            return self._column_cache[cache_key]

        # Try to resolve through AliasManager first
        try:
            column = self.alias_manager.resolve_column(field_spec, from_clause)
        except JSQLSyntaxError:
            # If qualified column not found in aliases, try lazy loading
            if '.' in field_spec:
                table_name, column_name = field_spec.split('.', 1)
                entity_name = parse_entity_name(table_name)
                try:
                    table = await self._get_table_async(entity_name)
                    if column_name in table.columns:
                        column = table.columns[column_name]
                    else:
                        raise JSQLSyntaxError(f'Column "{field_spec}" not found in table "{table_name}"')
                except (UMANotFoundError, RuntimeError) as e:
                    raise JSQLSyntaxError(f'Column "{field_spec}" not found: {e}') from e
            else:
                # Re-raise original error if lazy loading also fails
                raise

        # Cache the result
        self._column_cache[cache_key] = column
        return column

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
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            else:
                # Load table via metadata provider (lazy loading)
                entity_name = parse_entity_name(table_name)
                try:
                    table = await self._get_table_async(entity_name)
                except (UMANotFoundError, RuntimeError) as e:
                    raise JSQLSyntaxError(f'Table "{table_name}" not found: {e}', path) from e

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

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
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._comparison_handler(condition_spec, op)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(
        self,
        expr_spec: JSQLExpression,
        expected_type: Any | None = None
    ) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification
            expected_type: Expected SQLAlchemy type for literals/parameters (optional)

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            # Use expected type when provided to avoid dialects binding as VARCHAR
            # This preserves proper typing for date/time comparisons.
            if expected_type is not None:
                coerced_value = self._coerce_literal_value(value, expected_type)
                return literal(coerced_value, type_=expected_type)
            return literal(value)

        # Parameter - use bindparam for security (parameterized queries)
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            # Use bindparam() with explicit key to preserve JSQL parameter name
            # This ensures SQLAlchemy preserves the parameter name and we can map it correctly
            # Value will be provided at execution time from self.params
            if expected_type is not None:
                coerced_value = self._coerce_literal_value(self.params[param_name], expected_type)
                self.params[param_name] = coerced_value
                # Use bindparam with type annotation and explicit key for proper type handling
                param_bind = bindparam(key=param_name, value=None, type_=expected_type)
            else:
                # Use bindparam with explicit key to preserve parameter name
                param_bind = bindparam(key=param_name, value=None)
            # Store mapping: JSQL param name -> bindparam object for later use in executor
            self._param_bindparams[param_name] = param_bind
            return param_bind

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right
                    case JSQLOperator.CONCAT:
                        # String concatenation operator ||
                        # SQLAlchemy handles || differently per dialect
                        # Use op() to generate exact SQL operator
                        return left.op('||')(right)

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(f'Invalid expression: {expr_spec}')

    @staticmethod
    def _coerce_literal_value(value: Any, expected_type: Any) -> Any:
        """
        Coerce literal/parameter values based on expected SQLAlchemy type.

        Raises:
            JSQLSyntaxError: If value cannot be coerced to expected type
        """
        if value is None:
            return value

        try:
            if isinstance(expected_type, Date):
                if isinstance(value, date) and not isinstance(value, datetime):
                    return value
                if isinstance(value, datetime):
                    return value.date()
                if isinstance(value, str):
                    return date.fromisoformat(value)
            if isinstance(expected_type, (DateTime, TIMESTAMP)):
                if isinstance(value, datetime):
                    dt_value = value
                elif isinstance(value, date) and not isinstance(value, datetime):
                    dt_value = datetime.combine(value, time.min)
                elif isinstance(value, str):
                    try:
                        dt_value = datetime.fromisoformat(value)
                    except ValueError:
                        dt_value = datetime.combine(date.fromisoformat(value), time.min)
                else:
                    return value

                if getattr(expected_type, 'timezone', False) and dt_value.tzinfo is None:
                    return dt_value.replace(tzinfo=timezone.utc)
                return dt_value
        except (TypeError, ValueError) as exc:
            raise JSQLSyntaxError(
                f'Invalid value for expected type {expected_type}: {value}'
            ) from exc

        return value

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
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

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
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

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
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

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
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result
