"""Tests for JSQL converter module."""

import pytest

from namerec.uma.jsql.converter import jsql_to_sql
from namerec.uma.jsql.converter import sql_to_jsql
from namerec.uma.jsql.exceptions import JSQLSyntaxError


class TestJSQLToSQL:
    """Tests for JSQL to SQL conversion."""

    def test_simple_select(self) -> None:
        """Test simple SELECT query conversion."""
        jsql = {
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
        }

        sql = jsql_to_sql(jsql)

        assert 'SELECT' in sql
        assert 'FROM users' in sql
        assert 'id' in sql
        assert 'name' in sql

    def test_select_with_where(self) -> None:
        """Test SELECT with WHERE clause."""
        jsql = {
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': '=',
                'left': {'field': 'active'},
                'right': {'value': True},
            },
        }

        sql = jsql_to_sql(jsql)

        assert 'WHERE' in sql
        assert 'active' in sql

    def test_select_with_join(self) -> None:
        """Test SELECT with JOIN."""
        jsql = {
            'from': 'users',
            'select': [{'field': 'users.name'}, {'field': 'orders.total'}],
            'joins': [
                {
                    'type': 'LEFT',
                    'entity': 'orders',
                    'on': {
                        'op': '=',
                        'left': {'field': 'users.id'},
                        'right': {'field': 'orders.user_id'},
                    },
                },
            ],
        }

        sql = jsql_to_sql(jsql)

        assert 'JOIN' in sql
        assert 'orders' in sql

    def test_select_with_limit_offset(self) -> None:
        """Test SELECT with LIMIT and OFFSET."""
        jsql = {
            'from': 'users',
            'select': [{'field': '*'}],
            'limit': 10,
            'offset': 20,
        }

        sql = jsql_to_sql(jsql)

        assert 'LIMIT' in sql
        assert '10' in sql
        assert 'OFFSET' in sql
        assert '20' in sql

    def test_select_with_order_by(self) -> None:
        """Test SELECT with ORDER BY."""
        jsql = {
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'order_by': [
                {'field': 'name', 'direction': 'ASC'},
            ],
        }

        sql = jsql_to_sql(jsql)

        assert 'ORDER BY' in sql
        assert 'name' in sql

    def test_invalid_jsql(self) -> None:
        """Test handling of invalid JSQL."""
        jsql = {
            'select': [{'field': 'id'}],
            # Missing 'from' clause
        }

        with pytest.raises(JSQLSyntaxError):
            jsql_to_sql(jsql)


class TestSQLToJSQL:
    """Tests for SQL to JSQL conversion."""

    def test_simple_select(self) -> None:
        """Test simple SELECT query parsing."""
        sql = 'SELECT id, name FROM users'

        jsql = sql_to_jsql(sql)

        assert jsql['from'] == 'users'
        assert len(jsql['select']) == 2
        assert any(f.get('field') == 'id' for f in jsql['select'])
        assert any(f.get('field') == 'name' for f in jsql['select'])

    def test_select_star(self) -> None:
        """Test SELECT * parsing."""
        sql = 'SELECT * FROM users'

        jsql = sql_to_jsql(sql)

        assert jsql['from'] == 'users'
        assert len(jsql['select']) == 1
        assert jsql['select'][0]['field'] == '*'

    def test_select_with_where(self) -> None:
        """Test SELECT with WHERE clause."""
        sql = 'SELECT id FROM users WHERE active = 1'

        jsql = sql_to_jsql(sql)

        assert jsql['from'] == 'users'
        assert 'where' in jsql
        # Use left/right format (no longer using field/value format)
        assert jsql['where']['op'] == '='
        assert jsql['where']['left']['field'] == 'active'
        assert jsql['where']['right']['value'] == 1

    def test_select_with_limit(self) -> None:
        """Test SELECT with LIMIT."""
        sql = 'SELECT id FROM users LIMIT 10'

        jsql = sql_to_jsql(sql)

        assert jsql['from'] == 'users'
        assert jsql['limit'] == 10

    def test_select_with_order_by(self) -> None:
        """Test SELECT with ORDER BY."""
        sql = 'SELECT id, name FROM users ORDER BY name ASC'

        jsql = sql_to_jsql(sql)

        assert jsql['from'] == 'users'
        assert 'order_by' in jsql
        assert len(jsql['order_by']) == 1
        assert jsql['order_by'][0]['field'] == 'name'
        assert jsql['order_by'][0]['direction'] == 'ASC'

    def test_select_with_join(self) -> None:
        """Test SELECT with JOIN."""
        sql = '''
            SELECT u.name, o.total
            FROM users u
            LEFT JOIN orders o ON u.id = o.user_id
        '''

        jsql = sql_to_jsql(sql)

        assert 'joins' in jsql
        assert len(jsql['joins']) == 1
        assert jsql['joins'][0]['type'] == 'LEFT'
        assert jsql['joins'][0]['entity'] == 'orders'

    def test_invalid_sql(self) -> None:
        """Test handling of invalid SQL."""
        sql = 'INVALID SQL QUERY'

        with pytest.raises(JSQLSyntaxError):
            sql_to_jsql(sql)

    def test_non_select_query(self) -> None:
        """Test handling of non-SELECT queries."""
        sql = 'UPDATE users SET active = 1'

        with pytest.raises(JSQLSyntaxError) as exc_info:
            sql_to_jsql(sql)

        assert 'Only SELECT queries are supported' in str(exc_info.value)


class TestRoundTrip:
    """Tests for JSQL -> SQL -> JSQL round-trip conversion."""

    def test_simple_round_trip(self) -> None:
        """Test round-trip conversion for simple query."""
        original_jsql = {
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {
                'op': '=',
                'left': {'field': 'active'},
                'right': {'value': 1},
            },
            'limit': 10,
        }

        # JSQL -> SQL
        sql = jsql_to_sql(original_jsql)

        # SQL -> JSQL
        converted_jsql = sql_to_jsql(sql)

        # Check key elements are preserved
        assert converted_jsql['from'] == 'users'
        assert 'select' in converted_jsql
        assert 'where' in converted_jsql
        assert converted_jsql['limit'] == 10
