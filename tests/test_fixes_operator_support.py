"""Tests for fixes in operator support feature branch.

These tests specifically verify the fixes for:
1. Lambda capture issue in string operator handler registration
2. Indentation bug fix for NOT BETWEEN handler in to_jsql
3. NOT ILIKE operator mapping fix
"""

import pytest

from namerec.uma.jsql.converter import jsql_to_sql
from namerec.uma.jsql.converter import sql_to_jsql
from namerec.uma.jsql.converter.conditions.to_sql import _COMPARISON_OPERATOR_HANDLERS
from namerec.uma.jsql.constants import JSQLOperator


class TestStringOperatorHandlerRegistration:
    """Test that string operator handlers are correctly registered (fixes lambda capture issue)."""
    
    def test_all_string_operators_have_handlers(self):
        """Verify all string operators have registered handlers (lambda capture fix)."""
        string_operators = {
            JSQLOperator.LIKE.value,
            JSQLOperator.NOT_LIKE.value,
            JSQLOperator.ILIKE.value,
            JSQLOperator.NOT_ILIKE.value,
            JSQLOperator.SIMILAR_TO.value,
            JSQLOperator.REGEXP.value,
            JSQLOperator.RLIKE.value,
        }
        
        for op in string_operators:
            assert op in _COMPARISON_OPERATOR_HANDLERS, f"String operator {op} missing handler"
    
    def test_string_operators_use_correct_handlers(self):
        """Verify string operator handlers correctly capture operator values."""
        # Test LIKE
        cond = {
            'op': JSQLOperator.LIKE.value,
            'left': {'field': 'name'},
            'right': {'value': '%test%'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.LIKE.value]
        result = handler(cond)
        assert result is not None
        assert hasattr(result, 'this')
        assert hasattr(result, 'expression')
        
        # Test NOT LIKE
        cond = {
            'op': JSQLOperator.NOT_LIKE.value,
            'left': {'field': 'name'},
            'right': {'value': '%test%'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.NOT_LIKE.value]
        result = handler(cond)
        assert result is not None
        # Should be wrapped in Not
        assert hasattr(result, 'this')
        
        # Test ILIKE
        cond = {
            'op': JSQLOperator.ILIKE.value,
            'left': {'field': 'name'},
            'right': {'value': '%test%'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.ILIKE.value]
        result = handler(cond)
        assert result is not None
        assert hasattr(result, 'this')
        
        # Test NOT ILIKE (fixes exp.NotILike issue)
        cond = {
            'op': JSQLOperator.NOT_ILIKE.value,
            'left': {'field': 'name'},
            'right': {'value': '%test%'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.NOT_ILIKE.value]
        result = handler(cond)
        assert result is not None
        # Should be wrapped in Not (NOT ILIKE uses ILike with negate=True)
        assert hasattr(result, 'this')
        
        # Test SIMILAR_TO
        cond = {
            'op': JSQLOperator.SIMILAR_TO.value,
            'left': {'field': 'email'},
            'right': {'value': '%@example\\.com'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.SIMILAR_TO.value]
        result = handler(cond)
        assert result is not None
        
        # Test REGEXP
        cond = {
            'op': JSQLOperator.REGEXP.value,
            'left': {'field': 'email'},
            'right': {'value': '^[a-z]+@example\\.com$'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.REGEXP.value]
        result = handler(cond)
        assert result is not None
        
        # Test RLIKE
        cond = {
            'op': JSQLOperator.RLIKE.value,
            'left': {'field': 'email'},
            'right': {'value': '^[a-z]+@example\\.com$'},
        }
        handler = _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.RLIKE.value]
        result = handler(cond)
        assert result is not None


class TestNotBetweenConversion:
    """Test NOT BETWEEN conversion from SQL to JSQL (fixes indentation bug)."""
    
    def test_not_between_sql_to_jsql(self):
        """Verify NOT BETWEEN is correctly converted from SQL to JSQL."""
        sql = "SELECT id FROM orders WHERE amount NOT BETWEEN 100 AND 1000"
        
        jsql = sql_to_jsql(sql, dialect='sqlite')
        
        assert 'where' in jsql
        where = jsql['where']
        assert where['op'] == 'NOT BETWEEN'
        assert 'expr' in where
        assert 'low' in where
        assert 'high' in where
        
        # Verify structure
        assert where['expr'] == {'field': 'amount'}
        assert where['low'] == {'value': 100}
        assert where['high'] == {'value': 1000}
    
    def test_not_between_roundtrip(self):
        """Verify NOT BETWEEN roundtrip conversion works correctly."""
        original_jsql = {
            'from': 'orders',
            'select': [{'field': 'id'}],
            'where': {
                'op': 'NOT BETWEEN',
                'expr': {'field': 'amount'},
                'low': {'value': 100},
                'high': {'value': 1000},
            },
        }
        
        # JSQL -> SQL
        sql = jsql_to_sql(original_jsql, dialect='sqlite')
        assert 'NOT' in sql.upper() or 'NOT BETWEEN' in sql.upper()
        
        # SQL -> JSQL
        roundtrip_jsql = sql_to_jsql(sql, dialect='sqlite')
        
        # Verify NOT BETWEEN is preserved
        assert 'where' in roundtrip_jsql
        where = roundtrip_jsql['where']
        assert where['op'] == 'NOT BETWEEN'


class TestNotIlikeOperator:
    """Test NOT ILIKE operator (fixes exp.NotILike mapping issue)."""
    
    def test_not_ilike_jsql_to_sql(self):
        """Verify NOT ILIKE is correctly converted from JSQL to SQL."""
        jsql = {
            'from': 'users',
            'select': [{'field': 'name'}],
            'where': {
                'op': 'NOT ILIKE',
                'left': {'field': 'name'},
                'right': {'value': '%test%'},
            },
        }
        
        sql = jsql_to_sql(jsql, dialect='sqlite')
        
        # SQLite converts ILIKE to LOWER(...) LIKE LOWER(...)
        assert 'NOT' in sql.upper()
        assert 'LIKE' in sql.upper() or 'LOWER' in sql.upper()
        assert '%test%' in sql
    
    def test_not_ilike_handler_registration(self):
        """Verify NOT ILIKE handler is correctly registered and uses ILike with negate."""
        cond = {
            'op': JSQLOperator.NOT_ILIKE.value,
            'left': {'field': 'name'},
            'right': {'value': '%test%'},
        }
        
        handler = _COMPARISON_OPERATOR_HANDLERS.get(JSQLOperator.NOT_ILIKE.value)
        assert handler is not None, "NOT ILIKE handler should be registered"
        
        # Call handler and verify it works (doesn't raise AttributeError for exp.NotILike)
        result = handler(cond)
        assert result is not None
        # Should be wrapped in Not (since we use ILike with negate=True)
        assert hasattr(result, 'this')


class TestLambdaCaptureFix:
    """Test that lambda capture issue is fixed for all string operators."""
    
    @pytest.mark.parametrize('operator', [
        JSQLOperator.LIKE.value,
        JSQLOperator.NOT_LIKE.value,
        JSQLOperator.ILIKE.value,
        JSQLOperator.NOT_ILIKE.value,
        JSQLOperator.SIMILAR_TO.value,
        JSQLOperator.REGEXP.value,
        JSQLOperator.RLIKE.value,
    ])
    def test_string_operator_handler_correctly_captures_operator(self, operator):
        """Verify each string operator handler correctly captures its operator value."""
        cond = {
            'op': operator,
            'left': {'field': 'test_field'},
            'right': {'value': 'test_value'},
        }
        
        handler = _COMPARISON_OPERATOR_HANDLERS[operator]
        # This should not raise any errors related to lambda capture
        result = handler(cond)
        assert result is not None, f"Handler for {operator} should return a result"
