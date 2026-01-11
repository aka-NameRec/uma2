"""Request models for API endpoints."""

from typing import Any

from pydantic import BaseModel


class SelectRequest(BaseModel):
    """Request for JSQL select endpoint."""

    jsql: dict[str, Any]
    params: dict[str, Any] | None = None


class ReadRequest(BaseModel):
    """Request for read endpoint."""

    entity_name: str
    id: Any  # Can be int, str, UUID, etc.


class SaveRequest(BaseModel):
    """Request for save endpoint."""

    entity_name: str
    data: dict[str, Any]


class DeleteRequest(BaseModel):
    """Request for delete endpoint."""

    entity_name: str
    id: Any  # Can be int, str, UUID, etc.


class EntityListRequest(BaseModel):
    """Request for entity list endpoint."""

    namespace: str | None = None


class EntityDetailsRequest(BaseModel):
    """Request for entity details endpoint."""

    entity_name: str


class SQL2JSQLRequest(BaseModel):
    """Request for SQL to JSQL transformation."""

    data: str  # SQL query string
    dialect: str = 'generic'  # SQL dialect (generic, postgresql, mysql, sqlite, etc.)


class JSQL2SQLRequest(BaseModel):
    """Request for JSQL to SQL transformation."""

    data: dict[str, Any]  # JSQL query dictionary
    dialect: str = 'generic'  # SQL dialect (generic, postgresql, mysql, sqlite, etc.)
