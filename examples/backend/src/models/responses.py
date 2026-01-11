"""Response models for API endpoints."""

from typing import Any

from pydantic import BaseModel


class ErrorResponse(BaseModel):
    """Unified error response format."""

    id: str  # Exception type (e.g., "UMAAccessDeniedError")
    message: str  # User-friendly error message
    details: dict[str, Any]  # Structured details for frontend processing


class EntityListResponse(BaseModel):
    """Response for entity list endpoint."""

    entities: list[str]


class DeleteResponse(BaseModel):
    """Response for delete endpoint."""

    deleted: bool


class SelectResponse(BaseModel):
    """Response for select endpoint."""

    meta: list[dict[str, Any]]
    data: list[dict[str, Any]]
