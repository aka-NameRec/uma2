"""UMA API endpoints."""

import structlog
from fastapi import APIRouter
from fastapi import Depends

from namerec.uma import UMA
from namerec.uma.jsql.converter import jsql_to_sql
from namerec.uma.jsql.converter import sql_to_jsql
from src.dependencies import get_uma
from src.dependencies import get_user_context
from src.models.requests import DeleteRequest
from src.models.requests import EntityDetailsRequest
from src.models.requests import EntityListRequest
from src.models.requests import JSQL2SQLRequest
from src.models.requests import ReadRequest
from src.models.requests import SaveRequest
from src.models.requests import SelectRequest
from src.models.requests import SQL2JSQLRequest
from src.models.responses import DeleteResponse
from src.models.responses import EntityListResponse
from src.models.responses import SelectResponse

logger = structlog.get_logger()

router = APIRouter(prefix='/api/uma', tags=['UMA'])


@router.post('/select', response_model=SelectResponse)
async def select_endpoint(
    request: SelectRequest,
    user_context: dict = Depends(get_user_context),
    uma: UMA = Depends(get_uma),
) -> dict:
    """
    Execute JSQL query.

    Args:
        request: JSQL query with optional parameters
        user_context: User context for access control
        uma: UMA application instance

    Returns:
        Query result with metadata and data
    """
    logger.info(
        'JSQL select request',
        entity=request.jsql.get('from'),
        has_params=request.params is not None,
    )

    result = await uma.select(
        jsql=request.jsql,
        params=request.params,
        user_context=user_context,
    )

    logger.info('JSQL select completed', row_count=len(result.get('data', [])))
    return result


@router.post('/read')
async def read_endpoint(
    request: ReadRequest,
    user_context: dict = Depends(get_user_context),
    uma: UMA = Depends(get_uma),
) -> dict:
    """
    Read a single record by ID.

    Args:
        request: Entity name and record ID
        user_context: User context for access control
        uma: UMA application instance

    Returns:
        Record data as dictionary
    """
    logger.info('Read request', entity_name=request.entity_name, id=request.id)

    result = await uma.read(
        entity_name=request.entity_name,
        id_value=request.id,
        user_context=user_context,
    )

    logger.info('Read completed', entity_name=request.entity_name)
    return result


@router.post('/save')
async def save_endpoint(
    request: SaveRequest,
    user_context: dict = Depends(get_user_context),
    uma: UMA = Depends(get_uma),
) -> dict:
    """
    Save a record (create or update).

    Returns the full record body after save.

    Args:
        request: Entity name and record data
        user_context: User context for access control
        uma: UMA application instance

    Returns:
        Full record data after save
    """
    has_id = any(request.data.get(key) is not None for key in ['id', 'ID'])
    operation = 'update' if has_id else 'create'

    logger.info(
        'Save request',
        entity_name=request.entity_name,
        operation=operation,
    )

    # Save record (returns ID)
    saved_id = await uma.save(
        entity_name=request.entity_name,
        data=request.data,
        user_context=user_context,
    )

    # Read back the full record
    record = await uma.read(
        entity_name=request.entity_name,
        id_value=saved_id,
        user_context=user_context,
    )

    logger.info('Save completed', entity_name=request.entity_name, id=saved_id)
    return record


@router.post('/delete', response_model=DeleteResponse)
async def delete_endpoint(
    request: DeleteRequest,
    user_context: dict = Depends(get_user_context),
    uma: UMA = Depends(get_uma),
) -> dict:
    """
    Delete a record by ID.

    Args:
        request: Entity name and record ID
        user_context: User context for access control
        uma: UMA application instance

    Returns:
        Deletion status
    """
    logger.info('Delete request', entity_name=request.entity_name, id=request.id)

    deleted = await uma.delete(
        entity_name=request.entity_name,
        id_value=request.id,
        user_context=user_context,
    )

    logger.info('Delete completed', entity_name=request.entity_name, deleted=deleted)
    return {'deleted': deleted}


@router.post('/meta/entity_list', response_model=EntityListResponse)
async def entity_list_endpoint(
    request: EntityListRequest,
    user_context: dict = Depends(get_user_context),
    uma: UMA = Depends(get_uma),
) -> dict:
    """
    Get list of available entities.

    Args:
        request: Optional namespace filter
        user_context: User context for access control
        uma: UMA application instance

    Returns:
        List of entity names
    """
    logger.info('Entity list request', namespace=request.namespace)

    entities = await uma.entity_list(
        user_context=user_context,
        namespace=request.namespace,
    )

    logger.info('Entity list completed', count=len(entities))
    return {'entities': entities}


@router.post('/meta/entity_details')
async def entity_details_endpoint(
    request: EntityDetailsRequest,
    user_context: dict = Depends(get_user_context),
    uma: UMA = Depends(get_uma),
) -> dict:
    """
    Get entity metadata (structure).

    Args:
        request: Entity name
        user_context: User context for access control
        uma: UMA application instance

    Returns:
        Entity metadata dictionary
    """
    logger.info('Entity details request', entity_name=request.entity_name)

    details = await uma.entity_details(
        entity_name=request.entity_name,
        user_context=user_context,
    )

    logger.info('Entity details completed', entity_name=request.entity_name)
    return details


@router.post('/transform/sql2jsql')
async def sql2jsql_endpoint(request: SQL2JSQLRequest) -> dict:
    """
    Transform SQL query to JSQL format.

    Args:
        request: SQL query string and optional dialect

    Returns:
        JSQL query dictionary
    """
    logger.info('SQL to JSQL transformation', dialect=request.dialect)

    jsql = sql_to_jsql(sql=request.data, dialect=request.dialect)

    logger.info('SQL to JSQL completed')
    return jsql


@router.post('/transform/jsql2sql')
async def jsql2sql_endpoint(request: JSQL2SQLRequest) -> dict:
    """
    Transform JSQL query to SQL string.

    Args:
        request: JSQL query dictionary and optional dialect

    Returns:
        SQL query string
    """
    logger.info('JSQL to SQL transformation', dialect=request.dialect)

    sql = jsql_to_sql(jsql=request.data, dialect=request.dialect)

    logger.info('JSQL to SQL completed')
    return {'sql': sql}
