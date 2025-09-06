@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vista de Consumo del historial'
@Metadata.allowExtensions: true
define view entity zc_dt_inct_h_as as projection on ZDD_INCT_H_AS
{
    key HisUuid,
    key IncUuid,
    HisId,
    PreviousStatus,
    NewStatus,
    Text,
    LocalCreatedBy,
    LocalCreatedAt,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,
    /* Associations */
    _Incident : redirected to parent zc_dt_inct_as
}
