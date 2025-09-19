@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Vista de Consumo del historial'
@Metadata.allowExtensions: true
define view entity zc_dt_inct_h_as as projection on zdd_inct_h_as
{
    key HisUuid,
    key IncUuid,
    HisId,
    PreviousStatus,
    NewStatus,
    Text,
    Responsible,
    LocalCreatedBy,
    LocalCreatedAt,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,
    /* Associations */
    _Incident : redirected to parent zc_dt_inct_as
}
