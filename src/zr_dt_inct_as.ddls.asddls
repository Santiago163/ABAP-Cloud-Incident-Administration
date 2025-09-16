@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Incident Root View'
define root view entity ZR_DT_INCT_AS
  as select from zdt_inct_as
  composition [0..*] of zdd_inct_h_as as _History
{
  key inc_uuid              as IncUuid,
      incident_id           as IncidentId,
      title                 as Title,
      description           as Description,
      cast(status as zde_status_as)                as Status,
      cast(priority as zde_priority_as)             as Priority,
      creation_date         as CreationDate,
      changed_date          as ChangedDate,
      responsible           as Responsible,
      @Semantics.user.createdBy: true
      local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at      as LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      //  Associations
      _History
      
}
