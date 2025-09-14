@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vista de consumo de incidentes'
@Metadata.allowExtensions: true
define root view entity zc_dt_inct_as
  provider contract transactional_query
  as projection on ZR_DT_INCT_AS

{
  key     IncUuid,
          IncidentId,
          Title,
          Description,
          Status,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_BRT_V_ELEMENT_AS'
          @EndUserText.label: 'StatusDescription'
  virtual StatusDescription   : zde_status_description_as,
          Priority,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_BRT_V_ELEMENT_AS'
          @EndUserText.label: 'PriorityDescription'
  virtual PriorityDescription : zde_priority_description_as,
          CreationDate,
          ChangedDate,
          Responsible,
          LocalCreatedBy,
          LocalCreatedAt,
          LocalLastChangedBy,
          LocalLastChangedAt,
          LastChangedAt,
          /* Associations */
          _History : redirected to composition child zc_dt_inct_h_as
}
