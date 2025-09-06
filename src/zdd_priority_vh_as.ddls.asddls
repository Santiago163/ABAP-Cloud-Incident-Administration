@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Value Help para el estatus'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
 /*+[hideWarning] { "IDS" : [ "KEY_CHECK" ] } */
define view entity ZDD_PRIORITY_VH_AS as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZDO_PRIORITY_AS') 
{
    //key domain_name,
    //key value_position,
    @Semantics.language: true
    //key language,
    key value_low  as PriorityCode, 
    @Semantics.text: true
    text  as PriorityDescription
}
