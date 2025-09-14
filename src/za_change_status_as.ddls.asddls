@EndUserText.label: 'Entidad abstracta para nuevo estatus'
define abstract entity ZA_CHANGE_STATUS_AS
{
  @EndUserText.label:'Change Status'
  @Consumption.valueHelpDefinition: [ {
      entity.name  : 'zdd_status_vh_as',
      entity.element: 'StatusCode',
      useForValidation: true
    } ]
  status           : zde_status_as;
  @EndUserText.label:'Add Observation Text'
  text             : zde_text_as;
  @EndUserText.label:'Responsible Usr(Required with Status=IP)'      
  responsible      : zde_responsible_as;
}
