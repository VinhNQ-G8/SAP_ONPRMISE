@EndUserText.label : 'Material List Structure'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #STRUCTURE
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define type ZST_MATERIAL_LIST {
  @EndUserText.label : 'Material Number'
  matnr : matnr;
  
  @EndUserText.label : 'Material Description'
  maktx : maktx;
  
  @EndUserText.label : 'Material Type'
  mtart : mtart;
  
  @EndUserText.label : 'Material Group'
  matkl : matkl;
  
  @EndUserText.label : 'Base Unit of Measure'
  meins : meins;
  
  @EndUserText.label : 'Created On'
  ersda : ersda;
  
  @EndUserText.label : 'Created By'
  ernam : ernam;
  
  @EndUserText.label : 'Last Changed On'
  laeda : laeda;
  
  @EndUserText.label : 'Last Changed By'
  aenam : aenam;
} 