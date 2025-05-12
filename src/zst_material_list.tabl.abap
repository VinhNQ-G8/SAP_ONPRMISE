*&---------------------------------------------------------------------*
*& Structure: ZST_MATERIAL_LIST
*& Description: Structure for Material List ALV Display
*&---------------------------------------------------------------------*
@EndUserText.label : 'Material List Structure'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #STRUCTURE
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
@Analytics.dataExtraction : #ENABLED
@Analytics.internalName : #LOCAL
@ObjectModel.modelCategory : #BUSINESS_OBJECT
@ObjectModel.representativeKey : 'Matnr'
@VDM.viewType : #BASIC
define type ZST_MATERIAL_LIST {
  @EndUserText.label : 'Material Number'
  @Analytics.internalName : 'MATNR'
  @ObjectModel.text.element : ['Maktx']
  matnr : matnr;
  
  @EndUserText.label : 'Material Description'
  @Analytics.internalName : 'MAKTX'
  @Semantics.text : true
  maktx : maktx;
  
  @EndUserText.label : 'Material Type'
  @Analytics.internalName : 'MTART'
  @ObjectModel.text.element : ['MtartText']
  mtart : mtart;
  
  @EndUserText.label : 'Material Group'
  @Analytics.internalName : 'MATKL'
  @ObjectModel.text.element : ['MatklText']
  matkl : matkl;
  
  @EndUserText.label : 'Base Unit of Measure'
  @Analytics.internalName : 'MEINS'
  @Semantics.unitOfMeasure : true
  meins : meins;
  
  @EndUserText.label : 'Created On'
  @Analytics.internalName : 'ERSDA'
  @Semantics.systemDateTime.createdAt : true
  ersda : ersda;
  
  @EndUserText.label : 'Created By'
  @Analytics.internalName : 'ERNAM'
  @Semantics.user.createdBy : true
  ernam : ernam;
  
  @EndUserText.label : 'Last Changed On'
  @Analytics.internalName : 'LAEDA'
  @Semantics.systemDateTime.lastChangedAt : true
  laeda : laeda;
  
  @EndUserText.label : 'Last Changed By'
  @Analytics.internalName : 'AENAM'
  @Semantics.user.lastChangedBy : true
  aenam : aenam;
} 