*&---------------------------------------------------------------------*
*& Report  ZPG_BOM_MATERIAL_EXTRACT
*& Author: [Author name]
*& Date: [Current date]
*&---------------------------------------------------------------------*
*& Description: Trích xuất dữ liệu từ SAP theo quy trình BOM và Material
*& Master theo các bước:
*& 1. Material Plant
*& 2. BOM Master 
*& 3. Material Master
*& 4. Material Plant từ Material Master
*& 5. Sales Price
*& 6. Customer Master
*& 7. Customer Material
*& 8. Purchasing Price
*& 9. Purchasing Info
*& 10. Source Supply
*& 11. Vendor Master
*& 12. Production Version
*& 13. Routing Master
*& 14. Work Center
*&---------------------------------------------------------------------*

REPORT zpg_bom_material_extract.

*&---------------------------------------------------------------------*
*& Types Declaration
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_marc_plant,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         ladgr TYPE marc-ladgr,
         ekgrp TYPE marc-ekgrp,
         kautb TYPE marc-kautb,
         insmk TYPE marc-insmk,
         kordb TYPE marc-kordb,
         dismm TYPE marc-dismm,
         fxhor TYPE marc-fxhor,
         dispo TYPE marc-dispo,
         disgr TYPE marc-disgr,
         disls TYPE marc-disls,
         bstrf TYPE marc-bstrf,
         beskz TYPE marc-beskz,
         sobsl TYPE marc-sobsl,
         rgekz TYPE marc-rgekz,
         lgfsb TYPE marc-lgfsb,
         lgpro TYPE marc-lgpro,
         dzeit TYPE marc-dzeit,
         plifz TYPE marc-plifz,
         fhori TYPE marc-fhori,
         eisbe TYPE marc-eisbe,
         perkz TYPE marc-perkz,
         strgr TYPE marc-strgr,
         miskz TYPE marc-miskz,
         sbdkz TYPE marc-sbdkz,
         fevor TYPE marc-fevor,
         matgr TYPE marc-matgr,
         prctr TYPE marc-prctr,
         ncost TYPE marc-ncost,
         awsls TYPE marc-awsls,
         sobsk TYPE marc-sobsk,
         losgr TYPE marc-losgr,
       END OF gts_marc_plant.

TYPES: BEGIN OF gts_bom_item,
         matnr    TYPE matnr,    " Material Number
         idnrk    TYPE matnr,    " Component Material
         menge    TYPE menge_d,  " Component Quantity
         meins    TYPE meins,    " Unit of Measure
         posnr    TYPE posnr,    " Item Number
         stlal    TYPE stlal,    " Alternative BOM
         stlan    TYPE stlan,    " BOM Usage
         stlty    TYPE stlty,    " BOM Category
         werks    TYPE werks_d,  " Plant
         bmeng    TYPE bmeng,    " Base Quantity
         bmein    TYPE bmein,    " Base Unit of Measure
         potx1    TYPE potx1,    " Item Text
         potx2    TYPE potx2,    " Item Text 2
         postp    TYPE postp,    " Item Category
         sortf    TYPE sortf,    " Sort String
         stlkn    TYPE stlkn,    " BOM Item Node Number
         stpoz    TYPE stpoz,    " BOM Item Counter
         zaehl    TYPE zaehl,    " Counter
       END OF gts_bom_item.

*&---------------------------------------------------------------------*
*& Types Declaration cho Material Master
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_material_master,
         matnr TYPE mara-matnr,  " Material Number
         mtart TYPE mara-mtart,  " Material Type
         maktx TYPE makt-maktx,  " Material Description
         meins TYPE mara-meins,  " Base Unit of Measure
         matkl TYPE mara-matkl,  " Material Group
         bismt TYPE mara-bismt,  " Old Material Number
         extwg TYPE mara-extwg,  " External Material Group
         prdha TYPE mara-prdha,  " Product Hierarchy
         mtpos TYPE mara-mtpos,  " General Item Category Group
         groes TYPE mara-groes,  " Size/Dimensions
         ferth TYPE mara-ferth,  " Production/Inspection Memo
         normt TYPE mara-normt,  " Industry Standard Description
         zeinr TYPE mara-zeinr,  " Drawing Number
         formt TYPE mara-formt,  " Technical Drawing Format
         aeszn TYPE mara-aeszn,  " Document Number
         tragr TYPE mara-tragr,  " Transportation Group
         raube TYPE mara-raube,  " Storage Conditions
       END OF gts_material_master.

*&---------------------------------------------------------------------*
*& Types Declaration cho Material Plant Extended
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_material_plant_ext,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         ladgr TYPE marc-ladgr,
         ekgrp TYPE marc-ekgrp,
         kautb TYPE marc-kautb,
         insmk TYPE marc-insmk,
         kordb TYPE marc-kordb,
         dismm TYPE marc-dismm,
         fxhor TYPE marc-fxhor,
         dispo TYPE marc-dispo,
         disgr TYPE marc-disgr,
         disls TYPE marc-disls,
         bstrf TYPE marc-bstrf,
         beskz TYPE marc-beskz,
         sobsl TYPE marc-sobsl,
         rgekz TYPE marc-rgekz,
         lgfsb TYPE marc-lgfsb,
         lgpro TYPE marc-lgpro,
         dzeit TYPE marc-dzeit,
         plifz TYPE marc-plifz,
         fhori TYPE marc-fhori,
         eisbe TYPE marc-eisbe,
         perkz TYPE marc-perkz,
         strgr TYPE marc-strgr,
         miskz TYPE marc-miskz,
         sbdkz TYPE marc-sbdkz,
         fevor TYPE marc-fevor,
         matgr TYPE marc-matgr,
         prctr TYPE marc-prctr,
         bklas TYPE mbew-bklas,
         oklas TYPE mbew-oklas,
         mlmaa TYPE mbew-mlmaa,
         mlast TYPE mbew-mlast,
         stprs TYPE mbew-stprs,
         vprsv TYPE mbew-vprsv,
         ncost TYPE marc-ncost,
         ekalr TYPE mbew-ekalr,
         hkmat TYPE mbew-hkmat,
         awsls TYPE marc-awsls,
         sobsk TYPE marc-sobsk,
         losgr TYPE marc-losgr,
       END OF gts_material_plant_ext.

*&---------------------------------------------------------------------*
*& Types Declaration cho Sales Price
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_sales_price,
         kschl    TYPE a501-kschl,
         vkorg    TYPE a501-vkorg,
         kunnr    TYPE a501-kunnr,
         pltyp    TYPE a501-pltyp,
         matnr    TYPE a501-matnr,
         datab    TYPE a501-datab,
         kbetr    TYPE konp-kbetr,
         konwa    TYPE konp-konwa,
         loevm_ko TYPE konp-loevm_ko,
       END OF gts_sales_price.

*&---------------------------------------------------------------------*
*& Types Declaration cho Customer Master
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_customer,
         kunnr      TYPE kna1-kunnr,
         bu_group   TYPE but000-bu_group,
         rltgr      TYPE tb003i-rltgr,
         name1      TYPE adrc-name1,
         name2      TYPE adrc-name2,
         name3      TYPE adrc-name3,
         sort1      TYPE adrc-sort1,
         sort2      TYPE adrc-sort2,
         street     TYPE adrc-street,
         city1      TYPE adrc-city1,
         post_code1 TYPE adrc-post_code1,
         tel_number TYPE adrc-tel_number,
         fax_number TYPE adrc-fax_number,
         country    TYPE adrc-country,
         region     TYPE adrc-region,
         remark     TYPE adrct-remark,
         lifnr      TYPE kna1-lifnr,
         katr1      TYPE kna1-katr1,
       END OF gts_customer.

*&---------------------------------------------------------------------*
*& Types Declaration cho Customer Material
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_customer_material,
         kunnr TYPE knmt-kunnr,
         vkorg TYPE knmt-vkorg,
         matnr TYPE knmt-matnr,
         kdmat TYPE knmt-kdmat,
         postx TYPE knmt-postx,
         sortl TYPE knmt-sortl,
         werks TYPE knmt-werks,
         kztlf TYPE knmt-kztlf,
         antlf TYPE knmt-antlf,
       END OF gts_customer_material.

*&---------------------------------------------------------------------*
*& Types Declaration cho Purchasing Price
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_purchasing_price,
         lifnr     TYPE a605-lifnr,
         matnr_a17 TYPE a017-matnr,
         matnr_a60 TYPE a605-matnr,
         ekorg_a17 TYPE a017-ekorg,
         ekorg_a60 TYPE a605-ekorg,
         werks_a17 TYPE a017-werks,
         werks_a60 TYPE a605-werks,
         esokz_a17 TYPE a017-esokz,
         esokz_a60 TYPE a605-esokz,
         bsgru     TYPE a605-bsgru,
         kbetr     TYPE konp-kbetr,
         waers     TYPE konp-waers,
         datab_a17 TYPE a017-datab,
         datab_a60 TYPE a605-datab,
         loevm_ko  TYPE konp-loevm_ko,
       END OF gts_purchasing_price.

*&---------------------------------------------------------------------*
*& Types Declaration cho Purchasing Info
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_purchasing_info,
         lifnr TYPE eina-lifnr,
         matnr TYPE eina-matnr,
         ekorg TYPE eine-ekorg,
         werks TYPE eine-werks,
         idnlf TYPE eina-idnlf,
         telf1 TYPE eina-telf1,
         kolif TYPE eina-kolif,
         urztp TYPE eina-urztp,
         urzla TYPE eina-urzla,
         urzzt TYPE eina-urzzt,
         aplfz TYPE eine-aplfz,
         norbm TYPE eine-norbm,
         uebtk TYPE eine-uebtk,
         mwskz TYPE eine-mwskz,
         netpr TYPE eine-netpr,
         peinh TYPE eine-peinh,
         loekz_eina TYPE eina-loekz,
         loekz_eine TYPE eine-loekz,
       END OF gts_purchasing_info.

*&---------------------------------------------------------------------*
*& Types Declaration cho Source Supply
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_source_supply,
         matnr TYPE eord-matnr,
         werks TYPE eord-werks,
         vdatu TYPE eord-vdatu,
         ekorg TYPE eord-ekorg,
       END OF gts_source_supply.

*&---------------------------------------------------------------------*
*& Types Declaration cho Vendor Master
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_vendor,
         bu_group    TYPE but000-bu_group,
         rltgr       TYPE tb003i-rltgr,
         lifnr       TYPE lfa1-lifnr,
         title       TYPE adrc-title,
         name1       TYPE adrc-name1,
         sort1       TYPE adrc-sort1,
         sort2       TYPE adrc-sort2,
         street      TYPE adrc-street,
         city1       TYPE adrc-city1,
         post_code1  TYPE adrc-post_code1,
         tel_number  TYPE adrc-tel_number,
         fax_number  TYPE adrc-fax_number,
         country     TYPE adrc-country,
         region      TYPE adrc-region,
         name_co     TYPE adrc-name_co,
         banks       TYPE but0bk-banks,
         bankl       TYPE but0bk-bankl,
         bankn       TYPE but0bk-bankn,
         bkont       TYPE but0bk-bkont,
         koinh       TYPE but0bk-koinh,
         kunnr       TYPE lfa1-kunnr,
         dtaws       TYPE lfa1-dtaws,
         taxtype     TYPE dfkkbptaxnum-taxtype,
         taxnumxl    TYPE dfkkbptaxnum-taxnumxl,
       END OF gts_vendor.

*&---------------------------------------------------------------------*
*& Types Declaration cho Production Version
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_production_version,
         matnr TYPE mkal-matnr,
         werks TYPE mkal-werks,
         verid TYPE mkal-verid,
         text1 TYPE mkal-text1,
         mksp  TYPE mkal-mksp,
         adatu TYPE mkal-adatu,
         bstmi TYPE mkal-bstmi,
         bstma TYPE mkal-bstma,
         stlal TYPE mkal-stlal,
         plnnr TYPE mkal-plnnr,
         alnal TYPE mkal-alnal,
       END OF gts_production_version.

*&---------------------------------------------------------------------*
*& Types Declaration cho Routing Master
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_routing,
         matnr TYPE mapl-matnr,
         werks TYPE mapl-werks,
         plnnr TYPE mapl-plnnr,
         datuv TYPE mapl-datuv,
         plnal TYPE mapl-plnal,
       END OF gts_routing.

*&---------------------------------------------------------------------*
*& Types Declaration cho Work Center
*&---------------------------------------------------------------------*
TYPES: BEGIN OF gts_work_center,
         werks  TYPE crhd-werks,
         arbpl  TYPE crhd-arbpl,
         stext  TYPE crtx-stext,
         rgekz  TYPE crhd-rgekz,
         kostl  TYPE crco-kostl,
       END OF gts_work_center.

*&---------------------------------------------------------------------*
*& Global Variables
*&---------------------------------------------------------------------*
* Messages
DATA: gv_msg TYPE string.

* Global Internal Tables
DATA: gdt_material_plant TYPE TABLE OF gts_marc_plant,
      gdt_material      TYPE TABLE OF mara.

* Global Structures  
DATA: gds_material_plant TYPE gts_marc_plant,
      gds_material      TYPE mara.

DATA: gdt_bom_items TYPE TABLE OF gts_bom_item.

*&---------------------------------------------------------------------*
*& Global Variables cho Material Master
*&---------------------------------------------------------------------*
DATA: gdt_material_master TYPE TABLE OF gts_material_master.

DATA: gdt_material_plant_ext   TYPE TABLE OF gts_material_plant_ext,
      gdt_sales_price         TYPE TABLE OF gts_sales_price,
      gdt_customer            TYPE TABLE OF gts_customer,
      gdt_customer_material   TYPE TABLE OF gts_customer_material,
      gdt_purchasing_price    TYPE TABLE OF gts_purchasing_price,
      gdt_purchasing_info     TYPE TABLE OF gts_purchasing_info,
      gdt_source_supply      TYPE TABLE OF gts_source_supply,
      gdt_vendor             TYPE TABLE OF gts_vendor,
      gdt_production_version TYPE TABLE OF gts_production_version,
      gdt_routing            TYPE TABLE OF gts_routing,
      gdt_work_center        TYPE TABLE OF gts_work_center.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_matnr TYPE matnr OBLIGATORY,
             p_werks TYPE werks_d OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN Event
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_input.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION Event
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*& Form VALIDATE_INPUT
*&---------------------------------------------------------------------*
FORM validate_input.
  " Check Material exists in MARA
  SELECT SINGLE matnr 
    FROM mara 
    INTO @DATA(lv_matnr)
    WHERE matnr = @p_matnr.
  IF sy-subrc <> 0.
    MESSAGE e000(00) WITH 'Material' p_matnr 'không tồn tại'.
  ENDIF.

  " Check Plant exists in T001W
  SELECT SINGLE werks 
    FROM t001w 
    INTO @DATA(lv_werks)
    WHERE werks = @p_werks.
  IF sy-subrc <> 0.
    MESSAGE e000(00) WITH 'Plant' p_werks 'không tồn tại'.
  ENDIF.

  " Check Material-Plant exists in MARC
  SELECT SINGLE matnr 
    FROM marc 
    INTO @DATA(lv_matnr_plant)
    WHERE matnr = @p_matnr
      AND werks = @p_werks.
  IF sy-subrc <> 0.
    MESSAGE e000(00) WITH 'Material-Plant' p_matnr p_werks 'không tồn tại'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
FORM main.

  " Step 1: Get Material Plant Data
  PERFORM get_material_plant.
  
  " Step 2: Get BOM Master using CS_BOM_EXPL_MAT_V2
  PERFORM get_bom_master.
  
  " Step 3: Get Material Master
  PERFORM get_material_master.
  
  " Step 4: Get Material Plant from Material Master
  PERFORM get_material_plant_extend.
  
  " Step 5: Get Sales Price
  PERFORM get_sales_price.
  
  " Step 6: Get Customer Master
  PERFORM get_customer_master.
  
  " Step 7: Get Customer Material
  PERFORM get_customer_material.
  
  " Step 8: Get Purchasing Price
  PERFORM get_purchasing_price.
  
  " Step 9: Get Purchasing Info
  PERFORM get_purchasing_info.
  
  " Step 10: Get Source of Supply
  PERFORM get_source_supply.
  
  " Step 11: Get Vendor Master
  PERFORM get_vendor_master.
  
  " Step 12: Get Production Version
  PERFORM get_production_version.
  
  " Step 13: Get Routing Master
  PERFORM get_routing_master.
  
  " Step 14: Get Work Center
  PERFORM get_work_center.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_PLANT
*&---------------------------------------------------------------------*
FORM get_material_plant.
  " Clear global table
  CLEAR: gdt_material_plant.

  " Get data from MARC
  SELECT matnr werks ladgr ekgrp kautb insmk kordb dismm
         fxhor dispo disgr disls bstrf beskz sobsl rgekz
         lgfsb lgpro dzeit plifz fhori eisbe perkz strgr
         miskz sbdkz fevor matgr prctr ncost awsls sobsk losgr
    FROM marc
    INTO TABLE @gdt_material_plant
    WHERE matnr = @p_matnr
      AND werks = @p_werks.
  
  IF sy-subrc <> 0.
    MESSAGE e000(00) WITH 'Không tìm thấy dữ liệu Material Plant'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_BOM_MASTER
*&---------------------------------------------------------------------*
FORM get_bom_master.
  " Local variables
  DATA: lds_material_plant TYPE gts_marc_plant,
        lds_bom_item      TYPE gts_bom_item,
        ldt_stb           TYPE TABLE OF stpox,
        lds_stb           TYPE stpox.

  " Clear global table
  CLEAR: gdt_bom_items.

  " Loop qua toàn bộ dữ liệu từ bước 1
  LOOP AT gdt_material_plant INTO lds_material_plant.
    " Clear temporary table
    CLEAR: ldt_stb.

    " Call BOM explosion function cho từng material-plant
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        matnr                = lds_material_plant-matnr
        werks               = lds_material_plant-werks
        stlan               = '1'    " BOM Usage
        stlal               = '01'   " Alternative BOM
        mtnrv               = lds_material_plant-matnr
        datuv               = sy-datum
        mktls               = 'X'    " Get Material Texts
        mehrs               = 'X'    " Multi-level BOM
        rndkz               = 'X'    " Rounding
        kapid               = 'PP01' " Application
      TABLES
        stb                 = ldt_stb
      EXCEPTIONS
        alt_not_found       = 1
        call_invalid        = 2
        material_not_found  = 3
        missing_authorization = 4
        no_bom_found        = 5
        no_plant_data       = 6
        no_suitable_bom_found = 7
        conversion_error    = 8
        OTHERS              = 9.

    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          MESSAGE w000(00) WITH 'Không tìm thấy BOM thay thế cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN 2.
          MESSAGE w000(00) WITH 'Lỗi khi gọi function cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN 3.
          MESSAGE w000(00) WITH 'Không tìm thấy vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN 4.
          MESSAGE w000(00) WITH 'Không có quyền truy cập cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN 5.
          MESSAGE w000(00) WITH 'Không tìm thấy BOM cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN 6.
          MESSAGE w000(00) WITH 'Không có dữ liệu nhà máy' lds_material_plant-werks.
        WHEN 7.
          MESSAGE w000(00) WITH 'Không tìm thấy BOM phù hợp cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN 8.
          MESSAGE w000(00) WITH 'Lỗi chuyển đổi đơn vị cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
        WHEN OTHERS.
          MESSAGE w000(00) WITH 'Lỗi không xác định cho vật tư' lds_material_plant-matnr 'nhà máy' lds_material_plant-werks.
      ENDCASE.
      CONTINUE. " Tiếp tục với material-plant tiếp theo
    ENDIF.

    " Convert STPOX to our internal table format
    LOOP AT ldt_stb INTO lds_stb.
      CLEAR lds_bom_item.
      MOVE-CORRESPONDING lds_stb TO lds_bom_item.
      " Thêm thông tin từ bước 1 vào kết quả
      lds_bom_item-matnr = lds_material_plant-matnr.  " Material chính
      lds_bom_item-werks = lds_material_plant-werks.  " Plant
      APPEND lds_bom_item TO gdt_bom_items.
    ENDLOOP.

  ENDLOOP.

  " Sort BOM items
  SORT gdt_bom_items BY matnr werks stlkn stpoz.

ENDFORM. 

*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_MASTER
*&---------------------------------------------------------------------*
FORM get_material_master.
  " Local variables
  DATA: lds_material_plant TYPE gts_marc_plant,
        lds_bom_item      TYPE gts_bom_item,
        lds_material      TYPE gts_material_master,
        ldt_matnr         TYPE TABLE OF matnr,
        lds_matnr         TYPE matnr.

  " Clear global table
  CLEAR: gdt_material_master.

  " Collect all material numbers from material plant
  LOOP AT gdt_material_plant INTO lds_material_plant.
    lds_matnr = lds_material_plant-matnr.
    COLLECT lds_matnr INTO ldt_matnr.
  ENDLOOP.

  " Collect all component materials from BOM
  LOOP AT gdt_bom_items INTO lds_bom_item.
    lds_matnr = lds_bom_item-idnrk.
    COLLECT lds_matnr INTO ldt_matnr.
  ENDLOOP.

  " If no materials found
  IF ldt_matnr IS INITIAL.
    MESSAGE w000(00) WITH 'No data - Không tìm thấy vật tư nào'.
    RETURN.
  ENDIF.

  " Get material master data
  SELECT mara~matnr,
         mara~mtart,
         makt~maktx,
         mara~meins,
         mara~matkl,
         mara~bismt,
         mara~extwg,
         mara~prdha,
         mara~mtpos,
         mara~groes,
         mara~ferth,
         mara~normt,
         mara~zeinr,
         mara~formt,
         mara~aeszn,
         mara~tragr,
         mara~raube
    FROM mara
    LEFT JOIN makt ON makt~matnr = mara~matnr
                  AND makt~spras = @sy-langu
    INTO TABLE @gdt_material_master
    FOR ALL ENTRIES IN @ldt_matnr
    WHERE mara~matnr = @ldt_matnr-table.

  " Check if data found
  IF sy-subrc <> 0.
    MESSAGE w000(00) WITH 'No data - Không tìm thấy thông tin vật tư'.
    RETURN.
  ENDIF.

  " Sort result
  SORT gdt_material_master BY matnr.

ENDFORM. 

*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_PLANT_EXTEND
*&---------------------------------------------------------------------*
FORM get_material_plant_extend.
  DATA: lds_material      TYPE gts_material_master,
        lds_plant_ext    TYPE gts_material_plant_ext.

  " Clear global table
  CLEAR: gdt_material_plant_ext.

  " Get material plant data for all materials
  IF NOT gdt_material_master IS INITIAL.
    SELECT marc~matnr marc~werks marc~ladgr marc~ekgrp marc~kautb
           marc~insmk marc~kordb marc~dismm marc~fxhor marc~dispo
           marc~disgr marc~disls marc~bstrf marc~beskz marc~sobsl
           marc~rgekz marc~lgfsb marc~lgpro marc~dzeit marc~plifz
           marc~fhori marc~eisbe marc~perkz marc~strgr marc~miskz
           marc~sbdkz marc~fevor marc~matgr marc~prctr marc~ncost
           marc~awsls marc~sobsk marc~losgr
           mbew~bklas mbew~oklas mbew~mlmaa mbew~mlast mbew~stprs
           mbew~vprsv mbew~ekalr mbew~hkmat
      FROM marc
      LEFT JOIN mbew ON mbew~matnr = marc~matnr
                    AND mbew~werks = marc~werks
      INTO TABLE @gdt_material_plant_ext
      FOR ALL ENTRIES IN @gdt_material_master
      WHERE marc~matnr = @gdt_material_master-matnr.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Material Plant'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_material_plant_ext BY matnr werks.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_SALES_PRICE
*&---------------------------------------------------------------------*
FORM get_sales_price.
  DATA: lds_material TYPE gts_material_master.

  " Clear global table
  CLEAR: gdt_sales_price.

  " Get sales price data for all materials
  IF NOT gdt_material_master IS INITIAL.
    SELECT a501~kschl a501~vkorg a501~kunnr a501~pltyp
           a501~matnr a501~datab konp~kbetr konp~konwa
           konp~loevm_ko
      FROM a501
      INNER JOIN konp ON konp~knumh = a501~knumh
      INTO TABLE @gdt_sales_price
      FOR ALL ENTRIES IN @gdt_material_master
      WHERE a501~matnr = @gdt_material_master-matnr
        AND a501~kappl = 'V'  " Sales
        AND konp~loevm_ko = ''.  " Not deleted

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Sales Price'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_sales_price BY matnr vkorg kunnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_CUSTOMER_MASTER
*&---------------------------------------------------------------------*
FORM get_customer_master.
  DATA: ldt_kunnr TYPE TABLE OF kunnr,
        lds_kunnr TYPE kunnr.

  " Clear global table
  CLEAR: gdt_customer.

  " Collect unique customer numbers from sales price
  LOOP AT gdt_sales_price INTO DATA(lds_sales_price).
    lds_kunnr = lds_sales_price-kunnr.
    COLLECT lds_kunnr INTO ldt_kunnr.
  ENDLOOP.

  " Get customer master data
  IF NOT ldt_kunnr IS INITIAL.
    SELECT kna1~kunnr but000~bu_group tb003i~rltgr
           adrc~name1 adrc~name2 adrc~name3 adrc~sort1
           adrc~sort2 adrc~street adrc~city1 adrc~post_code1
           adrc~tel_number adrc~fax_number adrc~country
           adrc~region adrct~remark kna1~lifnr kna1~katr1
      FROM kna1
      LEFT JOIN but000 ON but000~partner = kna1~kunnr
      LEFT JOIN tb003i ON tb003i~partner = kna1~kunnr
      LEFT JOIN adrc ON adrc~addrnumber = kna1~adrnr
      LEFT JOIN adrct ON adrct~addrnumber = kna1~adrnr
                    AND adrct~langu = @sy-langu
      INTO TABLE @gdt_customer
      FOR ALL ENTRIES IN @ldt_kunnr
      WHERE kna1~kunnr = @ldt_kunnr-table.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Customer Master'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_customer BY kunnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_CUSTOMER_MATERIAL
*&---------------------------------------------------------------------*
FORM get_customer_material.
  " Clear global table
  CLEAR: gdt_customer_material.

  " Get customer material data
  IF NOT gdt_material_master IS INITIAL AND NOT gdt_customer IS INITIAL.
    SELECT knmt~kunnr knmt~vkorg knmt~matnr knmt~kdmat
           knmt~postx knmt~sortl knmt~werks knmt~kztlf
           knmt~antlf
      FROM knmt
      INTO TABLE @gdt_customer_material
      FOR ALL ENTRIES IN @gdt_material_master
      WHERE knmt~matnr = @gdt_material_master-matnr
        AND EXISTS ( SELECT 1
                    FROM @gdt_customer AS cust
                    WHERE cust~kunnr = knmt~kunnr ).

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Customer Material'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_customer_material BY kunnr matnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_PURCHASING_PRICE
*&---------------------------------------------------------------------*
FORM get_purchasing_price.
  " Clear global table
  CLEAR: gdt_purchasing_price.

  " Get purchasing price data
  IF NOT gdt_material_master IS INITIAL.
    SELECT a605~lifnr a017~matnr AS matnr_a17 a605~matnr AS matnr_a60
           a017~ekorg AS ekorg_a17 a605~ekorg AS ekorg_a60
           a017~werks AS werks_a17 a605~werks AS werks_a60
           a017~esokz AS esokz_a17 a605~esokz AS esokz_a60
           a605~bsgru konp~kbetr konp~waers
           a017~datab AS datab_a17 a605~datab AS datab_a60
           konp~loevm_ko
      FROM a605
      LEFT JOIN a017 ON a017~knumh = a605~knumh
      INNER JOIN konp ON konp~knumh = a605~knumh
      INTO TABLE @gdt_purchasing_price
      FOR ALL ENTRIES IN @gdt_material_master
      WHERE ( a605~matnr = @gdt_material_master-matnr
          OR a017~matnr = @gdt_material_master-matnr )
        AND konp~loevm_ko = ''.  " Not deleted

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Purchasing Price'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_purchasing_price BY matnr_a17 matnr_a60 lifnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_PURCHASING_INFO
*&---------------------------------------------------------------------*
FORM get_purchasing_info.
  " Clear global table
  CLEAR: gdt_purchasing_info.

  " Get purchasing info data
  IF NOT gdt_material_master IS INITIAL.
    SELECT eina~lifnr eina~matnr eine~ekorg eine~werks
           eina~idnlf eina~telf1 eina~kolif eina~urztp
           eina~urzla eina~urzzt eine~aplfz eine~norbm
           eine~uebtk eine~mwskz eine~netpr eine~peinh
           eina~loekz AS loekz_eina eine~loekz AS loekz_eine
      FROM eina
      INNER JOIN eine ON eine~infnr = eina~infnr
      INTO TABLE @gdt_purchasing_info
      FOR ALL ENTRIES IN @gdt_material_master
      WHERE eina~matnr = @gdt_material_master-matnr
        AND eina~loekz = ''  " Not deleted
        AND eine~loekz = ''. " Not deleted

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Purchasing Info'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_purchasing_info BY matnr lifnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_SOURCE_SUPPLY
*&---------------------------------------------------------------------*
FORM get_source_supply.
  " Clear global table
  CLEAR: gdt_source_supply.

  " Get source of supply data
  IF NOT gdt_material_master IS INITIAL.
    SELECT matnr werks vdatu ekorg
      FROM eord
      INTO TABLE @gdt_source_supply
      FOR ALL ENTRIES IN @gdt_material_master
      WHERE matnr = @gdt_material_master-matnr.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Source of Supply'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_source_supply BY matnr werks.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_VENDOR_MASTER
*&---------------------------------------------------------------------*
FORM get_vendor_master.
  DATA: ldt_lifnr TYPE TABLE OF lifnr,
        lds_lifnr TYPE lifnr.

  " Clear global table
  CLEAR: gdt_vendor.

  " Collect unique vendor numbers from source supply and purchasing info
  LOOP AT gdt_purchasing_info INTO DATA(lds_pinfo).
    lds_lifnr = lds_pinfo-lifnr.
    COLLECT lds_lifnr INTO ldt_lifnr.
  ENDLOOP.

  " Get vendor master data
  IF NOT ldt_lifnr IS INITIAL.
    SELECT but000~bu_group tb003i~rltgr lfa1~lifnr
           adrc~title adrc~name1 adrc~sort1 adrc~sort2
           adrc~street adrc~city1 adrc~post_code1
           adrc~tel_number adrc~fax_number adrc~country
           adrc~region adrc~name_co but0bk~banks but0bk~bankl
           but0bk~bankn but0bk~bkont but0bk~koinh lfa1~kunnr
           lfa1~dtaws dfkkbptaxnum~taxtype AS taxtype
           dfkkbptaxnum~taxnumxl
      FROM lfa1
      LEFT JOIN but000 ON but000~partner = lfa1~lifnr
      LEFT JOIN tb003i ON tb003i~partner = lfa1~lifnr
      LEFT JOIN adrc ON adrc~addrnumber = lfa1~adrnr
      LEFT JOIN but0bk ON but0bk~partner = lfa1~lifnr
      LEFT JOIN dfkkbptaxnum ON dfkkbptaxnum~partner = lfa1~lifnr
      INTO TABLE @gdt_vendor
      FOR ALL ENTRIES IN @ldt_lifnr
      WHERE lfa1~lifnr = @ldt_lifnr-table.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Vendor Master'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_vendor BY lifnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_PRODUCTION_VERSION
*&---------------------------------------------------------------------*
FORM get_production_version.
  " Clear global table
  CLEAR: gdt_production_version.

  " Get production version data from BOM items
  IF NOT gdt_bom_items IS INITIAL.
    SELECT mkal~matnr mkal~werks mkal~verid mkal~text1
           mkal~mksp mkal~adatu mkal~bstmi mkal~bstma
           mkal~stlal mkal~plnnr mkal~alnal
      FROM mkal
      INTO TABLE @gdt_production_version
      FOR ALL ENTRIES IN @gdt_bom_items
      WHERE mkal~matnr = @gdt_bom_items-matnr
        AND mkal~werks = @gdt_bom_items-werks.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Production Version'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_production_version BY matnr werks verid.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_ROUTING_MASTER
*&---------------------------------------------------------------------*
FORM get_routing_master.
  " Clear global table
  CLEAR: gdt_routing.

  " Get routing master data from production version
  IF NOT gdt_production_version IS INITIAL.
    SELECT matnr werks plnnr datuv plnal
      FROM mapl
      INTO TABLE @gdt_routing
      FOR ALL ENTRIES IN @gdt_production_version
      WHERE matnr = @gdt_production_version-matnr
        AND werks = @gdt_production_version-werks
        AND plnnr = @gdt_production_version-plnnr.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Routing Master'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_routing BY matnr werks plnnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_WORK_CENTER
*&---------------------------------------------------------------------*
FORM get_work_center.
  DATA: ldt_arbpl TYPE TABLE OF arbpl,
        lds_arbpl TYPE arbpl.

  " Clear global table
  CLEAR: gdt_work_center.

  " Get work center data from routing
  IF NOT gdt_routing IS INITIAL.
    SELECT crhd~werks crhd~arbpl crtx~stext
           crhd~rgekz crco~kostl
      FROM crhd
      LEFT JOIN crtx ON crtx~objid = crhd~objid
                    AND crtx~spras = @sy-langu
      LEFT JOIN crco ON crco~objid = crhd~objid
      INTO TABLE @gdt_work_center
      FOR ALL ENTRIES IN @gdt_routing
      WHERE crhd~werks = @gdt_routing-werks.

    IF sy-subrc <> 0.
      MESSAGE w000(00) WITH 'No data - Không tìm thấy dữ liệu Work Center'.
    ENDIF.
  ENDIF.

  " Sort result
  SORT gdt_work_center BY werks arbpl.
ENDFORM. 