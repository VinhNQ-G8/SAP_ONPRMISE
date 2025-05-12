*&---------------------------------------------------------------------*
*& Report  : ZR_MATERIAL_LIST
*& Title   : Material Master List Report
*& Author  : [Author Name]
*& Date    : [Current Date]
*& Version : 1.0
*&---------------------------------------------------------------------*
*& Description:
*& - Display material master data in ALV grid format
*& - Allow users to filter materials by number, type and group
*& - Show key material information including description and base UoM
*& - Support sorting and filtering in ALV grid
*& - Export data to spreadsheet
*&---------------------------------------------------------------------*
*& Change History:
*& - [Current Date] [Author]: Initial version
*&   + Basic ALV grid implementation
*&   + Selection screen with filter options
*&   + Field catalog customization
*&---------------------------------------------------------------------*
REPORT zr_material_list.

*----------------------------------------------------------------------*
* Types Declaration
*----------------------------------------------------------------------*
" Global types for material data handling
" - Used in selection screen and ALV display
" - Matches standard SAP table structure
TYPES: 
  " Structure for material master data
  " - Contains key fields from MARA, MAKT
  " - Optimized for ALV grid output
  BEGIN OF gts_material,
    matnr TYPE mara-matnr,  " Material Number - Primary key
    maktx TYPE makt-maktx,  " Material Description - From MAKT
    mtart TYPE mara-mtart,  " Material Type - From MARA
    matkl TYPE mara-matkl,  " Material Group - From MARA
    meins TYPE mara-meins,  " Base Unit of Measure - From MARA
    ersda TYPE mara-ersda,  " Created On - From MARA
    ernam TYPE mara-ernam,  " Created By - From MARA
    laeda TYPE mara-laeda,  " Last Changed On - From MARA
    aenam TYPE mara-aenam,  " Last Changed By - From MARA
  END OF gts_material,
  " Internal table type for material data
  " - Standard table for better performance
  " - Used for database selection results
  gtt_material TYPE TABLE OF gts_material.

*----------------------------------------------------------------------*
* Global Data Declaration
*----------------------------------------------------------------------*
" Global variables for data handling and display
DATA: 
  " Internal table for material data
  " - Holds database query results
  " - Used as data source for ALV grid
  gdt_material    TYPE TABLE OF gts_material,

  " Work area for material data
  " - Used for data manipulation
  " - Referenced in selection screen
  gds_material    TYPE gts_material,

  " ALV grid related objects
  " - Field catalog for column definition
  " - Layout settings for display format
  gdf_fieldcat    TYPE slis_t_fieldcat_alv,
  gds_layout      TYPE slis_layout_alv,

  " Constants
  " - Fixed values used in program
  " - Centralized for easy maintenance
  gcf_title       TYPE string VALUE 'Material Master List'.

*----------------------------------------------------------------------*
* Selection Screen Definition
*----------------------------------------------------------------------*
" Selection screen for filter criteria
" - Allows users to restrict data selection
" - Provides multiple filter options
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: 
  " Material number selection
  " - Required field
  " - Supports pattern matching
  s_matnr FOR gds_material-matnr OBLIGATORY,  

  " Material type selection
  " - Optional filter
  " - Helps categorize materials
  s_mtart FOR gds_material-mtart,             

  " Material group selection
  " - Optional filter
  " - Supports material grouping
  s_matkl FOR gds_material-matkl.             
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
" Program initialization
" - Called before selection screen display
" - Sets up initial program state
INITIALIZATION.
  PERFORM initialize_data.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
" Main processing block
" - Triggered after user executes report
" - Controls main program flow
START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*& Form INITIALIZE_DATA
*&---------------------------------------------------------------------*
*& Description:
*& - Initialize all global data objects
*& - Set up ALV grid layout parameters
*& - Prepare program for execution
*&---------------------------------------------------------------------*
*& Parameters: None
*&---------------------------------------------------------------------*
FORM initialize_data.
  " Clear all global data
  " - Ensures clean program state
  " - Prevents data mixing between runs
  CLEAR: gdt_material,
         gds_material,
         gdf_fieldcat,
         gds_layout.

  " Set up ALV grid layout
  " - Define display characteristics
  " - Optimize for readability
  gds_layout-zebra             = abap_true.  " Alternate row colors
  gds_layout-colwidth_optimize = abap_true.  " Auto-adjust columns
  gds_layout-window_titlebar   = gcf_title.  " Set window title
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_DATA
*&---------------------------------------------------------------------*
*& Description:
*& - Retrieve material data from SAP standard tables
*& - Join MARA (General Data) with MAKT (Descriptions)
*& - Apply user-specified filters
*& - Sort data for display
*&---------------------------------------------------------------------*
*& Parameters: None
*&---------------------------------------------------------------------*
FORM get_material_data.
  " Clear existing data
  " - Prepare for new selection
  " - Prevent data accumulation
  CLEAR gdt_material.

  " Select data from SAP standard tables
  " - MARA: Material General Data
  " - MAKT: Material Descriptions
  " - Join on Material Number and Language
  SELECT mara~matnr,
         makt~maktx,
         mara~mtart,
         mara~matkl,
         mara~meins,
         mara~ersda,
         mara~ernam,
         mara~laeda,
         mara~aenam
    FROM mara AS mara
    LEFT OUTER JOIN makt AS makt
      ON  makt~matnr = mara~matnr
      AND makt~spras = @sy-langu
    INTO TABLE @gdt_material
    WHERE mara~matnr IN @s_matnr
      AND mara~mtart IN @s_mtart
      AND mara~matkl IN @s_matkl.

  " Check for data
  " - Handle empty result set
  " - Provide user feedback
  IF sy-subrc <> 0.
    " No data found - display message and exit
    MESSAGE s398(00) WITH 'No data found for selection criteria'
                          space space space
                          DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Sort data for display
  " - Improve readability
  " - Prepare for ALV display
  SORT gdt_material BY matnr.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
*& Description:
*& - Set up and display ALV grid
*& - Handle ALV grid related functions
*& - Process user interactions
*&---------------------------------------------------------------------*
*& Parameters: None
*&---------------------------------------------------------------------*
FORM display_alv.
  " Build field catalog for ALV grid
  " - Define column properties
  " - Set up display format
  PERFORM build_fieldcat.

  " Display ALV grid
  " - Show data to user
  " - Enable interactive features
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid           " Current program
      is_layout         = gds_layout          " Layout settings
      it_fieldcat       = gdf_fieldcat        " Field catalog
    TABLES
      t_outtab          = gdt_material        " Data to display
    EXCEPTIONS
      program_error     = 1                   " Handle errors
      OTHERS           = 2.

  " Handle errors
  " - Provide user feedback
  " - Ensure graceful error handling
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& Description:
*& - Build and customize field catalog for ALV grid
*& - Define column properties and texts
*& - Set up display format for each field
*&---------------------------------------------------------------------*
*& Parameters: None
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  " Get base field catalog from structure
  " - Use standard function module
  " - Get technical field properties
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid           " Current program
      i_structure_name   = 'GTS_MATERIAL'     " Structure name
      i_client_never_display = abap_true      " Hide client field
    CHANGING
      ct_fieldcat        = gdf_fieldcat       " Field catalog
    EXCEPTIONS
      inconsistent_interface = 1              " Handle errors
      program_error         = 2
      OTHERS               = 3.

  " Handle errors in field catalog creation
  " - Ensure proper error messages
  " - Prevent program termination
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  " Customize field catalog
  " - Set user-friendly column headers
  " - Define technical properties
  LOOP AT gdf_fieldcat ASSIGNING FIELD-SYMBOL(<lfs_fcat>).
    CASE <lfs_fcat>-fieldname.
      WHEN 'MATNR'.  " Material Number
        <lfs_fcat>-seltext_l = 'Material Number'.
        <lfs_fcat>-key      = abap_true.     " Mark as key field
        <lfs_fcat>-hotspot  = abap_true.     " Enable as hotspot
      WHEN 'MAKTX'.  " Material Description
        <lfs_fcat>-seltext_l = 'Material Description'.
        <lfs_fcat>-outputlen = 40.           " Set column width
      WHEN 'MTART'.  " Material Type
        <lfs_fcat>-seltext_l = 'Material Type'.
        <lfs_fcat>-emphasize = 'C310'.       " Highlight column
      WHEN 'MATKL'.  " Material Group
        <lfs_fcat>-seltext_l = 'Material Group'.
        <lfs_fcat>-emphasize = 'C310'.       " Highlight column
      WHEN 'MEINS'.  " Base Unit of Measure
        <lfs_fcat>-seltext_l = 'Base UoM'.
        <lfs_fcat>-no_zero = abap_true.      " Hide zero values
      WHEN 'ERSDA'.  " Created On
        <lfs_fcat>-seltext_l = 'Created On'.
        <lfs_fcat>-just = 'C'.               " Center align
      WHEN 'ERNAM'.  " Created By
        <lfs_fcat>-seltext_l = 'Created By'.
        <lfs_fcat>-emphasize = 'C500'.       " Highlight column
      WHEN 'LAEDA'.  " Last Changed On
        <lfs_fcat>-seltext_l = 'Last Changed On'.
        <lfs_fcat>-just = 'C'.               " Center align
      WHEN 'AENAM'.  " Last Changed By
        <lfs_fcat>-seltext_l = 'Last Changed By'.
        <lfs_fcat>-emphasize = 'C500'.       " Highlight column
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& Description:
*& - Main processing form
*& - Controls program execution flow
*& - Orchestrates data retrieval and display
*&---------------------------------------------------------------------*
*& Parameters: None
*&---------------------------------------------------------------------*
FORM main.
  " Get material data based on selection criteria
  PERFORM get_material_data.
  
  " Display data in ALV grid
  PERFORM display_alv.
ENDFORM. 